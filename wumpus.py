from Settings import *
from entity import Entity


class WumpusAIState:
    """AI state constants for Wumpus"""
    ROAMING = "roaming"          # Exploring map randomly
    INVESTIGATING = "investigating"  # Moving to sound source
    CHASING = "chasing"          # Actively pursuing detected sound
    SEARCHING = "searching"      # Lost player, searching area
    STUNNED = "stunned"          # Hit by arrow
    DEAD = "dead"                # Defeated


class Wumpus(Entity):
    """Wumpus enemy - Sound-based AI hunter with complete map knowledge"""

    def __init__(self, pos, groups, collision_sprites, prolog_engine=None, map_knowledge=None, sound_manager=None):
        # Initialize base Entity
        super().__init__(
            pos, groups, collision_sprites, prolog_engine, entity_type="wumpus"
        )

        # Core systems
        self.map_knowledge = map_knowledge  # Complete map awareness
        self.sound_manager = sound_manager  # Sound detection system

        # Wumpus-specific attributes
        self.speed = 250  # เดินเร็วกว่า player walk (200), ช้ากว่า player dash (400)
        self.chase_speed = 450  # Chase เร็วกว่า player dash!
        self.damage = 25  # Damage dealt to player
        self.max_health = 150  # More HP than player
        self.health = self.max_health

        # Combat ranges (REMOVED vision detection)
        self.attack_range = 50  # Pixels within which Wumpus can attack

        # Hearing system (replaces vision)
        self.hearing_radius = WUMPUS_BASE_HEARING  # Base hearing (350px)
        self.chase_hearing_bonus = WUMPUS_CHASE_BONUS  # Bonus when chasing (+200px)
        self.current_hearing_radius = self.hearing_radius

        # Stun mechanics (arrow combat)
        self.is_stunned = False
        self.stun_timer = 0
        self.stun_duration = ARROW_STUN_DURATION  # From Settings.py

        # Sound-based AI state
        self.ai_state = WumpusAIState.ROAMING
        self.target_position = None  # Where Wumpus is heading
        self.last_heard_sound = None  # Last detected sound
        self.roaming_target = None  # Current roaming destination
        self.search_timer = 0  # Time left searching

        # Roar system
        self.is_roaring = False
        self.roar_cooldown = WUMPUS_ROAR_COOLDOWN  # 5000ms
        self.last_roar_time = 0

        # Load Wumpus animations
        self.animations = self.load_animations()

        print(f"[Wumpus] Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")

        # Setup sprite and hitbox
        self.current_animation = "idle"
        initial_image = self.animations[self.current_animation][0]
        self.setup_sprite(initial_image, hitbox_inflate=(-80, -80))

        print(f"[Wumpus] Initialized at {pos}, HP: {self.health}/{self.max_health}")
        print(f"[Wumpus] Hearing: {self.hearing_radius}px (base), +{self.chase_hearing_bonus}px (chase)")
        print(f"[Wumpus] AI: Sound-based detection, map knowledge enabled")
    
    @property
    def pos(self):
        """Get current position as Vector2 (for compatibility with sound-based AI)"""
        return pygame.math.Vector2(self.hitbox_rect.center)

    def load_animations(self):
        """Load Wumpus animation frames from sprite sheet"""
        animations = {}

        try:
            # Load the sprite sheet
            sprite_sheet = pygame.image.load(
                join("assets", "Wumpus", "Shardsoul Slayer Sprite Sheet.png")
            ).convert_alpha()
            sheet_width = sprite_sheet.get_width()
            sheet_height = sprite_sheet.get_height()

            print(f"[Wumpus] Sprite sheet size: {sheet_width}x{sheet_height}")

            # The sprite sheet has 8 columns and appears to have taller sprites
            # Let's try to extract the ENTIRE sprite for each frame
            frames_per_row = 8

            # First, let's check if sprites are arranged in standard rows
            # Common sizes: 32x32, 48x48, 64x64, 64x96, 96x96
            # Looking at the sheet, let's try larger frame sizes

            # Try to determine frame size by examining the sheet
            # Assume 8 frames per row, and calculate height per row
            frame_width = sheet_width // frames_per_row

            # Count total animations to determine frame height
            # Looking at the sprite sheet: idle (8), walk (8), attack (5), death (6) = 27 frames
            # That's about 4 rows, so let's try dividing by 4-6
            estimated_rows = 4
            frame_height = (
                sheet_height // estimated_rows
            ) - 5  # Subtract 2 pixels to avoid overlap

            print(f"[Wumpus] Calculated frame size: {frame_width}x{frame_height}")
            print(f"[Wumpus] If sprites look cut off, frame_height may need adjustment")

            # Scale factor for display
            scale_factor = 2

            # Idle animation from first row (frames 0-7)
            idle_frames = []
            for i in range(frames_per_row):
                frame_rect = pygame.Rect(i * frame_width, 0, frame_width, frame_height)
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                idle_frames.append(scaled_frame)
            animations["idle"] = (
                idle_frames if idle_frames else [self.create_placeholder()]
            )

            # Walk animation from second row (frames 8-15)
            walk_frames = []
            for i in range(frames_per_row):
                frame_rect = pygame.Rect(
                    i * frame_width, frame_height, frame_width, frame_height
                )
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                walk_frames.append(scaled_frame)
            animations["walk"] = walk_frames if walk_frames else animations["idle"]

            # Attack animation from third row (frames 16-20, only 5 frames)
            attack_frames = []
            for i in range(5):  # Only 5 attack frames
                frame_rect = pygame.Rect(
                    i * frame_width, frame_height * 2, frame_width, frame_height
                )
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                attack_frames.append(scaled_frame)
            animations["attack"] = (
                attack_frames if attack_frames else animations["idle"]
            )

            # Death animation - 6 frames starting from row 4
            death_frames = []
            # These 6 frames should be in one row or split across rows
            for i in range(6):
                # Calculate which row and column this frame is in
                row = 3 + (i // frames_per_row)
                col = i % frames_per_row
                frame_rect = pygame.Rect(
                    col * frame_width, row * frame_height, frame_width, frame_height
                )
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                death_frames.append(scaled_frame)
            animations["death"] = death_frames if death_frames else animations["idle"]

        except Exception as e:
            print(f"[Wumpus] Warning: Could not load sprite sheet: {e}")
            print(f"[Wumpus] Error details: {type(e).__name__}")
            import traceback

            traceback.print_exc()
            animations["idle"] = [self.create_placeholder()]
            animations["walk"] = animations["idle"]
            animations["attack"] = animations["idle"]
            animations["death"] = animations["idle"]

        return animations

    def create_placeholder(self):
        """Create a placeholder sprite for Wumpus"""
        placeholder = pygame.Surface((128, 128), pygame.SRCALPHA)
        # Red circle for Wumpus
        pygame.draw.circle(placeholder, (200, 50, 50), (64, 64), 50)
        # Eyes
        pygame.draw.circle(placeholder, (255, 255, 0), (50, 50), 8)
        pygame.draw.circle(placeholder, (255, 255, 0), (78, 50), 8)
        # Pupils
        pygame.draw.circle(placeholder, (0, 0, 0), (50, 50), 4)
        pygame.draw.circle(placeholder, (0, 0, 0), (78, 50), 4)
        return placeholder

    def ai_update(self, player_pos, dt):
        """
        Sound-based AI update - Wumpus reacts to sounds, not direct vision
        
        Args:
            player_pos: Player position (for attack range check only, NOT detection)
            dt: Delta time
        """
        if not self.is_alive or self.is_stunned:
            return

        # Listen for sounds
        if self.sound_manager:
            loudest_sound, loudness = self.sound_manager.get_loudest_sound(
                self.pos,
                min_threshold=10.0
            )
            
            # React to detected sound
            if loudest_sound and loudness > 0:
                self._handle_sound_detected(loudest_sound, loudness)
            else:
                self._handle_no_sound()
        
        # Execute behavior based on current state
        if self.ai_state == WumpusAIState.ROAMING:
            self._behavior_roaming()
        elif self.ai_state == WumpusAIState.INVESTIGATING:
            self._behavior_investigating()
        elif self.ai_state == WumpusAIState.CHASING:
            self._behavior_chasing()
        elif self.ai_state == WumpusAIState.SEARCHING:
            self._behavior_searching(dt)
        elif self.ai_state == WumpusAIState.STUNNED:
            # Just stay still
            self.direction = pygame.math.Vector2(0, 0)
        
        # Check if in attack range (proximity-based, not sound)
        distance_to_player = self.pos.distance_to(pygame.math.Vector2(player_pos))
        if distance_to_player <= self.attack_range and self.ai_state == WumpusAIState.CHASING:
            # Player is very close - attack!
            self.ai_state = "attack"
            self.direction = pygame.math.Vector2(0, 0)
    
    def _handle_sound_detected(self, sound, loudness):
        """React to detected sound"""
        if sound.source_type == 'rock_impact':
            # LOUD distraction - investigate immediately!
            print(f"[Wumpus] Heard rock impact (loudness: {loudness:.1f})! Investigating...")
            self.ai_state = WumpusAIState.INVESTIGATING
            self.target_position = sound.position.copy()
            self.last_heard_sound = sound
            
        elif sound.source_type in ['walk', 'dash', 'arrow_shot']:
            # Player sounds detected
            if loudness > 30:  # Loud enough to chase
                print(f"[Wumpus] Heard player (loudness: {loudness:.1f})! Chasing...")
                self.ai_state = WumpusAIState.CHASING
                self.target_position = sound.position.copy()
                self._trigger_roar()
            else:
                # Faint sound - just investigate
                self.ai_state = WumpusAIState.INVESTIGATING
                self.target_position = sound.position.copy()
    
    def _handle_no_sound(self):
        """No sound detected - continue current behavior"""
        # If chasing but lost sound, switch to searching
        if self.ai_state == WumpusAIState.CHASING:
            self.ai_state = WumpusAIState.SEARCHING
            self.search_timer = 8.0  # Search for 8 seconds
            print("[Wumpus] Lost sound... searching area")
    
    def _behavior_roaming(self):
        """Wander the map randomly using map knowledge"""
        if self.roaming_target is None or self._reached_target():
            # Pick new random safe position
            if self.map_knowledge:
                new_target = self.map_knowledge.get_safe_random_position()
                if new_target:
                    self.roaming_target = new_target
                    self.target_position = pygame.math.Vector2(new_target)
        
        # Move towards target with pit avoidance
        if self.target_position:
            direction = (self.target_position - self.pos)
            if direction.length() > 0:
                self.direction = direction.normalize()
    
    def _behavior_investigating(self):
        """Move to sound source location"""
        if self._reached_target():
            # Reached source, nothing here - start searching
            self.ai_state = WumpusAIState.SEARCHING
            self.search_timer = 5.0
            print("[Wumpus] Reached sound location, nothing here... searching")
        else:
            # Move towards sound
            direction = (self.target_position - self.pos)
            if direction.length() > 0:
                self.direction = direction.normalize()
    
    def _behavior_chasing(self):
        """Chase last heard player sound"""
        if self._reached_target():
            # Lost player at last known position
            self.ai_state = WumpusAIState.SEARCHING
            self.search_timer = 8.0
            self.is_roaring = False
            self.current_hearing_radius = self.hearing_radius  # Reset hearing
            print("[Wumpus] Lost player... searching")
        else:
            # Continue to last heard position
            direction = (self.target_position - self.pos)
            if direction.length() > 0:
                self.direction = direction.normalize()
    
    def _behavior_searching(self, dt):
        """Search around last known position"""
        self.search_timer -= dt
        
        if self.search_timer <= 0:
            # Give up, return to roaming
            self.ai_state = WumpusAIState.ROAMING
            self.current_hearing_radius = self.hearing_radius
            self.is_roaring = False
            print("[Wumpus] Search timeout... resuming roaming")
        else:
            # Circle/wander around search area (simplified)
            # Could implement spiral pattern here
            if not hasattr(self, 'search_wander_timer'):
                self.search_wander_timer = 0
            
            self.search_wander_timer += dt
            if self.search_wander_timer > 2.0:  # Change direction every 2 seconds
                self.search_wander_timer = 0
                # Random direction
                import random
                angle = random.uniform(0, 360)
                self.direction = pygame.math.Vector2(1, 0).rotate(angle)
    
    def _trigger_roar(self):
        """Roar when starting chase - increases hearing radius"""
        current_time = pygame.time.get_ticks()
        if current_time - self.last_roar_time > self.roar_cooldown:
            self.is_roaring = True
            self.current_hearing_radius = self.hearing_radius + self.chase_hearing_bonus
            self.last_roar_time = current_time
            
            # Emit roar sound
            if self.sound_manager:
                self.sound_manager.play_sound("roar", volume=0.1)
                self.sound_manager.emit_sound(
                    self.pos,
                    SOUND_LEVELS['wumpus_roar'],
                    SOUND_DURATIONS['wumpus_roar'],
                    'wumpus_roar'
                )
            
            print(f"[Wumpus] ROAR! Hearing increased to {self.current_hearing_radius}px")
    
    def _reached_target(self, threshold=30):
        """Check if reached target position"""
        if self.target_position is None:
            return True
        return self.pos.distance_to(self.target_position) < threshold

    def apply_stun(self, duration=None):
        """
        Apply stun effect to Wumpus (from arrow hit).
        Wumpus freezes in place and cannot move or attack.
        """
        if duration is None:
            duration = self.stun_duration

        self.is_stunned = True
        self.stun_timer = duration
        self.ai_state = WumpusAIState.STUNNED
        self.direction = pygame.math.Vector2(0, 0)

        print(f"[Wumpus] Stunned for {duration} seconds!")

    def take_damage(self, amount):
        """Override to handle Wumpus death"""
        damage_taken = super().take_damage(amount)

        if self.health <= 0 and self.is_alive:
            self.on_death()

        return damage_taken

    def on_death(self):
        """Handle Wumpus death"""
        super().on_death()
        self.ai_state = WumpusAIState.DEAD
        self.direction = pygame.math.Vector2(0, 0)
        print("[Wumpus] Wumpus defeated!")

    def animate(self, dt):
        """Update animation frames"""
        # Determine animation state
        if self.ai_state == WumpusAIState.DEAD or not self.is_alive:
            self.current_animation = "death"
        elif self.ai_state == "attack":  # Attack still uses string for now
            self.current_animation = "attack"
        elif self.direction.magnitude() > 0:
            self.current_animation = "walk"
        else:
            self.current_animation = "idle"

        # Reset timer if animation changed
        if self.current_animation != self.previous_animation:
            self.animation_timer = 0
            self.previous_animation = self.current_animation

        # Get current animation frames
        animation_frames = self.animations.get(self.current_animation, [])

        if not animation_frames:
            return

        # Update animation timer
        self.animation_timer += dt

        # Calculate frame index
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)

        # Update image
        self.image = animation_frames[frame_index]

        # Flip sprite based on direction
        if self.direction.x < 0:
            self.image = pygame.transform.flip(self.image, True, False)

    def update(self, dt):
        """Update Wumpus every frame with pit avoidance"""
        # Update stun timer
        if self.is_stunned:
            self.stun_timer -= dt
            if self.stun_timer <= 0:
                self.is_stunned = False
                self.ai_state = WumpusAIState.ROAMING
                print("[Wumpus] Recovered from stun!")

        # Handle death state
        if not self.is_alive:
            self.animate(dt)
            return

        # Can't move while stunned
        if self.is_stunned:
            self.direction = pygame.math.Vector2(0, 0)

        # Move with pit avoidance (use chase speed when chasing)
        current_speed = self.chase_speed if self.ai_state == WumpusAIState.CHASING else self.speed
        self._move_with_pit_avoidance(dt, current_speed)
        self.animate(dt)
    
    def _move_with_pit_avoidance(self, dt, speed_override=None):
        """Move while avoiding pits using map knowledge"""
        if self.direction.length() == 0:
            return
        
        # Use speed override for chasing
        movement_speed = speed_override if speed_override is not None else self.speed
        
        # Check if current direction leads to danger
        if self.map_knowledge:
            # Look ahead
            look_ahead_distance = 50
            next_pos = self.pos + self.direction * look_ahead_distance
            
            # Check if near pit
            if self.map_knowledge.is_near_pit(next_pos.x, next_pos.y, danger_radius=50):
                # Find safe alternative direction
                safe_directions = self.map_knowledge.find_safe_alternative_directions(
                    self.pos,
                    self.direction
                )
                
                if safe_directions:
                    # Use first safe direction
                    self.direction = safe_directions[0]
                    print("[Wumpus] Avoiding pit, changing direction")
                else:
                    # No safe direction found - stop
                    self.direction = pygame.math.Vector2(0, 0)
                    # Switch to roaming to find new path
                    if self.ai_state != WumpusAIState.ROAMING:
                        self.ai_state = WumpusAIState.ROAMING
                        self.roaming_target = None
                    return
        
        # Normal movement with custom speed
        self.move(dt, speed_override=movement_speed)