from Settings import *
from entity import Entity

class Wumpus(Entity):
    """
    Wumpus enemy - AI-controlled entity that patrols and chases player
    """
    def __init__(self, pos, groups, collision_sprites, prolog_engine=None):
        # Initialize base Entity
        super().__init__(pos, groups, collision_sprites, prolog_engine, entity_type='wumpus')
        
        # Wumpus-specific attributes
        self.speed = 150  # Slower than player (200)
        self.damage = 25  # Damage dealt to player
        self.max_health = 150  # More HP than player
        self.health = self.max_health
        self.attack_range = 50  # Pixels within which Wumpus can attack
        self.detection_range = 300  # Pixels within which Wumpus detects player
        
        # Stun mechanics (arrow combat)
        self.is_stunned = False
        self.stun_timer = 0
        self.stun_duration = ARROW_STUN_DURATION  # From Settings.py
        
        # AI state
        self.ai_state = 'patrol'  # 'patrol', 'chase', 'attack', 'stunned', 'dead'
        self.patrol_points = []  # Will be set by map/AI
        self.current_patrol_index = 0
        self.target_position = None
        self.player_last_seen = None
        
        # Load Wumpus animations
        self.animations = self.load_animations()
        
        print(f"[Wumpus] Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")
        
        # Setup sprite and hitbox
        self.current_animation = 'idle'
        initial_image = self.animations[self.current_animation][0]
        self.setup_sprite(initial_image, hitbox_inflate=(-40, -30))
        
        # Initialize Wumpus in Prolog
        if self.prolog and getattr(self.prolog, 'available', False):
            self.prolog.init_wumpus(int(pos[0]), int(pos[1]))
        
        print(f"[Wumpus] Initialized at {pos}, HP: {self.health}/{self.max_health}")
        
    def load_animations(self):
        """Load Wumpus animation frames from sprite sheet"""
        animations = {}
        
        # Wumpus sprite sheet is a single file: "Shardsoul Slayer Sprite Sheet.png"
        # We need to determine the frame layout
        # For now, let's create placeholder animations
        
        try:
            # Try loading the sprite sheet to determine dimensions
            sprite_sheet = pygame.image.load(join('assets', 'Wumpus', 'Shardsoul Slayer Sprite Sheet.png')).convert_alpha()
            sheet_width = sprite_sheet.get_width()
            sheet_height = sprite_sheet.get_height()
            
            print(f"[Wumpus] Sprite sheet size: {sheet_width}x{sheet_height}")
            
            # Assuming standard sprite sheet layout (need to verify actual dimensions)
            # Common layouts: 4 rows (directions) x N columns (frames)
            # Let's assume 48x48 or 64x64 frames for now
            
            # We'll use simple extraction for now - adjust based on actual sprite sheet
            frame_width = 64  # Adjust if needed
            frame_height = 64  # Adjust if needed
            
            # For now, create basic idle animation from first row
            frames_per_row = sheet_width // frame_width
            
            idle_frames = []
            for i in range(min(4, frames_per_row)):  # Take up to 4 frames for idle
                frame_rect = pygame.Rect(i * frame_width, 0, frame_width, frame_height)
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(frame, (frame_width * 2, frame_height * 2))
                idle_frames.append(scaled_frame)
            
            animations['idle'] = idle_frames if idle_frames else [self.create_placeholder()]
            
            # Walk animation from second row (if exists)
            if sheet_height >= frame_height * 2:
                walk_frames = []
                for i in range(min(4, frames_per_row)):
                    frame_rect = pygame.Rect(i * frame_width, frame_height, frame_width, frame_height)
                    frame = sprite_sheet.subsurface(frame_rect).copy()
                    scaled_frame = pygame.transform.scale(frame, (frame_width * 2, frame_height * 2))
                    walk_frames.append(scaled_frame)
                animations['walk'] = walk_frames if walk_frames else animations['idle']
            else:
                animations['walk'] = animations['idle']
            
            # Attack animation from third row (if exists)
            if sheet_height >= frame_height * 3:
                attack_frames = []
                for i in range(min(4, frames_per_row)):
                    frame_rect = pygame.Rect(i * frame_width, frame_height * 2, frame_width, frame_height)
                    frame = sprite_sheet.subsurface(frame_rect).copy()
                    scaled_frame = pygame.transform.scale(frame, (frame_width * 2, frame_height * 2))
                    attack_frames.append(scaled_frame)
                animations['attack'] = attack_frames if attack_frames else animations['idle']
            else:
                animations['attack'] = animations['idle']
            
            # Death animation from fourth row (if exists)
            if sheet_height >= frame_height * 4:
                death_frames = []
                for i in range(min(4, frames_per_row)):
                    frame_rect = pygame.Rect(i * frame_width, frame_height * 3, frame_width, frame_height)
                    frame = sprite_sheet.subsurface(frame_rect).copy()
                    scaled_frame = pygame.transform.scale(frame, (frame_width * 2, frame_height * 2))
                    death_frames.append(scaled_frame)
                animations['death'] = death_frames if death_frames else animations['idle']
            else:
                animations['death'] = animations['idle']
                
        except Exception as e:
            print(f"[Wumpus] Warning: Could not load sprite sheet: {e}")
            animations['idle'] = [self.create_placeholder()]
            animations['walk'] = animations['idle']
            animations['attack'] = animations['idle']
            animations['death'] = animations['idle']
        
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
        Update AI behavior based on player position and current state
        Uses Prolog for decision-making (with Python fallback)
        """
        if not self.is_alive:
            return
        
        # Get current positions
        wumpus_pos = pygame.math.Vector2(self.hitbox_rect.center)
        
        # =========================================================================
        # PROLOG AUTHORITATIVE AI DECISION-MAKING
        # =========================================================================
        if self.prolog and getattr(self.prolog, 'available', False):
            try:
                # Update Prolog with current position
                self.prolog.update_wumpus_position(int(wumpus_pos.x), int(wumpus_pos.y))
                
                # Query Prolog for AI decision
                new_state, direction_x, direction_y = self.prolog.get_wumpus_decision(
                    int(wumpus_pos.x), int(wumpus_pos.y),
                    int(player_pos.x), int(player_pos.y),
                    self.ai_state
                )
                
                # Update state
                self.ai_state = new_state
                
                # Handle state-specific behavior
                if self.ai_state == 'patrol':
                    # Prolog doesn't know patrol points, use Python for patrol navigation
                    self.patrol()
                    
                elif self.ai_state == 'chase':
                    # Use Prolog's calculated direction
                    self.direction.x = direction_x
                    self.direction.y = direction_y
                    
                elif self.ai_state == 'attack':
                    # Stop and attack
                    self.direction = pygame.math.Vector2(0, 0)
                    # TODO: Trigger attack animation/damage
                    
                elif self.ai_state == 'dead':
                    self.direction = pygame.math.Vector2(0, 0)
                
                return  # Successfully used Prolog AI
                
            except Exception as e:
                print(f"[Wumpus] Prolog AI query failed: {e}, falling back to Python")
        
        # =========================================================================
        # FALLBACK: PYTHON AI (if Prolog unavailable)
        # =========================================================================
        distance_to_player = wumpus_pos.distance_to(player_pos)
        
        if distance_to_player <= self.attack_range:
            self.ai_state = 'attack'
            self.direction = pygame.math.Vector2(0, 0)  # Stop moving
            
        elif distance_to_player <= self.detection_range:
            # Chase player
            self.ai_state = 'chase'
            direction_to_player = player_pos - wumpus_pos
            if direction_to_player.length() > 0:
                self.direction = direction_to_player.normalize()
            
        else:
            # Patrol
            self.ai_state = 'patrol'
            self.patrol()
    
    def patrol(self):
        """Navigate to patrol points"""
        if self.patrol_points and len(self.patrol_points) > 0:
            wumpus_pos = pygame.math.Vector2(self.hitbox_rect.center)
            target = self.patrol_points[self.current_patrol_index]
            direction_to_target = pygame.math.Vector2(target) - wumpus_pos
            
            if direction_to_target.length() < 10:  # Reached patrol point
                self.current_patrol_index = (self.current_patrol_index + 1) % len(self.patrol_points)
            else:
                if direction_to_target.length() > 0:
                    self.direction = direction_to_target.normalize()
        else:
            # No patrol points - stand still
            self.direction = pygame.math.Vector2(0, 0)
    
    def animate(self, dt):
        """Override animate to handle Wumpus-specific animation states"""
        # Update animation timer
        self.animation_timer += dt
        
        # Determine animation based on AI state
        if self.ai_state == 'dead' or not self.is_alive:
            self.current_animation = 'death'
        elif self.ai_state == 'attack':
            self.current_animation = 'attack'
        elif self.direction.length() > 0:
            self.current_animation = 'walk'
        else:
            self.current_animation = 'idle'
        
        # Get animation frames
        animation_frames = self.animations.get(self.current_animation, self.animations['idle'])
        
        if not animation_frames:
            return
        
        # Calculate frame index
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)
        
        # Update image
        self.image = animation_frames[frame_index]
        
        # Flip sprite based on direction
        if self.direction.x < 0:
            self.image = pygame.transform.flip(self.image, True, False)
    
    def take_damage(self, amount):
        """Override to handle Wumpus death"""
        damage_taken = super().take_damage(amount)
        
        if self.health <= 0 and self.is_alive:
            self.on_death()
        
        return damage_taken
    
    def apply_stun(self, duration=None):
        """
        Apply stun effect to Wumpus (from arrow hit).
        Wumpus freezes in place and cannot move or attack.
        """
        if duration is None:
            duration = self.stun_duration
        
        self.is_stunned = True
        self.stun_timer = duration
        self.ai_state = 'stunned'
        self.direction = pygame.math.Vector2(0, 0)  # Stop moving
        
        print(f"[Wumpus] Stunned for {duration} seconds!")
    
    def on_death(self):
        """Handle Wumpus death"""
        super().on_death()
        self.ai_state = 'dead'
        self.direction = pygame.math.Vector2(0, 0)
        print(f"[Wumpus] Wumpus defeated!")
        # TODO: Trigger death animation, drop items, update game state
    
    def update(self, dt):
        """Update Wumpus every frame - AI update must be called separately"""
        # Update stun timer
        if self.is_stunned:
            self.stun_timer -= dt
            if self.stun_timer <= 0:
                self.is_stunned = False
                self.ai_state = 'patrol'  # Return to patrol after stun
                print("[Wumpus] Recovered from stun!")
        
        if not self.is_alive:
            # Still animate death
            self.animate(dt)
            return
        
        # Can't move while stunned
        if self.is_stunned:
            self.direction = pygame.math.Vector2(0, 0)
            self.animate(dt)
            return
        
        # Move using Entity's Prolog-authoritative movement
        self.move(dt)
        
        # Animate
        self.animate(dt)
