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
    
    # Class variable to track Wumpus IDs
    _next_id = 1

    def __init__(self, pos, groups, collision_sprites, prolog_engine=None, map_knowledge=None, sound_manager=None):
        # Initialize base Entity
        super().__init__(
            pos, groups, collision_sprites, prolog_engine, entity_type="wumpus"
        )

        # Assign unique ID for Prolog tracking
        self.wumpus_id = Wumpus._next_id
        Wumpus._next_id += 1

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

        # Sound-based AI state (synchronized with Prolog)
        self.ai_state = WumpusAIState.ROAMING
        self.target_position = None  # Where Wumpus is heading
        self.last_heard_sound = None  # Last detected sound
        self.roaming_target = None  # Current roaming destination
        self.search_timer = 0  # Time left searching (synced with Prolog)

        # Patrol route system
        self.patrol_route = []  # List of waypoints for patrol
        self.current_waypoint_index = 0  # Current waypoint in patrol route
        self.patrol_mode = True  # Whether using patrol route or random roaming
        self._setup_patrol_route()  # Initialize patrol route
        
        # Initialize AI in Prolog
        if self.prolog and self.prolog.available:
            self.prolog.init_wumpus_ai(self.wumpus_id, self.hearing_radius)
        
        # A* pathfinding
        self.current_path = None  # Current A* path being followed
        self.path_index = 0  # Current position in path

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
        print(f"[Wumpus] Patrol route: {len(self.patrol_route)} waypoints")
    
    def _setup_patrol_route(self):
        """Setup patrol route with safe waypoints from map knowledge"""
        if not self.map_knowledge:
            return
        
        # Generate 5-8 random patrol waypoints in safe positions
        import random
        num_waypoints = random.randint(5, 8)
        
        for _ in range(num_waypoints):
            safe_pos = self.map_knowledge.get_safe_random_position()
            if safe_pos:
                self.patrol_route.append(pygame.math.Vector2(safe_pos))
        
        if self.patrol_route:
            self.patrol_mode = True
            print(f"[Wumpus] Created patrol route with {len(self.patrol_route)} waypoints")
        else:
            print("[Wumpus] Failed to create patrol route, using random roaming")
    
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
        Uses Prolog for all AI decision-making
        
        Args:
            player_pos: Player position (for attack range check only, NOT detection)
            dt: Delta time
        """
        if not self.is_alive or self.is_stunned:
            return

        # Sync current state with Prolog
        if self.prolog and self.prolog.available:
            # Get state from Prolog (authoritative)
            prolog_state = self.prolog.get_wumpus_state(self.wumpus_id)
            self.ai_state = prolog_state
            
            # Update hearing radius based on state
            self.current_hearing_radius = self.prolog.calculate_hearing_radius(
                self.wumpus_id,
                self.hearing_radius,
                self.chase_hearing_bonus
            )

        # Listen for sounds
        if self.sound_manager:
            loudest_sound, loudness = self.sound_manager.get_loudest_sound(
                self.pos,
                min_threshold=10.0
            )
            
            # React to detected sound (Prolog decides action)
            if loudest_sound and loudness > 0:
                self._handle_sound_detected_prolog(loudest_sound, loudness)
            else:
                self._handle_no_sound_prolog()
        
        # Execute behavior based on current state
        if self.ai_state == WumpusAIState.ROAMING:
            self._behavior_roaming()
        elif self.ai_state == WumpusAIState.INVESTIGATING:
            self._behavior_investigating()
        elif self.ai_state == WumpusAIState.CHASING:
            self._behavior_chasing()
        elif self.ai_state == WumpusAIState.SEARCHING:
            self._behavior_searching_prolog(dt)
        elif self.ai_state == WumpusAIState.STUNNED:
            # Just stay still
            self.direction = pygame.math.Vector2(0, 0)
        
        # Check if in attack range (Prolog decides if should attack)
        if self.prolog and self.prolog.available:
            should_attack = self.prolog.should_attack(
                int(self.pos.x), int(self.pos.y),
                int(player_pos[0]), int(player_pos[1]),
                self.attack_range,
                self.ai_state
            )
            if should_attack:
                self.ai_state = "attack"
                self.prolog.set_wumpus_state(self.wumpus_id, "attack")
                self.direction = pygame.math.Vector2(0, 0)
        else:
            # Fallback: distance check
            distance_to_player = self.pos.distance_to(pygame.math.Vector2(player_pos))
            if distance_to_player <= self.attack_range and self.ai_state == WumpusAIState.CHASING:
                self.ai_state = "attack"
                self.direction = pygame.math.Vector2(0, 0)
    
    def _handle_sound_detected_prolog(self, sound, loudness):
        """React to detected sound using Prolog decision logic"""
        if not self.prolog or not self.prolog.available:
            # Fallback to old logic
            self._handle_sound_detected(sound, loudness)
            return
        
        # Ask Prolog what to do
        new_state, should_roar, target_x, target_y = self.prolog.decide_wumpus_action(
            self.wumpus_id,
            sound.source_type,
            int(sound.position.x),
            int(sound.position.y),
            loudness,
            self.ai_state
        )
        
        # Apply Prolog's decision
        if new_state != self.ai_state:
            print(f"[Wumpus {self.wumpus_id}] Heard {sound.source_type} (loudness: {loudness:.1f})! {self.ai_state} → {new_state}")
            self.ai_state = new_state
            self.prolog.set_wumpus_state(self.wumpus_id, new_state)
            
            # Set target if provided
            if target_x != 0 or target_y != 0:
                self.target_position = pygame.math.Vector2(target_x, target_y)
                self.prolog.set_wumpus_target(self.wumpus_id, target_x, target_y)
                self.current_path = None  # Clear path when changing target
                self.path_index = 0
            
            # Roar if Prolog says so
            if should_roar:
                self._trigger_roar()
    
    def _handle_no_sound_prolog(self):
        """No sound detected - use Prolog to decide action"""
        if not self.prolog or not self.prolog.available:
            # Fallback to old logic
            self._handle_no_sound()
            return
        
        # Ask Prolog what to do when no sound detected
        new_state, search_time = self.prolog.decide_no_sound_action(
            self.wumpus_id,
            self.ai_state
        )
        
        # Apply Prolog's decision
        if new_state != self.ai_state:
            print(f"[Wumpus {self.wumpus_id}] No sound detected. {self.ai_state} → {new_state}")
            self.ai_state = new_state
            self.prolog.set_wumpus_state(self.wumpus_id, new_state)
            
            if search_time > 0:
                self.search_timer = search_time
                self.prolog.set_search_timer(self.wumpus_id, search_time)
                self.current_path = None
                self.path_index = 0
    
    def _behavior_searching_prolog(self, dt):
        """Search around last known position using Prolog timer"""
        if self.prolog and self.prolog.available:
            # Update timer in Prolog
            self.prolog.update_search_timer(self.wumpus_id, dt)
            
            # Check if timeout
            if self.prolog.search_timeout(self.wumpus_id):
                # Give up, return to roaming
                self.ai_state = WumpusAIState.ROAMING
                self.prolog.set_wumpus_state(self.wumpus_id, WumpusAIState.ROAMING)
                self.current_hearing_radius = self.hearing_radius
                self.prolog.set_hearing_radius(self.wumpus_id, self.hearing_radius)
                self.is_roaring = False
                self.prolog.set_roaring(self.wumpus_id, False)
                print(f"[Wumpus {self.wumpus_id}] Search timeout... resuming roaming")
        else:
            # Fallback to old behavior
            self._behavior_searching(dt)
    
    def _handle_sound_detected(self, sound, loudness):
        """React to detected sound"""
        if sound.source_type == 'rock_impact':
            # LOUD distraction - investigate immediately!
            print(f"[Wumpus] Heard rock impact (loudness: {loudness:.1f})! Investigating...")
            self.ai_state = WumpusAIState.INVESTIGATING
            self.target_position = sound.position.copy()
            self.last_heard_sound = sound
            self.current_path = None  # Clear path when changing target
            self.path_index = 0
            
        elif sound.source_type in ['walk', 'dash', 'arrow_shot']:
            # Player sounds detected
            if loudness > 30:  # Loud enough to chase
                print(f"[Wumpus] Heard player (loudness: {loudness:.1f})! Chasing...")
                self.ai_state = WumpusAIState.CHASING
                self.target_position = sound.position.copy()
                self.current_path = None  # Clear path when changing target
                self.path_index = 0
                self._trigger_roar()
            else:
                # Faint sound - just investigate
                self.ai_state = WumpusAIState.INVESTIGATING
                self.target_position = sound.position.copy()
                self.current_path = None  # Clear path when changing target
                self.path_index = 0
    
    def _handle_no_sound(self):
        """No sound detected - continue current behavior"""
        # If chasing but lost sound, switch to searching
        if self.ai_state == WumpusAIState.CHASING:
            self.ai_state = WumpusAIState.SEARCHING
            self.search_timer = 8.0  # Search for 8 seconds
            self.current_path = None  # Clear path when searching
            self.path_index = 0
            print("[Wumpus] Lost sound... searching area")
    
    def _behavior_roaming(self):
        """Wander the map using patrol route or random positions"""
        if self.patrol_mode and self.patrol_route:
            # Follow patrol route
            if self.current_waypoint_index >= len(self.patrol_route):
                self.current_waypoint_index = 0  # Loop back to start
            
            current_waypoint = self.patrol_route[self.current_waypoint_index]
            
            # Check if reached current waypoint
            if self.pos.distance_to(current_waypoint) < 30:
                # Move to next waypoint
                self.current_waypoint_index += 1
                if self.current_waypoint_index >= len(self.patrol_route):
                    self.current_waypoint_index = 0
                current_waypoint = self.patrol_route[self.current_waypoint_index]
                print(f"[Wumpus] Patrol: Moving to waypoint {self.current_waypoint_index + 1}/{len(self.patrol_route)}")
            
            # Use A* to navigate to waypoint
            if self.map_knowledge:
                # Calculate path if we don't have one or reached end of path
                if not self.current_path or self.path_index >= len(self.current_path):
                    self.current_path = self.map_knowledge.find_path_astar(
                        (self.pos.x, self.pos.y),
                        (current_waypoint.x, current_waypoint.y)
                    )
                    self.path_index = 0
                
                # Follow path
                if self.current_path and self.path_index < len(self.current_path):
                    next_pos = pygame.math.Vector2(self.current_path[self.path_index])
                    
                    # Check if reached current path node
                    if self.pos.distance_to(next_pos) < 20:
                        self.path_index += 1
                    
                    # Move towards next path node
                    if self.path_index < len(self.current_path):
                        direction = pygame.math.Vector2(self.current_path[self.path_index]) - self.pos
                        if direction.length() > 0:
                            self.direction = direction.normalize()
                    else:
                        # Reached end of path, move directly to waypoint
                        direction = current_waypoint - self.pos
                        if direction.length() > 0:
                            self.direction = direction.normalize()
                else:
                    # No path found, move directly (will be blocked by walls)
                    direction = current_waypoint - self.pos
                    if direction.length() > 0:
                        self.direction = direction.normalize()
            else:
                # No map knowledge, move directly
                direction = current_waypoint - self.pos
                if direction.length() > 0:
                    self.direction = direction.normalize()
        else:
            # Random roaming (old behavior)
            if self.roaming_target is None or self._reached_target():
                # Pick new random safe position
                if self.map_knowledge:
                    new_target = self.map_knowledge.get_safe_random_position()
                    if new_target:
                        self.roaming_target = new_target
                        self.target_position = pygame.math.Vector2(new_target)
                        self.current_path = None  # Clear path
            
            # Move towards target with pit avoidance
            if self.target_position:
                direction = (self.target_position - self.pos)
                if direction.length() > 0:
                    self.direction = direction.normalize()
    
    def _behavior_investigating(self):
        """Move to sound source location using A* pathfinding"""
        # Check if reached target using Prolog
        if self.target_position and self.prolog and self.prolog.available:
            reached = self.prolog.reached_target(
                int(self.pos.x), int(self.pos.y),
                int(self.target_position.x), int(self.target_position.y),
                30
            )
            if reached:
                # Ask Prolog what to do when target reached
                new_state, search_time = self.prolog.decide_target_reached(
                    self.wumpus_id,
                    self.ai_state
                )
                self.ai_state = new_state
                self.prolog.set_wumpus_state(self.wumpus_id, new_state)
                if search_time > 0:
                    self.search_timer = search_time
                    self.prolog.set_search_timer(self.wumpus_id, search_time)
                self.current_path = None
                print(f"[Wumpus {self.wumpus_id}] Reached sound location → {new_state}")
                return
        elif self._reached_target():
            # Fallback without Prolog
            self.ai_state = WumpusAIState.SEARCHING
            self.search_timer = 5.0
            self.current_path = None
            print("[Wumpus] Reached sound location, nothing here... searching")
            return
        
        # Use A* to navigate to sound location
        if self.map_knowledge and self.target_position:
            # Calculate path if we don't have one
            if not self.current_path or self.path_index >= len(self.current_path):
                self.current_path = self.map_knowledge.find_path_astar(
                    (self.pos.x, self.pos.y),
                    (self.target_position.x, self.target_position.y)
                )
                self.path_index = 0
            
            # Follow path
            if self.current_path and self.path_index < len(self.current_path):
                next_pos = pygame.math.Vector2(self.current_path[self.path_index])
                
                # Check if reached current path node
                if self.pos.distance_to(next_pos) < 20:
                    self.path_index += 1
                
                # Move towards next path node
                if self.path_index < len(self.current_path):
                    direction = pygame.math.Vector2(self.current_path[self.path_index]) - self.pos
                else:
                    direction = self.target_position - self.pos
                
                if direction.length() > 0:
                    self.direction = direction.normalize()
            else:
                # No path found, move directly
                direction = (self.target_position - self.pos)
                if direction.length() > 0:
                    self.direction = direction.normalize()
        else:
            # No map knowledge, move directly
            direction = (self.target_position - self.pos)
            if direction.length() > 0:
                self.direction = direction.normalize()
    
    def _behavior_chasing(self):
        """Chase last heard player sound using A* pathfinding"""
        # Check if reached target using Prolog
        if self.target_position and self.prolog and self.prolog.available:
            reached = self.prolog.reached_target(
                int(self.pos.x), int(self.pos.y),
                int(self.target_position.x), int(self.target_position.y),
                30
            )
            if reached:
                # Ask Prolog what to do when target reached
                new_state, search_time = self.prolog.decide_target_reached(
                    self.wumpus_id,
                    self.ai_state
                )
                self.ai_state = new_state
                self.prolog.set_wumpus_state(self.wumpus_id, new_state)
                if search_time > 0:
                    self.search_timer = search_time
                    self.prolog.set_search_timer(self.wumpus_id, search_time)
                self.is_roaring = False
                self.prolog.set_roaring(self.wumpus_id, False)
                self.current_hearing_radius = self.hearing_radius
                self.prolog.set_hearing_radius(self.wumpus_id, self.hearing_radius)
                self.current_path = None
                print(f"[Wumpus {self.wumpus_id}] Lost player → {new_state}")
                return
        elif self._reached_target():
            # Fallback without Prolog
            self.ai_state = WumpusAIState.SEARCHING
            self.search_timer = 8.0
            self.is_roaring = False
            self.current_hearing_radius = self.hearing_radius
            self.current_path = None
            print("[Wumpus] Lost player... searching")
            return
        
        # Use A* to chase player
        if self.map_knowledge and self.target_position:
            # Recalculate path frequently during chase (every few frames)
            if not hasattr(self, '_chase_path_timer'):
                self._chase_path_timer = 0
            
            self._chase_path_timer += 1
            
            # Recalculate path every 30 frames (~0.5 seconds at 60fps)
            if not self.current_path or self._chase_path_timer >= 30:
                self.current_path = self.map_knowledge.find_path_astar(
                    (self.pos.x, self.pos.y),
                    (self.target_position.x, self.target_position.y)
                )
                self.path_index = 0
                self._chase_path_timer = 0
            
            # Follow path
            if self.current_path and self.path_index < len(self.current_path):
                next_pos = pygame.math.Vector2(self.current_path[self.path_index])
                
                # Check if reached current path node
                if self.pos.distance_to(next_pos) < 20:
                    self.path_index += 1
                
                # Move towards next path node
                if self.path_index < len(self.current_path):
                    direction = pygame.math.Vector2(self.current_path[self.path_index]) - self.pos
                else:
                    direction = self.target_position - self.pos
                
                if direction.length() > 0:
                    self.direction = direction.normalize()
            else:
                # No path found, move directly
                direction = (self.target_position - self.pos)
                if direction.length() > 0:
                    self.direction = direction.normalize()
        else:
            # No map knowledge, move directly
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
        """Roar when starting chase - increases hearing radius (uses Prolog for cooldown check)"""
        current_time = pygame.time.get_ticks()
        
        # Check if can roar using Prolog
        can_roar = True
        if self.prolog and self.prolog.available:
            can_roar = self.prolog.can_roar(self.wumpus_id, current_time, self.roar_cooldown)
        else:
            # Fallback: manual check
            can_roar = (current_time - self.last_roar_time > self.roar_cooldown)
        
        if can_roar:
            self.is_roaring = True
            self.current_hearing_radius = self.hearing_radius + self.chase_hearing_bonus
            self.last_roar_time = current_time
            
            # Update Prolog state
            if self.prolog and self.prolog.available:
                self.prolog.set_roaring(self.wumpus_id, True)
                self.prolog.set_hearing_radius(self.wumpus_id, self.current_hearing_radius)
                self.prolog.set_last_roar_time(self.wumpus_id, current_time)
            
            # Emit roar sound event (for AI detection)
            if self.sound_manager:
                self.sound_manager.emit_sound(
                    self.pos,
                    SOUND_LEVELS['wumpus_roar'],
                    SOUND_DURATIONS['wumpus_roar'],
                    'wumpus_roar'
                )
                
                # Play actual roar sound effect
                self.sound_manager.play_sound('roar', volume=0.5)
            
            print(f"[Wumpus {self.wumpus_id}] ROAR! Hearing increased to {self.current_hearing_radius}px")
    
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