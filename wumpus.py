from Settings import *
from entity import Entity


class Wumpus(Entity):
    """Wumpus enemy - AI-controlled entity that patrols and chases player"""

    def __init__(
        self, pos, groups, collision_sprites, prolog_engine=None, sound_manager=None
    ):
        # Initialize base Entity
        super().__init__(
            pos, groups, collision_sprites, prolog_engine, entity_type="wumpus"
        )

        # Sound manager for roar effect
        self.sound_manager = sound_manager

        # Wumpus-specific attributes
        self.speed = 150  # Slower than player (200)
        self.damage = 25  # Damage dealt to player
        self.max_health = 150  # More HP than player
        self.health = self.max_health

        # Combat ranges
        self.attack_range = 50  # Pixels within which Wumpus can attack
        self.detection_range = 300  # Pixels within which Wumpus detects player

        # Stun mechanics (arrow combat)
        self.is_stunned = False
        self.stun_timer = 0
        self.stun_duration = ARROW_STUN_DURATION  # From Settings.py

        # AI state
        self.ai_state = "patrol"  # 'patrol', 'chase', 'attack', 'stunned', 'dead'
        self.previous_ai_state = "patrol"  # Track state changes
        self.patrol_points = []  # Will be set by map/AI
        self.current_patrol_index = 0

        # Load Wumpus animations
        self.animations = self.load_animations()

        print(f"Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")

        # Setup sprite and hitbox
        self.current_animation = "idle"
        initial_image = self.animations[self.current_animation][0]
        # Tighter hitbox that fits the Wumpus body better
        # Negative values make hitbox smaller than sprite
        self.setup_sprite(initial_image, hitbox_inflate=(-80, -80))

        print(f"First frame size: {self.image.get_size()}")

        # Initialize Wumpus in Prolog
        if self.prolog and getattr(self.prolog, "available", False):
            self.prolog.init_wumpus(int(pos[0]), int(pos[1]))

        print(f"[Wumpus] Initialized at {pos}, HP: {self.health}/{self.max_health}")

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
        Update AI behavior based on player position and current state.
        Uses Prolog for decision-making (with Python fallback).
        This is called separately from update() to provide player position.
        """
        if not self.is_alive or self.is_stunned:
            return

        # Get current positions
        wumpus_pos = pygame.math.Vector2(self.hitbox_rect.center)

        # Store previous state to detect transitions (for roar sound)
        old_state = self.ai_state

        # Prolog AI decision-making
        if self.prolog and getattr(self.prolog, "available", False):
            try:
                # Update Prolog with current position
                self.prolog.update_wumpus_position(int(wumpus_pos.x), int(wumpus_pos.y))

                # Query Prolog for AI decision
                new_state, direction_x, direction_y = self.prolog.get_wumpus_decision(
                    int(wumpus_pos.x),
                    int(wumpus_pos.y),
                    int(player_pos.x),
                    int(player_pos.y),
                    self.ai_state,
                )

                # Update state
                self.ai_state = new_state

                # Handle state-specific behavior
                if self.ai_state == "patrol":
                    # Prolog doesn't know patrol points, use Python for patrol navigation
                    self.patrol()

                elif self.ai_state == "chase":
                    # Use Prolog's calculated direction
                    self.direction.x = direction_x
                    self.direction.y = direction_y

                elif self.ai_state == "attack":
                    # Stop and attack
                    self.direction = pygame.math.Vector2(0, 0)

                elif self.ai_state == "dead":
                    self.direction = pygame.math.Vector2(0, 0)

                # Trigger roar sound when entering chase mode (after Prolog decision)
                if (
                    old_state != "chase"
                    and self.ai_state == "chase"
                    and self.sound_manager
                ):
                    self.sound_manager.play_sound("roar", volume=0.1)
                    print("[Wumpus] 🦁 ROAR! Entering chase mode!")

                return  # Successfully used Prolog AI

            except Exception as e:
                print(f"[Wumpus] Prolog AI query failed: {e}, falling back to Python")

        # Python fallback AI
        distance_to_player = wumpus_pos.distance_to(player_pos)

        if distance_to_player <= self.attack_range:
            self.ai_state = "attack"
            self.direction = pygame.math.Vector2(0, 0)

        elif distance_to_player <= self.detection_range:
            self.ai_state = "chase"
            direction_to_player = player_pos - wumpus_pos
            if direction_to_player.length() > 0:
                self.direction = direction_to_player.normalize()

        else:
            self.ai_state = "patrol"
            self.patrol()

        # Trigger roar sound when entering chase mode (Python fallback)
        if old_state != "chase" and self.ai_state == "chase" and self.sound_manager:
            self.sound_manager.play_sound("roar", volume=0.1)
            print("[Wumpus] 🦁 ROAR! Entering chase mode!")

    def patrol(self):
        """Navigate to patrol points"""
        if self.patrol_points and len(self.patrol_points) > 0:
            wumpus_pos = pygame.math.Vector2(self.hitbox_rect.center)
            target = self.patrol_points[self.current_patrol_index]
            direction_to_target = pygame.math.Vector2(target) - wumpus_pos

            if direction_to_target.length() < 10:  # Reached patrol point
                self.current_patrol_index = (self.current_patrol_index + 1) % len(
                    self.patrol_points
                )
            else:
                if direction_to_target.length() > 0:
                    self.direction = direction_to_target.normalize()
        else:
            # No patrol points - stand still
            self.direction = pygame.math.Vector2(0, 0)

    def apply_stun(self, duration=None):
        """
        Apply stun effect to Wumpus (from arrow hit).
        Wumpus freezes in place and cannot move or attack.
        """
        if duration is None:
            duration = self.stun_duration

        self.is_stunned = True
        self.stun_timer = duration
        self.ai_state = "stunned"
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
        self.ai_state = "dead"
        self.direction = pygame.math.Vector2(0, 0)
        print("[Wumpus] Wumpus defeated!")

    def animate(self, dt):
        """Update animation frames"""
        # Determine animation state
        if self.ai_state == "dead" or not self.is_alive:
            self.current_animation = "death"
        elif self.ai_state == "attack":
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
        """Update Wumpus every frame"""
        # Update stun timer
        if self.is_stunned:
            self.stun_timer -= dt
            if self.stun_timer <= 0:
                self.is_stunned = False
                self.ai_state = "patrol"
                print("[Wumpus] Recovered from stun!")

        # Handle death state
        if not self.is_alive:
            self.animate(dt)
            return

        # Can't move while stunned
        if self.is_stunned:
            self.direction = pygame.math.Vector2(0, 0)

        # Move and animate
        self.move(dt)
        self.animate(dt)
