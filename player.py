from Settings import *
from entity import Entity


class Player(Entity):
    """Player character - controlled by user input"""

    def __init__(
        self, pos, groups, collision_sprites, prolog_engine=None, sound_manager=None
    ):
        # Initialize base Entity
        super().__init__(
            pos, groups, collision_sprites, prolog_engine, entity_type="player"
        )

        # Player-specific attributes
        self.speed = 200  # Player movement speed

        # Sound manager reference
        self.sound_manager = sound_manager
        self.is_walking = False  # Track if player is currently walking

        # Arrow combat system
        self.arrows = STARTING_ARROWS  # Current arrow count
        self.max_arrows = MAX_ARROWS  # Maximum arrows can carry
        self.attack_cooldown = ATTACK_COOLDOWN / 1000.0  # Convert ms to seconds
        self.attack_timer = 0  # Time since last shot
        self.is_attacking = False  # Currently shooting (prevents movement)
        self.attack_duration = 0.3  # Time player is frozen during shoot animation

        # Load player animations
        self.animations = self.load_animations()

        print(f"Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")

        # Setup sprite and hitbox
        self.current_animation = "idle_down"
        initial_image = self.animations[self.current_animation][0]
        self.setup_sprite(initial_image, hitbox_inflate=(-75, -75))

        print(f"First frame size: {self.image.get_size()}")

        # ... rest of the load_animations, load_sprite_strip, input, and animate methods stay the same ...

    def load_animations(self):
        """Load all animation frames from sprite strip files"""
        animations = {}

        # Idle animations
        animations["idle_down"] = self.load_sprite_strip("Player/Idle/Idle_Down.png")
        animations["idle_up"] = self.load_sprite_strip("Player/Idle/Idle_Up.png")
        animations["idle_left"] = self.load_sprite_strip(
            "Player/Idle/Idle_Left_Down.png"
        )
        animations["idle_right"] = self.load_sprite_strip(
            "Player/Idle/Idle_Right_Down.png"
        )
        animations["idle_left_up"] = self.load_sprite_strip(
            "Player/Idle/Idle_Left_Up.png"
        )
        animations["idle_right_up"] = self.load_sprite_strip(
            "Player/Idle/Idle_Right_Up.png"
        )

        # Walk animations
        animations["walk_down"] = self.load_sprite_strip("Player/Walk/walk_Down.png")
        animations["walk_up"] = self.load_sprite_strip("Player/Walk/walk_Up.png")
        animations["walk_left_down"] = self.load_sprite_strip(
            "Player/Walk/walk_Left_Down.png"
        )
        animations["walk_right_down"] = self.load_sprite_strip(
            "Player/Walk/walk_Right_Down.png"
        )
        animations["walk_left_up"] = self.load_sprite_strip(
            "Player/Walk/walk_Left_Up.png"
        )
        animations["walk_right_up"] = self.load_sprite_strip(
            "Player/Walk/walk_Right_Up.png"
        )

        # Aliases for simpler left/right (use left_down and right_down as defaults)
        animations["walk_left"] = animations["walk_left_down"]
        animations["walk_right"] = animations["walk_right_down"]

        # Optional: Dash animations
        try:
            animations["dash"] = self.load_sprite_strip("Player/Dash/Dash.png")
        except:
            animations["dash"] = animations["idle_down"]  # Fallback

        # Optional: Death animations
        try:
            animations["death"] = self.load_sprite_strip("Player/Death/Death.png")
        except:
            animations["death"] = animations["idle_down"]  # Fallback

        return animations

    def input(self):
        """Handle player input"""
        keys = pygame.key.get_pressed()

        # Can't move or shoot while attacking (shooting animation)
        if self.is_attacking:
            self.direction.x = 0
            self.direction.y = 0
            return

        # Reset direction
        self.direction.x = 0
        self.direction.y = 0

        # Movement input
        if keys[pygame.K_UP] or keys[pygame.K_w]:
            self.direction.y = -1
        elif keys[pygame.K_DOWN] or keys[pygame.K_s]:
            self.direction.y = 1

        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            self.direction.x = -1
        elif keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            self.direction.x = 1

        # Update facing based on direction (supports 8 directions)
        if self.direction.x != 0 or self.direction.y != 0:
            # Diagonal movements
            if self.direction.x > 0 and self.direction.y < 0:
                self.facing = "right_up"
            elif self.direction.x > 0 and self.direction.y > 0:
                self.facing = "right_down"
            elif self.direction.x < 0 and self.direction.y < 0:
                self.facing = "left_up"
            elif self.direction.x < 0 and self.direction.y > 0:
                self.facing = "left_down"
            # Cardinal directions
            elif self.direction.y < 0:
                self.facing = "up"
            elif self.direction.y > 0:
                self.facing = "down"
            elif self.direction.x < 0:
                self.facing = "left"
            elif self.direction.x > 0:
                self.facing = "right"

        # Normalize diagonal movement
        if self.direction.magnitude() > 0:
            self.direction = self.direction.normalize()

    def shoot_arrow(self):
        """
        Shoot an arrow in the direction player is facing.
        Returns arrow direction vector if shot successful, None otherwise.
        Can only shoot if:
        - Has arrows > 0
        - Not currently attacking
        - Attack cooldown finished
        - Player is NOT moving (must stop to shoot)
        """
        # Check if can shoot
        if self.arrows <= 0:
            print("[Player] No arrows left!")
            return None

        if self.is_attacking:
            return None

        if self.attack_timer > 0:
            return None

        # Must be standing still to shoot (direction magnitude == 0)
        if self.direction.magnitude() > 0:
            print("[Player] Must stop moving to shoot!")
            return None

        # Convert facing direction to vector
        direction_map = {
            "down": (0, 1),
            "up": (0, -1),
            "left": (-1, 0),
            "right": (1, 0),
        }

        arrow_direction = direction_map.get(self.facing, (0, 1))

        # Start attack animation
        self.is_attacking = True
        self.attack_timer = self.attack_cooldown
        self.arrows -= 1

        print(f"[Player] Shot arrow! Arrows remaining: {self.arrows}/{self.max_arrows}")
        return arrow_direction

    def add_arrows(self, amount=1):
        """Pick up arrows from ArrowPickup"""
        if self.arrows < self.max_arrows:
            self.arrows = min(self.arrows + amount, self.max_arrows)
            print(
                f"[Player] Picked up {amount} arrow(s)! Total: {self.arrows}/{self.max_arrows}"
            )
            return True
        else:
            print(
                f"[Player] Arrow capacity full! ({self.max_arrows}/{self.max_arrows})"
            )
            return False

    def attack(self):
        """Deprecated - replaced by shoot_arrow()"""
        pass

    def animate(self, dt):
        """Update animation frames"""
        # Determine animation state based on movement
        if self.direction.magnitude() > 0:
            # Moving - determine direction including diagonals
            # DON'T overwrite self.facing - it's already set correctly in input()
            if self.direction.y > 0 and self.direction.x < 0:
                # Moving down-left
                self.current_animation = "walk_left_down"
            elif self.direction.y > 0 and self.direction.x > 0:
                # Moving down-right
                self.current_animation = "walk_right_down"
            elif self.direction.y < 0 and self.direction.x < 0:
                self.current_animation = "walk_left_up"
            elif self.direction.y < 0 and self.direction.x > 0:
                self.current_animation = "walk_right_up"
            elif self.direction.x < 0:
                self.current_animation = "walk_left_down"
            elif self.direction.x > 0:
                self.current_animation = "walk_right_down"
            elif self.direction.y < 0:
                self.current_animation = "walk_up"
            else:
                self.current_animation = "walk_down"
        else:
            # Idle - match the last direction (supports 8 directions)
            if self.facing == "left":
                self.current_animation = "idle_left"
            elif self.facing == "right":
                self.current_animation = "idle_right"
            elif self.facing == "up":
                self.current_animation = "idle_up"
            elif self.facing == "down":
                self.current_animation = "idle_down"
            elif self.facing == "left_up":
                self.current_animation = "idle_left_up"
            elif self.facing == "right_up":
                self.current_animation = "idle_right_up"
            elif self.facing == "left_down":
                self.current_animation = "idle_left"  # Fallback to idle_left
            elif self.facing == "right_down":
                self.current_animation = "idle_right"  # Fallback to idle_right
            else:
                self.current_animation = "idle_down"

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

        # Calculate frame index using total time (like your old model)
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)

        # Update image
        self.image = animation_frames[frame_index]

    def update(self, dt):
        """Update player every frame"""
        # Update attack cooldown timer
        if self.attack_timer > 0:
            self.attack_timer -= dt

        self.input()
        self.move(dt)
        self.animate(dt)

        # Handle footstep sounds
        if self.sound_manager:
            # Check if player is moving
            is_currently_walking = self.direction.magnitude() > 0 and self.is_alive

            if is_currently_walking:
                # Player is walking - start looping footstep sound
                if not self.is_walking:
                    # Just started walking
                    self.is_walking = True
                    self.sound_manager.play_footstep_loop(0.3)
            else:
                # Player stopped walking - stop footstep sound
                if self.is_walking:
                    self.is_walking = False
                    self.sound_manager.stop_footstep_loop()

        # Check if attack animation finished
        if self.is_attacking:
            # Attack animation duration
            if self.attack_timer <= self.attack_cooldown - self.attack_duration:
                self.is_attacking = False
