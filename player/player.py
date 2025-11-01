"""Player character - Main player class"""
import pygame
from Settings import *
from entities import Entity
from player.inventory import PlayerInventory
from player.input import PlayerInput
from player.animations import PlayerAnimations


class Player(Entity):
    """Player character - controlled by user input"""

    def __init__(self, pos, groups, collision_sprites, prolog_engine=None, sound_manager=None):
        # Initialize base Entity
        super().__init__(
            pos, groups, collision_sprites, prolog_engine, entity_type="player"
        )

        # Player-specific attributes
        self.speed = 200  # Player movement speed

        # Sound manager reference
        self.sound_manager = sound_manager
        self.is_walking = False  # Track if player is currently walking

        # Combat system
        self.attack_cooldown = ATTACK_COOLDOWN / 1000.0  # Convert ms to seconds
        self.attack_timer = 0  # Time since last shot
        self.is_attacking = False  # Currently shooting (prevents movement)
        self.attack_duration = 0.3  # Time player is frozen during shoot animation

        # Rock throwing system
        self.throw_cooldown = ROCK_THROW_COOLDOWN / 1000.0  # Convert ms to seconds
        self.last_throw_time = 0  # Time since last throw

        # Initialize subsystems
        self.inventory = PlayerInventory()
        self.input_handler = PlayerInput(self)
        self.animator = PlayerAnimations()  # Create without player first
        
        # Initialize animator with self (after Entity.__init__ sets up frame size)
        self.animator.initialize(self)

        # Setup sprite and hitbox (with fallback for empty animations)
        idle_frames = self.animator.animations.get("idle_down", [])
        if idle_frames:
            initial_image = idle_frames[0]
        else:
            # Create placeholder if animations failed to load
            initial_image = pygame.Surface((96, 128), pygame.SRCALPHA)
            pygame.draw.circle(initial_image, (100, 150, 255), (48, 64), 30)
        
        self.setup_sprite(initial_image, hitbox_inflate=(-75, -75))

        print(f"First frame size: {self.image.get_size()}")
    
    @property
    def pos(self):
        """Get current position as Vector2"""
        return pygame.math.Vector2(self.hitbox_rect.center)
    
    @property
    def arrows(self):
        """Arrow count (for compatibility)"""
        return self.inventory.arrows
    
    @property
    def max_arrows(self):
        """Max arrows (for compatibility)"""
        return self.inventory.max_arrows
    
    @property
    def rocks(self):
        """Rock count (for compatibility)"""
        return self.inventory.rocks
    
    @property
    def max_rocks(self):
        """Max rocks (for compatibility)"""
        return self.inventory.max_rocks

    def input(self):
        """Handle player input (delegates to input handler)"""
        self.input_handler.process_input()

    def shoot_arrow(self):
        """
        Shoot an arrow in the direction player is facing.
        Returns arrow direction vector if shot successful, None otherwise.
        """
        # Check preconditions
        if self.inventory.arrows <= 0:
            print("[Player] No arrows left!")
            return None

        if self.is_attacking:
            return None

        if self.attack_timer > 0:
            return None

        # Must be standing still to shoot
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
        self.inventory.use_arrow()

        return arrow_direction

    def add_arrows(self, amount=1):
        """Pick up arrows from ArrowPickup"""
        return self.inventory.add_arrows(amount)

    def throw_rock(self, sound_manager):
        """
        Throw a rock in the direction player is facing
        
        Args:
            sound_manager: SoundManager instance for emitting throw sound
            
        Returns:
            Vector2: direction to throw rock, or None if can't throw
        """
        # Check if can throw
        if self.inventory.rocks <= 0:
            print("[Player] No rocks left!")
            return None
        
        current_time = pygame.time.get_ticks() / 1000.0
        time_since_throw = current_time - self.last_throw_time
        
        if time_since_throw < self.throw_cooldown:
            print(f"[Player] Throw on cooldown! Wait {self.throw_cooldown - time_since_throw:.1f}s")
            return None
        
        # Calculate direction from facing
        facing_vectors = {
            "up": pygame.math.Vector2(0, -1),
            "down": pygame.math.Vector2(0, 1),
            "left": pygame.math.Vector2(-1, 0),
            "right": pygame.math.Vector2(1, 0),
            "left_up": pygame.math.Vector2(-1, -1).normalize(),
            "left_down": pygame.math.Vector2(-1, 1).normalize(),
            "right_up": pygame.math.Vector2(1, -1).normalize(),
            "right_down": pygame.math.Vector2(1, 1).normalize(),
        }
        
        direction = facing_vectors.get(self.facing, pygame.math.Vector2(0, 1))
        
        if direction.length() == 0:
            return None
        
        direction = direction.normalize()
        
        # Emit throw sound
        sound_manager.emit_sound(
            self.pos,
            SOUND_LEVELS['rock_throw'],
            SOUND_DURATIONS['rock_throw'],
            'rock_throw'
        )
        
        # Consume rock
        self.inventory.use_rock()
        self.last_throw_time = current_time
        
        return direction
    
    def add_rocks(self, amount=1):
        """Pick up rocks from RockPickup"""
        return self.inventory.add_rocks(amount)

    def animate(self, dt):
        """Update animation frames (delegates to animator)"""
        self.image = self.animator.update(dt, self.direction, self.facing)

    def update(self, dt, sound_manager=None):
        """Update player every frame"""
        # Update attack cooldown timer
        if self.attack_timer > 0:
            self.attack_timer -= dt

        self.input()
        
        # Apply speed modifier for dashing
        current_speed = self.speed * 2.0 if self.is_dashing else self.speed
        
        # Emit movement sounds
        if sound_manager and self.direction.length() > 0:
            if self.is_dashing:
                sound_manager.emit_sound(
                    self.pos,
                    SOUND_LEVELS['dash'],
                    SOUND_DURATIONS['dash'],
                    'dash'
                )
            else:
                sound_manager.emit_sound(
                    self.pos,
                    SOUND_LEVELS['walk'],
                    SOUND_DURATIONS['walk'],
                    'walk'
                )
        
        # Movement
        self.move(dt, speed_override=current_speed)
        self.animate(dt)

        # Handle footstep sounds
        if self.sound_manager:
            is_currently_walking = self.direction.magnitude() > 0 and self.is_alive

            if is_currently_walking:
                if not self.is_walking:
                    self.is_walking = True
                    self.sound_manager.play_footstep_loop(0.3)
            else:
                if self.is_walking:
                    self.is_walking = False
                    self.sound_manager.stop_footstep_loop()

        # Check if attack animation finished
        if self.is_attacking:
            if self.attack_timer <= self.attack_cooldown - self.attack_duration:
                self.is_attacking = False
