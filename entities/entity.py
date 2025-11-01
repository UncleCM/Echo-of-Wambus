"""
Base entity class combining all entity subsystems

This module provides the Entity class which combines:
1. Health system (damage, healing, death)
2. Animation system (sprite loading, frame updates)
3. Movement system (collision detection, Prolog integration)
"""
from Settings import *
from entities.health_system import HealthSystem
from entities.animation_system import AnimationSystem
from entities.movement_system import MovementSystem


class Entity(pygame.sprite.Sprite):
    """
    Base class for all moving entities (Player, Wumpus, etc.)
    Handles common functionality: movement, collision, animations, health
    """
    
    def __init__(self, pos, groups, collision_sprites, prolog_engine=None, entity_type='entity'):
        """
        Initialize entity
        
        Args:
            pos: (x, y) starting position
            groups: Pygame sprite groups to add to
            collision_sprites: Sprite group for collision detection
            prolog_engine: Prolog engine for authoritative collision
            entity_type: Type identifier ('player', 'wumpus', etc.)
        """
        super().__init__(groups)
        
        self.entity_type = entity_type
        self.prolog = prolog_engine
        self.collision_sprites = collision_sprites
        
        # Position
        self.position = pygame.math.Vector2(pos)
        
        # Initialize subsystems
        self.health_system = HealthSystem(max_health=100)
        self.animation_system = AnimationSystem(animation_speed=12)
        self.movement_system = MovementSystem(prolog_engine, collision_sprites, speed=200)
        
        # Sprite & hitbox (to be set by child classes)
        self.image = None
        self.rect = None
        self.hitbox_rect = None
        
        # Z-order for rendering (entities on top of tiles)
        self.z = 1
    
    # =========================================================================
    # Properties for backward compatibility and easy access
    # =========================================================================
    
    @property
    def health(self):
        """Get current health"""
        return self.health_system.health
    
    @health.setter
    def health(self, value):
        """Set current health"""
        self.health_system.health = value
    
    @property
    def max_health(self):
        """Get maximum health"""
        return self.health_system.max_health
    
    @max_health.setter
    def max_health(self, value):
        """Set maximum health"""
        self.health_system.max_health = value
    
    @property
    def is_alive(self):
        """Check if entity is alive"""
        return self.health_system.is_alive
    
    @property
    def direction(self):
        """Get movement direction"""
        return self.movement_system.direction
    
    @property
    def speed(self):
        """Get movement speed"""
        return self.movement_system.speed
    
    @speed.setter
    def speed(self, value):
        """Set movement speed"""
        self.movement_system.speed = value
    
    @property
    def facing(self):
        """Get facing direction"""
        return self.movement_system.facing
    
    @facing.setter
    def facing(self, value):
        """Set facing direction"""
        self.movement_system.facing = value
    
    @property
    def animations(self):
        """Get animations dict"""
        return self.animation_system.animations
    
    @property
    def current_animation(self):
        """Get current animation name"""
        return self.animation_system.current_animation
    
    @current_animation.setter
    def current_animation(self, value):
        """Set current animation"""
        self.animation_system.current_animation = value
    
    @property
    def animation_speed(self):
        """Get animation speed"""
        return self.animation_system.animation_speed
    
    @animation_speed.setter
    def animation_speed(self, value):
        """Set animation speed"""
        self.animation_system.animation_speed = value
    
    @property
    def frame_width(self):
        """Get frame width"""
        return self.animation_system.frame_width
    
    @frame_width.setter
    def frame_width(self, value):
        """Set frame width"""
        self.animation_system.frame_width = value
    
    @property
    def frame_height(self):
        """Get frame height"""
        return self.animation_system.frame_height
    
    @frame_height.setter
    def frame_height(self, value):
        """Set frame height"""
        self.animation_system.frame_height = value
    
    @property
    def scale_factor(self):
        """Get scale factor"""
        return self.animation_system.scale_factor
    
    @scale_factor.setter
    def scale_factor(self, value):
        """Set scale factor"""
        self.animation_system.scale_factor = value
    
    # =========================================================================
    # Core entity methods
    # =========================================================================
    
    def setup_sprite(self, initial_image, hitbox_inflate=(-75, -75)):
        """
        Setup sprite rect and hitbox
        
        Args:
            initial_image: Initial pygame.Surface for sprite
            hitbox_inflate: (x, y) tuple to inflate hitbox
        """
        self.image = initial_image
        self.rect = self.image.get_rect(center=self.position)
        self.hitbox_rect = self.rect.inflate(*hitbox_inflate)
    
    def move(self, dt, speed_override=None):
        """
        Move the entity using Prolog for authoritative collision resolution
        
        Args:
            dt: Delta time
            speed_override: Optional speed to use instead of self.speed
        """
        # Update hitbox via movement system
        self.hitbox_rect = self.movement_system.move(self.hitbox_rect, dt, speed_override)
        
        # Sync sprite rect with hitbox
        self.rect.center = self.hitbox_rect.center
        self.position.x = self.rect.centerx
        self.position.y = self.rect.centery
    
    def animate(self, dt):
        """
        Update animation frames based on current state
        
        Args:
            dt: Delta time
        """
        self.image = self.animation_system.animate(dt, self.image)
    
    def take_damage(self, amount):
        """
        Apply damage to entity
        
        Args:
            amount: Damage amount
            
        Returns:
            int: Actual damage dealt
        """
        actual_damage = self.health_system.take_damage(amount)
        
        if self.health_system.is_dead():
            self.on_death()
        
        return actual_damage
    
    def heal(self, amount):
        """
        Heal entity
        
        Args:
            amount: Healing amount
        """
        self.health_system.heal(amount)
    
    def on_death(self):
        """Called when entity dies (to be overridden by child classes)"""
        print(f"{self.entity_type} has died!")
    
    def update(self, dt):
        """
        Update entity (to be overridden by child classes)
        Default: just move and animate
        
        Args:
            dt: Delta time
        """
        self.move(dt)
        self.animate(dt)
    
    def load_sprite_strip(self, filepath):
        """
        Extract individual frames from a horizontal sprite strip
        
        Args:
            filepath: Path to sprite strip image (relative to assets/)
            
        Returns:
            list: List of pygame.Surface frames
        """
        return self.animation_system.load_sprite_strip(filepath)
