"""
Projectile sprites (Arrow and Rock)
"""

from Settings import *
import math


class Arrow(pygame.sprite.Sprite):
    """Arrow projectile shot by player"""
    def __init__(self, pos, direction, groups, collision_sprites):
        super().__init__(groups)
        
        # Arrow properties
        self.direction = pygame.math.Vector2(direction).normalize() if direction != (0, 0) else pygame.math.Vector2(1, 0)
        self.speed = ARROW_SPEED
        self.max_distance = ARROW_MAX_DISTANCE
        self.distance_traveled = 0
        self.collision_sprites = collision_sprites
        
        # Create arrow visual (8x32 pixels - thin and long)
        arrow_length = 32
        arrow_width = 8
        self.base_image = pygame.Surface((arrow_length, arrow_width), pygame.SRCALPHA)
        
        # Draw arrow (brown shaft, gray tip)
        pygame.draw.rect(self.base_image, (139, 90, 43), (0, 2, 28, 4))  # Shaft
        pygame.draw.polygon(self.base_image, (128, 128, 128), [(28, 0), (32, 4), (28, 8)])  # Arrowhead
        
        # Rotate based on direction
        angle = math.degrees(math.atan2(-self.direction.y, self.direction.x))
        self.image = pygame.transform.rotate(self.base_image, angle)
        
        # Position
        self.pos = pygame.math.Vector2(pos)
        self.rect = self.image.get_rect(center=self.pos)
        # Use full rect for better hit detection (no shrinking)
        self.hitbox_rect = self.rect.copy()
    
    def update(self, dt):
        """Move arrow and check for destruction"""
        # Move arrow
        movement = self.direction * self.speed
        self.pos += movement
        self.rect.center = self.pos
        self.hitbox_rect.center = self.pos
        
        # Track distance
        self.distance_traveled += self.speed
        
        # Check if exceeded max distance
        if self.distance_traveled >= self.max_distance:
            self.kill()
            return
        
        # Check collision with walls
        if self.check_wall_collision():
            self.kill()
            return
    
    def check_wall_collision(self):
        """Check if arrow hit a wall"""
        for sprite in self.collision_sprites:
            if hasattr(sprite, 'rect') and self.hitbox_rect.colliderect(sprite.rect):
                return True
        return False


class Rock(pygame.sprite.Sprite):
    """Throwable rock projectile with physics"""
    def __init__(self, pos, direction, throw_power, groups, collision_sprites):
        super().__init__(groups)
        
        # Physics properties
        self.pos = pygame.math.Vector2(pos)
        self.velocity = pygame.math.Vector2(direction) * throw_power
        self.gravity = ROCK_GRAVITY
        self.bounce_damping = ROCK_BOUNCE_DAMPING
        self.rolling_friction = ROCK_ROLLING_FRICTION
        
        # Visual
        size = 12
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        # Draw rock (gray circle with shading)
        pygame.draw.circle(self.image, (100, 100, 100), (size//2, size//2), size//2)
        pygame.draw.circle(self.image, (140, 140, 140), (size//2-2, size//2-2), size//4)  # Highlight
        
        self.rect = self.image.get_rect(center=pos)
        self.hitbox_rect = self.rect.copy()
        
        # Collision
        self.collision_sprites = collision_sprites
        self.has_impacted = False  # Track first impact for loud sound
        
        # Store sound manager reference (will be passed in update)
        self.lifetime = 0
        self.max_lifetime = 10.0  # Auto-remove after 10 seconds
    
    def update(self, dt, sound_manager=None):
        """Update rock physics"""
        self.lifetime += dt
        
        # Remove if too old
        if self.lifetime > self.max_lifetime:
            self.kill()
            return
        
        # Apply gravity
        self.velocity.y += self.gravity
        
        # Move
        self.pos += self.velocity
        self.rect.center = self.pos
        self.hitbox_rect.center = self.pos
        
        # Check wall collision
        if self._check_wall_collision():
            # Bounce
            self.velocity *= self.bounce_damping
            
            # Play LOUD impact sound on first bounce
            if not self.has_impacted and sound_manager:
                sound_manager.emit_sound(
                    self.pos,
                    SOUND_LEVELS['rock_impact'],
                    SOUND_DURATIONS['rock_impact'],
                    'rock_impact'
                )
                self.has_impacted = True
        
        # Apply rolling friction
        self.velocity *= self.rolling_friction
        
        # Stop if too slow
        if self.velocity.length() < 0.5:
            self.kill()
    
    def _check_wall_collision(self):
        """Check and handle wall collision with bounce"""
        for sprite in self.collision_sprites:
            if hasattr(sprite, 'rect') and self.hitbox_rect.colliderect(sprite.rect):
                # Simple bounce (reverse velocity)
                # Determine which side we hit
                overlap_x = min(self.hitbox_rect.right - sprite.rect.left,
                               sprite.rect.right - self.hitbox_rect.left)
                overlap_y = min(self.hitbox_rect.bottom - sprite.rect.top,
                               sprite.rect.bottom - self.hitbox_rect.top)
                
                if overlap_x < overlap_y:
                    # Hit from side
                    self.velocity.x *= -1
                else:
                    # Hit from top/bottom
                    self.velocity.y *= -1
                
                # Move out of collision
                if self.velocity.x < 0:
                    self.pos.x = sprite.rect.right + self.hitbox_rect.width // 2
                elif self.velocity.x > 0:
                    self.pos.x = sprite.rect.left - self.hitbox_rect.width // 2
                
                if self.velocity.y < 0:
                    self.pos.y = sprite.rect.bottom + self.hitbox_rect.height // 2
                elif self.velocity.y > 0:
                    self.pos.y = sprite.rect.top - self.hitbox_rect.height // 2
                
                return True
        
        return False
