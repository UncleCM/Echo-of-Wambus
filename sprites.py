from Settings import *
import math

class Tile(pygame.sprite.Sprite):
    def __init__(self, pos, surf, groups, scale=2, layer_name='Floor'):
        super().__init__(groups)
        self.layer_name = layer_name  # Track which layer this tile is from
        
        # Scale the tile
        scaled_size = (surf.get_width() * scale, surf.get_height() * scale)
        self.image = pygame.transform.scale(surf, scaled_size)
        # Adjust position for scaled tiles
        self.rect = self.image.get_rect(topleft=(pos[0] * scale, pos[1] * scale))

class CollisionSprite(pygame.sprite.Sprite):
    """Invisible collision boxes from object layer"""
    def __init__(self, pos, size, *groups):
        super().__init__(*groups)
        
        # Ensure position and size are integers
        pos = (int(round(pos[0])), int(round(pos[1])))
        size = (max(1, int(round(size[0]))), max(1, int(round(size[1]))))
        
        self.image = pygame.Surface(size)
        self.image.fill('red')
        self.image.set_alpha(0)  # Make invisible (change to 128 for debugging)
        self.rect = self.image.get_rect(topleft=pos)

class FallZone(pygame.sprite.Sprite):
    """Invisible fall zones that trigger game over"""
    def __init__(self, pos, size, *groups):
        super().__init__(*groups)
        
        # Ensure position and size are integers
        pos = (int(round(pos[0])), int(round(pos[1])))
        size = (max(1, int(round(size[0]))), max(1, int(round(size[1]))))
        
        self.image = pygame.Surface(size)
        self.image.fill('yellow')
        self.image.set_alpha(0)  # Make invisible (change to 128 for debugging)
        self.rect = self.image.get_rect(topleft=pos)

class Treasure(pygame.sprite.Sprite):
    """Gold treasure collectible - main objective"""
    def __init__(self, pos, groups):
        super().__init__(groups)
        
        # Create treasure visual (golden circle with glow effect)
        size = 32
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        
        # Draw golden treasure
        pygame.draw.circle(self.image, (255, 215, 0), (size//2, size//2), size//3)  # Gold
        pygame.draw.circle(self.image, (255, 255, 100), (size//2, size//2), size//4)  # Shine
        
        self.rect = self.image.get_rect(center=pos)
        self.hitbox_rect = self.rect.inflate(-8, -8)  # Smaller hitbox
        
        # Animation
        self.collected = False
        self.glow_timer = 0
        self.glow_speed = 3
    
    def update(self, dt):
        """Animate glowing effect"""
        if not self.collected:
            self.glow_timer += dt * self.glow_speed
            
            # Pulsing glow effect
            glow_alpha = int(128 + 127 * abs(math.sin(self.glow_timer)))
            
            # Redraw with pulsing glow
            size = 32
            self.image = pygame.Surface((size, size), pygame.SRCALPHA)
            
            # Outer glow
            glow_surf = pygame.Surface((size, size), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (255, 215, 0, glow_alpha), (size//2, size//2), size//2)
            self.image.blit(glow_surf, (0, 0))
            
            # Core treasure
            pygame.draw.circle(self.image, (255, 215, 0), (size//2, size//2), size//3)
            pygame.draw.circle(self.image, (255, 255, 100), (size//2, size//2), size//4)
    
    def collect(self):
        """Collect treasure and remove from world"""
        self.collected = True
        self.kill()
        print("[Treasure] Collected! Find the exit!")

class ExitPortal(pygame.sprite.Sprite):
    """Exit portal - only active after treasure collected"""
    def __init__(self, pos, groups):
        super().__init__(groups)
        
        self.locked = True
        size = 64
        
        # Create locked and unlocked images
        self.images = {
            'locked': self.create_portal_image(size, locked=True),
            'unlocked': self.create_portal_image(size, locked=False)
        }
        
        self.image = self.images['locked']
        self.rect = self.image.get_rect(center=pos)
        self.hitbox_rect = self.rect.inflate(-16, -16)
        
        # Animation
        self.glow_timer = 0
        self.glow_speed = 2
    
    def create_portal_image(self, size, locked=True):
        """Create portal visual"""
        surf = pygame.Surface((size, size), pygame.SRCALPHA)
        
        if locked:
            # Gray locked portal
            pygame.draw.circle(surf, (80, 80, 80), (size//2, size//2), size//3)
            pygame.draw.circle(surf, (50, 50, 50), (size//2, size//2), size//4)
        else:
            # Golden glowing unlocked portal
            pygame.draw.circle(surf, (255, 215, 0), (size//2, size//2), size//3)
            pygame.draw.circle(surf, (255, 255, 200), (size//2, size//2), size//4)
        
        return surf
    
    def unlock(self):
        """Activate exit after treasure collected"""
        if self.locked:
            self.locked = False
            self.image = self.images['unlocked']
            print("[Exit] Portal unlocked! Escape to win!")
    
    def update(self, dt):
        """Animate portal if unlocked"""
        if not self.locked:
            self.glow_timer += dt * self.glow_speed
            
            # Pulsing effect
            size = 64
            glow_alpha = int(128 + 127 * abs(math.sin(self.glow_timer)))
            
            # Redraw with pulsing glow
            self.image = pygame.Surface((size, size), pygame.SRCALPHA)
            
            # Outer glow
            glow_surf = pygame.Surface((size, size), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (255, 215, 0, glow_alpha), (size//2, size//2), size//2)
            self.image.blit(glow_surf, (0, 0))
            
            # Core portal
            pygame.draw.circle(self.image, (255, 215, 0), (size//2, size//2), size//3)
            pygame.draw.circle(self.image, (255, 255, 200), (size//2, size//2), size//4)

class Arrow(pygame.sprite.Sprite):
    """Arrow projectile shot by player"""
    def __init__(self, pos, direction, groups, collision_sprites):
        super().__init__(groups)
        
        from Settings import ARROW_SPEED, ARROW_MAX_DISTANCE
        
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

class ArrowPickup(pygame.sprite.Sprite):
    """Arrow pickup collectible - gives +1 arrow"""
    def __init__(self, pos, groups):
        super().__init__(groups)
        
        # Create quiver/arrow bundle visual
        size = 24
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        
        # Draw quiver icon (brown rectangle with arrows)
        pygame.draw.rect(self.image, (101, 67, 33), (4, 8, 16, 12))  # Quiver
        pygame.draw.line(self.image, (200, 200, 200), (8, 6), (8, 2), 2)  # Arrow 1
        pygame.draw.line(self.image, (200, 200, 200), (12, 6), (12, 2), 2)  # Arrow 2
        pygame.draw.line(self.image, (200, 200, 200), (16, 6), (16, 2), 2)  # Arrow 3
        
        self.base_image = self.image.copy()
        self.rect = self.image.get_rect(center=pos)
        self.hitbox_rect = self.rect.inflate(-8, -8)
        
        # Animation
        self.glow_timer = 0
        self.glow_speed = 3
    
    def update(self, dt):
        """Animate glow effect"""
        self.glow_timer += dt * self.glow_speed
        
        # Pulsing glow effect (cyan/blue)
        glow_alpha = int(128 + 127 * abs(math.sin(self.glow_timer)))
        
        # Recreate image with glow
        size = 24
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        
        # Outer glow
        glow_surf = pygame.Surface((size, size), pygame.SRCALPHA)
        pygame.draw.circle(glow_surf, (100, 200, 255, glow_alpha), (size//2, size//2), size//2)
        self.image.blit(glow_surf, (0, 0))
        
        # Draw quiver on top
        self.image.blit(self.base_image, (0, 0))


class Rock(pygame.sprite.Sprite):
    """Throwable rock projectile with physics"""
    def __init__(self, pos, direction, throw_power, groups, collision_sprites):
        super().__init__(groups)
        
        from Settings import ROCK_GRAVITY, ROCK_BOUNCE_DAMPING, ROCK_ROLLING_FRICTION, SOUND_LEVELS, SOUND_DURATIONS
        
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
                from Settings import SOUND_LEVELS, SOUND_DURATIONS
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


class RockPickup(pygame.sprite.Sprite):
    """Collectible rock"""
    def __init__(self, pos, groups):
        super().__init__(groups)
        
        # Visual
        size = 20
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        
        # Draw rock pile (3 gray rocks)
        pygame.draw.circle(self.image, (100, 100, 100), (7, 12), 6)
        pygame.draw.circle(self.image, (120, 120, 120), (13, 12), 6)
        pygame.draw.circle(self.image, (110, 110, 110), (10, 6), 5)
        
        # Highlights
        pygame.draw.circle(self.image, (150, 150, 150), (5, 10), 2)
        pygame.draw.circle(self.image, (150, 150, 150), (11, 10), 2)
        
        self.base_image = self.image.copy()
        self.rect = self.image.get_rect(center=pos)
        self.hitbox_rect = self.rect.inflate(-4, -4)
        
        # Animation
        self.glow_timer = 0
        self.glow_speed = 2
    
    def update(self, dt):
        """Animate subtle glow"""
        self.glow_timer += dt * self.glow_speed
        
        # Subtle pulsing (brown/orange glow)
        glow_alpha = int(64 + 63 * abs(math.sin(self.glow_timer)))
        
        # Recreate image with glow
        size = 20
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        
        # Glow effect
        glow_surf = pygame.Surface((size, size), pygame.SRCALPHA)
        pygame.draw.circle(glow_surf, (180, 140, 100, glow_alpha), (size//2, size//2), size//2)
        self.image.blit(glow_surf, (0, 0))
        
        # Draw rocks on top
        self.image.blit(self.base_image, (0, 0))
