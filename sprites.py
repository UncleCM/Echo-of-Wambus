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