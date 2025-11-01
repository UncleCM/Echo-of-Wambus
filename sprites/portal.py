"""
Exit portal sprite
"""

from Settings import *
import math


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
