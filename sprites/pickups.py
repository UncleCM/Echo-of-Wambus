"""
Pickup sprites (ArrowPickup and RockPickup)
"""

from Settings import *
import math


class ArrowPickup(pygame.sprite.Sprite):
    """Arrow pickup collectible - gives +1 arrow"""
    def __init__(self, pos, groups, pickup_id=None):
        super().__init__(groups)
        
        self.pickup_id = pickup_id  # For Prolog tracking
        
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


class RockPickup(pygame.sprite.Sprite):
    """Collectible rock"""
    def __init__(self, pos, groups, pickup_id=None):
        super().__init__(groups)
        
        self.pickup_id = pickup_id  # For Prolog tracking
        
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
