"""
Base sprite classes for tiles and collision detection
"""

from Settings import *


class Tile(pygame.sprite.Sprite):
    """Tile sprite for rendering map layers"""
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
