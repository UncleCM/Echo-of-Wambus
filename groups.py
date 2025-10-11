from Settings import *

class AllSprites(pygame.sprite.Group):
    def __init__(self):
        super().__init__()
        self.offset = pygame.math.Vector2()
        self.display_surface = pygame.display.get_surface()
        self.half_w = self.display_surface.get_width() // 2
        self.half_h = self.display_surface.get_height() // 2
    
    def center_camera(self, target):
        """Center the camera on the target (player)"""
        self.offset.x = target.rect.centerx - self.half_w
        self.offset.y = target.rect.centery - self.half_h
    
    def draw(self, surface, target):
        """Draw all sprites with camera offset and Y-sorting"""
        # Update camera position
        self.center_camera(target)
        
        # Separate sprites into layers
        floor_tiles = []  # Always on bottom
        wall_tiles = []   # Y-sorted with player
        player_sprite = None
        other_sprites = []
        
        for sprite in self.sprites():
            sprite_class = sprite.__class__.__name__
            
            if sprite_class == 'Tile':
                # Check if it's a wall/pillar tile (needs Y-sorting) or floor tile
                if hasattr(sprite, 'layer_name'):
                    if sprite.layer_name in ['Wall', 'Pillar', 'Rock', 'Shadow']:
                        wall_tiles.append(sprite)
                    else:
                        floor_tiles.append(sprite)
                else:
                    floor_tiles.append(sprite)
            elif sprite_class == 'Player':
                player_sprite = sprite
            else:
                other_sprites.append(sprite)
        
        # Draw floor tiles first (always on bottom)
        for sprite in floor_tiles:
            offset_pos = sprite.rect.topleft - self.offset
            surface.blit(sprite.image, offset_pos)
        
        # Combine walls and player for Y-sorting (creates depth effect)
        y_sorted_sprites = wall_tiles + ([player_sprite] if player_sprite else []) + other_sprites
        
        # Sort by bottom Y position (sprites with lower Y drawn first)
        for sprite in sorted(y_sorted_sprites, key=lambda spr: spr.rect.bottom):
            offset_pos = sprite.rect.topleft - self.offset
            surface.blit(sprite.image, offset_pos)