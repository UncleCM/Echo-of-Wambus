from Settings import *

class CollisionSprite(pygame.sprite.Sprite):
    def __init__(self, pos, size, all_sprites, collision_sprites):
        super().__init__(all_sprites, collision_sprites)
        self.image = pygame.Surface(size)
        self.image.fill('red')
        self.rect = self.image.get_rect(center = pos)
