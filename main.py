from Settings import *
from player import Player
from sprites import *

from random import randint

class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Echo of Wumpus")
        self.clock = pygame.time.Clock()
        self.running = True

        # Create sprite groups
        self.all_sprites = pygame.sprite.Group()
        self.collision_sprites = pygame.sprite.Group()
            
        # Create player and add to sprite group
        self.player = Player((WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2), self.all_sprites, self.collision_sprites)
        for i in range(6):
            x, y = randint(0, WINDOW_WIDTH), randint(0, WINDOW_HEIGHT)
            w, h = randint(60, 100), randint(50, 100)
            CollisionSprite((x, y), (w, h), self.all_sprites, self.collision_sprites)
        
        print(f"Player created at position: {self.player.rect.topleft}")
        print(f"Player size: {self.player.rect.size}")
        print(f"Number of animations loaded: {len(self.player.animations)}")

    def run(self):
        while self.running:
            # Delta time in seconds
            dt = self.clock.tick(60) / 1000.0

            # Event handling
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
            
            # Update all sprites
            self.all_sprites.update(dt)
            
            # Draw everything
            self.screen.fill((50, 50, 50))  # Gray background
            
            # # Debug: Draw a cyan rectangle behind player
            # debug_rect = self.player.rect.inflate(10, 10)
            # pygame.draw.rect(self.screen, (0, 255, 255), debug_rect)
            
            # # Debug: Draw a red rectangle where the player rect is
            # pygame.draw.rect(self.screen, (255, 0, 0), self.player.rect, 2)
            
            # Draw sprites
            self.all_sprites.draw(self.screen)
            
            # Debug info on screen
            font = pygame.font.Font(None, 36)
            debug_text = font.render(f"Player pos: {self.player.rect.topleft} | Animation: {self.player.current_animation}", True, (255, 255, 255))
            self.screen.blit(debug_text, (10, 10))

            # Update display
            pygame.display.update()

        pygame.quit()

if __name__ == "__main__":
    game = Game()
    game.run()