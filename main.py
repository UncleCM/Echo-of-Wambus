from Settings import *
from player import Player

class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Echo of Wumpus")
        self.clock = pygame.time.Clock()
        self.running = True

        self.player = Player((400, 300), (pygame.sprite.Group(), pygame.sprite.Group()))

    def run(self):
        while self.running:

            dt = self.clock.tick(60) / 1000.0

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
        
            pygame.display.update()

        pygame.quit()

if __name__ == "__main__":
    game = Game()
    game.run()