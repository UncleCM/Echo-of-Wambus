from Settings import *
from player import Player
from sprites import *
from pytmx.util_pygame import load_pygame

from random import randint

class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Echo of Wumpus")
        self.clock = pygame.time.Clock()
        self.running = True
        self.game_over = False
        self.debug_mode = True  # Toggle with 'F' key

        # Create sprite groups
        self.all_sprites = pygame.sprite.Group()
        self.collision_sprites = pygame.sprite.Group()
        self.fall_sprites = pygame.sprite.Group()  # For fall zones

        self.setup()
            
        # Create player and add to sprite group
        self.player = Player((WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2), self.all_sprites, self.collision_sprites)
        
        print(f"Player created at position: {self.player.rect.topleft}")
        print(f"Player size: {self.player.rect.size}")
        print(f"Player hitbox: {self.player.hitbox_rect}")
        print(f"Number of animations loaded: {len(self.player.animations)}")
        print(f"Total collision sprites: {len(self.collision_sprites)}")
        print(f"Total fall zones: {len(self.fall_sprites)}")

    def setup(self):
        # Load the TMX map
        self.tmx_map = load_pygame(join('assets', 'Map', 'test_wall_size.tmx'))
        self.map_scale = 2  # Adjust this for map size
        
        # Print map info for debugging
        print(f"Map size: {self.tmx_map.width} x {self.tmx_map.height}")
        print(f"Tile size: {self.tmx_map.tilewidth} x {self.tmx_map.tileheight}")
        print(f"All Layers: {[layer.name for layer in self.tmx_map.layers]}")
        
        # Load tile layers (visual only)
        for layer in self.tmx_map.visible_layers:
            if hasattr(layer, 'data'):  # Tile layer
                for x, y, surf in layer.tiles():
                    pos = (x * self.tmx_map.tilewidth, y * self.tmx_map.tileheight)
                    Tile(pos, surf, self.all_sprites, self.map_scale)
        
        # Load object layers - Use type checking instead
        for layer in self.tmx_map.layers:
            # Check if it's an object layer by type name
            if type(layer).__name__ == 'TiledObjectGroup':
                print(f"Found object layer: '{layer.name}' with {len(layer)} objects")
                
                # Use case-insensitive comparison
                layer_name_lower = layer.name.lower()
                
                if 'collision' in layer_name_lower:
                    # Create collision sprites from objects
                    print(f"  -> Creating collision sprites")
                    for obj in layer:
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        CollisionSprite(pos, size, self.collision_sprites)
                        print(f"     Collision at {pos} size {size}")
                
                elif 'fall' in layer_name_lower:
                    # Create fall zone sprites from objects
                    print(f"  -> Creating fall zones")
                    for obj in layer:
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        FallZone(pos, size, self.fall_sprites)
                        print(f"     Fall zone at {pos} size {size}")
        
        print(f"\nFinal counts:")
        print(f"  Collision sprites created: {len(self.collision_sprites)}")
        print(f"  Fall zones created: {len(self.fall_sprites)}")
    def check_game_over(self):
        """Check if player fell into a fall zone"""
        if not self.game_over:
            for fall_zone in self.fall_sprites:
                if self.player.hitbox_rect.colliderect(fall_zone.rect):
                    self.game_over = True
                    print("GAME OVER - Fell into a hole!")
                    return True
        return False

    def run(self):
        while self.running:
            # Delta time in seconds
            dt = self.clock.tick(60) / 1000.0

            # Event handling
            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_r and self.game_over:
                        # Restart game
                        self.__init__()
                    if event.key == pygame.K_f:
                        # Toggle debug mode
                        self.debug_mode = not self.debug_mode
                        print(f"Debug mode: {self.debug_mode}")
            
            # Update all sprites (only if not game over)
            if not self.game_over:
                self.all_sprites.update(dt)
                self.check_game_over()
            
            # Draw everything
            self.screen.fill((50, 50, 50))  # Gray background
            
            # Draw sprites
            self.all_sprites.draw(self.screen)
            
            # Debug visualization
            if self.debug_mode:
                # Draw collision zones in RED
                for sprite in self.collision_sprites:
                    pygame.draw.rect(self.screen, (255, 0, 0), sprite.rect, 2)
                
                # Draw fall zones in YELLOW
                for sprite in self.fall_sprites:
                    pygame.draw.rect(self.screen, (255, 255, 0), sprite.rect, 2)
                
                # Draw player hitbox in GREEN
                pygame.draw.rect(self.screen, (0, 255, 0), self.player.hitbox_rect, 2)
                
                # Draw player rect in BLUE
                pygame.draw.rect(self.screen, (0, 0, 255), self.player.rect, 2)
            
            # Debug info on screen
            font = pygame.font.Font(None, 36)
            if self.game_over:
                game_over_text = font.render("GAME OVER! Press R to Restart", True, (255, 0, 0))
                text_rect = game_over_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
                self.screen.blit(game_over_text, text_rect)
            else:
                debug_text = font.render(f"Player pos: {self.player.rect.topleft} | Animation: {self.player.current_animation}", True, (255, 255, 255))
                self.screen.blit(debug_text, (10, 10))
                
                if self.debug_mode:
                    debug_text2 = font.render(f"Collisions: {len(self.collision_sprites)} | Falls: {len(self.fall_sprites)} | Press F to toggle", True, (255, 255, 255))
                    self.screen.blit(debug_text2, (10, 50))

            # Update display
            pygame.display.update()

        pygame.quit()

if __name__ == "__main__":
    game = Game()
    game.run()