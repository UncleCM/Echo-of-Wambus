from Settings import *
from player import Player
from sprites import *
from groups import AllSprites
from pytmx.util_pygame import load_pygame
from prolog_interface import PrologEngine

from random import randint

class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Echo of Wumpus")
        self.clock = pygame.time.Clock()
        self.running = True
        self.game_over = False
        self.debug_mode = False
        self.game_start_time = 0

        # Initialize Prolog engine
        self.prolog = PrologEngine()
        print("✓ Prolog engine initialized")

        # Create sprite groups
        self.all_sprites = AllSprites()
        self.collision_sprites = pygame.sprite.Group()
        self.fall_sprites = pygame.sprite.Group()

        self.setup()
        
        spawn_pos = self.find_spawn_position()
        self.player = Player(spawn_pos, self.all_sprites, self.collision_sprites, self.prolog)
        
        # Update Prolog with player position
        self.prolog.update_player_position(self.player.rect.x, self.player.rect.y)
        
        print(f"\n=== PROLOG GAME STATE ===")
        coll_count, fall_count = self.prolog.count_hazards()
        print(f"Collision boxes in Prolog: {coll_count}")
        print(f"Fall zones in Prolog: {fall_count}")
        print(f"Player position: ({self.player.rect.x}, {self.player.rect.y})")
        print(f"Is safe position: {self.prolog.is_safe_position(self.player.rect.x, self.player.rect.y, self.player.hitbox_rect.width, self.player.hitbox_rect.height)}")
        
        self.game_start_time = pygame.time.get_ticks()

    def find_spawn_position(self):
        """Find a safe spawn position for the player"""
        entrance_layer = None
        for layer in self.tmx_map.layers:
            if layer.name == 'Entrance' and hasattr(layer, 'data'):
                entrance_layer = layer
                break
        
        if entrance_layer:
            for x, y, surf in entrance_layer.tiles():
                spawn_x = (x * self.tmx_map.tilewidth * self.map_scale) + (self.tmx_map.tilewidth * self.map_scale // 2)
                spawn_y = (y * self.tmx_map.tileheight * self.map_scale) + (self.tmx_map.tileheight * self.map_scale // 2 + 100)
                print(f"Found entrance at tile ({x}, {y}) -> world position ({spawn_x}, {spawn_y})")
                return (spawn_x, spawn_y)
        
        map_center_x = (self.tmx_map.width * self.tmx_map.tilewidth * self.map_scale) // 2
        map_center_y = (self.tmx_map.height * self.tmx_map.tileheight * self.map_scale) // 2
        print(f"No entrance found, spawning at map center: ({map_center_x}, {map_center_y})")
        return (map_center_x, map_center_y)

    def setup(self):
        self.tmx_map = load_pygame(join('assets', 'Map', 'test_wall_size.tmx'))
        self.map_scale = 2
        
        print(f"Map size: {self.tmx_map.width} x {self.tmx_map.height}")
        print(f"Tile size: {self.tmx_map.tilewidth} x {self.tmx_map.tileheight}")
        
        # Load tile layers
        for layer in self.tmx_map.visible_layers:
            if hasattr(layer, 'data'):
                for x, y, surf in layer.tiles():
                    pos = (x * self.tmx_map.tilewidth, y * self.tmx_map.tileheight)
                    Tile(pos, surf, self.all_sprites, self.map_scale, layer.name)
        
        # Load object layers and register with Prolog
        for layer in self.tmx_map.layers:
            if type(layer).__name__ == 'TiledObjectGroup':
                
                if 'collision' in layer.name.lower():
                    for obj in layer:
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        CollisionSprite(pos, size, self.collision_sprites)
                        # Register with Prolog
                        self.prolog.add_collision_box(int(pos[0]), int(pos[1]), int(size[0]), int(size[1]))
                
                if 'fall' in layer.name.lower():
                    for obj in layer:
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        FallZone(pos, size, self.fall_sprites)
                        # Register with Prolog
                        self.prolog.add_fall_zone(int(pos[0]), int(pos[1]), int(size[0]), int(size[1]))
        
        print(f"Collision sprites: {len(self.collision_sprites)}")
        print(f"Fall zones: {len(self.fall_sprites)}")

    def check_game_over(self):
        """Check if player fell into a fall zone using Prolog"""
        if pygame.time.get_ticks() - self.game_start_time < 500:
            return False
        
        if not self.game_over:
            # Use Prolog to check if player should fall
            if self.prolog.check_fall(
                int(self.player.hitbox_rect.x),
                int(self.player.hitbox_rect.y),
                int(self.player.hitbox_rect.width),
                int(self.player.hitbox_rect.height),
                10  # feet_height
            ):
                self.game_over = True
                self.prolog.set_game_over(True)
                print("GAME OVER - Fell into a hole! (Prolog detected)")
                return True
        return False

    def run(self):
        while self.running:
            dt = self.clock.tick(60) / 1000.0

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_r and self.game_over:
                        self.__init__()
                    if event.key == pygame.K_f:
                        self.debug_mode = not self.debug_mode
                        print(f"Debug mode: {self.debug_mode}")
            
            if not self.game_over:
                self.all_sprites.update(dt)
                # Update Prolog with current player position
                self.prolog.update_player_position(int(self.player.rect.x), int(self.player.rect.y))
                self.check_game_over()
            
            self.screen.fill((30, 30, 30))
            self.all_sprites.draw(self.screen, self.player)
            
            if self.debug_mode:
                offset = self.all_sprites.offset
                
                for sprite in self.collision_sprites:
                    offset_rect = sprite.rect.copy()
                    offset_rect.topleft -= offset
                    pygame.draw.rect(self.screen, (255, 0, 0), offset_rect, 2)
                
                for sprite in self.fall_sprites:
                    offset_rect = sprite.rect.copy()
                    offset_rect.topleft -= offset
                    pygame.draw.rect(self.screen, (255, 255, 0), offset_rect, 2)
                
                offset_hitbox = self.player.hitbox_rect.copy()
                offset_hitbox.topleft -= offset
                pygame.draw.rect(self.screen, (0, 255, 0), offset_hitbox, 2)
                
                feet_height = 10
                feet_rect = pygame.Rect(
                    self.player.hitbox_rect.left,
                    self.player.hitbox_rect.bottom - feet_height,
                    self.player.hitbox_rect.width,
                    feet_height
                )
                offset_feet = feet_rect.copy()
                offset_feet.topleft -= offset
                pygame.draw.rect(self.screen, (0, 255, 255), offset_feet, 3)
                
                offset_player = self.player.rect.copy()
                offset_player.topleft -= offset
                pygame.draw.rect(self.screen, (0, 0, 255), offset_player, 2)
            
            font = pygame.font.Font(None, 36)
            if self.game_over:
                game_over_text = font.render("GAME OVER! Press R to Restart", True, (255, 0, 0))
                text_rect = game_over_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2))
                self.screen.blit(game_over_text, text_rect)
            else:
                debug_text = font.render(f"Player pos: {self.player.rect.topleft} | Animation: {self.player.current_animation}", True, (255, 255, 255))
                self.screen.blit(debug_text, (10, 10))
                
                if self.debug_mode:
                    coll_count, fall_count = self.prolog.count_hazards()
                    debug_text2 = font.render(f"Prolog: Coll={coll_count} Falls={fall_count} | Press F", True, (255, 255, 255))
                    self.screen.blit(debug_text2, (10, 50))

            pygame.display.update()

        pygame.quit()

if __name__ == "__main__":
    game = Game()
    game.run()