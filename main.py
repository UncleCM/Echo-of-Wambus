from Settings import *
from player import Player
from wumpus import Wumpus
from sprites import *
from groups import AllSprites
from pytmx.util_pygame import load_pygame
from prolog_interface import PrologEngine

from random import randint
import math

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
        self.wumpus_sprites = pygame.sprite.Group()

        self.setup()
        
        spawn_pos = self.find_spawn_position()
        # Create player and give it a reference to the Prolog engine (single instance)
        self.player = Player(spawn_pos, self.all_sprites, self.collision_sprites, self.prolog)
        
        # Create Wumpus enemy
        wumpus_pos = self.find_wumpus_spawn()
        self.wumpus = Wumpus(wumpus_pos, [self.all_sprites, self.wumpus_sprites], self.collision_sprites, self.prolog)
        # Set patrol points for Wumpus
        self.wumpus.patrol_points = [
            wumpus_pos,
            (wumpus_pos[0] + 200, wumpus_pos[1]),
            (wumpus_pos[0] + 200, wumpus_pos[1] + 200),
            (wumpus_pos[0], wumpus_pos[1] + 200),
        ]
        
        # Update Prolog with player position (use hitbox for accuracy)
        self.prolog.update_player_position(int(self.player.hitbox_rect.x), int(self.player.hitbox_rect.y))

        # Flashlight and darkness setup
        self.flashlight_on = True
        self.dark_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        self.dark_surface.fill((0, 0, 0))
        self.dark_surface.set_alpha(6000)  # adjust transparency for darkness
        
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
    
    def find_wumpus_spawn(self):
        """Find spawn position for Wumpus enemy"""
        # Look for 'WumpusSpawn' object layer in TMX
        for layer in self.tmx_map.layers:
            if type(layer).__name__ == 'TiledObjectGroup' and 'wumpus' in layer.name.lower():
                if len(layer) > 0:
                    obj = layer[0]  # Take first spawn point
                    spawn_x = (obj.x * self.map_scale) + (obj.width * self.map_scale // 2)
                    spawn_y = (obj.y * self.map_scale) + (obj.height * self.map_scale // 2)
                    print(f"Found Wumpus spawn at ({spawn_x}, {spawn_y})")
                    return (spawn_x, spawn_y)
        
        # Fallback: spawn Wumpus far from player (bottom-right corner)
        fallback_x = (self.tmx_map.width * self.tmx_map.tilewidth * self.map_scale) - 200
        fallback_y = (self.tmx_map.height * self.tmx_map.tileheight * self.map_scale) - 200
        print(f"No Wumpus spawn found, using fallback: ({fallback_x}, {fallback_y})")
        return (fallback_x, fallback_y)

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

    def draw_flashlight(self):
        """Draw a directional flashlight beam based on player facing direction."""
        darkness = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        darkness.fill((0, 0, 0, 240))  # darkness alpha 240–255 for cave

        beam_length = 450
        beam_angle = 50  # cone width
        light_color = (255, 255, 200)
        player_screen_pos = self.player.rect.center - self.all_sprites.offset
        px, py = player_screen_pos

        # --- Create cone beam surface ---
        beam_surface = pygame.Surface((beam_length * 2, beam_length * 2), pygame.SRCALPHA)
        cx, cy = beam_length, beam_length

        # Draw cone shape filled with light color
        cone_points = [(cx, cy)]
        for a in range(-beam_angle // 2, beam_angle // 2 + 1, 1):
            rad = math.radians(a)
            x = cx + math.cos(rad) * beam_length
            y = cy + math.sin(rad) * beam_length
            cone_points.append((x, y))
        pygame.draw.polygon(beam_surface, (255, 255, 200, 220), cone_points)

        # --- Add directional gradient inside the cone (fades outward) ---
        gradient = pygame.Surface((beam_length * 2, beam_length * 2), pygame.SRCALPHA)
        for i in range(beam_length):
            alpha = int(255 * (1 - (i / beam_length)))
            pygame.draw.line(gradient, (255, 255, 180, alpha // 3), (cx, cy), (cx + i, cy), 3)
        beam_surface.blit(gradient, (0, 0), special_flags=pygame.BLEND_RGBA_ADD)

        # --- Rotate cone based on facing direction ---
        facing = self.player.facing
        rotation = 0
        if "up" in facing:
            rotation = 90
        elif "down" in facing:
            rotation = -90
        elif "left" in facing:
            rotation = 180
        elif "right" in facing:
            rotation = 0

        rotated_beam = pygame.transform.rotate(beam_surface, rotation)
        beam_rect = rotated_beam.get_rect(center=(px, py))

        # --- Subtract light cone from darkness ---
        darkness.blit(rotated_beam, beam_rect, special_flags=pygame.BLEND_RGBA_SUB)

        # --- Add subtle player glow (small, dim circle) ---
        glow_radius = 60
        glow_surface = pygame.Surface((glow_radius * 2, glow_radius * 2), pygame.SRCALPHA)
        for r in range(glow_radius, 0, -4):
            alpha = max(0, 180 - (r / glow_radius) * 180)
            pygame.draw.circle(glow_surface, (255, 255, 200, int(alpha / 2)), (glow_radius, glow_radius), r)
        darkness.blit(glow_surface, (px - glow_radius, py - glow_radius), special_flags=pygame.BLEND_RGBA_SUB)

        # --- Draw final result ---
        self.screen.blit(darkness, (0, 0))




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
                # Update Wumpus AI first (before sprite group update)
                if self.wumpus.is_alive:
                    player_center = pygame.math.Vector2(self.player.hitbox_rect.center)
                    self.wumpus.ai_update(player_center, dt)
                
                # Update all sprites (Player and Wumpus movement/animation)
                self.all_sprites.update(dt)
                
                # Update Prolog with current player position
                self.prolog.update_player_position(int(self.player.rect.x), int(self.player.rect.y))
                self.check_game_over()
            
            self.screen.fill((30, 30, 30))
            self.all_sprites.draw(self.screen, self.player)
            
            self.draw_flashlight()

            # Debug visualization (with camera offset)
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
                
                # Draw Wumpus hitbox and detection range
                if self.wumpus.is_alive:
                    wumpus_hitbox = self.wumpus.hitbox_rect.copy()
                    wumpus_hitbox.topleft -= offset
                    pygame.draw.rect(self.screen, (255, 0, 255), wumpus_hitbox, 2)
                    
                    # Detection range circle
                    wumpus_center = self.wumpus.hitbox_rect.center
                    screen_center = (int(wumpus_center[0] - offset.x), int(wumpus_center[1] - offset.y))
                    pygame.draw.circle(self.screen, (255, 100, 100), screen_center, self.wumpus.detection_range, 1)
                    
                    # AI state text
                    font = pygame.font.Font(None, 24)
                    state_text = font.render(f"AI: {self.wumpus.ai_state}", True, (255, 255, 255))
                    self.screen.blit(state_text, (screen_center[0] - 40, screen_center[1] - 100))
                
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