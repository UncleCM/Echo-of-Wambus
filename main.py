from Settings import *
from player import Player
from sprites import *
from groups import AllSprites
from pytmx.util_pygame import load_pygame

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
        self.debug_mode = False  # Toggle with 'F' key
        self.game_start_time = 0  # Track when game starts

        # Create sprite groups
        self.all_sprites = AllSprites()
        self.collision_sprites = pygame.sprite.Group()
        self.fall_sprites = pygame.sprite.Group()

        self.setup()
        
        # Find a safe spawn position (look for Entrance layer or use map center)
        spawn_pos = self.find_spawn_position()
        
        # Create player and add to sprite group
        self.player = Player(spawn_pos, self.all_sprites, self.collision_sprites)

        # Flashlight and darkness setup
        self.flashlight_on = True
        self.dark_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        self.dark_surface.fill((0, 0, 0))
        self.dark_surface.set_alpha(6000)  # adjust transparency for darkness
        
        print(f"\n=== COLLISION DEBUG ===")
        print(f"Total collision sprites: {len(self.collision_sprites)}")
        if len(self.collision_sprites) > 0:
            print(f"First collision sprite rect: {list(self.collision_sprites)[0].rect}")
            print(f"Player hitbox: {self.player.hitbox_rect}")
            print(f"Player has {len(self.player.collision_sprites)} collision sprites to check")
        else:
            print("WARNING: NO COLLISION SPRITES FOUND!")
        
        # Start the game timer
        self.game_start_time = pygame.time.get_ticks()

    def find_spawn_position(self):
        """Find a safe spawn position for the player"""
        # Try to find an Entrance tile
        entrance_layer = None
        for layer in self.tmx_map.layers:
            if layer.name == 'Entrance' and hasattr(layer, 'data'):
                entrance_layer = layer
                break
        
        if entrance_layer:
            # Find first entrance tile
            for x, y, surf in entrance_layer.tiles():
                spawn_x = (x * self.tmx_map.tilewidth * self.map_scale) + (self.tmx_map.tilewidth * self.map_scale // 2)
                spawn_y = (y * self.tmx_map.tileheight * self.map_scale) + (self.tmx_map.tileheight * self.map_scale // 2 + 100)
                print(f"Found entrance at tile ({x}, {y}) -> world position ({spawn_x}, {spawn_y})")
                return (spawn_x, spawn_y)
        
        # Fallback: spawn at map center
        map_center_x = (self.tmx_map.width * self.tmx_map.tilewidth * self.map_scale) // 2
        map_center_y = (self.tmx_map.height * self.tmx_map.tileheight * self.map_scale) // 2
        print(f"No entrance found, spawning at map center: ({map_center_x}, {map_center_y})")
        return (map_center_x, map_center_y)


    def setup(self):
        self.tmx_map = load_pygame(join('assets', 'Map', 'test_wall_size.tmx'))
        self.map_scale = 2
        
        print(f"Map size: {self.tmx_map.width} x {self.tmx_map.height}")
        print(f"Tile size: {self.tmx_map.tilewidth} x {self.tmx_map.tileheight}")
        
        # 1. LOAD TILE LAYERS (e.g., Floor, Wall)
        for layer in self.tmx_map.visible_layers:
            if hasattr(layer, 'data'):
                for x, y, surf in layer.tiles():
                    pos = (x * self.tmx_map.tilewidth, y * self.tmx_map.tileheight)
                    # Pass layer name so we know which tiles need Y-sorting
                    Tile(pos, surf, self.all_sprites, self.map_scale, layer.name)
                    
                    # OPTIONAL: If you still want Wall tiles to be collision sprites
                    # if layer.name == 'Wall':
                    #     collision_pos = (x * self.tmx_map.tilewidth * self.map_scale, 
                    #                 y * self.tmx_map.tileheight * self.map_scale)
                    #     collision_size = (self.tmx_map.tilewidth * self.map_scale, 
                    #                     self.tmx_map.tileheight * self.map_scale)
                    #     CollisionSprite(collision_pos, collision_size, self.collision_sprites)
        
        # 2. LOAD OBJECT LAYERS (e.g., Collision, Fall zones)
        for layer in self.tmx_map.layers:
            if type(layer).__name__ == 'TiledObjectGroup':
                
                # Load Collision boxes from object layer
                if 'collision' in layer.name.lower():
                    for obj in layer:
                        # Scale object position and size
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        
                        # Create the invisible collision sprite
                        CollisionSprite(pos, size, self.collision_sprites)
                
                # Load Fall zones from object layer
                if 'fall' in layer.name.lower():
                    for obj in layer:
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        FallZone(pos, size, self.fall_sprites)
        
        # 3. FINAL DEBUG PRINTS
        print(f"Collision sprites: {len(self.collision_sprites)}")
        print(f"Fall zones: {len(self.fall_sprites)}")

    def check_game_over(self):
        """Check if player fell into a fall zone"""
        # Don't check for the first 0.5 seconds (grace period at spawn)
        if pygame.time.get_ticks() - self.game_start_time < 500:
            return False
        
        if not self.game_over:
            # Create a small rect at the player's feet
            feet_height = 10  # Height of the feet collision area
            feet_rect = pygame.Rect(
                self.player.hitbox_rect.left,
                self.player.hitbox_rect.bottom - feet_height,
                self.player.hitbox_rect.width,
                feet_height
            )

            for fall_zone in self.fall_sprites:
                if fall_zone.rect.colliderect(feet_rect):
                    self.game_over = True
                    print("GAME OVER - Fell into a hole!")
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
            self.screen.fill((30, 30, 30))  # Dark background
            
            # Draw sprites with camera
            self.all_sprites.draw(self.screen, self.player)
            
            self.draw_flashlight()

            # Debug visualization (with camera offset)
            if self.debug_mode:
                offset = self.all_sprites.offset
                
                # Draw collision zones in RED
                for sprite in self.collision_sprites:
                    offset_rect = sprite.rect.copy()
                    offset_rect.topleft -= offset
                    pygame.draw.rect(self.screen, (255, 0, 0), offset_rect, 2)
                
                # Draw fall zones in YELLOW
                for sprite in self.fall_sprites:
                    offset_rect = sprite.rect.copy()
                    offset_rect.topleft -= offset
                    pygame.draw.rect(self.screen, (255, 255, 0), offset_rect, 2)
                
                # Draw player hitbox in GREEN
                offset_hitbox = self.player.hitbox_rect.copy()
                offset_hitbox.topleft -= offset
                pygame.draw.rect(self.screen, (0, 255, 0), offset_hitbox, 2)
                
                # Draw player feet hitbox in CYAN
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
                
                # Draw player rect in BLUE
                offset_player = self.player.rect.copy()
                offset_player.topleft -= offset
                pygame.draw.rect(self.screen, (0, 0, 255), offset_player, 2)
            
            # Debug info on screen (UI - no camera offset)
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