from Settings import *
from player import Player
from wumpus import Wumpus
from sprites import *
from groups import AllSprites
from pytmx.util_pygame import load_pygame
from prolog_interface import PrologEngine
from sound_manager import SoundManager
from game_state import GameState
from ui import MenuScreens, get_pixel_font

from random import randint
import math


class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Echo of Wumpus")
        self.clock = pygame.time.Clock()
        self.running = True
        self.game_state = GameState.MAIN_MENU  # Start at main menu
        self.debug_mode = False
        self.game_start_time = None
        self.game_end_time = None
        self.death_reason = None  # Track how player died

        # Initialize sound manager
        self.sound_manager = SoundManager()

        # Initialize UI/Menu screens
        self.menu_screens = MenuScreens(self.screen)

        # Main menu state
        self.menu_selection = 0  # 0 = Start, 1 = Controls, 2 = Quit
        self.menu_options = ["START GAME", "CONTROLS", "QUIT"]

        # Initialize game variables (will be set when starting game)
        self.game_initialized = False

    def initialize_game(self):
        """Initialize/reset the game (called when starting a new game)"""
        # Treasure & Exit system
        self.has_treasure = False
        self.exit_unlocked = False
        self.time_limit = 180  # 3 minutes in seconds
        self.time_remaining = self.time_limit

        # Initialize Prolog engine (only once)
        if not hasattr(self, "prolog"):
            self.prolog = PrologEngine()
            print("✓ Prolog engine initialized")

        # Create sprite groups
        self.all_sprites = AllSprites()
        self.collision_sprites = pygame.sprite.Group()
        self.fall_sprites = pygame.sprite.Group()
        self.wumpus_sprites = pygame.sprite.Group()
        self.treasure_sprites = pygame.sprite.Group()
        self.exit_sprites = pygame.sprite.Group()
        self.arrow_sprites = pygame.sprite.Group()  # Flying arrows
        self.arrow_pickup_sprites = pygame.sprite.Group()  # Arrow pickups to collect

        self.setup()

        spawn_pos = self.find_spawn_position()
        # Create player and give it a reference to the Prolog engine (single instance)
        self.player = Player(
            spawn_pos,
            self.all_sprites,
            self.collision_sprites,
            self.prolog,
            self.sound_manager,
        )

        # Create Wumpus enemy
        wumpus_pos = self.find_wumpus_spawn()
        self.wumpus = Wumpus(
            wumpus_pos,
            [self.all_sprites, self.wumpus_sprites],
            self.collision_sprites,
            self.prolog,
        )
        # Set patrol points for Wumpus
        self.wumpus.patrol_points = [
            wumpus_pos,
            (wumpus_pos[0] + 200, wumpus_pos[1]),
            (wumpus_pos[0] + 200, wumpus_pos[1] + 200),
            (wumpus_pos[0], wumpus_pos[1] + 200),
        ]

        # Update Prolog with player position (use hitbox for accuracy)
        self.prolog.update_player_position(
            int(self.player.hitbox_rect.x), int(self.player.hitbox_rect.y)
        )

        # Spawn treasure, exit, and arrow pickups
        self.spawn_treasure()
        self.spawn_exit()
        self.spawn_arrow_pickups()

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
        print(
            f"Is safe position: {self.prolog.is_safe_position(self.player.rect.x, self.player.rect.y, self.player.hitbox_rect.width, self.player.hitbox_rect.height)}"
        )

        self.game_start_time = pygame.time.get_ticks()
        self.game_state = GameState.PLAYING
        self.game_initialized = True

    def find_spawn_position(self):
        """Find a safe spawn position for the player"""
        entrance_layer = None
        for layer in self.tmx_map.layers:
            if layer.name == "Entrance" and hasattr(layer, "data"):
                entrance_layer = layer
                break

        if entrance_layer:
            for x, y, surf in entrance_layer.tiles():
                spawn_x = (x * self.tmx_map.tilewidth * self.map_scale) + (
                    self.tmx_map.tilewidth * self.map_scale // 2
                )
                spawn_y = (y * self.tmx_map.tileheight * self.map_scale) + (
                    self.tmx_map.tileheight * self.map_scale // 2 + 100
                )
                print(
                    f"Found entrance at tile ({x}, {y}) -> world position ({spawn_x}, {spawn_y})"
                )
                return (spawn_x, spawn_y)

        map_center_x = (
            self.tmx_map.width * self.tmx_map.tilewidth * self.map_scale
        ) // 2
        map_center_y = (
            self.tmx_map.height * self.tmx_map.tileheight * self.map_scale
        ) // 2
        print(
            f"No entrance found, spawning at map center: ({map_center_x}, {map_center_y})"
        )
        return (map_center_x, map_center_y)

    def find_wumpus_spawn(self):
        """Find spawn position for Wumpus enemy"""
        # Look for 'WumpusSpawn' object layer in TMX
        for layer in self.tmx_map.layers:
            if (
                type(layer).__name__ == "TiledObjectGroup"
                and "wumpus" in layer.name.lower()
            ):
                if len(layer) > 0:
                    obj = layer[0]  # Take first spawn point
                    spawn_x = (obj.x * self.map_scale) + (
                        obj.width * self.map_scale // 2
                    )
                    spawn_y = (obj.y * self.map_scale) + (
                        obj.height * self.map_scale // 2
                    )
                    print(f"Found Wumpus spawn at ({spawn_x}, {spawn_y})")
                    return (spawn_x, spawn_y)

        # Fallback: spawn Wumpus far from player (bottom-right corner)
        fallback_x = (
            self.tmx_map.width * self.tmx_map.tilewidth * self.map_scale
        ) - 200
        fallback_y = (
            self.tmx_map.height * self.tmx_map.tileheight * self.map_scale
        ) - 200
        print(f"No Wumpus spawn found, using fallback: ({fallback_x}, {fallback_y})")
        return (fallback_x, fallback_y)

    def spawn_treasure(self):
        """Spawn treasure collectible"""
        from sprites import Treasure

        # Look for 'Treasure' object layer in TMX
        for layer in self.tmx_map.layers:
            if (
                type(layer).__name__ == "TiledObjectGroup"
                and "treasure" in layer.name.lower()
            ):
                if len(layer) > 0:
                    obj = layer[0]  # Take first treasure
                    treasure_x = (obj.x * self.map_scale) + (
                        obj.width * self.map_scale // 2
                    )
                    treasure_y = (obj.y * self.map_scale) + (
                        obj.height * self.map_scale // 2
                    )
                    treasure = Treasure(
                        (treasure_x, treasure_y),
                        [self.all_sprites, self.treasure_sprites],
                    )
                    print(f"Spawned treasure at ({treasure_x}, {treasure_y})")
                    return

        # Fallback: spawn treasure deep in cave (opposite corner from entrance)
        fallback_x = (
            self.tmx_map.width * self.tmx_map.tilewidth * self.map_scale
        ) - 300
        fallback_y = (
            self.tmx_map.height * self.tmx_map.tileheight * self.map_scale
        ) - 300
        treasure = Treasure(
            (fallback_x, fallback_y), [self.all_sprites, self.treasure_sprites]
        )
        print(f"No treasure spawn found, using fallback: ({fallback_x}, {fallback_y})")

    def spawn_exit(self):
        """Spawn exit portal at entrance"""
        from sprites import ExitPortal

        # Exit is at entrance position
        entrance_pos = self.find_spawn_position()
        exit_portal = ExitPortal(entrance_pos, [self.all_sprites, self.exit_sprites])
        print(f"Spawned exit portal at entrance: {entrance_pos}")

    def spawn_arrow_pickups(self):
        """Spawn arrow pickups in map (3 locations, no respawn)"""
        from sprites import ArrowPickup

        pickup_count = 0

        # Look for 'ArrowPickup' object layer in TMX
        for layer in self.tmx_map.layers:
            if (
                type(layer).__name__ == "TiledObjectGroup"
                and "arrow" in layer.name.lower()
            ):
                for obj in layer:
                    if pickup_count >= ARROW_PICKUP_COUNT:
                        break
                    pickup_x = (obj.x * self.map_scale) + (
                        obj.width * self.map_scale // 2
                    )
                    pickup_y = (obj.y * self.map_scale) + (
                        obj.height * self.map_scale // 2
                    )
                    pickup = ArrowPickup(
                        (pickup_x, pickup_y),
                        [self.all_sprites, self.arrow_pickup_sprites],
                    )
                    pickup_count += 1
                    print(
                        f"Spawned arrow pickup {pickup_count} at ({pickup_x}, {pickup_y})"
                    )

                if pickup_count >= ARROW_PICKUP_COUNT:
                    return

        # Fallback: spawn 3 arrow pickups at predefined locations
        map_width = self.tmx_map.width * self.tmx_map.tilewidth * self.map_scale
        map_height = self.tmx_map.height * self.tmx_map.tileheight * self.map_scale

        fallback_positions = [
            (map_width // 4, map_height // 3),  # Upper left area
            (map_width // 2, map_height * 2 // 3),  # Center-bottom
            (map_width * 3 // 4, map_height // 2),  # Right-middle
        ]

        for i, pos in enumerate(
            fallback_positions[: ARROW_PICKUP_COUNT - pickup_count]
        ):
            pickup = ArrowPickup(pos, [self.all_sprites, self.arrow_pickup_sprites])
            pickup_count += 1
            print(f"No arrow spawn found, using fallback {pickup_count}: {pos}")

    def setup(self):
        self.tmx_map = load_pygame(join("assets", "Map", "test_wall_size.tmx"))
        self.map_scale = 2

        print(f"Map size: {self.tmx_map.width} x {self.tmx_map.height}")
        print(f"Tile size: {self.tmx_map.tilewidth} x {self.tmx_map.tileheight}")

        # Load tile layers
        for layer in self.tmx_map.visible_layers:
            if hasattr(layer, "data"):
                for x, y, surf in layer.tiles():
                    pos = (x * self.tmx_map.tilewidth, y * self.tmx_map.tileheight)
                    Tile(pos, surf, self.all_sprites, self.map_scale, layer.name)

        # Load object layers and register with Prolog
        for layer in self.tmx_map.layers:
            if type(layer).__name__ == "TiledObjectGroup":

                if "collision" in layer.name.lower():
                    for obj in layer:
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        CollisionSprite(pos, size, self.collision_sprites)
                        # Register with Prolog
                        self.prolog.add_collision_box(
                            int(pos[0]), int(pos[1]), int(size[0]), int(size[1])
                        )

                if "fall" in layer.name.lower():
                    for obj in layer:
                        pos = (obj.x * self.map_scale, obj.y * self.map_scale)
                        size = (obj.width * self.map_scale, obj.height * self.map_scale)
                        FallZone(pos, size, self.fall_sprites)
                        # Register with Prolog
                        self.prolog.add_fall_zone(
                            int(pos[0]), int(pos[1]), int(size[0]), int(size[1])
                        )

        print(f"Collision sprites: {len(self.collision_sprites)}")
        print(f"Fall zones: {len(self.fall_sprites)}")

    def check_game_over(self):
        """Check if player fell into a fall zone using Prolog"""
        if pygame.time.get_ticks() - self.game_start_time < 500:
            return False

        if self.game_state == GameState.PLAYING:
            # Use Prolog to check if player should fall
            if self.prolog.check_fall(
                int(self.player.hitbox_rect.x),
                int(self.player.hitbox_rect.y),
                int(self.player.hitbox_rect.width),
                int(self.player.hitbox_rect.height),
                10,  # feet_height
            ):
                self.game_state = GameState.GAME_OVER
                self.game_end_time = pygame.time.get_ticks()
                self.death_reason = "Fell into a pit!"
                self.prolog.set_game_over(True)
                self.sound_manager.stop_footstep_loop()
                self.sound_manager.play_sound("game_over")
                print("GAME OVER - Fell into a hole! (Prolog detected)")
                return True
        return False

    def check_treasure_collection(self):
        """Check if player collects treasure"""
        if not self.has_treasure and len(self.treasure_sprites) > 0:
            # Check collision between player and treasure
            treasure = self.treasure_sprites.sprites()[0]
            player_rect = self.player.hitbox_rect
            treasure_rect = treasure.hitbox_rect

            if player_rect.colliderect(treasure_rect):
                self.has_treasure = True
                treasure.collect()

                # Unlock exit
                if len(self.exit_sprites) > 0:
                    exit_portal = self.exit_sprites.sprites()[0]
                    exit_portal.unlock()
                    self.exit_unlocked = True

                # Enrage Wumpus (50% speed boost)
                self.wumpus.speed *= 1.5
                print("[Game] Treasure collected! Wumpus is enraged! Find the exit!")

    def check_exit_reached(self):
        """Check if player reaches unlocked exit"""
        if self.has_treasure and self.exit_unlocked and len(self.exit_sprites) > 0:
            # Check collision between player and exit
            exit_portal = self.exit_sprites.sprites()[0]
            player_rect = self.player.hitbox_rect
            exit_rect = exit_portal.hitbox_rect

            if player_rect.colliderect(exit_rect):
                self.game_state = GameState.VICTORY
                self.game_end_time = pygame.time.get_ticks()
                print("VICTORY - Escaped with the treasure!")

    def handle_arrow_shooting(self):
        """Handle player shooting arrow (Space key pressed)"""
        if not self.player.is_alive:
            return

        # Try to shoot arrow
        arrow_direction = self.player.shoot_arrow()

        if arrow_direction is not None:
            # Create arrow sprite
            from sprites import Arrow

            arrow_pos = self.player.hitbox_rect.center
            arrow = Arrow(
                arrow_pos,
                arrow_direction,
                [self.all_sprites, self.arrow_sprites],
                self.collision_sprites,
            )
            print(f"[Arrow] Shot arrow from {arrow_pos} in direction {arrow_direction}")

    def check_arrow_hits(self):
        """Check if any arrows hit the Wumpus"""
        for arrow in self.arrow_sprites:
            if not self.wumpus.is_alive:
                continue

            # Method 1: Direct hitbox collision
            hit_by_collision = arrow.hitbox_rect.colliderect(self.wumpus.hitbox_rect)

            # Method 2: Distance-based detection (more forgiving)
            arrow_center = pygame.math.Vector2(arrow.rect.center)
            wumpus_center = pygame.math.Vector2(self.wumpus.hitbox_rect.center)
            distance = arrow_center.distance_to(wumpus_center)
            hit_by_distance = distance < 60  # Hit if within 60 pixels

            # Hit if either method detects collision
            if hit_by_collision or hit_by_distance:
                # Apply stun to Wumpus
                self.wumpus.apply_stun()

                # Remove arrow
                arrow.kill()
                print(
                    f"[Combat] Arrow hit Wumpus! Distance: {distance:.1f}px, Wumpus stunned for 3 seconds!"
                )

    def check_arrow_pickups(self):
        """Check if player collects arrow pickups"""
        for pickup in self.arrow_pickup_sprites:
            if self.player.hitbox_rect.colliderect(pickup.hitbox_rect):
                # Try to add arrow to player
                if self.player.add_arrows(1):
                    # Remove pickup permanently (no respawn)
                    pickup.kill()
                    print(
                        f"[Pickup] Collected arrow pickup! Total arrows: {self.player.arrows}/{self.player.max_arrows}"
                    )

    def check_player_attack(self):
        """DEPRECATED - Player now uses arrow combat system instead of melee"""
        pass

    def check_wumpus_attack(self):
        """Check if Wumpus's attack hits the Player"""
        # Only check if game is still playing
        if self.game_state != GameState.PLAYING:
            return

        # Calculate distance between Wumpus and Player
        player_pos = pygame.math.Vector2(self.player.hitbox_rect.center)
        wumpus_pos = pygame.math.Vector2(self.wumpus.hitbox_rect.center)
        distance = player_pos.distance_to(wumpus_pos)

        # Check if Player is in Wumpus attack range
        if distance <= self.wumpus.attack_range:
            damage = self.player.take_damage(self.wumpus.damage)
            print(
                f"[Combat] Wumpus hit Player for {damage} damage! Player HP: {self.player.health}/{self.player.max_health}"
            )

            # Check if Player died
            if not self.player.is_alive:
                self.game_state = GameState.GAME_OVER
                self.game_end_time = pygame.time.get_ticks()
                self.death_reason = "Defeated by the Wumpus!"
                self.sound_manager.stop_footstep_loop()
                self.sound_manager.play_sound("game_over")
                print("GAME OVER - Player defeated by Wumpus!")

    def cast_ray_to_wall(self, start_world_pos, angle_degrees, max_distance):
        """Cast a ray from start position and find distance to nearest wall.
        
        Args:
            start_world_pos: (x, y) tuple in world coordinates
            angle_degrees: angle in degrees (0 = right, 90 = down in pygame coords)
            max_distance: maximum distance to check
            
        Returns:
            distance to wall, or max_distance if no wall hit
        """
        # Convert angle to radians
        angle_rad = math.radians(angle_degrees)
        dx = math.cos(angle_rad)
        dy = math.sin(angle_rad)
        
        # Check along the ray in steps
        step_size = 8
        steps = int(max_distance / step_size)
        
        for step in range(1, steps + 1):
            distance = step * step_size
            check_x = start_world_pos[0] + dx * distance
            check_y = start_world_pos[1] + dy * distance
            
            # Create a small rect to check collision
            check_rect = pygame.Rect(check_x - 3, check_y - 3, 6, 6)
            
            # Check against all walls
            for wall in self.collision_sprites:
                if check_rect.colliderect(wall.rect):
                    return distance
        
        return max_distance

    def draw_flashlight(self):
        """Draw a directional flashlight beam with wall occlusion based on player facing direction."""
        darkness = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        darkness.fill((0, 0, 0, 240))  # darkness alpha 240–255 for cave

        beam_length = 450
        beam_angle = 50  # cone width in degrees
        player_screen_pos = self.player.rect.center - self.all_sprites.offset
        player_world_pos = self.player.rect.center
        px, py = player_screen_pos

        # --- Get base rotation angle based on facing direction ---
        facing = self.player.facing
        base_angle = 0

        # Map all 8 directions to angles (0° = right, 90° = down, 180° = left, 270° = up)
        if facing == "right":
            base_angle = 0
        elif facing == "right_down":
            base_angle = 45
        elif facing == "down":
            base_angle = 90
        elif facing == "left_down":
            base_angle = 135
        elif facing == "left":
            base_angle = 180
        elif facing == "left_up":
            base_angle = 225
        elif facing == "up":
            base_angle = 270
        elif facing == "right_up":
            base_angle = 315
        else:
            base_angle = 0  # Default to right

        # --- Cast rays to create light polygon with wall occlusion ---
        num_rays = 50  # Number of rays in the cone
        light_points = [player_screen_pos]  # Start with player position
        
        for i in range(num_rays + 1):
            # Calculate angle for this ray within the cone
            angle_ratio = i / num_rays  # 0.0 to 1.0
            angle_offset = (angle_ratio - 0.5) * beam_angle  # -25 to +25 degrees
            ray_angle = base_angle + angle_offset
            
            # Cast ray to find wall distance (in world coordinates)
            distance = self.cast_ray_to_wall(player_world_pos, ray_angle, beam_length)
            
            # Convert to screen coordinates
            angle_rad = math.radians(ray_angle)
            end_x = px + math.cos(angle_rad) * distance
            end_y = py + math.sin(angle_rad) * distance
            light_points.append((end_x, end_y))
        
        # --- Draw the light cone polygon ---
        if len(light_points) > 2:
            # Create a surface for the light
            light_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
            pygame.draw.polygon(light_surface, (255, 255, 200, 220), light_points, 0)
            
            # Subtract light from darkness
            darkness.blit(light_surface, (0, 0), special_flags=pygame.BLEND_RGBA_SUB)

        # --- Add subtle player glow (small, dim circle) ---
        glow_radius = 60
        glow_surface = pygame.Surface(
            (glow_radius * 2, glow_radius * 2), pygame.SRCALPHA
        )
        for r in range(glow_radius, 0, -4):
            alpha = max(0, 180 - (r / glow_radius) * 180)
            pygame.draw.circle(
                glow_surface,
                (255, 255, 200, int(alpha / 2)),
                (glow_radius, glow_radius),
                r,
            )
        darkness.blit(
            glow_surface,
            (px - glow_radius, py - glow_radius),
            special_flags=pygame.BLEND_RGBA_SUB,
        )

        # --- Draw final result ---
        self.screen.blit(darkness, (0, 0))

    def run(self):
        while self.running:
            dt = self.clock.tick(60) / 1000.0

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    self.running = False

                if event.type == pygame.KEYDOWN:
                    # Main Menu controls
                    if self.game_state == GameState.MAIN_MENU:
                        if event.key == pygame.K_UP:
                            self.sound_manager.play_sound("button", 0.3)
                            self.menu_selection = (self.menu_selection - 1) % len(
                                self.menu_options
                            )
                        elif event.key == pygame.K_DOWN:
                            self.sound_manager.play_sound("button", 0.3)
                            self.menu_selection = (self.menu_selection + 1) % len(
                                self.menu_options
                            )
                        elif (
                            event.key == pygame.K_RETURN or event.key == pygame.K_SPACE
                        ):
                            self.sound_manager.play_sound("button")
                            if self.menu_selection == 0:  # Start Game
                                self.initialize_game()
                            elif self.menu_selection == 1:  # Controls
                                self.game_state = GameState.CONTROLS
                            elif self.menu_selection == 2:  # Quit
                                self.running = False

                    # Controls screen
                    elif self.game_state == GameState.CONTROLS:
                        if event.key == pygame.K_ESCAPE:
                            self.sound_manager.play_sound("button")
                            self.game_state = GameState.MAIN_MENU

                    # In-game controls
                    elif self.game_state == GameState.PLAYING:
                        if event.key == pygame.K_ESCAPE:
                            self.game_state = GameState.MAIN_MENU
                        elif event.key == pygame.K_f:
                            self.debug_mode = not self.debug_mode
                            print(f"Debug mode: {self.debug_mode}")
                        elif event.key == pygame.K_SPACE:
                            self.handle_arrow_shooting()

                    # Game over / Victory screens
                    else:
                        if event.key == pygame.K_r:
                            self.initialize_game()
                        elif event.key == pygame.K_ESCAPE:
                            self.game_state = GameState.MAIN_MENU

            # Update game logic based on state
            if self.game_state == GameState.PLAYING and self.game_initialized:
                # Update time remaining
                elapsed_time = (
                    pygame.time.get_ticks() - self.game_start_time
                ) / 1000.0  # seconds
                self.time_remaining = self.time_limit - elapsed_time

                # Check timeout
                if self.time_remaining <= 0:
                    self.game_state = GameState.GAME_OVER
                    self.game_end_time = pygame.time.get_ticks()
                    self.death_reason = "Time's up!"
                    self.player.is_alive = False
                    self.sound_manager.stop_footstep_loop()
                    self.sound_manager.play_sound("game_over")
                    print("GAME OVER - Time's up!")
                else:
                    # Only update game if time hasn't run out
                    # Update Wumpus AI first (before sprite group update)
                    if self.wumpus.is_alive:
                        player_center = pygame.math.Vector2(
                            self.player.hitbox_rect.center
                        )
                        self.wumpus.ai_update(player_center, dt)

                    # Update all sprites (Player, Wumpus, Arrows movement/animation)
                    self.all_sprites.update(dt)
                    self.arrow_sprites.update(dt)  # Update flying arrows

                    # Check arrow collisions
                    self.check_arrow_hits()
                    self.check_arrow_pickups()

                    # Check Wumpus attack hit Player (only if not stunned)
                    if (
                        self.wumpus.ai_state == "attack"
                        and self.player.is_alive
                        and not self.wumpus.is_stunned
                    ):
                        self.check_wumpus_attack()

                    # Check treasure and exit
                    self.check_treasure_collection()
                    self.check_exit_reached()

                    # Update Prolog with current player position
                    self.prolog.update_player_position(
                        int(self.player.rect.x), int(self.player.rect.y)
                    )
                    self.check_game_over()

            # Rendering
            self.screen.fill((30, 30, 30))

            # Draw based on game state
            if self.game_state == GameState.MAIN_MENU:
                self.draw_main_menu()

            elif self.game_state == GameState.CONTROLS:
                self.draw_controls_screen()

            elif self.game_state == GameState.PLAYING and self.game_initialized:
                # Draw game world
                self.all_sprites.draw(self.screen, self.player)
                self.draw_flashlight()

                # Debug visualization (with camera offset) - only in PLAYING state
                if self.debug_mode and self.game_initialized:
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
                        screen_center = (
                            int(wumpus_center[0] - offset.x),
                            int(wumpus_center[1] - offset.y),
                        )
                        pygame.draw.circle(
                            self.screen,
                            (255, 100, 100),
                            screen_center,
                            self.wumpus.detection_range,
                            1,
                        )

                        # AI state text
                        font = get_pixel_font(24)
                        state_text = font.render(
                            f"AI: {self.wumpus.ai_state}", True, (255, 255, 255)
                        )
                        self.screen.blit(
                            state_text, (screen_center[0] - 40, screen_center[1] - 100)
                        )

                        # Wumpus health bar
                        health_percent = self.wumpus.health / self.wumpus.max_health
                        bar_width = 100
                        bar_height = 10
                        bar_x = screen_center[0] - bar_width // 2
                        bar_y = screen_center[1] - 120
                        # Background (red)
                        pygame.draw.rect(
                            self.screen,
                            (100, 0, 0),
                            (bar_x, bar_y, bar_width, bar_height),
                        )
                        # Health (green)
                        pygame.draw.rect(
                            self.screen,
                            (0, 255, 0),
                            (bar_x, bar_y, int(bar_width * health_percent), bar_height),
                        )
                        # Border
                        pygame.draw.rect(
                            self.screen,
                            (255, 255, 255),
                            (bar_x, bar_y, bar_width, bar_height),
                            1,
                        )

                    # Player health bar
                    player_center = self.player.hitbox_rect.center
                    screen_center = (
                        int(player_center[0] - offset.x),
                        int(player_center[1] - offset.y),
                    )
                    health_percent = self.player.health / self.player.max_health
                    bar_width = 100
                    bar_height = 10
                    bar_x = screen_center[0] - bar_width // 2
                    bar_y = screen_center[1] - 80
                    # Background (red)
                    pygame.draw.rect(
                        self.screen, (100, 0, 0), (bar_x, bar_y, bar_width, bar_height)
                    )
                    # Health (green)
                    pygame.draw.rect(
                        self.screen,
                        (0, 255, 0),
                        (bar_x, bar_y, int(bar_width * health_percent), bar_height),
                    )
                    # Border
                    pygame.draw.rect(
                        self.screen,
                        (255, 255, 255),
                        (bar_x, bar_y, bar_width, bar_height),
                        1,
                    )

                    feet_height = 10
                    feet_rect = pygame.Rect(
                        self.player.hitbox_rect.left,
                        self.player.hitbox_rect.bottom - feet_height,
                        self.player.hitbox_rect.width,
                        feet_height,
                    )
                    offset_feet = feet_rect.copy()
                    offset_feet.topleft -= offset
                    pygame.draw.rect(self.screen, (0, 255, 255), offset_feet, 3)

                    offset_player = self.player.rect.copy()
                    offset_player.topleft -= offset
                    pygame.draw.rect(self.screen, (0, 0, 255), offset_player, 2)

                # Draw HUD (Timer, arrows, treasure, and exit status)
                font = get_pixel_font(36)

                # Timer (top center)
                minutes = int(self.time_remaining // 60)
                seconds = int(self.time_remaining % 60)
                timer_color = (
                    (255, 255, 255) if self.time_remaining > 30 else (255, 100, 100)
                )  # Red if <30s
                timer_text = font.render(
                    f"Time: {minutes:02d}:{seconds:02d}", True, timer_color
                )
                timer_rect = timer_text.get_rect(center=(WINDOW_WIDTH // 2, 30))
                self.screen.blit(timer_text, timer_rect)

                # Arrow count (top left)
                arrow_color = (
                    (200, 200, 200) if self.player.arrows > 0 else (255, 100, 100)
                )
                arrow_text = font.render(
                    f"🏹 x{self.player.arrows}/{self.player.max_arrows}",
                    True,
                    arrow_color,
                )
                self.screen.blit(arrow_text, (20, 20))

                # Treasure status (top left, below arrows)
                treasure_status = "✓ Treasure" if self.has_treasure else "⬜ Treasure"
                treasure_color = (255, 215, 0) if self.has_treasure else (150, 150, 150)
                treasure_text = font.render(treasure_status, True, treasure_color)
                self.screen.blit(treasure_text, (20, 60))

                # Exit status (top left, below treasure)
                if self.has_treasure:
                    exit_status = (
                        "🔓 Exit Unlocked!" if self.exit_unlocked else "🔒 Exit"
                    )
                    exit_color = (0, 255, 0) if self.exit_unlocked else (150, 150, 150)
                    exit_text = font.render(exit_status, True, exit_color)
                    self.screen.blit(exit_text, (20, 100))

            # Draw game state screens
            if self.game_state == GameState.VICTORY:
                self.draw_victory_screen()
            elif self.game_state == GameState.GAME_OVER:
                self.draw_game_over_screen()
            elif self.debug_mode:
                # Debug info (only in PLAYING state)
                font = get_pixel_font(28)
                debug_text = font.render(
                    f"Player pos: {self.player.rect.topleft} | Animation: {self.player.current_animation}",
                    True,
                    (255, 255, 255),
                )
                self.screen.blit(debug_text, (10, 10))

                coll_count, fall_count = self.prolog.count_hazards()
                debug_text2 = font.render(
                    f"Prolog: Coll={coll_count} Falls={fall_count} | Press F",
                    True,
                    (255, 255, 255),
                )
                self.screen.blit(debug_text2, (10, 40))

            pygame.display.update()

        pygame.quit()

    def draw_main_menu(self):
        """Draw the main menu screen"""
        self.menu_screens.draw_main_menu(self.menu_selection, self.menu_options)

    def draw_controls_screen(self):
        """Draw the controls/how to play screen"""
        self.menu_screens.draw_controls_screen()

    def draw_victory_screen(self):
        """Draw victory screen with stats"""
        elapsed_time = (self.game_end_time - self.game_start_time) / 1000.0
        game_stats = {
            "elapsed_time": elapsed_time,
            "player_health": self.player.health,
            "player_max_health": self.player.max_health,
        }
        self.menu_screens.draw_victory_screen(game_stats)

    def draw_game_over_screen(self):
        """Draw game over screen"""
        elapsed_time = (self.game_end_time - self.game_start_time) / 1000.0
        game_stats = {
            "elapsed_time": elapsed_time,
            "death_reason": self.death_reason if self.death_reason else "You died!",
        }
        self.menu_screens.draw_game_over_screen(game_stats)

    def restart(self):
        """Restart the game by reinitializing"""
        print("[Game] Restarting...")
        self.initialize_game()


if __name__ == "__main__":
    game = Game()
    game.run()
