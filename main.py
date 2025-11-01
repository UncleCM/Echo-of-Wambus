from Settings import *
from player import Player
from wumpus import Wumpus
from sprites import *
from groups import AllSprites
from pytmx.util_pygame import load_pygame
from prolog_interface import PrologEngine

from random import randint
import math
from enum import Enum


class GameState(Enum):
    """Game states for win/lose conditions"""

    PLAYING = "playing"
    VICTORY = "victory"
    GAME_OVER = "game_over"


class Game:
    def __init__(self):
        pygame.init()
        self.screen = pygame.display.set_mode((WINDOW_WIDTH, WINDOW_HEIGHT))
        pygame.display.set_caption("Echo of Wumpus")
        self.clock = pygame.time.Clock()
        self.running = True
        self.game_state = GameState.PLAYING
        self.debug_mode = False
        self.game_start_time = pygame.time.get_ticks()
        self.game_end_time = None
        self.death_reason = None  # Track how player died

        # Treasure & Exit system
        self.has_treasure = False
        self.exit_unlocked = False
        self.time_limit = 180  # 3 minutes in seconds
        self.time_remaining = self.time_limit

        # Initialize Prolog engine
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
            spawn_pos, self.all_sprites, self.collision_sprites, self.prolog
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
                print("GAME OVER - Player defeated by Wumpus!")

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
        beam_surface = pygame.Surface(
            (beam_length * 2, beam_length * 2), pygame.SRCALPHA
        )
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
            pygame.draw.line(
                gradient, (255, 255, 180, alpha // 3), (cx, cy), (cx + i, cy), 3
            )
        beam_surface.blit(gradient, (0, 0), special_flags=pygame.BLEND_RGBA_ADD)

        # --- Rotate cone based on facing direction (supports 8 directions) ---
        facing = self.player.facing
        rotation = 0

        # Map all 8 directions to rotation angles
        # 0° = right, 90° = up, 180° = left, 270° = down
        if facing == "right":
            rotation = 0
        elif facing == "right_up":
            rotation = 45
        elif facing == "up":
            rotation = 90
        elif facing == "left_up":
            rotation = 135
        elif facing == "left":
            rotation = 180
        elif facing == "left_down":
            rotation = 225
        elif facing == "down":
            rotation = 270
        elif facing == "right_down":
            rotation = 315
        else:
            rotation = 0  # Default to right if unknown

        rotated_beam = pygame.transform.rotate(beam_surface, rotation)
        beam_rect = rotated_beam.get_rect(center=(px, py))

        # --- Subtract light cone from darkness ---
        darkness.blit(rotated_beam, beam_rect, special_flags=pygame.BLEND_RGBA_SUB)

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
                    if event.key == pygame.K_r and self.game_state != GameState.PLAYING:
                        self.restart()
                    if event.key == pygame.K_f:
                        self.debug_mode = not self.debug_mode
                        print(f"Debug mode: {self.debug_mode}")

                    # Arrow shooting (Space key)
                    if (
                        event.key == pygame.K_SPACE
                        and self.game_state == GameState.PLAYING
                    ):
                        self.handle_arrow_shooting()

            if self.game_state == GameState.PLAYING:
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
                    font = pygame.font.Font(None, 24)
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
            if self.game_state == GameState.PLAYING:
                font = pygame.font.Font(None, 36)

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
                font = pygame.font.Font(None, 36)
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
                self.screen.blit(debug_text2, (10, 50))

            pygame.display.update()

        pygame.quit()

    def draw_victory_screen(self):
        """Draw victory screen with stats"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 180))
        self.screen.blit(overlay, (0, 0))

        # Calculate game stats
        elapsed_time = (
            self.game_end_time - self.game_start_time
        ) / 1000.0  # Convert to seconds
        minutes = int(elapsed_time // 60)
        seconds = int(elapsed_time % 60)

        # Fonts
        title_font = pygame.font.Font(None, 96)
        font = pygame.font.Font(None, 48)
        small_font = pygame.font.Font(None, 36)

        # Title
        title_text = title_font.render("VICTORY!", True, (255, 215, 0))
        title_rect = title_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 150)
        )
        self.screen.blit(title_text, title_rect)

        # Success message
        success_text = font.render("Escaped with the treasure!", True, (100, 255, 100))
        success_rect = success_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 80)
        )
        self.screen.blit(success_text, success_rect)

        # Stats
        time_text = small_font.render(
            f"Escape Time: {minutes:02d}:{seconds:02d}", True, (255, 255, 255)
        )
        time_rect = time_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 10)
        )
        self.screen.blit(time_text, time_rect)

        health_text = font.render(
            f"Health Remaining: {self.player.health}/{self.player.max_health}",
            True,
            (0, 255, 0),
        )
        health_rect = health_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 20)
        )
        self.screen.blit(health_text, health_rect)

        # Restart prompt
        restart_text = small_font.render("Press R to Restart", True, (200, 200, 200))
        restart_rect = restart_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 120)
        )
        self.screen.blit(restart_text, restart_rect)

    def draw_game_over_screen(self):
        """Draw game over screen"""
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 180))
        self.screen.blit(overlay, (0, 0))

        # Calculate game stats
        elapsed_time = (
            self.game_end_time - self.game_start_time
        ) / 1000.0  # Convert to seconds
        minutes = int(elapsed_time // 60)
        seconds = int(elapsed_time % 60)

        # Fonts
        title_font = pygame.font.Font(None, 96)
        font = pygame.font.Font(None, 48)
        small_font = pygame.font.Font(None, 36)

        # Title
        title_text = title_font.render("GAME OVER", True, (255, 0, 0))
        title_rect = title_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 150)
        )
        self.screen.blit(title_text, title_rect)

        # Death message
        death_message = self.death_reason if self.death_reason else "You died!"
        death_text = font.render(death_message, True, (255, 100, 100))
        death_rect = death_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 50)
        )
        self.screen.blit(death_text, death_rect)

        # Stats
        time_text = small_font.render(
            f"Survived: {minutes:02d}:{seconds:02d}", True, (200, 200, 200)
        )
        time_rect = time_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 20)
        )
        self.screen.blit(time_text, time_rect)

        # Restart prompt
        restart_text = small_font.render("Press R to Restart", True, (200, 200, 200))
        restart_rect = restart_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 120)
        )
        self.screen.blit(restart_text, restart_rect)

    def restart(self):
        """Restart the game by reinitializing"""
        print("[Game] Restarting...")
        self.__init__()


if __name__ == "__main__":
    game = Game()
    game.run()
