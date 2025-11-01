"""
Core Game Module
Main game loop, state management, and game initialization
Uses SpawnSystem for entity spawning
"""

from Settings import *
from player import Player
from wumpus import Wumpus
from sprites import *
from groups import AllSprites
from pytmx.util_pygame import load_pygame
from prolog_interface import PrologEngine
from sound_system import SoundManager
from map_knowledge import MapKnowledge
from game_state import GameState
from ui import MenuScreens, get_pixel_font
from lighting import FlashlightSystem
from config import GameplayConfig, MenuConfig, WorldConfig
from fade_transition import FadeTransition
from systems.spawn_system import SpawnSystem

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
        self.menu_options = MenuConfig.MENU_OPTIONS

        # Initialize game variables (will be set when starting game)
        self.game_initialized = False

        # Initialize fade transition system
        self.fade = FadeTransition(fade_speed=10)
        self.pending_state = None  # State to transition to after fade

        # Start menu music
        self.sound_manager.play_menu_music()

    def initialize_game(self):
        """Initialize/reset the game (called when starting a new game)"""
        # Stop menu music and start in-game music
        self.sound_manager.stop_music()
        self.sound_manager.play_ingame_music()

        # Treasure & Exit system
        self.has_treasure = False
        self.exit_unlocked = False
        self.time_limit = GameplayConfig.TIME_LIMIT
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
        self.rock_sprites = pygame.sprite.Group()  # Flying rocks
        self.rock_pickup_sprites = pygame.sprite.Group()  # Rock pickups to collect
        
        # Sound system for stealth gameplay
        self.sound_manager = SoundManager()

        self.setup()
        
        # Initialize SpawnSystem (after setup to load tmx_map first)
        self.spawn_system = SpawnSystem(self.tmx_map, self.map_scale, self.prolog)
        
        # Map knowledge system (after setup to load tmx_map first)
        self.map_knowledge = MapKnowledge(
            self.tmx_map, 
            self.collision_sprites, 
            self.fall_sprites
        )

        # Spawn player using SpawnSystem
        spawn_pos = self.spawn_system.find_player_spawn()
        # Create player and give it a reference to the Prolog engine (single instance)
        self.player = Player(
            spawn_pos, self.all_sprites, self.collision_sprites, self.prolog, self.sound_manager
        )

        # Spawn Wumpus using SpawnSystem
        wumpus_pos = self.spawn_system.find_wumpus_spawn()
        self.wumpus = Wumpus(
            wumpus_pos,
            [self.all_sprites, self.wumpus_sprites],
            self.collision_sprites,
            self.prolog,
            self.map_knowledge,
            self.sound_manager,
        )
        # Set patrol points for Wumpus
        self.wumpus.patrol_points = [
            wumpus_pos,
            (wumpus_pos[0] + 200, wumpus_pos[1]),
            (wumpus_pos[0] + 200, wumpus_pos[1] + 200),
            (wumpus_pos[0], wumpus_pos[1] + 200),
        ]

        # Track chase state for music switching
        self.wumpus_was_chasing = False

        # Update Prolog with player position (use hitbox for accuracy)
        self.prolog.update_player_position(
            int(self.player.hitbox_rect.x), int(self.player.hitbox_rect.y)
        )

        # Spawn all game entities using SpawnSystem
        self.spawn_system.spawn_treasure([self.all_sprites, self.treasure_sprites])
        self.spawn_system.spawn_exit([self.all_sprites, self.exit_sprites])
        self.spawn_system.spawn_arrow_pickups([self.all_sprites, self.arrow_pickup_sprites])
        self.spawn_system.spawn_rock_pickups([self.all_sprites, self.rock_pickup_sprites]) 

        # Initialize lighting system
        self.lighting_system = FlashlightSystem(self.collision_sprites)

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

    # ========================================================================
    # SPAWN METHODS MOVED TO systems/spawn_system.py
    # The following methods are now handled by SpawnSystem:
    # - find_spawn_position()
    # - find_wumpus_spawn()
    # - spawn_wumpus_pack()
    # - spawn_treasure()
    # - spawn_exit()
    # - spawn_arrow_pickups()
    # - spawn_rock_pickups()
    # ========================================================================

    def setup(self):
        self.map_scale = 3.0

        # Load map
        self.tmx_map = load_pygame(
            r"assets\Map\test.tmx"
        )  # TODO: Let user select map later

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
        if (
            pygame.time.get_ticks() - self.game_start_time
            < GameplayConfig.SPAWN_PROTECTION_MS
        ):
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
        """Check if player opens treasure chests (press E near chest)"""
        if not self.has_treasure and len(self.treasure_sprites) > 0:
            # Check if player presses E near any chest
            keys = pygame.key.get_pressed()
            
            for chest in self.treasure_sprites:
                player_rect = self.player.hitbox_rect
                chest_rect = chest.hitbox_rect
                
                # Check distance to chest
                player_center = pygame.math.Vector2(player_rect.center)
                chest_center = pygame.math.Vector2(chest_rect.center)
                distance = player_center.distance_to(chest_center)
                
                # Player near chest and presses E
                if distance < 60 and keys[pygame.K_e]:
                    result = chest.open(self)
                    
                    if result == 'treasure':
                        # Real treasure found!
                        self.has_treasure = True
                        
                        # Unlock exit in both Python and Prolog
                        if len(self.exit_sprites) > 0:
                            exit_portal = self.exit_sprites.sprites()[0]
                            exit_portal.unlock()
                            self.exit_unlocked = True
                        
                        # Enrage all Wumpus
                        for wumpus in self.wumpus_sprites:
                            wumpus.speed *= 1.5
                        
                        print("[Game] Real treasure found! Wumpus enraged! Find exit!")
                        
                        # Update Prolog (collect treasure and unlock exit)
                        if self.prolog and self.prolog.available:
                            self.prolog.collect_treasure()
                            self.prolog.unlock_exit()
                        
                    elif result == 'mimic':
                        # Mimic! Spawn new Wumpus
                        print("[Game] MIMIC ACTIVATED! Additional Wumpus spawned!")
                        
                        # Spawn Wumpus at mimic location
                        from wumpus import Wumpus
                        mimic_pos = chest.pos
                        new_wumpus = Wumpus(
                            (mimic_pos.x, mimic_pos.y),
                            [self.all_sprites, self.wumpus_sprites],
                            self.collision_sprites,
                            self.prolog,
                            self.map_knowledge,
                            self.sound_manager
                        )
                        print(f"[Mimic] Spawned Wumpus at ({mimic_pos.x:.0f}, {mimic_pos.y:.0f})")
                    
                    # Prevent multiple opens in one frame
                    pygame.time.wait(200)
                    break

    def check_exit_reached(self):
        """Check if player reaches unlocked exit using Prolog"""
        if len(self.exit_sprites) > 0:
            # Use Prolog to check victory conditions
            can_exit = False
            if self.prolog and self.prolog.available:
                can_exit = self.prolog.can_exit_game(
                    int(self.player.hitbox_rect.x),
                    int(self.player.hitbox_rect.y),
                    int(self.player.hitbox_rect.width),
                    int(self.player.hitbox_rect.height)
                )
            else:
                # Fallback: Python collision check
                if self.has_treasure and self.exit_unlocked:
                    exit_portal = self.exit_sprites.sprites()[0]
                    can_exit = self.player.hitbox_rect.colliderect(exit_portal.hitbox_rect)
            
            if can_exit:
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
        """Check if any arrows hit the Wumpus using Prolog"""
        for arrow in self.arrow_sprites:
            # Check collision with all Wumpus enemies
            for wumpus in self.wumpus_sprites:
                if not wumpus.is_alive:
                    continue

                # Use Prolog to check if arrow hits Wumpus
                arrow_center = arrow.rect.center
                wumpus_center = wumpus.hitbox_rect.center
                
                hit = False
                if self.prolog and self.prolog.available:
                    # Use Prolog for authoritative hit detection
                    hit = self.prolog.arrow_hit_wumpus(
                        int(arrow_center[0]),
                        int(arrow_center[1]),
                        int(wumpus_center[0]),
                        int(wumpus_center[1]),
                        60  # Hit radius
                    )
                else:
                    # Fallback to distance check if Prolog unavailable
                    arrow_vec = pygame.math.Vector2(arrow_center)
                    wumpus_vec = pygame.math.Vector2(wumpus_center)
                    distance = arrow_vec.distance_to(wumpus_vec)
                    hit = distance < 60

                # Hit if either method detects collision
                if hit:
                    # Apply stun to Wumpus
                    wumpus.apply_stun()

                    # Remove arrow
                    arrow.kill()
                    print(
                        f"[Combat] Arrow hit Wumpus! Wumpus stunned for 3 seconds!"
                    )
                    break  # Arrow can only hit one Wumpus

    def check_arrow_pickups(self):
        """Check if player collects arrow pickups using Prolog"""
        if self.prolog and self.prolog.available:
            # Use Prolog to check pickup
            pickup_id = self.prolog.can_pickup_arrow(
                int(self.player.hitbox_rect.x),
                int(self.player.hitbox_rect.y),
                int(self.player.hitbox_rect.width),
                int(self.player.hitbox_rect.height)
            )
            
            if pickup_id is not None:
                # Find and remove the sprite
                for pickup in self.arrow_pickup_sprites:
                    if hasattr(pickup, 'pickup_id') and pickup.pickup_id == pickup_id:
                        if self.player.add_arrows(1):
                            pickup.kill()
                            self.prolog.remove_arrow_pickup(pickup_id)
                            print(f"[Pickup] Collected arrow! Total: {self.player.arrows}/{self.player.max_arrows}")
                        break
                else:
                    # If pickup doesn't have ID, use fallback collision check
                    for pickup in self.arrow_pickup_sprites:
                        if self.player.hitbox_rect.colliderect(pickup.hitbox_rect):
                            if self.player.add_arrows(1):
                                pickup.kill()
                                self.prolog.remove_arrow_pickup(pickup_id)
                                print(f"[Pickup] Collected arrow! Total: {self.player.arrows}/{self.player.max_arrows}")
                            break
        else:
            # Fallback: Python collision check
            for pickup in self.arrow_pickup_sprites:
                if self.player.hitbox_rect.colliderect(pickup.hitbox_rect):
                    if self.player.add_arrows(1):
                        pickup.kill()
                        print(f"[Pickup] Collected arrow! Total: {self.player.arrows}/{self.player.max_arrows}")
    
    def handle_rock_throw(self):
        """Handle player throwing rock (E key pressed - keyboard only)"""
        if not self.player.is_alive or self.player.rocks <= 0:
            return
        
        # Try to throw rock in facing direction
        rock_direction = self.player.throw_rock(self.sound_manager)
        
        if rock_direction is not None:
            # Create rock sprite
            rock_pos = self.player.hitbox_rect.center
            rock = Rock(
                rock_pos,
                rock_direction,
                ROCK_THROW_POWER,
                [self.all_sprites, self.rock_sprites],
                self.collision_sprites
            )
            print(f"[Rock] Threw rock in direction: {rock_direction}")
    
    def check_rock_pickups(self):
        """Check if player collects rock pickups using Prolog"""
        if self.prolog and self.prolog.available:
            # Use Prolog to check pickup
            pickup_id = self.prolog.can_pickup_rock(
                int(self.player.hitbox_rect.x),
                int(self.player.hitbox_rect.y),
                int(self.player.hitbox_rect.width),
                int(self.player.hitbox_rect.height)
            )
            
            if pickup_id is not None:
                # Find and remove the sprite
                for pickup in self.rock_pickup_sprites:
                    if hasattr(pickup, 'pickup_id') and pickup.pickup_id == pickup_id:
                        self.player.add_rocks(ROCK_PICKUP_COUNT)
                        pickup.kill()
                        self.prolog.remove_rock_pickup(pickup_id)
                        print(f"[Pickup] Collected rocks! Total: {self.player.rocks}/{self.player.max_rocks}")
                        break
                else:
                    # If pickup doesn't have ID, use fallback
                    for pickup in self.rock_pickup_sprites:
                        if self.player.hitbox_rect.colliderect(pickup.hitbox_rect):
                            self.player.add_rocks(ROCK_PICKUP_COUNT)
                            pickup.kill()
                            self.prolog.remove_rock_pickup(pickup_id)
                            print(f"[Pickup] Collected rocks! Total: {self.player.rocks}/{self.player.max_rocks}")
                            break
        else:
            # Fallback: Python collision check
            for pickup in self.rock_pickup_sprites:
                if self.player.hitbox_rect.colliderect(pickup.hitbox_rect):
                    self.player.add_rocks(ROCK_PICKUP_COUNT)
                    pickup.kill()
                    print(f"[Pickup] Collected rocks! Total: {self.player.rocks}/{self.player.max_rocks}")

    def check_player_attack(self):
        """DEPRECATED - Player now uses arrow combat system instead of melee"""
        pass

    def check_wumpus_attack(self):
        """Check if any Wumpus's attack hits the Player using Prolog"""
        # Only check if game is still playing
        if self.game_state != GameState.PLAYING:
            return

        player_center = self.player.hitbox_rect.center
        
        # Check all Wumpus enemies
        for wumpus in self.wumpus_sprites:
            if not wumpus.is_alive or wumpus.is_stunned:
                continue
            
            wumpus_center = wumpus.hitbox_rect.center
            
            # Use Prolog to check if Wumpus can attack
            can_attack = False
            if self.prolog and self.prolog.available:
                can_attack = self.prolog.wumpus_can_attack_player(
                    int(wumpus_center[0]),
                    int(wumpus_center[1]),
                    int(player_center[0]),
                    int(player_center[1]),
                    int(wumpus.attack_range),
                    wumpus.ai_state
                )
            else:
                # Fallback: Distance check
                wumpus_pos = pygame.math.Vector2(wumpus_center)
                player_pos = pygame.math.Vector2(player_center)
                distance = player_pos.distance_to(wumpus_pos)
                can_attack = distance <= wumpus.attack_range and wumpus.ai_state == "attack"

            if can_attack:
                damage = self.player.take_damage(wumpus.damage)
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
                    return  # Exit after death

    def render_lighting(self):
        """Render the lighting system (flashlight and darkness)."""
        player_screen_pos = self.player.rect.center - self.all_sprites.offset
        player_world_pos = self.player.rect.center

        self.lighting_system.render(
            self.screen, player_screen_pos, player_world_pos, self.player.facing
        )

    def transition_to_state(self, new_state, force_initialize=False):
        """
        Transition to a new game state with fade effect

        Args:
            new_state: GameState to transition to
            force_initialize: Force game initialization even if already in PLAYING state
        """
        if self.fade.is_fading():
            return  # Already transitioning

        self.pending_state = new_state
        self.force_init = force_initialize

        # Start fade out, then change state when complete
        def on_fade_complete():
            # Change state
            old_state = self.game_state
            self.game_state = self.pending_state
            self.pending_state = None

            # Handle state-specific transitions
            if self.game_state == GameState.PLAYING:
                # Starting or restarting game - initialize
                if (
                    self.force_init
                    or not self.game_initialized
                    or old_state != GameState.PLAYING
                ):
                    self.initialize_game()
                self.force_init = False

            elif self.game_state == GameState.MAIN_MENU:
                # Returning to menu
                self.sound_manager.stop_music()
                self.sound_manager.play_menu_music()

            # Fade back in
            self.fade.start_fade_in()

        self.fade.start_fade_out(callback=on_fade_complete)

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
                                self.transition_to_state(GameState.PLAYING)
                            elif self.menu_selection == 1:  # Controls
                                self.transition_to_state(GameState.CONTROLS)
                            elif self.menu_selection == 2:  # Quit
                                self.running = False

                    # Controls screen
                    elif self.game_state == GameState.CONTROLS:
                        if event.key == pygame.K_ESCAPE:
                            self.sound_manager.play_sound("button")
                            self.transition_to_state(GameState.MAIN_MENU)

                    # In-game controls
                    elif self.game_state == GameState.PLAYING:
                        if event.key == pygame.K_ESCAPE:
                            self.transition_to_state(GameState.MAIN_MENU)
                        elif event.key == pygame.K_r:
                            # Restart the game with fade (force re-initialization)
                            self.sound_manager.stop_all_sounds()
                            self.transition_to_state(
                                GameState.PLAYING, force_initialize=True
                            )
                        elif event.key == pygame.K_f:
                            self.debug_mode = not self.debug_mode
                            print(f"Debug mode: {self.debug_mode}")
                        elif event.key == pygame.K_SPACE:
                            self.handle_arrow_shooting()
                    
                    # Rock throwing (E key - keyboard only!)
                    if (
                        event.key == pygame.K_e
                        and self.game_state == GameState.PLAYING
                    ):
                        self.handle_rock_throw()

                    # Game over / Victory screens
                    else:
                        if event.key == pygame.K_r:
                            # Stop game_over sound before restarting
                            self.sound_manager.stop_all_sounds()
                            self.transition_to_state(
                                GameState.PLAYING, force_initialize=True
                            )
                        elif event.key == pygame.K_ESCAPE:
                            self.sound_manager.stop_all_sounds()
                            self.transition_to_state(GameState.MAIN_MENU)

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
                    # Update sound system
                    self.sound_manager.update()
                    
                    # Update all Wumpus AI (before sprite group update)
                    player_center = pygame.math.Vector2(self.player.hitbox_rect.center)
                    for wumpus in self.wumpus_sprites:
                        if wumpus.is_alive:
                            wumpus.ai_update(player_center, dt)

                        # Check if Wumpus entered chase mode (dynamic music switch)
                        is_chasing = self.wumpus.ai_state in ["chase", "attack"]
                        if is_chasing and not self.wumpus_was_chasing:
                            # Just entered chase mode - switch to chase music
                            self.sound_manager.play_chase_music()
                            print("[Game] 🔥 Wumpus is chasing! Chase music started!")
                        elif not is_chasing and self.wumpus_was_chasing:
                            # Just exited chase mode - back to normal in-game music
                            self.sound_manager.play_ingame_music()
                            print("[Game] ✅ Wumpus lost player. Normal music resumed.")

                        self.wumpus_was_chasing = is_chasing

                    # Update all sprites - but Player needs sound_manager now
                    # NOTE: We can't pass dt + sound_manager to all_sprites.update()
                    # So we need to update player separately
                    for sprite in self.all_sprites:
                        if sprite == self.player:
                            sprite.update(dt, self.sound_manager)  # Player with sound
                        else:
                            sprite.update(dt)  # Others normal
                    
                    # Update projectiles
                    self.arrow_sprites.update(dt)  # Arrows
                    for rock in self.rock_sprites:
                        rock.update(dt, self.sound_manager)  # Rocks with sound

                    # Check collisions
                    self.check_arrow_hits()
                    self.check_arrow_pickups()
                    self.check_rock_pickups()  # NEW: rock pickup collection

                    # Check Wumpus attacks (all of them)
                    self.check_wumpus_attack()

                    # Check treasure and exit
                    self.check_treasure_collection()
                    self.check_exit_reached()

                    # Update Prolog with current player position
                    self.prolog.update_player_position(
                        int(self.player.rect.x), int(self.player.rect.y)
                    )
                    self.check_game_over()

            # Update fade transition
            self.fade.update(dt)

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
                self.render_lighting()

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

                    # Draw all Wumpus hitboxes and detection ranges
                    for idx, wumpus in enumerate(self.wumpus_sprites):
                        if not wumpus.is_alive:
                            continue

                        wumpus_hitbox = wumpus.hitbox_rect.copy()
                        wumpus_hitbox.topleft -= offset
                        pygame.draw.rect(self.screen, (255, 0, 255), wumpus_hitbox, 2)

                        # Hearing range circle (sound-based detection)
                        wumpus_center = wumpus.hitbox_rect.center
                        screen_center = (
                            int(wumpus_center[0] - offset.x),
                            int(wumpus_center[1] - offset.y),
                        )
                        
                        # Draw hearing radius (changes color when roaring)
                        hearing_color = (255, 50, 50) if wumpus.is_roaring else (100, 100, 255)
                        pygame.draw.circle(
                            self.screen,
                            hearing_color,
                            screen_center,
                            wumpus.current_hearing_radius,
                            2,
                        )

                        # AI state text
                        font = get_pixel_font(24)
                        state_text = font.render(
                            f"W{idx+1} AI: {wumpus.ai_state.name if hasattr(wumpus.ai_state, 'name') else wumpus.ai_state}",
                            True,
                            (255, 255, 255)
                        )
                        self.screen.blit(
                            state_text, (screen_center[0] - 50, screen_center[1] - 100)
                        )

                        # Wumpus health bar
                        health_percent = wumpus.health / wumpus.max_health
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

                # Chest interaction hint (center bottom)
                if hasattr(self, 'player') and self.player and hasattr(self, 'treasure_sprites'):
                    player_center = pygame.math.Vector2(
                        self.player.hitbox_rect.centerx, self.player.hitbox_rect.centery
                    )
                    
                    # Check if player is near any unopened chest
                    near_chest = False
                    for chest in self.treasure_sprites:
                        if hasattr(chest, 'opened') and not chest.opened:
                            chest_center = pygame.math.Vector2(
                                chest.rect.centerx, chest.rect.centery
                            )
                            distance = player_center.distance_to(chest_center)
                            
                            if distance < 60:
                                near_chest = True
                                break
                    
                    # Display hint if near unopened chest
                    if near_chest:
                        hint_text = font.render("Press [E] to open chest", True, (255, 255, 100))
                        hint_rect = hint_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT - 40))
                        # Draw semi-transparent background
                        bg_rect = hint_rect.inflate(20, 10)
                        bg_surface = pygame.Surface(bg_rect.size, pygame.SRCALPHA)
                        bg_surface.fill((0, 0, 0, 180))
                        self.screen.blit(bg_surface, bg_rect.topleft)
                        self.screen.blit(hint_text, hint_rect)

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

            # Render fade transition (always on top)
            self.fade.render(self.screen)

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
