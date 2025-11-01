"""
Spawn System - Handles spawning of all game entities
Separates spawn logic from main game loop for better organization
"""

from Settings import *
from sprites import TreasureChest, ExitPortal, ArrowPickup, RockPickup
from wumpus import Wumpus
import random


class SpawnSystem:
    """
    Manages spawning logic for all game entities:
    - Player spawn positions
    - Enemy (Wumpus) spawns
    - Treasure chests (with Prolog integration)
    - Exit portals
    - Pickups (arrows, rocks)
    """
    
    def __init__(self, tmx_map, map_scale, prolog_engine=None):
        """
        Initialize spawn system
        
        Args:
            tmx_map: Loaded TMX map from pytmx
            map_scale: Scale factor for map coordinates
            prolog_engine: PrologEngine instance for game logic
        """
        self.tmx_map = tmx_map
        self.map_scale = map_scale
        self.prolog = prolog_engine
        
        # Cache map dimensions for quick access
        self.map_width = tmx_map.width * tmx_map.tilewidth * map_scale
        self.map_height = tmx_map.height * tmx_map.tileheight * map_scale
    
    # ==================== Player Spawn ====================
    
    def find_player_spawn(self):
        """
        Find a safe spawn position for the player
        Priority: Entrance layer -> Map center (fallback)
        
        Returns:
            tuple: (x, y) spawn position
        """
        # Look for Entrance layer
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
                    f"[SpawnSystem] Found entrance at tile ({x}, {y}) -> world ({spawn_x}, {spawn_y})"
                )
                return (spawn_x, spawn_y)
        
        # Fallback: map center
        map_center_x = self.map_width // 2
        map_center_y = self.map_height // 2
        print(f"[SpawnSystem] No entrance found, using center: ({map_center_x}, {map_center_y})")
        return (map_center_x, map_center_y)
    
    # ==================== Wumpus Spawn ====================
    
    def find_wumpus_spawn(self):
        """
        Find spawn position for single Wumpus enemy
        Priority: WumpusSpawn object layer -> Bottom-right corner (fallback)
        
        Returns:
            tuple: (x, y) spawn position
        """
        # Look for WumpusSpawn object layer
        for layer in self.tmx_map.layers:
            if (
                type(layer).__name__ == "TiledObjectGroup"
                and "wumpus" in layer.name.lower()
            ):
                if len(layer) > 0:
                    obj = layer[0]  # Take first spawn point
                    spawn_x = (obj.x * self.map_scale) + (obj.width * self.map_scale // 2)
                    spawn_y = (obj.y * self.map_scale) + (obj.height * self.map_scale // 2)
                    print(f"[SpawnSystem] Found Wumpus spawn at ({spawn_x}, {spawn_y})")
                    return (spawn_x, spawn_y)
        
        # Fallback: far from player (bottom-right)
        fallback_x = self.map_width - 200
        fallback_y = self.map_height - 200
        print(f"[SpawnSystem] No Wumpus spawn found, using fallback: ({fallback_x}, {fallback_y})")
        return (fallback_x, fallback_y)
    
    def spawn_wumpus_pack(self, sprite_groups, collision_sprites, map_knowledge, sound_manager):
        """
        Spawn 3-4 Wumpus enemies at different map areas
        
        Args:
            sprite_groups: [all_sprites, wumpus_sprites] list
            collision_sprites: Collision sprite group
            map_knowledge: MapKnowledge instance
            sound_manager: SoundManager instance
            
        Returns:
            int: Number of Wumpus spawned
        """
        wumpus_count = random.randint(3, 4)
        
        # Define spawn areas (divide map into quadrants)
        spawn_areas = [
            (self.map_width * 0.75, self.map_height * 0.25),  # Top-right
            (self.map_width * 0.75, self.map_height * 0.75),  # Bottom-right
            (self.map_width * 0.25, self.map_height * 0.75),  # Bottom-left
            (self.map_width * 0.50, self.map_height * 0.50),  # Center
        ]
        
        random.shuffle(spawn_areas)
        
        for i in range(wumpus_count):
            base_x, base_y = spawn_areas[i]
            # Add random offset for variation
            spawn_x = base_x + random.randint(-100, 100)
            spawn_y = base_y + random.randint(-100, 100)
            
            wumpus = Wumpus(
                (spawn_x, spawn_y),
                sprite_groups,
                collision_sprites,
                self.prolog,
                map_knowledge,
                sound_manager
            )
            print(f"[SpawnSystem] Wumpus {i+1}/{wumpus_count} spawned at ({spawn_x}, {spawn_y})")
        
        return wumpus_count
    
    # ==================== Treasure Spawn ====================
    
    def spawn_treasure(self, sprite_groups):
        """
        Spawn treasure chest system (1 real, 2 mimics) using Prolog
        
        Args:
            sprite_groups: [all_sprites, treasure_sprites] list
            
        Returns:
            int: Number of chests spawned
        """
        chest_positions = []
        
        # Look for treasure spawn points in TMX
        for layer in self.tmx_map.layers:
            if (
                type(layer).__name__ == "TiledObjectGroup"
                and "treasure" in layer.name.lower()
            ):
                for obj in layer:
                    chest_x = (obj.x * self.map_scale) + (obj.width * self.map_scale // 2)
                    chest_y = (obj.y * self.map_scale) + (obj.height * self.map_scale // 2)
                    chest_positions.append((chest_x, chest_y))
                    if len(chest_positions) >= 3:
                        break
        
        # Fallback: 3 positions in different areas
        if len(chest_positions) < 3:
            fallback_positions = [
                (self.map_width * 0.75, self.map_height * 0.25),  # Top-right
                (self.map_width * 0.75, self.map_height * 0.75),  # Bottom-right
                (self.map_width * 0.25, self.map_height * 0.75),  # Bottom-left
            ]
            
            while len(chest_positions) < 3:
                chest_positions.append(fallback_positions[len(chest_positions)])
        
        chest_positions = chest_positions[:3]
        
        # Setup treasure system in Prolog
        if self.prolog and self.prolog.available:
            self.prolog.setup_treasure_system(
                chest_positions[0],
                chest_positions[1],
                chest_positions[2]
            )
            
            # Create sprites from Prolog data
            chests = self.prolog.get_all_chests()
            
            for chest in chests:
                is_mimic = chest['type'] == 'mimic'
                chest_sprite = TreasureChest(
                    (chest['x'], chest['y']),
                    sprite_groups,
                    chest['id'],
                    is_mimic=is_mimic,
                    prolog_engine=self.prolog
                )
                chest_type = "MIMIC" if is_mimic else "REAL"
                print(f"[SpawnSystem] Chest {chest['id']} at ({chest['x']:.0f}, {chest['y']:.0f}) - {chest_type}")
        else:
            # Fallback without Prolog - random selection
            real_chest = random.randint(0, 2)
            
            for i, pos in enumerate(chest_positions):
                is_mimic = (i != real_chest)
                chest_sprite = TreasureChest(
                    pos,
                    sprite_groups,
                    i + 1,
                    is_mimic=is_mimic,
                    prolog_engine=None
                )
                chest_type = "MIMIC" if is_mimic else "REAL"
                print(f"[SpawnSystem] Chest {i+1} at {pos} - {chest_type}")
        
        return len(chest_positions)
    
    # ==================== Exit Spawn ====================
    
    def spawn_exit(self, sprite_groups):
        """
        Spawn exit portal at entrance position
        
        Args:
            sprite_groups: [all_sprites, exit_sprites] list
            
        Returns:
            tuple: (x, y) exit position
        """
        entrance_pos = self.find_player_spawn()
        exit_portal = ExitPortal(entrance_pos, sprite_groups)
        print(f"[SpawnSystem] Exit portal at entrance: {entrance_pos}")
        
        # Initialize exit in Prolog
        if self.prolog and self.prolog.available:
            self.prolog.init_exit(int(entrance_pos[0]), int(entrance_pos[1]))
        
        return entrance_pos
    
    # ==================== Arrow Pickups ====================
    
    def spawn_arrow_pickups(self, sprite_groups):
        """
        Spawn arrow pickups from TMX or fallback positions
        
        Args:
            sprite_groups: [all_sprites, arrow_pickup_sprites] list
            
        Returns:
            int: Number of arrow pickups spawned
        """
        pickup_count = 0
        
        # Look for ArrowPickup object layer
        for layer in self.tmx_map.layers:
            if (
                type(layer).__name__ == "TiledObjectGroup"
                and "arrow" in layer.name.lower()
            ):
                for obj in layer:
                    if pickup_count >= ARROW_PICKUP_COUNT:
                        break
                    pickup_x = (obj.x * self.map_scale) + (obj.width * self.map_scale // 2)
                    pickup_y = (obj.y * self.map_scale) + (obj.height * self.map_scale // 2)
                    
                    pickup = ArrowPickup(
                        (pickup_x, pickup_y),
                        sprite_groups,
                        pickup_count
                    )
                    pickup_count += 1
                    
                    # Add to Prolog
                    if self.prolog and self.prolog.available:
                        self.prolog.add_arrow_pickup(pickup_count, int(pickup_x), int(pickup_y))
                    
                    print(f"[SpawnSystem] Arrow pickup {pickup_count} at ({pickup_x}, {pickup_y})")
                
                if pickup_count >= ARROW_PICKUP_COUNT:
                    return pickup_count
        
        # Fallback positions
        fallback_positions = [
            (self.map_width // 4, self.map_height // 3),
            (self.map_width // 2, self.map_height * 2 // 3),
            (self.map_width * 3 // 4, self.map_height // 2),
        ]
        
        for i, pos in enumerate(fallback_positions[:ARROW_PICKUP_COUNT - pickup_count]):
            pickup = ArrowPickup(pos, sprite_groups, pickup_count)
            pickup_count += 1
            
            # Add to Prolog
            if self.prolog and self.prolog.available:
                self.prolog.add_arrow_pickup(pickup_count, int(pos[0]), int(pos[1]))
            
            print(f"[SpawnSystem] Arrow pickup {pickup_count} (fallback) at {pos}")
        
        return pickup_count
    
    # ==================== Rock Pickups ====================
    
    def spawn_rock_pickups(self, sprite_groups):
        """
        Spawn rock pickups from TMX 'rock' layer or fallback
        
        Args:
            sprite_groups: [all_sprites, rock_pickup_sprites] list
            
        Returns:
            int: Number of rock pickups spawned
        """
        pickup_count = 0
        
        # Try to load from TMX 'rock' object layer
        for layer in self.tmx_map.layers:
            if hasattr(layer, 'name') and layer.name == 'rock':
                if len(layer) > 0:
                    for obj in layer:
                        pos = (
                            obj.x * self.map_scale,
                            obj.y * self.map_scale
                        )
                        pickup_count += 1
                        pickup = RockPickup(pos, sprite_groups, pickup_count)
                        
                        # Add to Prolog
                        if self.prolog and self.prolog.available:
                            self.prolog.add_rock_pickup(pickup_count, int(pos[0]), int(pos[1]))
                        
                        print(f"[SpawnSystem] Rock pickup {pickup_count} at {pos}")
                    return pickup_count
        
        # Fallback positions
        print("[SpawnSystem] No 'rock' layer found, using fallback positions")
        fallback_positions = [
            (self.map_width // 3, self.map_height // 4),
            (self.map_width // 2, self.map_height // 2),
            (self.map_width * 2 // 3, self.map_height // 3),
            (self.map_width // 4, self.map_height * 3 // 4),
            (self.map_width * 3 // 4, self.map_height * 2 // 3),
        ]
        
        for i, pos in enumerate(fallback_positions[:ROCK_PICKUP_COUNT]):
            pickup_count += 1
            pickup = RockPickup(pos, sprite_groups, pickup_count)
            
            # Add to Prolog
            if self.prolog and self.prolog.available:
                self.prolog.add_rock_pickup(pickup_count, int(pos[0]), int(pos[1]))
            
            print(f"[SpawnSystem] Rock pickup {pickup_count} (fallback) at {pos}")
        
        return pickup_count
