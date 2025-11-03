from pyswip import Prolog
from Settings import *


class PrologEngine:
    def __init__(self):
        self.available = False
        self.prolog = None

        try:
            self.prolog = Prolog()

            # Choose between modular or monolithic Prolog
            if USE_MODULAR_PROLOG:
                self.prolog.consult("game_logic_modular.pl")
                print("[PrologEngine] Initialized with MODULAR architecture (Phase 1)")
            else:
                self.prolog.consult("game_logic.pl")
                print(
                    "[PrologEngine] Initialized with MONOLITHIC architecture (legacy)"
                )

            self.available = True
            self.init_game()
        except Exception as e:
            print(f"[PrologEngine] Failed to initialize: {e}")
            print("[PrologEngine] Running with fallback collision system")
            self.prolog = None
            self.available = False

    def _query(self, query_str):
        if not self.available or self.prolog is None:
            return []
        try:
            return list(self.prolog.query(query_str))
        except Exception as e:
            print(f"[PrologEngine] Query failed: {query_str} -> {e}")
            return []

    def init_game(self):
        self._query("init_game")

    def add_collision_box(self, x, y, w, h):
        query = f"add_collision_box({x}, {y}, {w}, {h})"
        self._query(query)

    def add_fall_zone(self, x, y, w, h):
        query = f"add_fall_zone({x}, {y}, {w}, {h})"
        self._query(query)

    def add_water_zone(self, x, y, w, h):
        """Add a water zone to Prolog knowledge base"""
        query = f"add_water_zone({x}, {y}, {w}, {h})"
        self._query(query)

    def check_collision(self, x, y, w, h):
        query = f"check_collision({x}, {y}, {w}, {h})"
        results = self._query(query)
        return len(results) > 0

    def check_fall(self, x, y, w, h, feet_height=10):
        query = f"check_fall({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0

    def check_water(self, x, y, w, h, feet_height=10):
        """Check if player feet are standing in any water zone"""
        query = f"check_water({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0

    # =========================================================================
    # NEW LOGIC: MOVEMENT RESOLUTION INTERFACE
    # =========================================================================
    def resolve_movement(self, current_x, current_y, delta_x, delta_y, w, h):
        query = f"resolve_movement({current_x}, {current_y}, {delta_x}, {delta_y}, {w}, {h}, FinalX, FinalY)"
        results = self._query(query)

        if results:
            try:
                final_x = int(results[0]["FinalX"])
                final_y = int(results[0]["FinalY"])
                return final_x, final_y
            except (KeyError, ValueError, TypeError) as e:
                print(f"[PrologEngine] Malformed resolve_movement result: {e}")
                return current_x, current_y

        return current_x, current_y

    def update_player_position(self, x, y):
        query = f"update_player_position({x}, {y})"
        self._query(query)

    def set_game_over(self, state):
        state_str = "true" if state else "false"
        query = f"set_game_over({state_str})"
        self._query(query)

    def is_game_over(self):
        query = "is_game_over(true)"
        results = self._query(query)
        return len(results) > 0

    def count_hazards(self):
        query = "count_hazards(CollisionCount, FallCount)"
        results = self._query(query)
        if results:
            try:
                return results[0]["CollisionCount"], results[0]["FallCount"]
            except (KeyError, IndexError):
                return 0, 0
        return 0, 0

    # =========================================================================
    # ENTITY SIZE SYSTEM
    # =========================================================================

    def init_entity_sizes(self):
        """Initialize entity size database"""
        query = "init_entity_sizes"
        self._query(query)

        # Debug: verify entity sizes were initialized
        test_query = "entity_size(wumpus, W, H)"
        results = self._query(test_query)
        if results:
            print(
                f"[PrologEngine] ✓ Entity sizes initialized - Wumpus: {results[0]['W']}x{results[0]['H']}"
            )
        else:
            print("[PrologEngine] ⚠ WARNING: entity_size(wumpus, W, H) query failed!")

    def get_entity_size(self, entity_type):
        """Get entity dimensions"""
        query = f"get_entity_size({entity_type}, Width, Height)"
        results = self._query(query)
        if results:
            try:
                return int(results[0]["Width"]), int(results[0]["Height"])
            except (KeyError, IndexError, ValueError):
                return 32, 32
        return 32, 32

    def is_safe_for_entity(self, entity_type, x, y):
        """Check if position is safe for entity type"""
        query = f"is_safe_for_entity_type({entity_type}, {x}, {y})"
        results = self._query(query)
        return len(results) > 0

    def is_safe_for_entity_size(self, x, y, width, height):
        """Check if position is safe for entity with specific size"""
        query = f"is_safe_for_entity({x}, {y}, {width}, {height})"
        results = self._query(query)
        return len(results) > 0

    def update_wumpus_position_with_id(self, wumpus_id, x, y):
        """Update Wumpus position by ID"""
        query = f"update_wumpus_position({wumpus_id}, {x}, {y})"
        self._query(query)

    def get_wumpus_position(self, wumpus_id):
        """Get Wumpus position by ID"""
        query = f"get_wumpus_position({wumpus_id}, X, Y)"
        results = self._query(query)
        if results:
            try:
                return int(results[0]["X"]), int(results[0]["Y"])
            except (KeyError, IndexError, ValueError):
                return None
        return None

    def find_nearest_reachable(self, entity_type, from_x, from_y, target_x, target_y):
        """Find nearest reachable position to target for entity type"""
        query = f"find_nearest_reachable({entity_type}, {from_x}, {from_y}, {target_x}, {target_y}, NearestX, NearestY)"
        results = self._query(query)
        if results:
            try:
                return (int(results[0]["NearestX"]), int(results[0]["NearestY"]))
            except (KeyError, IndexError, ValueError):
                return None
        return None

    def wumpus_can_reach_target(self, wumpus_id, target_x, target_y):
        """Check if Wumpus can reach target position"""
        query = f"wumpus_can_reach_target({wumpus_id}, {target_x}, {target_y})"
        results = self._query(query)
        return len(results) > 0

    def generate_wumpus_patrol_route(self, wumpus_id, count):
        """Generate patrol waypoints that Wumpus can reach"""
        query = f"generate_wumpus_patrol_route({wumpus_id}, {count}, Waypoints)"
        results = self._query(query)
        if results:
            try:
                waypoints_str = str(results[0]["Waypoints"])
                # Parse Prolog list format: [(X1, Y1), (X2, Y2), ...]
                import re

                matches = re.findall(
                    r"\((\d+(?:\.\d+)?),\s*(\d+(?:\.\d+)?)\)", waypoints_str
                )
                waypoints = [(int(float(x)), int(float(y))) for x, y in matches]
                if waypoints:
                    print(
                        f"[PrologEngine] Generated {len(waypoints)} patrol waypoints for Wumpus {wumpus_id}"
                    )
                    print(
                        f"[PrologEngine] Waypoints: {waypoints}"
                    )  # DEBUG: Show actual waypoints
                    return waypoints
                else:
                    print(
                        f"[PrologEngine] Prolog returned empty waypoint list for Wumpus {wumpus_id}"
                    )
                    return []
            except Exception as e:
                print(f"[PrologEngine] Failed to parse patrol waypoints: {e}")
                print(f"[PrologEngine] Raw result: {waypoints_str}")
                return []
        else:
            print(
                f"[PrologEngine] Prolog query failed for Wumpus {wumpus_id} waypoint generation"
            )
            return []

    # =========================================================================
    # LEGACY METHODS (kept for compatibility)
    # =========================================================================

    def is_safe_position(self, x, y, w, h, feet_height=10):
        query = f"is_safe_position({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0

    def find_safe_spawn(self, entrance_x, entrance_y, w, h, feet_height=10):
        query = f"find_safe_spawn({entrance_x}, {entrance_y}, SafeX, SafeY, {w}, {h}, {feet_height})"
        results = self._query(query)
        if results:
            try:
                return results[0]["SafeX"], results[0]["SafeY"]
            except (KeyError, IndexError):
                return entrance_x, entrance_y
        return entrance_x, entrance_y

    def init_wumpus(self, x, y):
        query = f"init_wumpus({x}, {y})"
        self._query(query)

    def update_wumpus_position(self, x, y):
        query = f"update_wumpus_position({x}, {y})"
        self._query(query)

    def update_wumpus_state(self, state):
        query = f"update_wumpus_state({state})"
        self._query(query)

    def get_wumpus_decision(
        self, wumpus_x, wumpus_y, player_x, player_y, current_state
    ):
        query = f"wumpus_decision({wumpus_x}, {wumpus_y}, {player_x}, {player_y}, {current_state}, NewState, DirectionX, DirectionY)"
        results = self._query(query)

        if results:
            try:
                new_state = results[0]["NewState"]
                direction_x = float(results[0]["DirectionX"])
                direction_y = float(results[0]["DirectionY"])
                return (new_state, direction_x, direction_y)
            except (KeyError, IndexError, ValueError) as e:
                print(f"[PrologEngine] Error parsing wumpus_decision: {e}")
                return (current_state, 0.0, 0.0)

        return (current_state, 0.0, 0.0)

    def setup_treasure_system(self, pos1, pos2, pos3):
        x1, y1 = pos1
        x2, y2 = pos2
        x3, y3 = pos3
        query = f"setup_treasure_system({x1}, {y1}, {x2}, {y2}, {x3}, {y3})"
        self._query(query)
        print(
            f"[PrologEngine] Setup treasure system at positions: {pos1}, {pos2}, {pos3}"
        )

    def open_chest(self, player_x, player_y):
        query = f"open_chest({player_x}, {player_y}, Result)"
        results = self._query(query)
        if results:
            try:
                result = str(results[0]["Result"])
                print(f"[PrologEngine] open_chest result: {result}")
                return result
            except (KeyError, IndexError):
                return "no_chest"
        return "no_chest"

    def get_all_chests(self):
        chests = []

        treasure_query = "treasure(ID, X, Y)"
        treasures = self._query(treasure_query)
        for t in treasures:
            chests.append(
                {
                    "id": t["ID"],
                    "x": float(t["X"]),
                    "y": float(t["Y"]),
                    "type": "treasure",
                }
            )

        mimic_query = "mimic(ID, X, Y, Activated)"
        mimics = self._query(mimic_query)
        for m in mimics:
            chests.append(
                {
                    "id": m["ID"],
                    "x": float(m["X"]),
                    "y": float(m["Y"]),
                    "type": "mimic",
                    "activated": m["Activated"] == "true",
                }
            )

        print(f"[PrologEngine] Found {len(chests)} chests total")
        return chests

    def spawn_wumpus(self, x, y):
        query = f"spawn_wumpus({x}, {y}, WumpusID)"
        results = self._query(query)
        if results:
            try:
                wumpus_id = results[0]["WumpusID"]
                print(f"[PrologEngine] Spawned Wumpus ID {wumpus_id} at ({x}, {y})")
                return wumpus_id
            except (KeyError, IndexError):
                return None
        return None

    def collect_treasure(self):
        query = "collect_treasure"
        self._query(query)
        print("[PrologEngine] Treasure collected")

    # =========================================================================
    # COMBAT SYSTEM
    # =========================================================================

    # PROJECTILE TRACKING
    # -------------------------------------------------------------------------

    def init_projectile_system(self):
        """Initialize projectile tracking system"""
        query = "init_projectile_system"
        self._query(query)
        print("[PrologEngine] Projectile tracking system initialized")

    def spawn_arrow(self, x, y, dir_x, dir_y):
        """
        Register new arrow projectile in Prolog
        Returns: Arrow ID
        """
        query = f"spawn_arrow({x}, {y}, {dir_x}, {dir_y}, ArrowID)"
        results = self._query(query)
        if results:
            try:
                arrow_id = results[0]["ArrowID"]
                print(f"[PrologEngine] Spawned arrow ID {arrow_id} at ({x}, {y})")
                return arrow_id
            except (KeyError, IndexError):
                return None
        return None

    def update_arrow_position(self, arrow_id, new_x, new_y):
        """Update arrow position in Prolog"""
        query = f"update_arrow_position({arrow_id}, {new_x}, {new_y})"
        self._query(query)

    def remove_arrow(self, arrow_id):
        """Remove arrow from tracking (hit or expired)"""
        query = f"remove_arrow({arrow_id})"
        self._query(query)
        print(f"[PrologEngine] Removed arrow ID {arrow_id}")

    def get_active_arrows(self):
        """
        Get all active arrows
        Returns: List of tuples (ID, X, Y, (DirX, DirY))
        """
        query = "get_active_arrows(Arrows)"
        results = self._query(query)
        if results:
            try:
                arrows_str = results[0]["Arrows"]
                # Parse Prolog list format
                if isinstance(arrows_str, str):
                    arrows = eval(arrows_str)
                else:
                    arrows = arrows_str
                return arrows
            except (KeyError, IndexError, SyntaxError):
                return []
        return []

    def count_active_arrows(self):
        """
        Count active arrows
        Returns: Integer count
        """
        query = "count_active_arrows(Count)"
        results = self._query(query)
        if results:
            try:
                return results[0]["Count"]
            except (KeyError, IndexError):
                return 0
        return 0

    def spawn_rock(self, x, y, vel_x, vel_y):
        """
        Register new rock projectile in Prolog
        Returns: Rock ID
        """
        query = f"spawn_rock({x}, {y}, {vel_x}, {vel_y}, RockID)"
        results = self._query(query)
        if results:
            try:
                rock_id = results[0]["RockID"]
                print(f"[PrologEngine] Spawned rock ID {rock_id} at ({x}, {y})")
                return rock_id
            except (KeyError, IndexError):
                return None
        return None

    def update_rock(self, rock_id, new_x, new_y, new_vel_x, new_vel_y):
        """Update rock position and velocity in Prolog"""
        query = f"update_rock({rock_id}, {new_x}, {new_y}, {new_vel_x}, {new_vel_y})"
        self._query(query)

    def remove_rock(self, rock_id):
        """Remove rock from tracking (expired or stopped)"""
        query = f"remove_rock({rock_id})"
        self._query(query)
        print(f"[PrologEngine] Removed rock ID {rock_id}")

    def get_active_rocks(self):
        """
        Get all active rocks
        Returns: List of tuples (ID, X, Y, VelX, VelY)
        """
        query = "get_active_rocks(Rocks)"
        results = self._query(query)
        if results:
            try:
                rocks_str = results[0]["Rocks"]
                # Parse Prolog list format
                if isinstance(rocks_str, str):
                    rocks = eval(rocks_str)
                else:
                    rocks = rocks_str
                return rocks
            except (KeyError, IndexError, SyntaxError):
                return []
        return []

    def count_active_rocks(self):
        """
        Count active rocks
        Returns: Integer count
        """
        query = "count_active_rocks(Count)"
        results = self._query(query)
        if results:
            try:
                return results[0]["Count"]
            except (KeyError, IndexError):
                return 0
        return 0

    def arrow_near_position(self, x, y, radius):
        """
        Check if any arrow is near a position (for Wumpus awareness)
        Returns: True if arrow nearby, False otherwise
        """
        query = f"arrow_near_position({x}, {y}, {radius})"
        results = self._query(query)
        return len(results) > 0

    def rock_near_position(self, x, y, radius):
        """
        Check if any rock is near a position (for sound detection)
        Returns: True if rock nearby, False otherwise
        """
        query = f"rock_near_position({x}, {y}, {radius})"
        results = self._query(query)
        return len(results) > 0

    def nearest_arrow_to_position(self, x, y):
        """
        Get nearest arrow to a position
        Returns: Tuple (arrow_id, distance) or None
        """
        query = f"nearest_arrow_to_position({x}, {y}, ArrowID, Distance)"
        results = self._query(query)
        if results:
            try:
                arrow_id = results[0]["ArrowID"]
                distance = results[0]["Distance"]
                return (arrow_id, distance)
            except (KeyError, IndexError):
                return None
        return None

    def arrow_hits_wumpus(self, arrow_id, wumpus_x, wumpus_y, hit_radius=60):
        """
        Check if specific arrow hits Wumpus position
        Returns: True if hit, False otherwise
        """
        query = f"arrow_hits_wumpus({arrow_id}, {wumpus_x}, {wumpus_y}, {hit_radius})"
        results = self._query(query)
        return len(results) > 0

    # COLLISION DETECTION
    # -------------------------------------------------------------------------

    def arrow_hit_wumpus(self, arrow_x, arrow_y, wumpus_x, wumpus_y, hit_radius=60):
        """
        Check if arrow hits Wumpus
        Returns: True if hit, False otherwise
        """
        query = f"arrow_hit_wumpus({arrow_x}, {arrow_y}, {wumpus_x}, {wumpus_y}, {hit_radius})"
        results = self._query(query)
        return len(results) > 0

    def wumpus_can_attack_player(
        self, wumpus_x, wumpus_y, player_x, player_y, attack_range, wumpus_state
    ):
        """
        Check if Wumpus can attack player
        Returns: True if can attack, False otherwise
        """
        query = f"wumpus_can_attack_player({wumpus_x}, {wumpus_y}, {player_x}, {player_y}, {attack_range}, {wumpus_state})"
        results = self._query(query)
        return len(results) > 0

    # =========================================================================
    # VICTORY & EXIT SYSTEM
    # =========================================================================

    def init_exit(self, exit_x, exit_y):
        """Initialize exit portal position"""
        query = f"init_exit({exit_x}, {exit_y})"
        self._query(query)

    def unlock_exit(self):
        """Unlock the exit portal (after collecting treasure)"""
        query = "unlock_exit"
        self._query(query)
        print("[PrologEngine] Exit unlocked")

    def can_exit_game(self, player_x, player_y, player_w, player_h):
        """
        Check if player can exit the game
        Returns: True if all victory conditions met, False otherwise
        """
        query = f"can_exit_game({player_x}, {player_y}, {player_w}, {player_h})"
        results = self._query(query)
        return len(results) > 0

    # =========================================================================
    # ITEM PICKUP SYSTEM
    # =========================================================================

    def add_arrow_pickup(self, pickup_id, x, y):
        """Add arrow pickup to Prolog world"""
        query = f"add_arrow_pickup({pickup_id}, {x}, {y})"
        self._query(query)

    def add_rock_pickup(self, pickup_id, x, y):
        """Add rock pickup to Prolog world"""
        query = f"add_rock_pickup({pickup_id}, {x}, {y})"
        self._query(query)

    def can_pickup_arrow(self, player_x, player_y, player_w, player_h):
        """
        Check if player can pickup any arrow
        Returns: Pickup ID if found, None otherwise
        """
        query = f"can_pickup_arrow({player_x}, {player_y}, {player_w}, {player_h}, PickupID)"
        results = self._query(query)
        if results:
            try:
                return results[0]["PickupID"]
            except (KeyError, IndexError):
                return None
        return None

    def can_pickup_rock(self, player_x, player_y, player_w, player_h):
        """
        Check if player can pickup any rock
        Returns: Pickup ID if found, None otherwise
        """
        query = (
            f"can_pickup_rock({player_x}, {player_y}, {player_w}, {player_h}, PickupID)"
        )
        results = self._query(query)
        if results:
            try:
                return results[0]["PickupID"]
            except (KeyError, IndexError):
                return None
        return None

    def remove_arrow_pickup(self, pickup_id):
        """Remove arrow pickup after collection"""
        query = f"remove_arrow_pickup({pickup_id})"
        self._query(query)

    def remove_rock_pickup(self, pickup_id):
        """Remove rock pickup after collection"""
        query = f"remove_rock_pickup({pickup_id})"
        self._query(query)

    def near_chest(self, player_x, player_y, interaction_radius=60):
        """
        Check if player is near any chest (for interaction prompt)
        Returns: True if near chest, False otherwise
        """
        query = f"near_chest({player_x}, {player_y}, {interaction_radius})"
        results = self._query(query)
        return len(results) > 0

    # =========================================================================
    # MAP SYSTEM - Enhanced map queries and navigation
    # =========================================================================

    def init_map_system(
        self, map_width, map_height, tile_width, tile_height, cell_size=32
    ):
        """Initialize map metadata in Prolog"""
        query = f"init_map_system({map_width}, {map_height}, {tile_width}, {tile_height}, {cell_size})"
        self._query(query)
        print(
            f"[Prolog] Map system initialized: {map_width}x{map_height}, cell={cell_size}px"
        )

    def build_navigation_grid(self):
        """Build navigation grid from collision/fall data"""
        query = "build_navigation_grid"
        self._query(query)

        # Count cells by type
        safe_count = self.count_grid_cells("safe")
        wall_count = self.count_grid_cells("wall")
        pit_count = self.count_grid_cells("pit")
        water_count = self.count_grid_cells("water")
        total = safe_count + wall_count + pit_count + water_count

        print(f"[Prolog] Navigation grid built: {total} cells total")
        print(
            f"[Prolog]   Safe: {safe_count}, Walls: {wall_count}, Pits: {pit_count}, Water: {water_count}"
        )

    def count_grid_cells(self, cell_type):
        """Count grid cells of specific type"""
        query = f"count_grid_cells({cell_type}, Count)"
        results = self._query(query)
        return results[0]["Count"] if results else 0

    def generate_safe_positions(self, entity_width=32, entity_height=32):
        """Generate all safe positions and cache them in Prolog"""
        query = f"generate_safe_positions({entity_width}, {entity_height})"
        self._query(query)

        count = self.count_safe_positions()
        print(f"[Prolog] Generated and cached {count} safe positions")
        return count

    def count_safe_positions(self):
        """Count cached safe positions"""
        query = "count_safe_positions(Count)"
        results = self._query(query)
        return results[0]["Count"] if results else 0

    def get_random_safe_position(self):
        """Get random safe position from Prolog cache"""
        query = "random_safe_position(X, Y)"
        results = self._query(query)
        if results:
            try:
                return (int(results[0]["X"]), int(results[0]["Y"]))
            except (KeyError, ValueError, TypeError):
                return None
        return None

    def get_safe_position_far_from(self, other_positions, min_distance=250):
        """
        Get safe position that's far from other positions

        Args:
            other_positions: List of (x, y) tuples
            min_distance: Minimum distance in pixels

        Returns:
            (x, y) tuple or None if not found
        """
        if not other_positions:
            return self.get_random_safe_position()

        # Convert positions to Prolog list format: [[X1, Y1], [X2, Y2], ...]
        pos_list = str(other_positions).replace("(", "[").replace(")", "]")

        query = f"safe_position_far_from(X, Y, {pos_list}, {min_distance})"
        results = self._query(query)

        if results:
            try:
                return (int(results[0]["X"]), int(results[0]["Y"]))
            except (KeyError, ValueError, TypeError):
                return None
        return None

    def is_position_safe(self, x, y, width, height):
        """Check if position is safe (no collision, no pit) - Prolog version"""
        query = f"is_safe_position({x}, {y}, {width}, {height})"
        results = self._query(query)
        return len(results) > 0

    def get_nearest_pit_distance(self, x, y):
        """Get distance to nearest pit"""
        query = f"nearest_pit_distance({x}, {y}, Distance)"
        results = self._query(query)
        if results:
            try:
                return float(results[0]["Distance"])
            except (KeyError, ValueError, TypeError):
                return 999999.0
        return 999999.0

    def is_near_pit(self, x, y, danger_radius=60):
        """Check if position is near a pit"""
        query = f"is_near_pit({x}, {y}, {danger_radius})"
        results = self._query(query)
        return len(results) > 0

    # =========================================================================
    # GAME STATE MANAGEMENT
    # =========================================================================

    def init_game_state(self):
        """Initialize game state (HP, inventory, time, etc.)"""
        query = "init_game_state"
        self._query(query)
        print("[Prolog] Game state initialized")

    def get_game_state(self):
        """
        Get current game state
        Returns: 'playing', 'paused', 'game_over_death', 'game_over_timeout', 'victory'
        """
        query = "get_game_state(State)"
        results = self._query(query)
        return results[0]["State"] if results else "playing"

    def set_game_state(self, state):
        """Set game state"""
        query = f"set_game_state({state})"
        self._query(query)

    def update_time(self, delta_time):
        """Update game time (decrements time_remaining)"""
        query = f"update_time({delta_time})"
        self._query(query)

    def get_time_remaining(self):
        """Get remaining time in seconds"""
        query = "get_time_remaining(Time)"
        results = self._query(query)
        if results:
            try:
                return float(results[0]["Time"])
            except (KeyError, ValueError, TypeError):
                return 180.0
        return 180.0

    def get_player_health(self):
        """Get player health (0-100)"""
        query = "get_player_health(HP)"
        results = self._query(query)
        if results:
            try:
                return int(results[0]["HP"])
            except (KeyError, ValueError, TypeError):
                return 100
        return 100

    def damage_player(self, amount):
        """
        Damage player (automatically checks for death)
        Sets game state to game_over_death if HP <= 0
        """
        query = f"damage_player({amount})"
        self._query(query)

    def heal_player(self, amount):
        """Heal player (max 100 HP)"""
        query = f"heal_player({amount})"
        self._query(query)

    def set_player_health(self, hp):
        """Set player health directly"""
        query = f"set_player_health({hp})"
        self._query(query)

    def get_player_inventory(self):
        """
        Get player inventory
        Returns: (arrows, rocks) tuple
        """
        query = "get_player_inventory(Arrows, Rocks)"
        results = self._query(query)
        if results:
            try:
                arrows = int(results[0]["Arrows"])
                rocks = int(results[0]["Rocks"])
                return (arrows, rocks)
            except (KeyError, ValueError, TypeError):
                return (3, 3)
        return (3, 3)

    def use_arrow(self):
        """
        Use one arrow from inventory
        Returns: True if arrow was used, False if no arrows
        """
        query = "use_arrow"
        results = self._query(query)
        return len(results) > 0

    def use_rock(self):
        """
        Use one rock from inventory
        Returns: True if rock was used, False if no rocks
        """
        query = "use_rock"
        results = self._query(query)
        return len(results) > 0

    def add_arrows(self, amount):
        """Add arrows to inventory (respects max limit)"""
        query = f"add_arrows({amount})"
        self._query(query)

    def add_rocks(self, amount):
        """Add rocks to inventory (respects max limit)"""
        query = f"add_rocks({amount})"
        self._query(query)

    def can_use_arrow(self):
        """Check if player has arrows"""
        query = "can_use_arrow"
        results = self._query(query)
        return len(results) > 0

    def can_use_rock(self):
        """Check if player has rocks"""
        query = "can_use_rock"
        results = self._query(query)
        return len(results) > 0

    def has_treasure(self):
        """Check if player has collected treasure"""
        query = "treasure_collected(true)"
        results = self._query(query)
        return len(results) > 0

    def is_exit_unlocked(self):
        """Check if exit is unlocked"""
        query = "exit_unlocked(true)"
        results = self._query(query)
        return len(results) > 0

    def check_victory_condition(self):
        """Check if player won (has treasure + at exit)"""
        query = "check_victory_condition"
        self._query(query)

    def check_game_over_conditions(self):
        """Check all game over conditions (death, timeout, victory)"""
        query = "check_game_over_conditions"
        self._query(query)

    def is_game_over(self):
        """Check if game is over (any end state)"""
        query = "is_game_over"
        results = self._query(query)
        return len(results) > 0

    def get_game_over_reason(self):
        """
        Get reason for game over
        Returns: String reason if game is over, None if still playing
        """
        query = "get_game_over_reason(Reason)"
        results = self._query(query)
        if results:
            reason = results[0]["Reason"]
            # Return None if game is still playing
            if reason == "none":
                return None
            return reason
        return None

    def init_spawn_config(self):
        """Initialize spawning configuration in Prolog"""
        query = "init_spawn_config"
        self._query(query)

    def generate_wumpus_spawns(self, count, min_distance=800):
        """Generate Wumpus spawn positions that are far apart"""
        query = f"generate_wumpus_spawns({count}, {min_distance}, Positions)"
        results = self._query(query)
        if results and "Positions" in results[0]:
            try:
                positions = results[0]["Positions"]
                spawn_list = []

                # Prolog returns list as [',(X,Y)', ',(X,Y)', ...]
                # Need to parse each string
                for pos_str in positions:
                    if isinstance(pos_str, str):
                        # Remove ',(' prefix and ')' suffix, then split
                        pos_str = pos_str.strip()
                        if pos_str.startswith(",(") and pos_str.endswith(")"):
                            # Extract "(X, Y)" part
                            coord_str = pos_str[1:]  # Remove leading ','
                            # Use eval to parse tuple (safe since we control the format)
                            pos = eval(coord_str)
                            if isinstance(pos, tuple) and len(pos) == 2:
                                spawn_list.append((int(pos[0]), int(pos[1])))
                    elif isinstance(pos, tuple) and len(pos) == 2:
                        # Already a tuple (shouldn't happen but handle it)
                        spawn_list.append((int(pos[0]), int(pos[1])))

                print(f"[Prolog] Generated {len(spawn_list)} Wumpus spawn positions")
                return spawn_list
            except (KeyError, ValueError, TypeError, SyntaxError) as e:
                print(f"[Prolog] Error parsing Wumpus spawns: {e}")
                return []
        return []

    def generate_pickup_spawns(self, count):
        """Generate pickup spawn positions (arrows/rocks)"""
        query = f"generate_pickup_spawns({count}, Positions)"
        results = self._query(query)
        if results and "Positions" in results[0]:
            try:
                positions = results[0]["Positions"]
                spawn_list = []

                # Parse Prolog list format [',(X,Y)', ...]
                for pos_str in positions:
                    if isinstance(pos_str, str):
                        pos_str = pos_str.strip()
                        if pos_str.startswith(",(") and pos_str.endswith(")"):
                            coord_str = pos_str[1:]
                            pos = eval(coord_str)
                            if isinstance(pos, tuple) and len(pos) == 2:
                                spawn_list.append((int(pos[0]), int(pos[1])))
                    elif isinstance(pos, tuple) and len(pos) == 2:
                        spawn_list.append((int(pos[0]), int(pos[1])))

                print(f"[Prolog] Generated {len(spawn_list)} pickup spawn positions")
                return spawn_list
            except (KeyError, ValueError, TypeError, SyntaxError) as e:
                print(f"[Prolog] Error parsing pickup spawns: {e}")
                return []
        return []

    # =========================================================================
    # WUMPUS AI SYSTEM
    # =========================================================================

    def init_wumpus_ai(self, wumpus_id, base_hearing):
        """Initialize Wumpus AI state"""
        query = f"init_wumpus_ai({wumpus_id}, {base_hearing})"
        self._query(query)
        print(
            f"[PrologEngine] Initialized Wumpus AI {wumpus_id} with hearing={base_hearing}"
        )

    def get_wumpus_state(self, wumpus_id):
        """Get Wumpus AI state"""
        query = f"get_wumpus_state({wumpus_id}, State)"
        results = self._query(query)
        if results:
            try:
                return results[0]["State"]
            except (KeyError, IndexError):
                return "roaming"
        return "roaming"

    def set_wumpus_state(self, wumpus_id, state):
        """Set Wumpus AI state"""
        query = f"set_wumpus_state({wumpus_id}, {state})"
        self._query(query)

    def get_wumpus_target(self, wumpus_id):
        """
        Get Wumpus target position
        Returns: (x, y) tuple or None
        """
        query = f"get_wumpus_target({wumpus_id}, X, Y)"
        results = self._query(query)
        if results:
            try:
                return (results[0]["X"], results[0]["Y"])
            except (KeyError, IndexError):
                return None
        return None

    def set_wumpus_target(self, wumpus_id, x, y):
        """Set Wumpus target position"""
        query = f"set_wumpus_target({wumpus_id}, {x}, {y})"
        self._query(query)

    def get_patrol_index(self, wumpus_id):
        """Get current patrol waypoint index"""
        query = f"get_patrol_index({wumpus_id}, Index)"
        results = self._query(query)
        if results:
            try:
                return results[0]["Index"]
            except (KeyError, IndexError):
                return 0
        return 0

    def set_patrol_index(self, wumpus_id, index):
        """Set patrol waypoint index"""
        query = f"set_patrol_index({wumpus_id}, {index})"
        self._query(query)

    def increment_patrol_index(self, wumpus_id, max_waypoints):
        """Increment patrol index with wrap-around"""
        query = f"increment_patrol_index({wumpus_id}, {max_waypoints})"
        self._query(query)

    def get_search_timer(self, wumpus_id):
        """Get search timer value"""
        query = f"get_search_timer({wumpus_id}, Time)"
        results = self._query(query)
        if results:
            try:
                return float(results[0]["Time"])
            except (KeyError, IndexError, ValueError):
                return 0.0
        return 0.0

    def set_search_timer(self, wumpus_id, time):
        """Set search timer value"""
        query = f"set_search_timer({wumpus_id}, {time})"
        self._query(query)

    def update_search_timer(self, wumpus_id, dt):
        """Update (decrease) search timer by dt"""
        query = f"update_search_timer({wumpus_id}, {dt})"
        self._query(query)

    def get_hearing_radius(self, wumpus_id):
        """Get current hearing radius"""
        query = f"get_hearing_radius({wumpus_id}, Radius)"
        results = self._query(query)
        if results:
            try:
                return float(results[0]["Radius"])
            except (KeyError, IndexError, ValueError):
                return 350.0
        return 350.0

    def set_hearing_radius(self, wumpus_id, radius):
        """Set hearing radius"""
        query = f"set_hearing_radius({wumpus_id}, {radius})"
        self._query(query)

    def is_roaring(self, wumpus_id):
        """Check if Wumpus is roaring"""
        query = f"is_roaring({wumpus_id})"
        results = self._query(query)
        return len(results) > 0

    def set_roaring(self, wumpus_id, is_roaring):
        """Set roaring state"""
        roar_str = "true" if is_roaring else "false"
        query = f"set_roaring({wumpus_id}, {roar_str})"
        self._query(query)

    def get_last_roar_time(self, wumpus_id):
        """Get last roar timestamp"""
        query = f"get_last_roar_time({wumpus_id}, Time)"
        results = self._query(query)
        if results:
            try:
                return float(results[0]["Time"])
            except (KeyError, IndexError, ValueError):
                return 0.0
        return 0.0

    def set_last_roar_time(self, wumpus_id, time):
        """Set last roar timestamp"""
        query = f"set_last_roar_time({wumpus_id}, {time})"
        self._query(query)

    def decide_wumpus_action(
        self, wumpus_id, sound_type, sound_x, sound_y, loudness, current_state
    ):
        """
        Decide Wumpus action based on sound
        Returns: (new_state, should_roar, target_x, target_y) tuple
        """
        query = (
            f"decide_wumpus_action({wumpus_id}, {sound_type}, {sound_x}, {sound_y}, "
            f"{loudness}, {current_state}, NewState, ShouldRoar, TargetX, TargetY)"
        )
        results = self._query(query)
        if results:
            try:
                new_state = results[0]["NewState"]
                should_roar = results[0]["ShouldRoar"] == "true"
                target_x = results[0]["TargetX"]
                target_y = results[0]["TargetY"]
                return (new_state, should_roar, target_x, target_y)
            except (KeyError, IndexError):
                return (current_state, False, 0, 0)
        return (current_state, False, 0, 0)

    def decide_no_sound_action(self, wumpus_id, current_state):
        """
        Decide action when no sound detected
        Returns: (new_state, search_time) tuple
        """
        query = f"decide_no_sound_action({wumpus_id}, {current_state}, NewState, SearchTime)"
        results = self._query(query)
        if results:
            try:
                new_state = results[0]["NewState"]
                search_time = float(results[0]["SearchTime"])
                return (new_state, search_time)
            except (KeyError, IndexError, ValueError):
                return (current_state, 0.0)
        return (current_state, 0.0)

    def reached_target(self, wumpus_x, wumpus_y, target_x, target_y, threshold=30):
        """Check if Wumpus reached target position"""
        query = f"reached_target({wumpus_x}, {wumpus_y}, {target_x}, {target_y}, {threshold})"
        results = self._query(query)
        return len(results) > 0

    def decide_target_reached(self, wumpus_id, current_state):
        """
        Decide behavior when target reached
        Returns: (new_state, search_time) tuple
        """
        query = (
            f"decide_target_reached({wumpus_id}, {current_state}, NewState, SearchTime)"
        )
        results = self._query(query)
        if results:
            try:
                new_state = results[0]["NewState"]
                search_time = float(results[0]["SearchTime"])
                return (new_state, search_time)
            except (KeyError, IndexError, ValueError):
                return ("roaming", 0.0)
        return ("roaming", 0.0)

    def search_timeout(self, wumpus_id):
        """Check if search timer has expired"""
        query = f"search_timeout({wumpus_id})"
        results = self._query(query)
        return len(results) > 0

    def can_roar(self, wumpus_id, current_time, cooldown):
        """Check if Wumpus can roar (cooldown elapsed)"""
        query = f"can_roar({wumpus_id}, {current_time}, {cooldown})"
        results = self._query(query)
        return len(results) > 0

    def should_attack(
        self, wumpus_x, wumpus_y, player_x, player_y, attack_range, current_state
    ):
        """Check if Wumpus should attack"""
        query = f"should_attack({wumpus_x}, {wumpus_y}, {player_x}, {player_y}, {attack_range}, {current_state})"
        results = self._query(query)
        return len(results) > 0

    def calculate_hearing_radius(self, wumpus_id, base_hearing, chase_bonus):
        """Calculate hearing radius based on current state"""
        query = f"calculate_hearing_radius({wumpus_id}, {base_hearing}, {chase_bonus}, Radius)"
        results = self._query(query)
        if results:
            try:
                return float(results[0]["Radius"])
            except (KeyError, IndexError, ValueError):
                return float(base_hearing)
        return float(base_hearing)
