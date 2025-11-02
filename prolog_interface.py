from pyswip import Prolog
from Settings import *


class PrologEngine:
    def __init__(self):
        self.available = False
        self.prolog = None
        
        try:
            self.prolog = Prolog()
            self.prolog.consult("game_logic.pl")
            self.available = True
            self.init_game()
            print("[PrologEngine] Initialized successfully")
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
                final_x = int(results[0]['FinalX'])
                final_y = int(results[0]['FinalY'])
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
                return results[0]['CollisionCount'], results[0]['FallCount']
            except (KeyError, IndexError):
                return 0, 0
        return 0, 0
    
    def is_safe_position(self, x, y, w, h, feet_height=10):
        query = f"is_safe_position({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0
    
    def find_safe_spawn(self, entrance_x, entrance_y, w, h, feet_height=10):
        query = f"find_safe_spawn({entrance_x}, {entrance_y}, SafeX, SafeY, {w}, {h}, {feet_height})"
        results = self._query(query)
        if results:
            try:
                return results[0]['SafeX'], results[0]['SafeY']
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
    
    def get_wumpus_decision(self, wumpus_x, wumpus_y, player_x, player_y, current_state):
        query = f"wumpus_decision({wumpus_x}, {wumpus_y}, {player_x}, {player_y}, {current_state}, NewState, DirectionX, DirectionY)"
        results = self._query(query)
        
        if results:
            try:
                new_state = results[0]['NewState']
                direction_x = float(results[0]['DirectionX'])
                direction_y = float(results[0]['DirectionY'])
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
        print(f"[PrologEngine] Setup treasure system at positions: {pos1}, {pos2}, {pos3}")
    
    def open_chest(self, player_x, player_y):
        query = f"open_chest({player_x}, {player_y}, Result)"
        results = self._query(query)
        if results:
            try:
                result = str(results[0]['Result'])
                print(f"[PrologEngine] open_chest result: {result}")
                return result
            except (KeyError, IndexError):
                return 'no_chest'
        return 'no_chest'
    
    def get_all_chests(self):
        chests = []
        
        treasure_query = "treasure(ID, X, Y)"
        treasures = self._query(treasure_query)
        for t in treasures:
            chests.append({
                'id': t['ID'],
                'x': float(t['X']),
                'y': float(t['Y']),
                'type': 'treasure'
            })
        
        mimic_query = "mimic(ID, X, Y, Activated)"
        mimics = self._query(mimic_query)
        for m in mimics:
            chests.append({
                'id': m['ID'],
                'x': float(m['X']),
                'y': float(m['Y']),
                'type': 'mimic',
                'activated': m['Activated'] == 'true'
            })
        
        print(f"[PrologEngine] Found {len(chests)} chests total")
        return chests
    
    def spawn_wumpus(self, x, y):
        query = f"spawn_wumpus({x}, {y}, WumpusID)"
        results = self._query(query)
        if results:
            try:
                wumpus_id = results[0]['WumpusID']
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
    
    def arrow_hit_wumpus(self, arrow_x, arrow_y, wumpus_x, wumpus_y, hit_radius=60):
        """
        Check if arrow hits Wumpus
        Returns: True if hit, False otherwise
        """
        query = f"arrow_hit_wumpus({arrow_x}, {arrow_y}, {wumpus_x}, {wumpus_y}, {hit_radius})"
        results = self._query(query)
        return len(results) > 0
    
    def wumpus_can_attack_player(self, wumpus_x, wumpus_y, player_x, player_y, attack_range, wumpus_state):
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
                return results[0]['PickupID']
            except (KeyError, IndexError):
                return None
        return None
    
    def can_pickup_rock(self, player_x, player_y, player_w, player_h):
        """
        Check if player can pickup any rock
        Returns: Pickup ID if found, None otherwise
        """
        query = f"can_pickup_rock({player_x}, {player_y}, {player_w}, {player_h}, PickupID)"
        results = self._query(query)
        if results:
            try:
                return results[0]['PickupID']
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