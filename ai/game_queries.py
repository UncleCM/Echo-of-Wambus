"""Prolog queries for game mechanics (treasure, combat, items)"""
from ai.prolog_engine import PrologEngine


class GameQueries(PrologEngine):
    """Prolog interface for game mechanics"""
    
    # =========================================================================
    # GAME STATE
    # =========================================================================
    
    def set_game_over(self, state):
        """Set game over state"""
        state_str = "true" if state else "false"
        query = f"set_game_over({state_str})"
        self._query(query)
    
    def is_game_over(self):
        """Check if game is over"""
        query = "is_game_over(true)"
        results = self._query(query)
        return len(results) > 0
    
    # =========================================================================
    # TREASURE SYSTEM
    # =========================================================================
    
    def setup_treasure_system(self, pos1, pos2, pos3):
        """
        Setup treasure system with 3 chest positions
        
        Args:
            pos1, pos2, pos3: (x, y) tuples for chest positions
        """
        x1, y1 = pos1
        x2, y2 = pos2
        x3, y3 = pos3
        query = f"setup_treasure_system({x1}, {y1}, {x2}, {y2}, {x3}, {y3})"
        self._query(query)
        print(f"[PrologEngine] Setup treasure system at positions: {pos1}, {pos2}, {pos3}")
    
    def open_chest(self, player_x, player_y):
        """
        Open chest at player position
        
        Returns:
            str: 'treasure', 'mimic', or 'no_chest'
        """
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
        """
        Get all chests (treasure and mimics)
        
        Returns:
            list: List of chest dicts with id, x, y, type
        """
        chests = []
        
        # Get treasure chests
        treasure_query = "treasure(ID, X, Y)"
        treasures = self._query(treasure_query)
        for t in treasures:
            chests.append({
                'id': t['ID'],
                'x': float(t['X']),
                'y': float(t['Y']),
                'type': 'treasure'
            })
        
        # Get mimic chests
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
    
    def collect_treasure(self):
        """Mark treasure as collected"""
        query = "collect_treasure"
        self._query(query)
        print("[PrologEngine] Treasure collected")
    
    def near_chest(self, player_x, player_y, interaction_radius=60):
        """
        Check if player is near any chest (for interaction prompt)
        
        Returns:
            bool: True if near chest
        """
        query = f"near_chest({player_x}, {player_y}, {interaction_radius})"
        results = self._query(query)
        return len(results) > 0
    
    # =========================================================================
    # COMBAT SYSTEM
    # =========================================================================
    
    def arrow_hit_wumpus(self, arrow_x, arrow_y, wumpus_x, wumpus_y, hit_radius=60):
        """
        Check if arrow hits Wumpus
        
        Returns:
            bool: True if hit
        """
        query = f"arrow_hit_wumpus({arrow_x}, {arrow_y}, {wumpus_x}, {wumpus_y}, {hit_radius})"
        results = self._query(query)
        return len(results) > 0
    
    def wumpus_can_attack_player(self, wumpus_x, wumpus_y, player_x, player_y, attack_range, wumpus_state):
        """
        Check if Wumpus can attack player
        
        Returns:
            bool: True if can attack
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
        
        Returns:
            bool: True if all victory conditions met
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
        
        Returns:
            int or None: Pickup ID if found
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
        
        Returns:
            int or None: Pickup ID if found
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
