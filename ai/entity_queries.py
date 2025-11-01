"""Prolog queries for AI and game entities"""
from ai.prolog_engine import PrologEngine


class EntityQueries(PrologEngine):
    """Prolog interface for game entities (player, wumpus, etc.)"""
    
    # =========================================================================
    # PLAYER
    # =========================================================================
    
    def update_player_position(self, x, y):
        """Update player position in Prolog world"""
        query = f"update_player_position({x}, {y})"
        self._query(query)
    
    # =========================================================================
    # WUMPUS AI
    # =========================================================================
    
    def init_wumpus(self, x, y):
        """Initialize Wumpus entity in Prolog world"""
        query = f"init_wumpus({x}, {y})"
        self._query(query)
    
    def update_wumpus_position(self, x, y):
        """Update Wumpus position in Prolog world"""
        query = f"update_wumpus_position({x}, {y})"
        self._query(query)
    
    def update_wumpus_state(self, state):
        """Update Wumpus AI state"""
        query = f"update_wumpus_state({state})"
        self._query(query)
    
    def get_wumpus_decision(self, wumpus_x, wumpus_y, player_x, player_y, current_state):
        """
        Get Wumpus AI decision
        
        Args:
            wumpus_x, wumpus_y: Wumpus position
            player_x, player_y: Player position
            current_state: Current AI state
            
        Returns:
            tuple: (new_state, direction_x, direction_y)
        """
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
    
    def spawn_wumpus(self, x, y):
        """
        Spawn Wumpus in Prolog world
        
        Returns:
            int or None: Wumpus ID if successful
        """
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
