from pyswip import Prolog
from Settings import *

class PrologEngine:
    def __init__(self):
        try:
            self.prolog = Prolog()
            # Load the Prolog knowledge base
            self.prolog.consult("game_logic.pl")
            self.init_game()
            self.available = True
            print("[PrologEngine] Initialized successfully")
        except Exception as e:
            print(f"[PrologEngine] Failed to initialize: {e}")
            print("[PrologEngine] Running with fallback collision system")
            self.prolog = None
            self.available = False
    
    def _query(self, query_str):
        """Safe wrapper around Prolog queries"""
        if not self.available or self.prolog is None:
            return []
        try:
            return list(self.prolog.query(query_str))
        except Exception as e:
            print(f"[PrologEngine] Query failed: {query_str} -> {e}")
            return []
    
    def init_game(self):
        """Initialize Prolog game state"""
        self._query("init_game")
    
    def add_collision_box(self, x, y, w, h):
        """Add a collision box to Prolog knowledge base"""
        query = f"add_collision_box({x}, {y}, {w}, {h})"
        self._query(query)
    
    def add_fall_zone(self, x, y, w, h):
        """Add a fall zone to Prolog knowledge base"""
        query = f"add_fall_zone({x}, {y}, {w}, {h})"
        self._query(query)
    
    def check_collision(self, x, y, w, h):
        """Check if position collides with any collision box"""
        query = f"check_collision({x}, {y}, {w}, {h})"
        results = self._query(query)
        return len(results) > 0
    
    def check_fall(self, x, y, w, h, feet_height=10):
        """Check if player feet touch any fall zone"""
        query = f"check_fall({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0
    
    # =========================================================================
    # NEW LOGIC: MOVEMENT RESOLUTION INTERFACE
    # =========================================================================
    def resolve_movement(self, current_x, current_y, delta_x, delta_y, w, h):
        """
        Calculates the resolved position after movement and collision checking 
        using the new Prolog rule.
        Updates player_position fact in Prolog.
        Returns the resolved (FinalX, FinalY) tuple.
        """
        # The 'resolve_movement' rule is responsible for updating player_position
        query = f"resolve_movement({current_x}, {current_y}, {delta_x}, {delta_y}, {w}, {h}, FinalX, FinalY)"
        results = self._query(query)
        
        if results:
            try:
                # Prolog returns floating point numbers from calculations like 'is'
                # Convert them back to integers for Pygame Rect
                final_x = int(results[0]['FinalX'])
                final_y = int(results[0]['FinalY'])
                return final_x, final_y
            except (KeyError, ValueError, TypeError) as e:
                print(f"[PrologEngine] Malformed resolve_movement result: {e}")
                return current_x, current_y
        
        # Fallback to current position if query fails
        return current_x, current_y

    # =========================================================================
    # EXISTING METHODS
    # =========================================================================
    
    def update_player_position(self, x, y):
        """Update player position in Prolog"""
        # This is now mainly called by resolve_movement, but kept for direct use
        query = f"update_player_position({x}, {y})"
        self._query(query)
    
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
    
    def count_hazards(self):
        """Count collision boxes and fall zones"""
        query = "count_hazards(CollisionCount, FallCount)"
        results = self._query(query)
        if results:
            try:
                return results[0]['CollisionCount'], results[0]['FallCount']
            except (KeyError, IndexError):
                return 0, 0
        return 0, 0
    
    def is_safe_position(self, x, y, w, h, feet_height=10):
        """Check if position is safe (no collision or fall)"""
        query = f"is_safe_position({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0

    def find_safe_spawn(self, entrance_x, entrance_y, w, h, feet_height=10):
        """Find a safe spawn position near the entrance"""
        query = f"find_safe_spawn({entrance_x}, {entrance_y}, SafeX, SafeY, {w}, {h}, {feet_height})"
        results = self._query(query)
        if results:
            try:
                return results[0]['SafeX'], results[0]['SafeY']
            except (KeyError, IndexError):
                return entrance_x, entrance_y
        return entrance_x, entrance_y