from pyswip import Prolog
from Settings import *

class PrologEngine:
    def __init__(self):
        self.prolog = Prolog()
        # Load the Prolog knowledge base
        self.prolog.consult("game_logic.pl")
        self.init_game()
    
    def init_game(self):
        """Initialize Prolog game state"""
        list(self.prolog.query("init_game"))
    
    def add_collision_box(self, x, y, w, h):
        """Add a collision box to Prolog knowledge base"""
        query = f"add_collision_box({x}, {y}, {w}, {h})"
        list(self.prolog.query(query))
    
    def add_fall_zone(self, x, y, w, h):
        """Add a fall zone to Prolog knowledge base"""
        query = f"add_fall_zone({x}, {y}, {w}, {h})"
        list(self.prolog.query(query))
    
    def check_collision(self, x, y, w, h):
        """Check if position collides with any collision box"""
        query = f"check_collision({x}, {y}, {w}, {h})"
        results = list(self.prolog.query(query))
        return len(results) > 0
    
    def check_fall(self, x, y, w, h, feet_height=10):
        """Check if player feet touch any fall zone"""
        query = f"should_fall({x}, {y}, {w}, {h}, {feet_height})"
        results = list(self.prolog.query(query))
        return len(results) > 0
    
    def can_move(self, new_x, new_y, w, h):
        """Check if player can move to new position"""
        query = f"can_move({new_x}, {new_y}, {w}, {h})"
        results = list(self.prolog.query(query))
        return len(results) > 0
    
    def update_player_position(self, x, y):
        """Update player position in Prolog"""
        query = f"update_player_position({x}, {y})"
        list(self.prolog.query(query))
    
    def set_game_over(self, state):
        """Set game over state"""
        state_str = "true" if state else "false"
        query = f"set_game_over({state_str})"
        list(self.prolog.query(query))
    
    def is_game_over(self):
        """Check if game is over"""
        query = "is_game_over(true)"
        results = list(self.prolog.query(query))
        return len(results) > 0
    
    def count_hazards(self):
        """Count collision boxes and fall zones"""
        query = "count_hazards(CollisionCount, FallCount)"
        results = list(self.prolog.query(query))
        if results:
            return results[0]['CollisionCount'], results[0]['FallCount']
        return 0, 0
    
    def is_safe_position(self, x, y, w, h, feet_height=10):
        """Check if position is safe (no collision or fall)"""
        query = f"is_safe_position({x}, {y}, {w}, {h}, {feet_height})"
        results = list(self.prolog.query(query))
        return len(results) > 0
    
    def clear_all(self):
        """Clear all collision boxes and fall zones"""
        list(self.prolog.query("clear_collision_boxes"))
        list(self.prolog.query("clear_fall_zones"))