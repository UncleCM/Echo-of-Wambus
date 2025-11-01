"""Prolog queries for physics and collision detection"""
from ai.prolog_engine import PrologEngine


class PhysicsQueries(PrologEngine):
    """Prolog interface for physics and collision system"""
    
    def add_collision_box(self, x, y, w, h):
        """Add a collision box to the Prolog world"""
        query = f"add_collision_box({x}, {y}, {w}, {h})"
        self._query(query)
    
    def add_fall_zone(self, x, y, w, h):
        """Add a fall zone (pit) to the Prolog world"""
        query = f"add_fall_zone({x}, {y}, {w}, {h})"
        self._query(query)
    
    def check_collision(self, x, y, w, h):
        """
        Check if position collides with any collision box
        
        Returns:
            bool: True if collision detected
        """
        query = f"check_collision({x}, {y}, {w}, {h})"
        results = self._query(query)
        return len(results) > 0
    
    def check_fall(self, x, y, w, h, feet_height=10):
        """
        Check if position would fall into pit
        
        Returns:
            bool: True if fall detected
        """
        query = f"check_fall({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0
    
    def resolve_movement(self, current_x, current_y, delta_x, delta_y, w, h):
        """
        Resolve movement with collision detection
        
        Args:
            current_x, current_y: Current position
            delta_x, delta_y: Desired movement delta
            w, h: Entity dimensions
            
        Returns:
            tuple: (final_x, final_y) after collision resolution
        """
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
    
    def is_safe_position(self, x, y, w, h, feet_height=10):
        """
        Check if position is safe (no collision, no fall)
        
        Returns:
            bool: True if position is safe
        """
        query = f"is_safe_position({x}, {y}, {w}, {h}, {feet_height})"
        results = self._query(query)
        return len(results) > 0
    
    def find_safe_spawn(self, entrance_x, entrance_y, w, h, feet_height=10):
        """
        Find safe spawn position near entrance
        
        Returns:
            tuple: (safe_x, safe_y) or entrance position if not found
        """
        query = f"find_safe_spawn({entrance_x}, {entrance_y}, SafeX, SafeY, {w}, {h}, {feet_height})"
        results = self._query(query)
        if results:
            try:
                return results[0]['SafeX'], results[0]['SafeY']
            except (KeyError, IndexError):
                return entrance_x, entrance_y
        return entrance_x, entrance_y
    
    def count_hazards(self):
        """
        Count collision boxes and fall zones
        
        Returns:
            tuple: (collision_count, fall_count)
        """
        query = "count_hazards(CollisionCount, FallCount)"
        results = self._query(query)
        if results:
            try:
                return results[0]['CollisionCount'], results[0]['FallCount']
            except (KeyError, IndexError):
                return 0, 0
        return 0, 0
