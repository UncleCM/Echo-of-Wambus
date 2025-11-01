"""
Map Knowledge System for Wumpus AI
Provides complete map awareness for navigation and pit avoidance
"""

import pygame
import random


class MapKnowledge:
    """
    Stores Wumpus's complete knowledge of the map layout
    Includes walls, pits, and safe navigation paths
    """
    
    def __init__(self, tmx_map, collision_sprites, fall_sprites):
        """
        Initialize map knowledge from game map
        
        Args:
            tmx_map: TMX map object
            collision_sprites: sprite group with collision boxes (walls)
            fall_sprites: sprite group with fall zones (pits)
        """
        # Map dimensions
        self.width = tmx_map.width * tmx_map.tilewidth
        self.height = tmx_map.height * tmx_map.tileheight
        
        print(f"[MapKnowledge] Map size: {self.width} x {self.height}")
        
        # Store hazard locations
        self.walls = []  # List of pygame.Rect
        self.pits = []   # List of pygame.Rect
        
        # Build knowledge from sprite groups
        for sprite in collision_sprites:
            if hasattr(sprite, 'rect'):
                self.walls.append(sprite.rect.copy())
        
        for sprite in fall_sprites:
            if hasattr(sprite, 'rect'):
                self.pits.append(sprite.rect.copy())
        
        print(f"[MapKnowledge] Loaded {len(self.walls)} walls, {len(self.pits)} pits")
        
        # Navigation grid for efficient pathfinding
        self.grid_size = 32  # 32x32 pixel cells
        self.navigation_grid = self._build_navigation_grid()
        
        # Cache for safe positions (performance optimization)
        self.safe_positions_cache = []
        self._build_safe_positions_cache()
    
    def _build_navigation_grid(self):
        """
        Build a 2D grid marking safe/unsafe cells
        True = safe, False = unsafe (wall/pit)
        """
        cols = (self.width // self.grid_size) + 1
        rows = (self.height // self.grid_size) + 1
        
        # Initialize all cells as safe
        grid = [[True for _ in range(cols)] for _ in range(rows)]
        
        # Mark walls as unsafe
        for wall in self.walls:
            self._mark_rect_unsafe(grid, wall)
        
        # Mark pits as unsafe (with margin)
        for pit in self.pits:
            # Expand pit rect to include danger margin
            danger_rect = pit.inflate(32, 32)  # 16px margin on each side
            self._mark_rect_unsafe(grid, danger_rect)
        
        # Count safe vs unsafe cells
        safe_count = sum(row.count(True) for row in grid)
        total_count = cols * rows
        print(f"[MapKnowledge] Navigation grid: {cols}x{rows} = {total_count} cells")
        print(f"[MapKnowledge] Safe cells: {safe_count}/{total_count} ({100*safe_count/total_count:.1f}%)")
        
        return grid
    
    def _mark_rect_unsafe(self, grid, rect):
        """Mark all grid cells overlapping with rect as unsafe"""
        start_col = max(0, rect.left // self.grid_size)
        end_col = min(len(grid[0]) - 1, rect.right // self.grid_size)
        start_row = max(0, rect.top // self.grid_size)
        end_row = min(len(grid) - 1, rect.bottom // self.grid_size)
        
        for row in range(start_row, end_row + 1):
            for col in range(start_col, end_col + 1):
                if 0 <= row < len(grid) and 0 <= col < len(grid[0]):
                    grid[row][col] = False
    
    def _build_safe_positions_cache(self):
        """Pre-compute a list of safe positions for quick random selection"""
        self.safe_positions_cache = []
        
        for row in range(len(self.navigation_grid)):
            for col in range(len(self.navigation_grid[0])):
                if self.navigation_grid[row][col]:
                    # Convert grid cell to world position (center of cell)
                    x = col * self.grid_size + self.grid_size // 2
                    y = row * self.grid_size + self.grid_size // 2
                    self.safe_positions_cache.append((x, y))
        
        print(f"[MapKnowledge] Cached {len(self.safe_positions_cache)} safe positions")
    
    def is_position_safe(self, x, y):
        """
        Check if a world position is safe (not in wall/pit)
        
        Args:
            x, y: world coordinates
            
        Returns:
            bool: True if safe, False if unsafe
        """
        col = int(x) // self.grid_size
        row = int(y) // self.grid_size
        
        # Check bounds
        if row < 0 or row >= len(self.navigation_grid):
            return False
        if col < 0 or col >= len(self.navigation_grid[0]):
            return False
        
        return self.navigation_grid[row][col]
    
    def is_near_pit(self, x, y, danger_radius=50):
        """
        Check if position is dangerously close to any pit
        
        Args:
            x, y: world coordinates
            danger_radius: distance to consider dangerous (pixels)
            
        Returns:
            bool: True if near pit, False if safe distance
        """
        pos = pygame.math.Vector2(x, y)
        
        for pit in self.pits:
            # Check distance to pit center
            pit_center = pygame.math.Vector2(pit.center)
            distance = pos.distance_to(pit_center)
            
            # Also check distance to pit edges (use rect corners)
            min_distance = distance
            for corner in [pit.topleft, pit.topright, pit.bottomleft, pit.bottomright]:
                corner_dist = pos.distance_to(pygame.math.Vector2(corner))
                min_distance = min(min_distance, corner_dist)
            
            if min_distance < danger_radius:
                return True
        
        return False
    
    def get_safe_random_position(self):
        """
        Get a random safe position for Wumpus roaming
        Uses pre-cached safe positions for performance
        
        Returns:
            tuple: (x, y) world coordinates, or None if no safe positions
        """
        if not self.safe_positions_cache:
            return None
        
        return random.choice(self.safe_positions_cache)
    
    def get_safe_direction_away_from_pit(self, current_pos, danger_radius=50):
        """
        Get a safe direction to move away from nearest pit
        
        Args:
            current_pos: (x, y) or Vector2 - current position
            danger_radius: how close is too close
            
        Returns:
            Vector2: safe direction to move, or None if already safe
        """
        pos = pygame.math.Vector2(current_pos)
        nearest_pit = None
        min_distance = float('inf')
        
        # Find nearest pit
        for pit in self.pits:
            pit_center = pygame.math.Vector2(pit.center)
            distance = pos.distance_to(pit_center)
            
            if distance < min_distance:
                min_distance = distance
                nearest_pit = pit_center
        
        # If near a pit, move away from it
        if nearest_pit and min_distance < danger_radius:
            # Direction away from pit
            away_direction = (pos - nearest_pit).normalize()
            return away_direction
        
        return None  # Already safe
    
    def find_safe_alternative_directions(self, current_pos, intended_direction, check_distance=50):
        """
        Find alternative safe directions when intended direction leads to danger
        
        Args:
            current_pos: (x, y) or Vector2
            intended_direction: Vector2 - where we want to go
            check_distance: how far ahead to check
            
        Returns:
            list of Vector2: alternative safe directions, sorted by similarity to intended
        """
        pos = pygame.math.Vector2(current_pos)
        intended_vec = pygame.math.Vector2(intended_direction)
        
        # Test positions at different angles
        test_angles = [0, 45, -45, 90, -90, 135, -135, 180]
        safe_directions = []
        
        for angle in test_angles:
            test_dir = intended_vec.rotate(angle)
            test_pos = pos + test_dir * check_distance
            
            # Check if test position is safe
            if self.is_position_safe(test_pos.x, test_pos.y):
                if not self.is_near_pit(test_pos.x, test_pos.y, danger_radius=40):
                    safe_directions.append((test_dir, abs(angle)))
        
        # Sort by angle difference (prefer directions closer to intended)
        safe_directions.sort(key=lambda x: x[1])
        
        return [direction for direction, angle in safe_directions]
    
    def get_grid_info(self):
        """Get grid dimensions for debugging"""
        return {
            'rows': len(self.navigation_grid),
            'cols': len(self.navigation_grid[0]) if self.navigation_grid else 0,
            'cell_size': self.grid_size,
            'safe_positions': len(self.safe_positions_cache)
        }
