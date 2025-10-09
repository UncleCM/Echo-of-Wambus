"""
Map system for the Wumpus game
Handles loading and rendering of the game world
"""

import pygame
import os
from typing import Tuple, Optional

class GameMap:
    """Manages the game map and collision detection"""
    
    def __init__(self, map_image_path: str):
        """Initialize the map with the given image path"""
        self.map_image_path = map_image_path
        self.map_surface = None
        self.map_rect = None
        self.collision_map = None
        
        self.load_map()
        
    def load_map(self):
        """Load the map image and prepare it for rendering"""
        try:
            # Load the map image
            self.map_surface = pygame.image.load(self.map_image_path)
            self.map_surface = self.map_surface.convert_alpha()
            
            # Store the map rectangle for easy access
            self.map_rect = self.map_surface.get_rect()
            
            # Create a simple collision map based on transparency
            # For now, we'll use a simple approach where non-transparent pixels are solid
            self._create_collision_map()
            
            print(f"Map loaded successfully: {self.map_rect.width}x{self.map_rect.height}")
            
        except pygame.error as e:
            print(f"Error loading map: {e}")
            # Create a fallback map if loading fails
            self.map_surface = pygame.Surface((800, 600))
            self.map_surface.fill((100, 150, 100))  # Green background
            self.map_rect = self.map_surface.get_rect()
            self.collision_map = None
    
    def _create_collision_map(self):
        """Create a collision map from the loaded image"""
        if not self.map_surface:
            return
            
        # Get the alpha channel to determine collision areas
        width, height = self.map_surface.get_size()
        
        # For now, we'll create a simple collision map
        # In a more advanced version, we could analyze pixel colors or use a separate collision layer
        self.collision_map = pygame.Surface((width, height))
        self.collision_map.fill((255, 255, 255))  # All white = walkable
        
        # Sample some areas as non-walkable for testing
        # This is a placeholder - in a real game, you'd define collision areas more precisely
        pygame.draw.rect(self.collision_map, (0, 0, 0), (100, 100, 200, 50))  # Black = solid
        pygame.draw.rect(self.collision_map, (0, 0, 0), (400, 300, 150, 100))
        pygame.draw.rect(self.collision_map, (0, 0, 0), (50, 400, 100, 150))
    
    def render(self, screen: pygame.Surface, camera_offset: Tuple[int, int] = (0, 0)):
        """Render the map to the screen with camera offset"""
        if self.map_surface:
            # Apply camera offset
            render_rect = self.map_rect.copy()
            render_rect.x -= camera_offset[0]
            render_rect.y -= camera_offset[1]
            
            screen.blit(self.map_surface, render_rect)
    
    def check_collision(self, rect: pygame.Rect, camera_offset: Tuple[int, int] = (0, 0)) -> bool:
        """
        Check if a rectangle collides with solid parts of the map
        
        Args:
            rect: The rectangle to check for collision
            camera_offset: Camera offset to adjust coordinates
            
        Returns:
            True if collision detected, False otherwise
        """
        if not self.collision_map:
            return False
            
        # Adjust rect position based on camera offset
        adjusted_rect = rect.copy()
        adjusted_rect.x += camera_offset[0]
        adjusted_rect.y += camera_offset[1]
        
        # Ensure the rect is within map bounds
        adjusted_rect = adjusted_rect.clamp(self.collision_map.get_rect())
        
        # Check if any pixels in the collision map are black (solid)
        try:
            # Create a subsurface of the collision map for the rect area
            collision_area = self.collision_map.subsurface(adjusted_rect)
            
            # Check if any pixels are black (non-walkable)
            for x in range(collision_area.get_width()):
                for y in range(collision_area.get_height()):
                    pixel_color = collision_area.get_at((x, y))
                    if pixel_color == (0, 0, 0, 255):  # Black pixel = solid
                        return True
                        
        except ValueError:
            # Rect is outside map bounds
            return True
            
        return False
    
    def get_map_size(self) -> Tuple[int, int]:
        """Get the size of the map"""
        if self.map_rect:
            return (self.map_rect.width, self.map_rect.height)
        return (800, 600)  # Default size
