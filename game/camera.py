"""
2.5D Camera system for top-down view
Handles camera positioning, zoom, and smooth following
"""

import pygame
import math
from typing import Tuple

class Camera2D5:
    """2.5D camera system for top-down gameplay"""
    
    def __init__(self, screen_width: int, screen_height: int):
        """Initialize the camera"""
        self.screen_width = screen_width
        self.screen_height = screen_height
        
        # Camera position (world coordinates)
        self.x = 0.0
        self.y = 0.0
        
        # Camera zoom
        self.zoom = 1.0
        self.min_zoom = 0.5
        self.max_zoom = 3.0
        
        # Smooth following
        self.follow_speed = 8.0
        self.target_x = 0.0
        self.target_y = 0.0
        
        # Camera shake
        self.shake_intensity = 0.0
        self.shake_duration = 0.0
        self.shake_timer = 0.0
        
        # Camera bounds
        self.bounds = None  # (min_x, min_y, max_x, max_y)
        
    def set_bounds(self, min_x: float, min_y: float, max_x: float, max_y: float):
        """Set camera movement bounds"""
        self.bounds = (min_x, min_y, max_x, max_y)
    
    def follow_target(self, target_x: float, target_y: float):
        """Set camera target for smooth following"""
        self.target_x = target_x
        self.target_y = target_y
    
    def update(self, dt: float):
        """Update camera position"""
        # Smooth following
        dx = self.target_x - self.x
        dy = self.target_y - self.y
        
        # Apply smooth following
        self.x += dx * self.follow_speed * dt
        self.y += dy * self.follow_speed * dt
        
        # Apply camera bounds
        if self.bounds:
            min_x, min_y, max_x, max_y = self.bounds
            self.x = max(min_x, min(max_x, self.x))
            self.y = max(min_y, min(max_y, self.y))
        
        # Update camera shake
        if self.shake_timer > 0:
            self.shake_timer -= dt
            if self.shake_timer <= 0:
                self.shake_intensity = 0.0
    
    def apply_shake(self, intensity: float, duration: float):
        """Apply camera shake effect"""
        self.shake_intensity = intensity
        self.shake_duration = duration
        self.shake_timer = duration
    
    def zoom_in(self, amount: float = 0.1):
        """Zoom in the camera"""
        self.zoom = min(self.max_zoom, self.zoom + amount)
    
    def zoom_out(self, amount: float = 0.1):
        """Zoom out the camera"""
        self.zoom = max(self.min_zoom, self.zoom - amount)
    
    def set_zoom(self, zoom: float):
        """Set camera zoom level"""
        self.zoom = max(self.min_zoom, min(self.max_zoom, zoom))
    
    def world_to_screen(self, world_x: float, world_y: float) -> Tuple[int, int]:
        """Convert world coordinates to screen coordinates"""
        # Apply camera position
        screen_x = world_x - self.x
        screen_y = world_y - self.y
        
        # Apply zoom
        screen_x *= self.zoom
        screen_y *= self.zoom
        
        # Center on screen
        screen_x += self.screen_width // 2
        screen_y += self.screen_height // 2
        
        # Apply camera shake
        if self.shake_intensity > 0:
            import random
            shake_x = random.uniform(-self.shake_intensity, self.shake_intensity)
            shake_y = random.uniform(-self.shake_intensity, self.shake_intensity)
            screen_x += shake_x
            screen_y += shake_y
        
        return int(screen_x), int(screen_y)
    
    def screen_to_world(self, screen_x: int, screen_y: int) -> Tuple[float, float]:
        """Convert screen coordinates to world coordinates"""
        # Center on screen
        world_x = screen_x - self.screen_width // 2
        world_y = screen_y - self.screen_height // 2
        
        # Apply zoom
        world_x /= self.zoom
        world_y /= self.zoom
        
        # Apply camera position
        world_x += self.x
        world_y += self.y
        
        return world_x, world_y
    
    def get_camera_offset(self) -> Tuple[float, float]:
        """Get camera offset for rendering"""
        # Calculate shake offset
        shake_x = 0.0
        shake_y = 0.0
        if self.shake_intensity > 0:
            import random
            shake_x = random.uniform(-self.shake_intensity, self.shake_intensity)
            shake_y = random.uniform(-self.shake_intensity, self.shake_intensity)
        
        return (self.x - self.screen_width // (2 * self.zoom) + shake_x,
                self.y - self.screen_height // (2 * self.zoom) + shake_y)
    
    def is_visible(self, world_x: float, world_y: float, width: float, height: float) -> bool:
        """Check if a world rectangle is visible on screen"""
        # Get camera bounds in world coordinates
        camera_left = self.x - self.screen_width // (2 * self.zoom)
        camera_right = self.x + self.screen_width // (2 * self.zoom)
        camera_top = self.y - self.screen_height // (2 * self.zoom)
        camera_bottom = self.y + self.screen_height // (2 * self.zoom)
        
        # Check overlap
        return not (world_x + width < camera_left or 
                   world_x > camera_right or
                   world_y + height < camera_top or
                   world_y > camera_bottom)
