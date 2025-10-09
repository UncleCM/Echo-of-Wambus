"""
Enhanced map system that can work with both PSD and PNG files
"""

import pygame
import os
from typing import Tuple, Optional
from .map import GameMap

class EnhancedGameMap(GameMap):
    """Enhanced map system that supports PSD files"""
    
    def __init__(self, map_path: str, prefer_psd: bool = True):
        """
        Initialize the enhanced map
        
        Args:
            map_path: Path to map file (can be .psd or .png)
            prefer_psd: If True, try to load PSD first, then fall back to PNG
        """
        self.prefer_psd = prefer_psd
        self.original_path = map_path
        
        # Determine which file to load
        self.map_image_path = self._determine_map_file(map_path)
        
        # Initialize the base GameMap
        super().__init__(self.map_image_path)
    
    def _determine_map_file(self, map_path: str) -> str:
        """
        Determine which map file to use (PSD or PNG)
        
        Args:
            map_path: Original map path
            
        Returns:
            Path to the file to load
        """
        # Try different variations of the path
        possible_paths = []
        
        if self.prefer_psd:
            # Try PSD first
            if map_path.endswith('.png'):
                # Replace .png with .psd
                psd_path = map_path.replace('.png', '.psd')
                possible_paths.append(psd_path)
            else:
                # Try adding .psd extension
                possible_paths.append(map_path + '.psd')
                possible_paths.append(map_path)
        
        # Always try the original path
        possible_paths.append(map_path)
        
        # Try PNG alternatives
        if not map_path.endswith('.png'):
            png_path = map_path + '.png'
            possible_paths.append(png_path)
        
        # Find the first existing file
        for path in possible_paths:
            if os.path.exists(path):
                print(f"Using map file: {path}")
                return path
        
        # If no file found, return the original path (will cause error with helpful message)
        print(f"Warning: No map file found. Tried: {possible_paths}")
        return map_path
    
    def load_map(self):
        """Load the map with PSD support"""
        # Check if it's a PSD file
        if self.map_image_path.lower().endswith('.psd'):
            self._load_psd_map()
        else:
            # Use the standard PNG loading
            super().load_map()
    
    def _load_psd_map(self):
        """Load a PSD file and convert it for use"""
        try:
            # Import PSD tools
            from PIL import Image
            from psd_tools import PSDImage
            
            print(f"Loading PSD file: {self.map_image_path}")
            
            # Load the PSD file
            psd = PSDImage.open(self.map_image_path)
            
            # Convert to PIL Image
            pil_image = psd.composite()
            
            # Convert PIL image to pygame surface
            # First convert PIL to bytes
            import io
            img_bytes = io.BytesIO()
            pil_image.save(img_bytes, format='PNG')
            img_bytes.seek(0)
            
            # Load into pygame
            self.map_surface = pygame.image.load(img_bytes)
            self.map_surface = self.map_surface.convert_alpha()
            
            # Store the map rectangle
            self.map_rect = self.map_surface.get_rect()
            
            # Create collision map
            self._create_collision_map()
            
            print(f"PSD map loaded successfully: {self.map_rect.width}x{self.map_rect.height}")
            
        except ImportError:
            print("Error: PSD tools not available. Please install: pip install psd-tools pillow")
            print("Falling back to standard map loading...")
            super().load_map()
        except Exception as e:
            print(f"Error loading PSD file: {e}")
            print("Falling back to standard map loading...")
            super().load_map()
