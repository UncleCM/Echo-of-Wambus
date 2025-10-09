"""
Sprite sheet parser for handling single images with multiple animation frames
"""

import pygame
from typing import List, Dict, Tuple, Optional

class SpriteSheet:
    """Handles parsing and animation of sprite sheets"""
    
    def __init__(self, image_path: str, frame_width: int, frame_height: int, 
                 frame_count: int, rows: int = 1, cols: int = None):
        """
        Initialize sprite sheet
        
        Args:
            image_path: Path to the sprite sheet image
            frame_width: Width of each frame
            frame_height: Height of each frame
            frame_count: Total number of frames
            rows: Number of rows in the sprite sheet
            cols: Number of columns (auto-calculated if None)
        """
        self.image_path = image_path
        self.frame_width = frame_width
        self.frame_height = frame_height
        self.frame_count = frame_count
        self.rows = rows
        self.cols = cols or (frame_count // rows)
        
        # Load the sprite sheet
        self.sheet = pygame.image.load(image_path)
        self.sheet = self.sheet.convert_alpha()
        
        # Extract individual frames
        self.frames: List[pygame.Surface] = []
        self._extract_frames()
        
    def _extract_frames(self):
        """Extract individual frames from the sprite sheet"""
        for row in range(self.rows):
            for col in range(self.cols):
                if len(self.frames) >= self.frame_count:
                    break
                    
                # Calculate frame position
                x = col * self.frame_width
                y = row * self.frame_height
                
                # Create a subsurface for this frame
                frame_rect = pygame.Rect(x, y, self.frame_width, self.frame_height)
                frame = self.sheet.subsurface(frame_rect)
                
                self.frames.append(frame)
    
    def get_frame(self, index: int) -> Optional[pygame.Surface]:
        """Get a specific frame by index"""
        if 0 <= index < len(self.frames):
            return self.frames[index]
        return None
    
    def get_all_frames(self) -> List[pygame.Surface]:
        """Get all frames"""
        return self.frames.copy()

class PlayerSpriteSheet:
    """Specialized sprite sheet handler for player animations"""
    
    def __init__(self, image_path: str):
        """Initialize player sprite sheet"""
        self.image_path = image_path
        
        # Try to load the sprite sheet
        try:
            self.sheet = pygame.image.load(image_path)
            self.sheet = self.sheet.convert_alpha()
            
            # Get sprite sheet dimensions
            self.sheet_width = self.sheet.get_width()
            self.sheet_height = self.sheet.get_height()
            
            # Try to detect frame dimensions automatically
            self._detect_frame_size()
            
            # Extract frames
            self.frames: List[pygame.Surface] = []
            self._extract_all_frames()
            
            print(f"Loaded sprite sheet: {image_path} ({len(self.frames)} frames)")
            
        except Exception as e:
            print(f"Error loading sprite sheet {image_path}: {e}")
            self.sheet = None
            self.frames = []
    
    def _detect_frame_size(self):
        """Try to automatically detect frame size from sprite sheet"""
        # Common frame sizes to try
        common_sizes = [16, 24, 32, 48, 64, 96, 128]
        
        for size in common_sizes:
            # Check if the sprite sheet dimensions are divisible by this size
            if (self.sheet_width % size == 0 and 
                self.sheet_height % size == 0):
                self.frame_width = size
                self.frame_height = size
                self.cols = self.sheet_width // size
                self.rows = self.sheet_height // size
                self.frame_count = self.cols * self.rows
                print(f"Auto-detected frame size: {size}x{size} ({self.cols}x{self.rows} grid)")
                return
        
        # Default fallback
        self.frame_width = 32
        self.frame_height = 32
        self.cols = self.sheet_width // 32
        self.rows = self.sheet_height // 32
        self.frame_count = self.cols * self.rows
        print(f"Using default frame size: 32x32 ({self.cols}x{self.rows} grid)")
    
    def _extract_all_frames(self):
        """Extract all frames from the sprite sheet"""
        for row in range(self.rows):
            for col in range(self.cols):
                # Calculate frame position
                x = col * self.frame_width
                y = row * self.frame_height
                
                # Create a subsurface for this frame
                frame_rect = pygame.Rect(x, y, self.frame_width, self.frame_height)
                frame = self.sheet.subsurface(frame_rect)
                
                self.frames.append(frame)
    
    def get_frame(self, index: int) -> Optional[pygame.Surface]:
        """Get a specific frame by index"""
        if 0 <= index < len(self.frames):
            return self.frames[index]
        return None
    
    def get_frame_count(self) -> int:
        """Get total number of frames"""
        return len(self.frames)
    
    def get_all_frames(self) -> List[pygame.Surface]:
        """Get all frames"""
        return self.frames.copy()

class AnimationManager:
    """Manages animations from sprite sheets"""
    
    def __init__(self):
        """Initialize animation manager"""
        self.animations: Dict[str, PlayerSpriteSheet] = {}
        self.current_animation = ""
        self.current_frame = 0
        self.animation_speed = 6.0  # frames per second (slower for better visibility)
        self.timer = 0.0
        
    def add_animation(self, name: str, sprite_sheet: PlayerSpriteSheet):
        """Add an animation"""
        self.animations[name] = sprite_sheet
    
    def set_animation(self, name: str):
        """Set current animation"""
        if name in self.animations:
            # Only reset frame counter if switching to a different animation
            if self.current_animation != name:
                self.current_animation = name
                self.current_frame = 0
                self.timer = 0.0
            # If same animation, keep looping continuously
        else:
            print(f"Warning: Animation '{name}' not found. Available animations: {list(self.animations.keys())}")
    
    def update(self, dt: float):
        """Update animation"""
        if not self.current_animation or self.current_animation not in self.animations:
            return
        
        animation = self.animations[self.current_animation]
        frame_count = animation.get_frame_count()
        
        if frame_count > 1:
            # Update animation timer
            self.timer += dt
            frame_duration = 1.0 / self.animation_speed
            
            # Advance frame
            self.current_frame = int(self.timer / frame_duration) % frame_count
    
    def get_current_frame(self) -> Optional[pygame.Surface]:
        """Get current animation frame"""
        if not self.current_animation or self.current_animation not in self.animations:
            return None
        
        animation = self.animations[self.current_animation]
        return animation.get_frame(self.current_frame)
    
    def get_frame_count(self) -> int:
        """Get frame count of current animation"""
        if not self.current_animation or self.current_animation not in self.animations:
            return 0
        
        return self.animations[self.current_animation].get_frame_count()
