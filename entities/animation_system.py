"""Animation system for entities"""
import pygame
from os.path import join


class AnimationSystem:
    """Manages sprite animations and frame updates"""
    
    def __init__(self, animation_speed=12):
        """
        Initialize animation system
        
        Args:
            animation_speed: Frames per second for animations
        """
        self.animations = {}  # Animation name -> list of frames
        self.animation_timer = 0
        self.animation_speed = animation_speed
        self.previous_animation = 'idle_down'
        self.current_animation = 'idle_down'
        
        # Frame settings
        self.frame_width = 48
        self.frame_height = 64
        self.scale_factor = 2
    
    def load_sprite_strip(self, filepath):
        """
        Extract individual frames from a horizontal sprite strip
        
        Args:
            filepath: Path to sprite strip image (relative to assets/)
            
        Returns:
            list: List of pygame.Surface frames
        """
        try:
            # Load the sprite strip
            sprite_strip = pygame.image.load(join('assets', filepath)).convert_alpha()
            strip_width = sprite_strip.get_width()
            
            # Calculate number of frames
            num_frames = strip_width // self.frame_width
            
            # Extract frames
            frames = []
            for i in range(num_frames):
                frame_rect = pygame.Rect(i * self.frame_width, 0, self.frame_width, self.frame_height)
                frame = sprite_strip.subsurface(frame_rect).copy()
                
                # Scale if needed
                if self.scale_factor != 1:
                    scaled_frame = pygame.transform.scale(
                        frame,
                        (self.frame_width * self.scale_factor,
                         self.frame_height * self.scale_factor)
                    )
                    frames.append(scaled_frame)
                else:
                    frames.append(frame)
            
            return frames if frames else self._create_placeholder()
            
        except FileNotFoundError:
            print(f"Warning: Could not load sprite strip: {filepath}")
            return self._create_placeholder()
    
    def _create_placeholder(self):
        """Create a placeholder sprite when asset loading fails"""
        placeholder = pygame.Surface(
            (self.frame_width * self.scale_factor,
             self.frame_height * self.scale_factor),
            pygame.SRCALPHA
        )
        pygame.draw.rect(placeholder, (255, 0, 255), placeholder.get_rect(), 2)
        return [placeholder]
    
    def animate(self, dt, current_image):
        """
        Update animation frames based on current state
        
        Args:
            dt: Delta time
            current_image: Current image surface
            
        Returns:
            pygame.Surface: Updated image
        """
        # Get current animation frames
        animation_frames = self.animations.get(self.current_animation, [])
        
        if not animation_frames:
            return current_image
        
        # Update animation timer
        self.animation_timer += dt
        
        # Calculate frame index using total time
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)
        
        # Return updated image
        return animation_frames[frame_index]
    
    def set_animation(self, animation_name):
        """
        Set current animation
        
        Args:
            animation_name: Name of animation to play
        """
        if animation_name != self.current_animation:
            self.previous_animation = self.current_animation
            self.current_animation = animation_name
            self.animation_timer = 0  # Reset timer on animation change
