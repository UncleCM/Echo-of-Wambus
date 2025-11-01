"""Sound event system for stealth gameplay"""
import pygame


class SoundEvent:
    """Represents a sound event in the game world"""
    
    def __init__(self, position, loudness, duration, source_type):
        """
        Initialize a sound event
        
        Args:
            position: (x, y) tuple or Vector2 - where sound originated
            loudness: float 0-100 - base loudness of sound
            duration: float - how long sound persists (seconds)
            source_type: str - type of sound ('walk', 'dash', 'arrow_shot', 'rock_impact', etc.)
        """
        self.position = pygame.math.Vector2(position)
        self.loudness = loudness  # 0-100 scale
        self.duration = duration  # seconds
        self.timestamp = pygame.time.get_ticks()
        self.source_type = source_type
    
    def is_active(self):
        """Check if sound event is still active (not expired)"""
        elapsed = (pygame.time.get_ticks() - self.timestamp) / 1000.0
        return elapsed < self.duration
    
    def get_loudness_at(self, listener_pos):
        """
        Calculate perceived loudness at listener position
        Uses distance attenuation (inverse square-ish law)
        
        Args:
            listener_pos: (x, y) or Vector2 - position of listener
            
        Returns:
            float - loudness perceived at listener position (0-100)
        """
        listener_vec = pygame.math.Vector2(listener_pos)
        distance = self.position.distance_to(listener_vec)
        
        # Very close - full loudness
        if distance < 10:
            return self.loudness
        
        # Distance attenuation: loudness / (distance / attenuation_factor)
        # Using 50px as reference distance for half loudness
        attenuation_factor = 50.0
        attenuated = self.loudness / (distance / attenuation_factor)
        
        # Clamp to 0-100
        return max(0.0, min(100.0, attenuated))
    
    def get_age(self):
        """Get age of sound event in seconds"""
        return (pygame.time.get_ticks() - self.timestamp) / 1000.0
