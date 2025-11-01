"""
Sound System for Echo of Wumpus
Handles sound events and detection for stealth-based gameplay
"""

import pygame
import math


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


class SoundManager:
    """Manages all sound events in the game"""
    
    def __init__(self):
        self.active_sounds = []  # List of active SoundEvent objects
        self.debug_mode = False
    
    def emit_sound(self, position, loudness, duration, source_type):
        """
        Emit a new sound event
        
        Args:
            position: (x, y) - sound origin
            loudness: float 0-100 - base loudness
            duration: float - how long sound lasts (seconds)
            source_type: str - type of sound
        """
        sound = SoundEvent(position, loudness, duration, source_type)
        self.active_sounds.append(sound)
        
        if self.debug_mode:
            print(f"[SoundSystem] Emitted {source_type} at {position} (loudness: {loudness}, duration: {duration}s)")
    
    def update(self):
        """Remove expired sound events"""
        initial_count = len(self.active_sounds)
        self.active_sounds = [s for s in self.active_sounds if s.is_active()]
        
        if self.debug_mode and len(self.active_sounds) < initial_count:
            expired_count = initial_count - len(self.active_sounds)
            print(f"[SoundSystem] {expired_count} sound(s) expired. Active: {len(self.active_sounds)}")
    
    def get_loudest_sound(self, listener_pos, min_threshold=10.0):
        """
        Get the loudest sound perceived at listener position
        
        Args:
            listener_pos: (x, y) - position of listener
            min_threshold: float - minimum loudness to detect (default: 10)
            
        Returns:
            tuple: (SoundEvent or None, loudness float)
        """
        loudest_sound = None
        max_loudness = min_threshold
        
        for sound in self.active_sounds:
            loudness_at_pos = sound.get_loudness_at(listener_pos)
            
            if loudness_at_pos > max_loudness:
                max_loudness = loudness_at_pos
                loudest_sound = sound
        
        return loudest_sound, max_loudness
    
    def get_all_sounds_at(self, listener_pos, min_threshold=10.0):
        """
        Get all sounds above threshold at listener position
        
        Args:
            listener_pos: (x, y) - position of listener
            min_threshold: float - minimum loudness to include
            
        Returns:
            list of tuples: [(SoundEvent, loudness), ...]
        """
        audible_sounds = []
        
        for sound in self.active_sounds:
            loudness_at_pos = sound.get_loudness_at(listener_pos)
            
            if loudness_at_pos >= min_threshold:
                audible_sounds.append((sound, loudness_at_pos))
        
        # Sort by loudness (descending)
        audible_sounds.sort(key=lambda x: x[1], reverse=True)
        
        return audible_sounds
    
    def clear_all(self):
        """Remove all active sounds (useful for restart/reset)"""
        self.active_sounds.clear()
        if self.debug_mode:
            print("[SoundSystem] All sounds cleared")
    
    def get_active_count(self):
        """Get number of active sounds"""
        return len(self.active_sounds)
    
    def toggle_debug(self):
        """Toggle debug mode"""
        self.debug_mode = not self.debug_mode
        print(f"[SoundSystem] Debug mode: {'ON' if self.debug_mode else 'OFF'}")
