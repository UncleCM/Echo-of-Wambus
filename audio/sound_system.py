"""
Complete sound system combining music/SFX management and sound events

This module provides the SoundSystem class which manages both:
1. Music and sound effect playback (via SoundManager)
2. Positional sound events for stealth gameplay (via SoundEvent)
"""
from audio.sound_event import SoundEvent
from audio.sound_manager import SoundManager


class SoundSystem(SoundManager):
    """
    Complete sound system for the game
    
    Inherits from SoundManager to handle music/SFX playback,
    and adds sound event tracking for stealth gameplay mechanics.
    """
    
    def __init__(self, music_volume=0.5, sfx_volume=0.7, debug_mode=False):
        """
        Initialize the complete sound system
        
        Args:
            music_volume: float 0.0-1.0 - background music volume
            sfx_volume: float 0.0-1.0 - sound effects volume
            debug_mode: bool - enable debug logging
        """
        super().__init__(music_volume, sfx_volume)
        
        # Sound event tracking
        self.active_sounds = []  # List of active SoundEvent objects
        self.debug_mode = debug_mode
        
        print(f"[SoundSystem] ✅ Initialized (debug: {'ON' if debug_mode else 'OFF'})")
    
    def emit_sound(self, position, loudness, duration, source_type):
        """
        Emit a new sound event
        
        Args:
            position: (x, y) - sound origin
            loudness: float 0-100 - base loudness
            duration: float - how long sound lasts (seconds)
            source_type: str - type of sound ('walk', 'dash', 'arrow_shot', etc.)
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
        """Get number of active sound events"""
        return len(self.active_sounds)
    
    def toggle_debug(self):
        """Toggle debug mode"""
        self.debug_mode = not self.debug_mode
        print(f"[SoundSystem] Debug mode: {'ON' if self.debug_mode else 'OFF'}")
