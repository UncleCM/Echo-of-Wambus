"""
Sound System for Echo of Wumpus
Handles sound events and detection for stealth-based gameplay
"""

import pygame
import math
import os
import numpy as np


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
    """Manages all sound events in the game AND actual pygame sound playback"""
    
    def __init__(self):
        # Sound event tracking (for stealth gameplay)
        self.active_sounds = []  # List of active SoundEvent objects
        self.debug_mode = False
        
        # Pygame sound playback
        pygame.mixer.init()
        self.sounds = {}
        self.sfx_volume = 0.7
        self.music_volume = 0.6  # Increased from 0.3 - music should be audible!
        self.footstep_channel = None  # Dedicated channel for footstep looping
        
        # Music tracking (support multiple music tracks)
        self.menu_music_loaded = False
        self.ingame_music_loaded = False
        self.chase_music_loaded = False
        self.current_music = None  # Track which music is currently loaded
        
        self.load_sounds()
        self.load_menu_music()  # Load menu music by default
    
    def speed_up_sound(self, sound, speed_factor=1.5):
        """Speed up a sound by resampling"""
        try:
            # Get the sound array
            sound_array = pygame.sndarray.array(sound)

            # Calculate new length
            original_length = len(sound_array)
            new_length = int(original_length / speed_factor)

            # Resample the audio (simple downsampling)
            if len(sound_array.shape) == 2:  # Stereo
                # Resample both channels
                indices = np.linspace(0, original_length - 1, new_length).astype(int)
                new_array = sound_array[indices]
            else:  # Mono
                indices = np.linspace(0, original_length - 1, new_length).astype(int)
                new_array = sound_array[indices]

            # Create new sound from array
            return pygame.sndarray.make_sound(new_array)
        except:
            # If speed adjustment fails, return original sound
            print(f"[SoundManager] Could not speed up sound, using original")
            return sound
    
    def load_sounds(self):
        """Load all sound files from assets/sounds/"""
        sound_dir = os.path.join("assets", "sounds")

        # Define available sounds
        sound_files = {
            "button": "button.wav",
            "footstep": "footstep.wav",
            "game_over": "game_over.wav",
            "roar": "Roar_Sound_Effect.mp3",  # Wumpus roar sound
        }

        # Load each sound file
        for name, filename in sound_files.items():
            filepath = os.path.join(sound_dir, filename)
            if os.path.exists(filepath):
                try:
                    loaded_sound = pygame.mixer.Sound(filepath)

                    # Speed up footstep sound for faster walking rhythm
                    if name == "footstep":
                        loaded_sound = self.speed_up_sound(
                            loaded_sound, speed_factor=1.6
                        )

                    self.sounds[name] = loaded_sound
                    print(f"[SoundManager] Loaded sound: {name}")
                except pygame.error as e:
                    print(f"[SoundManager] Failed to load {name}: {e}")
            else:
                print(f"[SoundManager] Sound file not found: {filepath}")

    def load_menu_music(self):
        """Load menu background music"""
        music_file = os.path.join("assets", "sounds", "Game_Background_Music.mp3")
        if os.path.exists(music_file):
            try:
                pygame.mixer.music.load(music_file)
                pygame.mixer.music.set_volume(self.music_volume)
                self.menu_music_loaded = True
                self.current_music = "menu"
                print(f"[SoundManager] ✅ Menu music loaded")
            except pygame.error as e:
                print(f"[SoundManager] Failed to load menu music: {e}")
        else:
            print(f"[SoundManager] Menu music file not found: {music_file}")

    def load_ingame_music(self):
        """Load in-game background music"""
        music_file = os.path.join("assets", "sounds", "InGameMusic.mp3")
        if os.path.exists(music_file):
            try:
                pygame.mixer.music.load(music_file)
                pygame.mixer.music.set_volume(self.music_volume)
                self.ingame_music_loaded = True
                self.current_music = "ingame"
                print(f"[SoundManager] ✅ In-game music loaded")
            except pygame.error as e:
                print(f"[SoundManager] Failed to load in-game music: {e}")
        else:
            print(f"[SoundManager] In-game music file not found: {music_file}")

    def load_chase_music(self):
        """Load chase scene music"""
        music_file = os.path.join("assets", "sounds", "Chase_Scene_Music.mp3")
        if os.path.exists(music_file):
            try:
                pygame.mixer.music.load(music_file)
                pygame.mixer.music.set_volume(self.music_volume)
                self.chase_music_loaded = True
                self.current_music = "chase"
                print(f"[SoundManager] ✅ Chase music loaded")
            except pygame.error as e:
                print(f"[SoundManager] Failed to load chase music: {e}")
        else:
            print(f"[SoundManager] Chase music file not found: {music_file}")

    def play_menu_music(self):
        """Start playing menu background music in loop"""
        if self.current_music != "menu":
            self.load_menu_music()
        if self.menu_music_loaded and not pygame.mixer.music.get_busy():
            pygame.mixer.music.play(loops=-1)  # Loop indefinitely
            print("[SoundManager] 🎵 Started menu music")

    def play_ingame_music(self):
        """Start playing in-game background music in loop"""
        if self.current_music != "ingame":
            self.load_ingame_music()
        if self.ingame_music_loaded:
            pygame.mixer.music.play(loops=-1)  # Loop indefinitely
            print("[SoundManager] 🎵 Started in-game music")

    def play_chase_music(self):
        """Start playing chase scene music in loop"""
        if self.current_music != "chase":
            self.load_chase_music()
        if self.chase_music_loaded:
            pygame.mixer.music.play(loops=-1)  # Loop indefinitely
            print("[SoundManager] 🎵 Started chase music")

    def stop_music(self):
        """Stop the background music"""
        if pygame.mixer.music.get_busy():
            pygame.mixer.music.stop()
            print("[SoundManager] Stopped music")

    def set_music_volume(self, volume):
        """Set the volume for background music (0.0 to 1.0)"""
        self.music_volume = max(0.0, min(1.0, volume))
        pygame.mixer.music.set_volume(self.music_volume)

    def play_sound(self, name, volume=None):
        """Play a sound effect by name"""
        if name in self.sounds:
            sound = self.sounds[name]
            if volume is not None:
                sound.set_volume(volume)
            else:
                sound.set_volume(self.sfx_volume)
            sound.play()
        else:
            print(f"[SoundManager] Sound '{name}' not found")

    def play_footstep_loop(self, volume=0.3):
        """Start playing footstep sound in a loop"""
        if "footstep" in self.sounds:
            if self.footstep_channel is None or not self.footstep_channel.get_busy():
                sound = self.sounds["footstep"]
                sound.set_volume(volume)
                self.footstep_channel = sound.play(loops=-1)  # Loop indefinitely

    def stop_footstep_loop(self):
        """Stop the looping footstep sound"""
        if self.footstep_channel and self.footstep_channel.get_busy():
            self.footstep_channel.stop()
            self.footstep_channel = None

    def set_sfx_volume(self, volume):
        """Set the volume for sound effects (0.0 to 1.0)"""
        self.sfx_volume = max(0.0, min(1.0, volume))
        for sound in self.sounds.values():
            sound.set_volume(self.sfx_volume)

    def stop_all_sounds(self):
        """Stop all currently playing sounds and music"""
        pygame.mixer.stop()
        self.stop_music()
        self.footstep_channel = None
    
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
