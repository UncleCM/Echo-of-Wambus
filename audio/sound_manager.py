"""Sound and music management system"""
import pygame
import os


class SoundManager:
    """Manages music playback and sound effects"""
    
    def __init__(self, music_volume=0.5, sfx_volume=0.7):
        """
        Initialize sound manager
        
        Args:
            music_volume: float 0.0-1.0 - background music volume
            sfx_volume: float 0.0-1.0 - sound effects volume
        """
        # Initialize pygame mixer
        pygame.mixer.init(frequency=44100, size=-16, channels=2, buffer=512)
        
        self.music_volume = music_volume
        self.sfx_volume = sfx_volume
        
        # Sound storage
        self.sounds = {}  # name -> Sound object
        
        # Music tracking
        self.menu_music_loaded = False
        self.ingame_music_loaded = False
        self.chase_music_loaded = False
        self.current_music = None
        
        # Footstep looping
        self.footstep_channel = None
        
        print("[SoundManager] ✅ Initialized pygame mixer")
    
    def load_sound(self, name, filepath):
        """
        Load a sound effect
        
        Args:
            name: str - identifier for this sound
            filepath: str - path to sound file
        """
        if os.path.exists(filepath):
            try:
                self.sounds[name] = pygame.mixer.Sound(filepath)
                print(f"[SoundManager] ✅ Loaded sound: {name}")
            except pygame.error as e:
                print(f"[SoundManager] ❌ Failed to load {name}: {e}")
        else:
            print(f"[SoundManager] ❌ Sound file not found: {filepath}")
    
    def load_menu_music(self):
        """Load and prepare menu music"""
        music_path = "assets/sounds/menu_music.mp3"
        if os.path.exists(music_path):
            try:
                pygame.mixer.music.load(music_path)
                pygame.mixer.music.set_volume(self.music_volume)
                self.menu_music_loaded = True
                self.current_music = "menu"
                print("[SoundManager] ✅ Loaded menu music")
            except pygame.error as e:
                print(f"[SoundManager] ❌ Failed to load menu music: {e}")
        else:
            print(f"[SoundManager] ❌ Menu music not found: {music_path}")
    
    def load_ingame_music(self):
        """Load and prepare in-game music"""
        music_path = "assets/sounds/ingame_music.mp3"
        if os.path.exists(music_path):
            try:
                pygame.mixer.music.load(music_path)
                pygame.mixer.music.set_volume(self.music_volume)
                self.ingame_music_loaded = True
                self.current_music = "ingame"
                print("[SoundManager] ✅ Loaded in-game music")
            except pygame.error as e:
                print(f"[SoundManager] ❌ Failed to load in-game music: {e}")
        else:
            print(f"[SoundManager] ❌ In-game music not found: {music_path}")
    
    def load_chase_music(self):
        """Load and prepare chase scene music"""
        music_path = "assets/sounds/chase_music.mp3"
        if os.path.exists(music_path):
            try:
                pygame.mixer.music.load(music_path)
                pygame.mixer.music.set_volume(self.music_volume)
                self.chase_music_loaded = True
                self.current_music = "chase"
                print("[SoundManager] ✅ Loaded chase music")
            except pygame.error as e:
                print(f"[SoundManager] ❌ Failed to load chase music: {e}")
        else:
            print(f"[SoundManager] ❌ Chase music not found: {music_path}")
    
    def play_menu_music(self):
        """Start playing menu music in loop"""
        if self.current_music != "menu":
            self.load_menu_music()
        if self.menu_music_loaded:
            pygame.mixer.music.play(loops=-1)  # Loop indefinitely
            print("[SoundManager] 🎵 Started menu music")
    
    def play_ingame_music(self):
        """Start playing in-game music in loop"""
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
        """
        Set the volume for background music
        
        Args:
            volume: float 0.0-1.0
        """
        self.music_volume = max(0.0, min(1.0, volume))
        pygame.mixer.music.set_volume(self.music_volume)
    
    def play_sound(self, name, volume=None):
        """
        Play a sound effect by name
        
        Args:
            name: str - sound identifier
            volume: float 0.0-1.0 or None to use default
        """
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
        """
        Start playing footstep sound in a loop
        
        Args:
            volume: float 0.0-1.0 - footstep volume
        """
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
        """
        Set the volume for all sound effects
        
        Args:
            volume: float 0.0-1.0
        """
        self.sfx_volume = max(0.0, min(1.0, volume))
        for sound in self.sounds.values():
            sound.set_volume(self.sfx_volume)
    
    def stop_all_sounds(self):
        """Stop all currently playing sounds and music"""
        pygame.mixer.stop()
        self.stop_music()
        self.footstep_channel = None
