import pygame
import os
import numpy as np


class SoundManager:
    """Manages all game sounds and music"""

    def __init__(self):
        """Initialize the sound manager and load sounds"""
        pygame.mixer.init()
        self.sounds = {}
        self.sfx_volume = 0.7
        self.music_volume = 0.3  # Lower volume for background music
        self.footstep_channel = None  # Dedicated channel for footstep looping
        self.menu_music_loaded = False
        self.ingame_music_loaded = False
        self.current_music = None  # Track which music is currently loaded
        self.load_sounds()
        self.load_menu_music()

    def speed_up_sound(self, sound, speed_factor=1.5):
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
