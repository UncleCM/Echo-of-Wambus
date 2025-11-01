import pygame
import os


class SoundManager:
    """Manages all game sounds and music"""

    def __init__(self):
        """Initialize the sound manager and load sounds"""
        pygame.mixer.init()
        self.sounds = {}
        self.sfx_volume = 0.7
        self.music_volume = 0.5
        self.footstep_channel = None  # Dedicated channel for footstep looping
        self.load_sounds()

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
                    self.sounds[name] = pygame.mixer.Sound(filepath)
                    print(f"[SoundManager] Loaded sound: {name}")
                except pygame.error as e:
                    print(f"[SoundManager] Failed to load {name}: {e}")
            else:
                print(f"[SoundManager] Sound file not found: {filepath}")

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
        """Stop all currently playing sounds"""
        pygame.mixer.stop()
        self.footstep_channel = None
