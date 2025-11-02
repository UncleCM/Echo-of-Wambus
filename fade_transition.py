import pygame
from Settings import WINDOW_WIDTH, WINDOW_HEIGHT


class FadeTransition:

    def __init__(self, fade_speed=5):
        self.fade_speed = fade_speed
        self.fade_alpha = 0
        self.fading_out = False
        self.fading_in = False
        self.fade_complete_callback = None

        # Create fade surface (black overlay)
        self.fade_surface = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT))
        self.fade_surface.fill((0, 0, 0))

    def start_fade_out(self, callback=None):
        self.fading_out = True
        self.fading_in = False
        self.fade_alpha = 0
        self.fade_complete_callback = callback
        print("[Fade] Starting fade out")

    def start_fade_in(self, callback=None):
        self.fading_in = True
        self.fading_out = False
        self.fade_alpha = 255
        self.fade_complete_callback = callback
        print("[Fade] Starting fade in")

    def update(self, dt):
        if self.fading_out:
            # Fade to black
            self.fade_alpha += self.fade_speed
            if self.fade_alpha >= 255:
                self.fade_alpha = 255
                self.fading_out = False
                print("[Fade] Fade out complete")
                if self.fade_complete_callback:
                    self.fade_complete_callback()

        elif self.fading_in:
            # Fade from black
            self.fade_alpha -= self.fade_speed
            if self.fade_alpha <= 0:
                self.fade_alpha = 0
                self.fading_in = False
                print("[Fade] Fade in complete")
                if self.fade_complete_callback:
                    self.fade_complete_callback()

    def render(self, screen):
        if self.fade_alpha > 0:
            self.fade_surface.set_alpha(self.fade_alpha)
            screen.blit(self.fade_surface, (0, 0))

    def is_fading(self):
        return self.fading_out or self.fading_in

    def skip_to_black(self):
        self.fade_alpha = 255
        self.fading_out = False
        self.fading_in = False

    def skip_to_clear(self):
        self.fade_alpha = 0
        self.fading_out = False
        self.fading_in = False
