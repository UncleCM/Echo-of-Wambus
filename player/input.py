"""Player input handling system"""
import pygame


class PlayerInput:
    """Handles keyboard input for player movement"""
    
    def __init__(self, player):
        """
        Initialize input handler
        
        Args:
            player: Reference to parent Player entity
        """
        self.player = player
    
    def process_input(self):
        """
        Handle player input from keyboard
        Updates player direction and facing based on key presses
        """
        keys = pygame.key.get_pressed()

        # Can't move while attacking (shooting animation)
        if self.player.is_attacking:
            self.player.direction.x = 0
            self.player.direction.y = 0
            return

        # Reset direction
        self.player.direction.x = 0
        self.player.direction.y = 0

        # Movement input (WASD or Arrow keys)
        if keys[pygame.K_UP] or keys[pygame.K_w]:
            self.player.direction.y = -1
        elif keys[pygame.K_DOWN] or keys[pygame.K_s]:
            self.player.direction.y = 1

        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            self.player.direction.x = -1
        elif keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            self.player.direction.x = 1
        
        # Check if dashing (Shift key)
        self.player.is_dashing = keys[pygame.K_LSHIFT] or keys[pygame.K_RSHIFT]

        # Update facing based on direction (supports 8 directions)
        if self.player.direction.x != 0 or self.player.direction.y != 0:
            # Diagonal movements
            if self.player.direction.x > 0 and self.player.direction.y < 0:
                self.player.facing = "right_up"
            elif self.player.direction.x > 0 and self.player.direction.y > 0:
                self.player.facing = "right_down"
            elif self.player.direction.x < 0 and self.player.direction.y < 0:
                self.player.facing = "left_up"
            elif self.player.direction.x < 0 and self.player.direction.y > 0:
                self.player.facing = "left_down"
            # Cardinal directions
            elif self.player.direction.y < 0:
                self.player.facing = "up"
            elif self.player.direction.y > 0:
                self.player.facing = "down"
            elif self.player.direction.x < 0:
                self.player.facing = "left"
            elif self.player.direction.x > 0:
                self.player.facing = "right"

        # Normalize diagonal movement
        if self.player.direction.magnitude() > 0:
            self.player.direction = self.player.direction.normalize()
