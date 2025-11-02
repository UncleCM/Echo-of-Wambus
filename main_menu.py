import pygame
from Settings import WINDOW_WIDTH, WINDOW_HEIGHT
from enum import Enum


class GameState(Enum):
    """Game states for win/lose conditions"""
    MAIN_MENU = "main_menu"
    CONTROLS = "controls"
    PLAYING = "playing"
    VICTORY = "victory"
    GAME_OVER = "game_over"


def get_pixel_font(size):
    """Get a pixel art style font"""
    # Try to use a monospace/pixel-style system font first
    pixel_fonts = ["Courier New", "Monaco", "Consolas", "Courier", "monospace"]

    for font_name in pixel_fonts:
        try:
            font = pygame.font.SysFont(font_name, size, bold=True)
            if font:
                return font
        except:
            continue

    # Fallback to default pygame font (already pixel-style)
    return pygame.font.Font(None, size)


class MainMenu:
    """Handles main menu rendering and input processing"""
    
    def __init__(self, screen):
        self.screen = screen
        self.menu_selection = 0  # 0 = Start, 1 = Controls, 2 = Quit
        self.menu_options = ["START GAME", "CONTROLS", "QUIT"]
    
    def handle_event(self, event):
        """
        Handle menu-related events.
        Returns tuple: (action, new_state)
        - action: 'start_game', 'quit', or None
        - new_state: GameState or None
        """
        if event.type == pygame.KEYDOWN:
            # Main Menu controls
            if event.key == pygame.K_UP:
                self.menu_selection = (self.menu_selection - 1) % len(
                    self.menu_options
                )
                return (None, None)
            elif event.key == pygame.K_DOWN:
                self.menu_selection = (self.menu_selection + 1) % len(
                    self.menu_options
                )
                return (None, None)
            elif event.key == pygame.K_RETURN or event.key == pygame.K_SPACE:
                if self.menu_selection == 0:  # Start Game
                    return ("start_game", None)
                elif self.menu_selection == 1:  # Controls
                    return (None, GameState.CONTROLS)
                elif self.menu_selection == 2:  # Quit
                    return ("quit", None)
        
        return (None, None)
    
    def draw(self):
        """Draw the main menu screen"""
        self.screen.fill((20, 20, 30))  # Dark background

        # Title
        title_font = get_pixel_font(120)
        title_text = title_font.render("ECHO OF WUMPUS", True, (255, 215, 0))
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH // 2, 150))

        # Add shadow effect to title
        shadow_text = title_font.render("ECHO OF WUMPUS", True, (100, 86, 0))
        shadow_rect = shadow_text.get_rect(center=(WINDOW_WIDTH // 2 + 4, 154))
        self.screen.blit(shadow_text, shadow_rect)
        self.screen.blit(title_text, title_rect)

        # Subtitle
        subtitle_font = get_pixel_font(36)
        subtitle_text = subtitle_font.render(
            "A Cave Adventure Game", True, (180, 180, 180)
        )
        subtitle_rect = subtitle_text.get_rect(center=(WINDOW_WIDTH // 2, 230))
        self.screen.blit(subtitle_text, subtitle_rect)

        # Menu options
        menu_font = get_pixel_font(64)
        menu_y_start = 350
        menu_spacing = 80

        for i, option in enumerate(self.menu_options):
            if i == self.menu_selection:
                # Highlighted option
                color = (255, 215, 0)
                text = f"> {option} <"
            else:
                # Normal option
                color = (200, 200, 200)
                text = option

            menu_text = menu_font.render(text, True, color)
            menu_rect = menu_text.get_rect(
                center=(WINDOW_WIDTH // 2, menu_y_start + i * menu_spacing)
            )
            self.screen.blit(menu_text, menu_rect)

        # Instructions at bottom
        instruction_font = get_pixel_font(32)
        instruction_text = instruction_font.render(
            "Use UP/DOWN to navigate, ENTER to select", True, (150, 150, 150)
        )
        instruction_rect = instruction_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT - 50)
        )
        self.screen.blit(instruction_text, instruction_rect)


class ControlsScreen:
    """Handles controls screen rendering"""
    
    def __init__(self, screen):
        self.screen = screen
    
    def handle_event(self, event):
        """
        Handle controls screen events.
        Returns: GameState or None
        """
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_ESCAPE:
                return GameState.MAIN_MENU
        return None
    
    def draw(self):
        """Draw the controls/how to play screen"""
        self.screen.fill((20, 20, 30))  # Dark background

        # Title
        title_font = get_pixel_font(64)
        title_text = title_font.render("CONTROLS", True, (255, 215, 0))
        title_rect = title_text.get_rect(center=(WINDOW_WIDTH // 2, 60))
        self.screen.blit(title_text, title_rect)

        # Controls
        font = get_pixel_font(32)
        controls = [
            ("WASD / Arrow Keys", "Move"),
            ("SPACE", "Shoot Arrow"),
            ("F", "Toggle Debug Mode"),
            ("R", "Restart (when game over)"),
            ("ESC", "Return to Menu"),
        ]

        y_pos = 140
        for key, action in controls:
            # Key
            key_text = font.render(key, True, (255, 215, 0))
            key_rect = key_text.get_rect(right=WINDOW_WIDTH // 2 - 30, top=y_pos)
            self.screen.blit(key_text, key_rect)

            # Separator
            separator = font.render("-", True, (150, 150, 150))
            sep_rect = separator.get_rect(center=(WINDOW_WIDTH // 2, y_pos + 12))
            self.screen.blit(separator, sep_rect)

            # Action
            action_text = font.render(action, True, (200, 200, 200))
            action_rect = action_text.get_rect(left=WINDOW_WIDTH // 2 + 30, top=y_pos)
            self.screen.blit(action_text, action_rect)

            y_pos += 50

        # Objective
        objective_font = get_pixel_font(42)
        objective_title = objective_font.render("OBJECTIVE:", True, (255, 100, 100))
        objective_rect = objective_title.get_rect(
            center=(WINDOW_WIDTH // 2, y_pos + 30)
        )
        self.screen.blit(objective_title, objective_rect)

        objective_font_small = get_pixel_font(28)
        objectives = [
            "1. Find the treasure in the cave",
            "2. Avoid or stun the Wumpus with arrows",
            "3. Return to the entrance to escape",
            "4. Complete before time runs out!",
        ]

        obj_y = y_pos + 80
        for obj in objectives:
            obj_text = objective_font_small.render(obj, True, (200, 200, 200))
            obj_rect = obj_text.get_rect(center=(WINDOW_WIDTH // 2, obj_y))
            self.screen.blit(obj_text, obj_rect)
            obj_y += 38

        # Back instruction
        back_font = get_pixel_font(28)
        back_text = back_font.render(
            "Press ESC to return to menu", True, (150, 150, 150)
        )
        back_rect = back_text.get_rect(center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT - 40))
        self.screen.blit(back_text, back_rect)

