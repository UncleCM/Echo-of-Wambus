"""
Menu and UI Screens for Echo of Wumpus
Handles all menu rendering and UI screens
"""

import pygame
from Settings import WINDOW_WIDTH, WINDOW_HEIGHT


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


class MenuScreens:
    """Handles rendering of all menu and UI screens"""

    def __init__(self, screen):
        """
        Initialize menu screens handler

        Args:
            screen: Pygame surface to render on
        """
        self.screen = screen

    def draw_main_menu(self, menu_selection, menu_options):
        """
        Draw the main menu screen

        Args:
            menu_selection: Currently selected menu index
            menu_options: List of menu option strings
        """
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

        for i, option in enumerate(menu_options):
            if i == menu_selection:
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

    def draw_controls_screen(self):
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

    def draw_victory_screen(self, game_stats):
        """
        Draw victory screen with stats

        Args:
            game_stats: Dictionary with keys: 'elapsed_time', 'player_health', 'player_max_health'
        """
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 180))
        self.screen.blit(overlay, (0, 0))

        # Calculate game stats
        elapsed_time = game_stats.get("elapsed_time", 0)
        minutes = int(elapsed_time // 60)
        seconds = int(elapsed_time % 60)

        # Fonts
        title_font = get_pixel_font(96)
        font = get_pixel_font(48)
        small_font = get_pixel_font(36)

        # Title
        title_text = title_font.render("VICTORY!", True, (255, 215, 0))
        title_rect = title_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 150)
        )
        self.screen.blit(title_text, title_rect)

        # Success message
        success_text = font.render("Escaped with the treasure!", True, (100, 255, 100))
        success_rect = success_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 80)
        )
        self.screen.blit(success_text, success_rect)

        # Stats
        time_text = small_font.render(
            f"Escape Time: {minutes:02d}:{seconds:02d}", True, (255, 255, 255)
        )
        time_rect = time_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 10)
        )
        self.screen.blit(time_text, time_rect)

        player_health = game_stats.get("player_health", 0)
        player_max_health = game_stats.get("player_max_health", 100)
        health_text = font.render(
            f"Health Remaining: {player_health}/{player_max_health}",
            True,
            (0, 255, 0),
        )
        health_rect = health_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 20)
        )
        self.screen.blit(health_text, health_rect)

        # Restart prompt
        restart_text = small_font.render("Press R to Restart", True, (200, 200, 200))
        restart_rect = restart_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 120)
        )
        self.screen.blit(restart_text, restart_rect)

    def draw_game_over_screen(self, game_stats):
        """
        Draw game over screen

        Args:
            game_stats: Dictionary with keys: 'elapsed_time', 'death_reason'
        """
        # Semi-transparent overlay
        overlay = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        overlay.fill((0, 0, 0, 180))
        self.screen.blit(overlay, (0, 0))

        # Calculate game stats
        elapsed_time = game_stats.get("elapsed_time", 0)
        minutes = int(elapsed_time // 60)
        seconds = int(elapsed_time % 60)

        # Fonts
        title_font = get_pixel_font(96)
        font = get_pixel_font(48)
        small_font = get_pixel_font(36)

        # Title
        title_text = title_font.render("GAME OVER", True, (255, 0, 0))
        title_rect = title_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 150)
        )
        self.screen.blit(title_text, title_rect)

        # Death message
        death_reason = game_stats.get("death_reason", "You died!")
        death_text = font.render(death_reason, True, (255, 100, 100))
        death_rect = death_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 - 50)
        )
        self.screen.blit(death_text, death_rect)

        # Stats
        time_text = small_font.render(
            f"Survived: {minutes:02d}:{seconds:02d}", True, (200, 200, 200)
        )
        time_rect = time_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 20)
        )
        self.screen.blit(time_text, time_rect)

        # Restart prompt
        restart_text = small_font.render("Press R to Restart", True, (200, 200, 200))
        restart_rect = restart_text.get_rect(
            center=(WINDOW_WIDTH // 2, WINDOW_HEIGHT // 2 + 120)
        )
        self.screen.blit(restart_text, restart_rect)
