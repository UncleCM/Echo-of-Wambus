"""
Game State Management
Defines all possible game states
"""
from enum import Enum


class GameState(Enum):
    """Game states for win/lose conditions and menu navigation"""

    MAIN_MENU = "main_menu"
    CONTROLS = "controls"
    PLAYING = "playing"
    VICTORY = "victory"
    GAME_OVER = "game_over"
