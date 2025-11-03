"""
Game Settings & Configuration
===============================================================================
Centralized configuration for all game parameters.
All game files import from this file using: from Settings import *

Used by:
- main.py: Window size, tile size, combat settings
- player.py: Movement, combat, sound parameters
- wumpus.py: AI hearing, roar settings
- sprites.py: Arrow/rock physics
- And ~15+ other files

Last updated: 2025-11-03 (Phase 2 - Wumpus AI complete)
===============================================================================
"""

import pygame 
from os.path import join 
from os import walk

# =============================================================================
# DISPLAY SETTINGS
# =============================================================================

WINDOW_WIDTH, WINDOW_HEIGHT = 1280, 720
TILE_SIZE = 64

# =============================================================================
# ARROW COMBAT SYSTEM
# =============================================================================

ARROW_SPEED = 12                 # Pixels per frame
ARROW_MAX_DISTANCE = 300         # Max travel distance (shorter than Wumpus hearing 350px!)
ARROW_STUN_DURATION = 3.0        # Wumpus stun duration in seconds
MAX_ARROWS = 2                   # Maximum arrows player can carry
STARTING_ARROWS = 1              # Arrows at game start
ATTACK_COOLDOWN = 600            # Milliseconds between shots
ARROW_PICKUP_COUNT = 3           # Number of arrow pickups in map

# =============================================================================
# SOUND SYSTEM
# =============================================================================
# Sound levels determine how far Wumpus can hear the sound
# Wumpus base hearing = 350px, chase hearing = 550px

SOUND_LEVELS = {
    'idle': 0,              # No sound
    'walk': 35,             # Quiet footsteps (increased from 30)
    'dash': 85,             # Loud running (increased from 70)
    'arrow_shot': 45,       # Bow release (increased from 40)
    'rock_throw': 55,       # Throwing sound (increased from 50)
    'rock_impact': 95,      # VERY LOUD - main distraction tool (increased from 90)
    'wumpus_roar': 100,     # Extremely loud - warning of chase mode
}

SOUND_DURATIONS = {
    'walk': 0.8,            # Quick decay (increased from 0.5)
    'dash': 1.5,            # Longer (increased from 1.0)
    'arrow_shot': 0.3,      # Brief
    'rock_throw': 0.5,      # Medium
    'rock_impact': 2.0,     # Lingers (important for distraction)
    'wumpus_roar': 3.0,     # Long roar
}

# =============================================================================
# WUMPUS AI SETTINGS (Phase 2 - Prolog-based)
# =============================================================================

WUMPUS_BASE_HEARING = 350        # Base hearing radius (increased from 200 - more scary!)
WUMPUS_CHASE_BONUS = 200         # Bonus hearing when chasing (increased from 150)
WUMPUS_ROAR_COOLDOWN = 5000      # Milliseconds between roars

# =============================================================================
# ROCK THROWING SYSTEM
# =============================================================================

MAX_ROCKS = 3                     # Maximum rocks player can carry
ROCK_PICKUP_COUNT = 5             # Number of rock pickups in map
ROCK_THROW_POWER = 15             # Initial throw velocity
ROCK_GRAVITY = 0.5                # Gravity acceleration
ROCK_BOUNCE_DAMPING = 0.6         # Energy loss on bounce (0-1)
ROCK_ROLLING_FRICTION = 0.95      # Rolling friction (0-1)
ROCK_THROW_COOLDOWN = 800         # Milliseconds between throws