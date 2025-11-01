import pygame 
from os.path import join 
from os import walk

WINDOW_WIDTH, WINDOW_HEIGHT = 1280,720 
TILE_SIZE = 64

# Arrow Combat System Settings
ARROW_SPEED = 12                 # Pixels per frame
ARROW_MAX_DISTANCE = 450         # Max travel distance in pixels
ARROW_STUN_DURATION = 3.0        # Wumpus stun duration in seconds
MAX_ARROWS = 2                   # Maximum arrows player can carry
STARTING_ARROWS = 1              # Arrows at game start
ATTACK_COOLDOWN = 600            # Milliseconds between shots
ARROW_PICKUP_COUNT = 3           # Number of arrow pickups in map

# Sound System Settings
SOUND_LEVELS = {
    'idle': 0,              # No sound
    'walk': 35,             # Quiet footsteps (เพิ่มจาก 30)
    'dash': 85,             # Loud running (เพิ่มจาก 70)
    'arrow_shot': 45,       # Bow release (เพิ่มจาก 40)
    'rock_throw': 55,       # Throwing sound (เพิ่มจาก 50)
    'rock_impact': 95,      # VERY LOUD - main distraction (เพิ่มจาก 90)
    'wumpus_roar': 100,     # Extremely loud
}

SOUND_DURATIONS = {
    'walk': 0.8,            # Quick decay (เพิ่มจาก 0.5)
    'dash': 1.5,            # Longer (เพิ่มจาก 1.0)
    'arrow_shot': 0.3,      # Brief
    'rock_throw': 0.5,      # Medium
    'rock_impact': 2.0,     # Lingers (important for distraction)
    'wumpus_roar': 3.0,     # Long roar
}

# Wumpus Hearing Settings
WUMPUS_BASE_HEARING = 350        # Base hearing radius (เพิ่มจาก 200 - น่ากลัวขึ้น!)
WUMPUS_CHASE_BONUS = 200         # Bonus hearing when chasing (เพิ่มจาก 150)
WUMPUS_ROAR_COOLDOWN = 5000      # Milliseconds between roars

# Rock Throwing System
MAX_ROCKS = 3                     # Maximum rocks player can carry
ROCK_PICKUP_COUNT = 5             # Number of rock pickups in map
ROCK_THROW_POWER = 15             # Initial throw velocity
ROCK_GRAVITY = 0.5                # Gravity acceleration
ROCK_BOUNCE_DAMPING = 0.6         # Energy loss on bounce (0-1)
ROCK_ROLLING_FRICTION = 0.95      # Rolling friction (0-1)
ROCK_THROW_COOLDOWN = 800         # Milliseconds between throws