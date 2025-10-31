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