"""
Sprite classes for Echo of Wumpus
Organized by category for better maintainability
"""

# Base sprites
from .base import Tile, CollisionSprite, FallZone

# Treasure system
from .treasure import Treasure, TreasureChest

# Exit portal
from .portal import ExitPortal

# Projectiles
from .projectiles import Arrow, Rock

# Pickups
from .pickups import ArrowPickup, RockPickup

# Export all
__all__ = [
    # Base
    'Tile',
    'CollisionSprite',
    'FallZone',
    # Treasure
    'Treasure',
    'TreasureChest',
    # Portal
    'ExitPortal',
    # Projectiles
    'Arrow',
    'Rock',
    # Pickups
    'ArrowPickup',
    'RockPickup',
]
