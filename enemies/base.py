"""Base enemy components and constants"""


class WumpusAIState:
    """AI state constants for Wumpus"""
    ROAMING = "roaming"          # Exploring map randomly
    INVESTIGATING = "investigating"  # Moving to sound source
    CHASING = "chasing"          # Actively pursuing detected sound
    SEARCHING = "searching"      # Lost player, searching area
    STUNNED = "stunned"          # Hit by arrow
    DEAD = "dead"                # Defeated
