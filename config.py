"""
Game Configuration Constants

Centralized configuration for all game parameters and magic numbers.
Separated into logical sections for easier maintenance.
"""

# =============================================================================
# GAME MECHANICS
# =============================================================================

class GameplayConfig:
    """Core gameplay mechanics constants."""
    
    # Time limit
    TIME_LIMIT = 180  # 3 minutes in seconds
    
    # Spawn protection
    SPAWN_PROTECTION_MS = 500  # Milliseconds before fall detection activates
    
    # Debug mode
    DEBUG_COLLISION_COLOR = (255, 0, 0)  # Red for collision boxes
    DEBUG_FALL_COLOR = (0, 0, 255)  # Blue for fall zones
    DEBUG_LINE_WIDTH = 2


# =============================================================================
# PLAYER CONFIGURATION
# =============================================================================

class PlayerConfig:
    """Player-related constants."""
    
    # Movement
    SPEED = 300  # Base movement speed
    
    # Combat
    HEALTH = 100  # Starting health
    ARROW_DAMAGE = 30  # Damage per arrow
    
    # Inventory
    STARTING_ARROWS = 3  # Initial arrow count


# =============================================================================
# ENEMY CONFIGURATION
# =============================================================================

class WumpusConfig:
    """Wumpus enemy constants."""
    
    # Combat
    ATTACK_DAMAGE = 25  # Damage per attack
    ATTACK_COOLDOWN = 2000  # Milliseconds between attacks
    ATTACK_RANGE = 50  # Distance within which Wumpus can attack
    
    # AI
    PATROL_SPEED = 100  # Movement speed while patrolling
    CHASE_SPEED = 150  # Movement speed while chasing player


# =============================================================================
# MENU CONFIGURATION
# =============================================================================

class MenuConfig:
    """Menu and UI constants."""
    
    # Menu options
    MENU_OPTIONS = ["START GAME", "CONTROLS", "QUIT"]
    
    # Font sizes (for pixel fonts)
    TITLE_FONT_SIZE = 72
    MENU_FONT_SIZE = 36
    BODY_FONT_SIZE = 24
    
    # Colors
    MENU_TEXT_COLOR = (255, 255, 255)
    MENU_HIGHLIGHT_COLOR = (255, 255, 0)
    MENU_BG_COLOR = (20, 20, 30)


# =============================================================================
# AUDIO CONFIGURATION
# =============================================================================

class AudioConfig:
    """Sound and music constants."""
    
    # Volume levels (0.0 to 1.0)
    MASTER_VOLUME = 1.0
    SFX_VOLUME = 0.5
    MUSIC_VOLUME = 0.3
    
    # Footstep settings
    FOOTSTEP_VOLUME = 0.3
    FOOTSTEP_SPEED_MULTIPLIER = 1.6  # Speed up factor
    
    # Sound file names
    SOUND_BUTTON = "button.wav"
    SOUND_FOOTSTEP = "footstep.wav"
    SOUND_GAME_OVER = "game_over.wav"


# =============================================================================
# WORLD CONFIGURATION
# =============================================================================

class WorldConfig:
    """World and level constants."""
    
    # Map file
    MAP_FILE = "test_wall_size.tmx"
    MAP_FOLDER = "Map"
    
    # Spawn points (safe zone coordinates)
    PLAYER_SPAWN_AREA = (100, 100, 300, 300)  # x, y, width, height
    WUMPUS_SPAWN_OFFSET = 200  # Distance from spawn
    
    # Collectibles
    NUM_ARROW_PICKUPS = 5  # Number of arrow pickups to spawn
    ARROWS_PER_PICKUP = 3  # Arrows gained per pickup


# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

def get_config_summary():
    """
    Return a formatted summary of all configuration settings.
    Useful for debugging and documentation.
    """
    summary = []
    summary.append("=== GAME CONFIGURATION ===")
    summary.append(f"Time Limit: {GameplayConfig.TIME_LIMIT}s")
    summary.append(f"Player Health: {PlayerConfig.HEALTH}")
    summary.append(f"Player Speed: {PlayerConfig.SPEED}")
    summary.append(f"Wumpus Damage: {WumpusConfig.ATTACK_DAMAGE}")
    summary.append(f"Arrow Pickups: {WorldConfig.NUM_ARROW_PICKUPS}")
    return "\n".join(summary)
