# Echo of Wambus - 2.5D Real-time Wumpus Game

A Vampire Survivors-style 2.5D top-down game where the Wumpus will think and move in real-time, built with Python and Pygame.

## Features

- **2.5D Top-down Gameplay**: Vampire Survivors-style gameplay with smooth 8-directional movement
- **Real-time Enemy AI**: Intelligent enemy spawning and pathfinding with different enemy types
- **Weapon System**: Multiple weapon types with auto-targeting and projectile physics
- **Advanced Camera**: Smooth following camera with zoom, shake effects, and bounds
- **PSD Support**: Direct support for Photoshop PSD files with automatic conversion
- **Animated Player**: Full sprite animation support with directional movement
- **Wave-based Combat**: Progressive difficulty with enemy waves and spawning
- **Collision Detection**: Precise collision system for player, enemies, and projectiles

## Installation

1. Install Python 3.8 or higher
2. Install required dependencies:
   ```bash
   pip install -r requirements.txt
   ```

## PSD File Support

The game now supports Photoshop PSD files directly! You can:

### Option 1: Use PSD files directly
The game will automatically try to load PSD files first, then fall back to PNG if needed.

### Option 2: Convert PSD to PNG
Run the conversion script to convert all PSD files to PNG format:
```bash
python convert_assets.py
```

This will convert all PSD files in `assets/Map/_PSD/` to PNG format for better performance.

## Running the Game

```bash
python main.py
```

## Controls

- **WASD** or **Arrow Keys**: Move the player (8-directional)
- **Space**: Dash (with cooldown)
- **Q/E**: Switch weapons
- **+/-**: Zoom camera in/out
- **ESC**: Close the game

## Project Structure

```
Echo-of-Wambus/
├── main.py                 # Main game entry point
├── requirements.txt        # Python dependencies
├── game/                   # Game systems
│   ├── __init__.py
│   ├── game_manager.py     # Main game controller
│   └── map.py             # Map system and collision
├── entities/              # Game entities
│   ├── __init__.py
│   └── player.py          # Player character
├── utils/                 # Utility functions
│   └── __init__.py
└── assets/                # Game assets
    ├── Map/               # Map images
    ├── Player/            # Player sprites
    └── Wumpus/            # Wumpus sprites
```

## Current Implementation

### 2.5D Gameplay System
- **Top-down Perspective**: True 2.5D camera system with smooth following and zoom
- **8-directional Movement**: Fluid player movement in all directions
- **Auto-targeting Combat**: Weapons automatically target nearest enemies
- **Wave-based Progression**: Increasing difficulty with enemy waves

### Enemy System
- **Multiple Enemy Types**: Basic, Fast, and Heavy enemies with different behaviors
- **Intelligent AI**: Enemies detect and pursue the player within range
- **Dynamic Spawning**: Enemies spawn at safe distances and follow the player
- **Health System**: Enemies have health bars and can be damaged/destroyed

### Weapon System
- **Multiple Weapon Types**: Basic Gun, Shotgun, and Laser weapons
- **Projectile Physics**: Realistic projectile movement and collision detection
- **Auto-fire Combat**: Weapons automatically fire at nearest enemies
- **Weapon Switching**: Q/E keys to cycle through available weapons

### Map System
- Loads the main map from `assets/Map/MainLev2.0.png` or `assets/Map/_PSD/MainLev2.0.psd`
- Direct PSD file support with automatic conversion
- Precise collision detection for all game entities
- Camera bounds and smooth following

### Player System
- Full sprite animation support for all movement directions
- Smooth 2.5D movement without gravity (top-down)
- Dash ability with visual feedback
- Direction-aware sprite rendering and animation

## Gameplay Features

### Current Gameplay Loop
1. **Survive Waves**: Fight through increasingly difficult enemy waves
2. **Auto-combat**: Weapons automatically target and fire at enemies
3. **Strategic Movement**: Use dash and positioning to avoid enemies
4. **Weapon Management**: Switch between different weapon types
5. **Progressive Difficulty**: Each wave spawns more and stronger enemies

### Enemy Types
- **Basic Enemies**: Standard red enemies with moderate speed and health
- **Fast Enemies**: Orange enemies that move quickly but have less health
- **Heavy Enemies**: Blue enemies that are slow but have high health

### Weapon Types
- **Basic Gun**: Standard yellow projectiles with good rate of fire
- **Shotgun**: Orange pellets with spread damage
- **Laser**: Cyan high-speed projectiles with high damage

## Next Steps

The foundation is now set for adding the intelligent Wumpus AI system that will:
1. **Real-time Strategy**: Wumpus will think and plan moves in real-time
2. **Advanced AI**: React to player behavior and adapt strategies
3. **Intelligent Navigation**: Navigate the map with pathfinding
4. **Boss Mechanics**: Provide challenging boss encounters
5. **Power-ups**: Add temporary abilities and upgrades

## Development Notes

- The game uses a simple collision detection system that can be enhanced
- Sprite loading is robust with fallback graphics
- Camera system supports smooth following and boundary constraints
- All systems are designed to be extensible for future features
