# Developer Quick Reference

## 🚀 Quick Start

### Running the Game
```bash
source venv/bin/activate
python main.py
```

### Debug Mode
Press `D` during gameplay to toggle collision box visualization

## 🔧 Common Modifications

### 1. Adjust Flashlight Settings
**File**: `lighting.py` → `LightingConfig`

```python
class LightingConfig:
    BEAM_LENGTH = 450      # How far light reaches
    BEAM_ANGLE = 50        # Width of cone (degrees)
    NUM_RAYS = 50          # Smoothness (higher = smoother but slower)
    DARKNESS_ALPHA = 240   # How dark (0-255, higher = darker)
```

### 2. Change Game Difficulty
**File**: `config.py`

```python
class GameplayConfig:
    TIME_LIMIT = 180       # Seconds to complete game

class WumpusConfig:
    ATTACK_DAMAGE = 25     # Enemy damage per hit
    ATTACK_RANGE = 50      # How close Wumpus must be to attack
```

### 3. Adjust Player Stats
**File**: `config.py` → `PlayerConfig`

```python
class PlayerConfig:
    HEALTH = 100           # Starting health
    SPEED = 300            # Movement speed
    STARTING_ARROWS = 3    # Initial arrow count
    ARROW_DAMAGE = 30      # Damage per arrow
```

### 4. Modify Sound Volumes
**File**: `config.py` → `AudioConfig`

```python
class AudioConfig:
    SFX_VOLUME = 0.5       # Sound effects (0.0 to 1.0)
    FOOTSTEP_VOLUME = 0.3  # Footstep sounds
```

### 5. Change Menu Options
**File**: `config.py` → `MenuConfig`

```python
class MenuConfig:
    MENU_OPTIONS = ["START GAME", "CONTROLS", "QUIT"]
    TITLE_FONT_SIZE = 72
    MENU_FONT_SIZE = 36
```

## 📂 File Locations

| What You Want to Modify | File to Edit |
|-------------------------|--------------|
| Flashlight appearance | `lighting.py` |
| Game rules/balance | `config.py` |
| Player abilities | `player.py` |
| Enemy AI | `wumpus.py` |
| Menu screens | `ui/menu_screens.py` |
| Sound system | `sound_manager.py` |
| Map/level | `assets/Map/test_wall_size.tmx` |

## 🎨 Adding New Features

### Add a New Sound Effect
1. **Add sound file**: Place `.wav` in `assets/sounds/`
2. **Register in config**:
   ```python
   # config.py
   class AudioConfig:
       SOUND_NEW_EFFECT = "new_sound.wav"
   ```
3. **Load in SoundManager**: `sound_manager.py`
4. **Play it**:
   ```python
   self.sound_manager.play_sound("new_sound")
   ```

### Add a New Menu Screen
1. **Add state**:
   ```python
   # game_state.py
   class GameState(Enum):
       NEW_SCREEN = 6
   ```
2. **Create render method**:
   ```python
   # ui/menu_screens.py
   def draw_new_screen(self):
       # Your rendering code
   ```
3. **Handle in main loop**:
   ```python
   # main.py run() method
   elif self.game_state == GameState.NEW_SCREEN:
       self.menu_screens.draw_new_screen()
   ```

### Add a New Enemy Type
1. **Create file**: `new_enemy.py`
2. **Inherit from Entity**:
   ```python
   from entity import Entity
   
   class NewEnemy(Entity):
       def __init__(self, pos, groups, collision_sprites):
           super().__init__(pos, groups, collision_sprites)
           # Your init code
   ```
3. **Add config**: `config.py`
4. **Spawn in game**: `main.py` `initialize_game()`

## 🐛 Debugging Tips

### Lighting Issues
- Check `collision_sprites` is populated
- Verify player world vs screen position
- Adjust `NUM_RAYS` for performance vs quality

### Sound Not Playing
- Check file exists in `assets/sounds/`
- Verify volume settings in `AudioConfig`
- Ensure pygame.mixer initialized

### Collision Problems
- Enable debug mode (press `D`)
- Check Prolog console output
- Verify collision boxes in Tiled map

### Performance Issues
- Reduce `lighting.NUM_RAYS` (try 30)
- Increase `lighting.RAY_STEP_SIZE` (try 12)
- Check number of collision sprites

## 🔍 Code Patterns

### Singleton Pattern (Sound Manager)
```python
# One instance shared everywhere
self.sound_manager = SoundManager()
player = Player(..., sound_manager=self.sound_manager)
```

### State Pattern (Game States)
```python
if self.game_state == GameState.PLAYING:
    # Game logic
elif self.game_state == GameState.MAIN_MENU:
    # Menu logic
```

### Observer Pattern (Collision Detection)
```python
# Prolog engine notifies when events occur
if self.prolog.check_fall(...):
    self.game_state = GameState.GAME_OVER
```

## ⚡ Performance Tips

### Optimization Checklist
- [ ] Limit raycasting distance
- [ ] Reduce number of rays
- [ ] Cache Prolog queries when possible
- [ ] Use sprite groups efficiently
- [ ] Profile with `python -m cProfile main.py`

### Typical Performance Bottlenecks
1. **Lighting system** (50 rays × walls)
   - Solution: Reduce `NUM_RAYS` or `BEAM_LENGTH`
2. **Prolog queries** (collision checks)
   - Solution: Cache results where safe
3. **Sprite rendering** (many sprites)
   - Solution: Cull off-screen sprites

## 📊 Metrics

### Current Settings (Default)
- **Flashlight**: 50 rays, 450px range, 50° angle
- **Game Time**: 180 seconds (3 minutes)
- **Player HP**: 100
- **Wumpus Damage**: 25
- **FPS Target**: 60

### Recommended Ranges
- **NUM_RAYS**: 30-100 (quality vs performance)
- **BEAM_LENGTH**: 300-600 (range)
- **TIME_LIMIT**: 120-300 (difficulty)

## 🎯 Testing Scenarios

### Must Test After Changes
1. [ ] Start game from menu
2. [ ] Walk around (footsteps play)
3. [ ] Stop walking (footsteps stop)
4. [ ] Shine flashlight at walls (blocks correctly)
5. [ ] Fall into pit (game over sound only)
6. [ ] Get hit by Wumpus (game over sound only)
7. [ ] Run out of time (game over sound only)
8. [ ] Collect treasure and exit (victory)

## 📞 Module Dependencies

```
main.py
  ├─ config.py (constants)
  ├─ game_state.py (states)
  ├─ lighting.py (flashlight)
  │   └─ Settings.py
  ├─ sound_manager.py (audio)
  ├─ ui/menu_screens.py (UI)
  ├─ player.py
  ├─ wumpus.py
  ├─ prolog_interface.py
  └─ groups.py
```

**Dependency Rule**: Lower modules should NOT import from higher modules

---

**Pro Tip**: When in doubt, check `ARCHITECTURE.md` for detailed explanations!
