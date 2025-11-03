# Prolog Code Refactoring Plan

## Objective
Refactor `game_logic.pl` (1200+ lines) into modular, maintainable components organized by functionality.

## Module Structure

### 1. **core/geometry.pl**
**Purpose:** Basic geometric calculations and collision detection
- `point_in_rect/6` - Point-in-rectangle test
- `rects_collide/8` - Rectangle collision detection
- `distance_squared/5` - Distance calculation (no sqrt)
- `normalize_direction/4` - Vector normalization

### 2. **core/map_system.pl**
**Purpose:** Map data structures, grid system, navigation
- Dynamic facts: `map_size/2`, `tile_size/2`, `grid_cell/3`, `grid_size/2`
- `init_map_system/5` - Initialize map dimensions
- `build_navigation_grid/0` - Build traversable grid
- `classify_cell_type/4` - Determine cell type (safe/wall/pit/water)
- `safe_position/2` - Cached safe spawn positions
- Pathfinding: `find_path/3`, `cached_path/3`

### 3. **core/movement.pl**
**Purpose:** Movement resolution and collision handling
- `resolve_movement/8` - Main movement resolution
- `can_move/6` - Check if position is valid
- `check_collision/5` - Collision with walls
- `check_fall/5` - Fall zone detection
- `check_water/5` - Water zone detection

### 4. **entities/player.pl**
**Purpose:** Player state management
- Dynamic facts: `player_position/2`, `player_health/1`, `player_inventory/2`, `player_max_inventory/2`
- `init_game_state/0` - Initialize player stats
- `get_player_inventory/2` - Query inventory
- `add_arrows/1`, `add_rocks/1` - Inventory management
- `use_arrow/0`, `use_rock/0` - Consume items
- `take_damage/2` - Health management

### 5. **entities/wumpus_ai.pl**
**Purpose:** Wumpus AI behavior and state machine
- Dynamic facts: `wumpus_ai_state/2`, `wumpus_target/3`, `wumpus_patrol_index/2`, etc.
- `init_wumpus/2` - Initialize Wumpus
- `decide_wumpus_action/10` - Main AI decision logic
- `decide_no_sound_action/4` - Behavior when no sound detected
- `should_attack/6` - Attack condition check
- `can_roar/3` - Roar cooldown check
- State management: `set_roaring/2`, `get_last_roar_time/2`

### 6. **entities/treasure.pl**
**Purpose:** Treasure and mimic system
- Dynamic facts: `chest_position/4`, `chest_type/2`, `treasure_collected/1`
- `setup_treasure_system/7` - Initialize 3 chests (1 real, 2 mimics)
- `get_chest_info/3` - Query chest data
- `collect_treasure/1` - Mark treasure as collected
- `is_mimic/1` - Check if chest is mimic

### 7. **systems/combat.pl**
**Purpose:** Projectile systems and combat mechanics
- **Arrows:**
  - Dynamic facts: `active_arrow/6`
  - `spawn_arrow/6`, `remove_arrow/1`, `count_active_arrows/1`
  - `check_arrow_wumpus_collision/7`
- **Rocks:**
  - Dynamic facts: `active_rock/6`
  - `spawn_rock/6`, `remove_rock/1`
- **Damage:**
  - `deal_damage/3`, `wumpus_can_attack_player/6`

### 8. **systems/spawning.pl**
**Purpose:** Entity spawning systems
- `init_spawn_config/0` - Setup spawn parameters
- `generate_wumpus_spawns/2` - Wumpus spawn positions
- `generate_pickup_spawns/2` - Arrow/rock pickup spawns
- `generate_chest_positions/4` - Treasure chest positions
- `find_spawn_position/3` - Safe spawn finding

### 9. **systems/game_state.pl**
**Purpose:** Global game state management
- Dynamic facts: `game_state/1`, `time_remaining/1`, `game_over/1`
- `init_game_state/0` - Reset game state
- `set_game_state/1`, `get_game_state/1`
- `update_time/1`, `get_time_remaining/1`
- `check_game_over_conditions/0`
- `get_game_over_reason/1`

### 10. **utils/helpers.pl**
**Purpose:** Utility predicates
- `count_facts/2` - Count dynamic facts
- `random_element/2` - Random list element
- `min/3`, `max/3` - Math utilities
- `clamp/4` - Value clamping

## Migration Strategy

1. **Phase 1:** Create all module files with headers and dynamic declarations
2. **Phase 2:** Extract and move predicates to appropriate modules
3. **Phase 3:** Create new `game_logic.pl` that includes all modules
4. **Phase 4:** Update `prolog_interface.py` to load new structure
5. **Phase 5:** Test and verify all functionality

## Benefits

✅ **Maintainability:** Each module has clear responsibility  
✅ **Readability:** Smaller files, easier to navigate  
✅ **Reusability:** Modules can be tested independently  
✅ **Scalability:** Easy to add new features to specific modules  
✅ **Debugging:** Isolate issues to specific modules  

## Compatibility

- All existing Python interface calls remain unchanged
- Module includes are transparent to external code
- Can be done incrementally without breaking functionality
