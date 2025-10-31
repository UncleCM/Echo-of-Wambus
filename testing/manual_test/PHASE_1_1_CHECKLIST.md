# Phase 1.1 Manual Verification Checklist

## ✅ Code Structure Verification

### 1. Entity Base Class (`entity.py`)
- [ ] File exists and has 250+ lines
- [ ] Has `__init__` with parameters: `pos, groups, collision_sprites, prolog_engine, entity_type`
- [ ] Has `move(dt)` method with Prolog authoritative movement
- [ ] Has `collision(direction)` fallback method
- [ ] Has `animate(dt)` generic animation method
- [ ] Has `take_damage(amount)` and `heal(amount)` health methods
- [ ] Has `on_death()` method
- [ ] Has `load_sprite_strip(filepath)` asset loading method
- [ ] Has fractional movement accumulators `_rem_x` and `_rem_y`
- [ ] Has health system with `max_health`, `health`, `is_alive`

### 2. Player Class Refactoring (`player.py`)
- [ ] File reduced from ~298 lines to ~161 lines (46% reduction)
- [ ] Imports Entity: `from entity import Entity`
- [ ] Inherits from Entity: `class Player(Entity):`
- [ ] `__init__` calls `super().__init__(..., entity_type='player')`
- [ ] **REMOVED** duplicate `load_sprite_strip` method
- [ ] **REMOVED** duplicate `move` method
- [ ] **REMOVED** duplicate `collision` method
- [ ] **KEPT** `load_animations()` - Player-specific
- [ ] **KEPT** `input()` - Player-specific keyboard handling
- [ ] **KEPT** `animate(dt)` - Player-specific 8-direction override
- [ ] **KEPT** `update(dt)` - Calls input(), move(), animate()

### 3. Prolog Interface Fix (`prolog_interface.py`)
- [ ] `__init__` sets `self.available = False` FIRST (line 6)
- [ ] `__init__` sets `self.prolog = None` FIRST (line 7)
- [ ] Sets `self.available = True` BEFORE calling `self.init_game()` (line 15)
- [ ] No AttributeError when accessing `self.available` in `_query()`

---

## 🎮 Runtime Testing

### Test 1: Game Launch
```powershell
python main.py
```

**Expected Output:**
```
[PrologEngine] Initialized successfully  # ← Must see this
✓ Prolog engine initialized
Map size: 80 x 70
Collision sprites: 114
Fall zones: 39
Loaded 16 animation states
```

**Check:**
- [ ] No error: `'PrologEngine' object has no attribute 'available'`
- [ ] Shows `[PrologEngine] Initialized successfully`
- [ ] Collision boxes in Prolog: 114 (NOT 0)
- [ ] Fall zones in Prolog: 39 (NOT 0)

### Test 2: Player Movement (WASD)
- [ ] Press W/A/S/D - Player moves smoothly
- [ ] Movement is smooth (no stuttering from fractional loss)
- [ ] Player cannot walk through walls (Prolog collision works)
- [ ] Diagonal movement works (W+A, W+D, S+A, S+D)

### Test 3: Animation System
- [ ] Idle: Player shows correct idle animation for facing direction
- [ ] Walk: Player shows walk animation when moving
- [ ] Direction: Animation changes based on 8 directions
  - [ ] Up, Down, Left, Right
  - [ ] Left-Up, Right-Up, Left-Down, Right-Down

### Test 4: Fall Detection (CRITICAL)
**Steps:**
1. Walk player towards a fall zone (hole in map)
2. Step into the fall zone

**Expected:**
- [ ] Game prints: `GAME OVER - Fell into a hole! (Prolog detected)`
- [ ] `check_fall` returns results (NOT empty list `[]`)
- [ ] Game over state is set
- [ ] Player cannot move after falling

**If NOT working:**
- Check `Collision boxes in Prolog: 0` → Bug still exists
- Check `Fall zones in Prolog: 0` → Bug still exists
- Check error message about `available` attribute

### Test 5: Prolog Debug (Press F for debug mode if available)
```python
# In main.py check_game_over(), verify output shows:
Collision boxes in Prolog: 114
Fall zones in Prolog: 39
```

- [ ] Counts are non-zero
- [ ] `check_fall` queries return actual results when stepping in holes

---

## 🐛 Known Issues to Watch For

### Issue 1: AttributeError on 'available'
**Symptom:** `'PrologEngine' object has no attribute 'available'`

**Fix Applied:** Set `self.available = False` BEFORE any other code in `__init__`

**Verify:**
```python
# prolog_interface.py __init__ should have:
self.available = False  # Line 6
self.prolog = None      # Line 7
# ... then try/except block
```

### Issue 2: Collision/Fall zones = 0
**Symptom:** `Collision boxes in Prolog: 0`, `Fall zones in Prolog: 0`

**Cause:** `init_game()` was called before `self.available = True`

**Verify:**
```python
# prolog_interface.py __init__ should have:
self.available = True   # Line 15 - BEFORE init_game
self.init_game()       # Line 16 - AFTER available=True
```

### Issue 3: Player can walk through walls
**Symptom:** No collision detection

**Causes:**
- Prolog not initialized → Check `available` flag
- Collision boxes not loaded → Check count != 0
- Entity.move() not being called → Check Player.update() calls self.move(dt)

---

## ✅ Phase 1.1 Completion Criteria

**All of the following must be TRUE:**

1. ✅ `entity.py` exists with complete base class
2. ✅ `player.py` inherits from Entity (< 200 lines)
3. ✅ No duplicate code between Player and Entity
4. ✅ PrologEngine initializes without AttributeError
5. ✅ Collision boxes in Prolog > 0
6. ✅ Fall zones in Prolog > 0
7. ✅ Player movement works smoothly
8. ✅ Player cannot walk through walls
9. ✅ **Player CAN fall into holes and trigger game over**
10. ✅ Animations work for all 8 directions

---

## 📝 Manual Test Script

Run this test sequence:

```powershell
# 1. Clear terminal
cls

# 2. Run game
python main.py

# 3. Check startup output for:
#    - "[PrologEngine] Initialized successfully"
#    - "Collision sprites: 114"
#    - "Fall zones: 39"

# 4. Test movement (WASD keys)
#    - Move in all 8 directions
#    - Try to walk through walls (should block)

# 5. Test fall detection
#    - Find a hole on the map
#    - Walk into it
#    - Should see "GAME OVER - Fell into a hole!"

# 6. If game over doesn't trigger:
#    - Check console for "check_fall query results: []"
#    - If empty results → Prolog initialization bug
#    - Re-check prolog_interface.py __init__ order
```

---

## 🎯 Next Steps After Phase 1.1

Once ALL checks pass:

**Phase 1.2: Create Wumpus Class**
- Create `wumpus.py` inheriting from Entity
- Add basic sprite/animations
- Test Wumpus spawning and rendering

**Phase 1.3: Implement Wumpus AI**
- Add AI predicates to `game_logic.pl`
- Implement patrol behavior
- Implement chase behavior (detect player)

**Phase 1.4: Combat System**
- Player can attack (spacebar/click)
- Wumpus can be damaged/killed
- Wumpus can attack player

**Phase 1.5: Win/Lose Conditions**
- Win: Kill all Wumpus
- Lose: Player dies OR falls in hole
- Game over screen
