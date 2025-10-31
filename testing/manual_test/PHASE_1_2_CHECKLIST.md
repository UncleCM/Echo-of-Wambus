# Phase 1.2 Manual Testing Checklist

## 🎯 Goal
Verify that Wumpus enemy entity is properly implemented, spawns correctly, and has working AI behavior.

---

## ✅ Pre-requisites
- [ ] Phase 1.1 passed (Entity base class working)
- [ ] All dependencies installed (`pip install -r requirements.txt`)
- [ ] Game runs without errors

---

## 🧪 Test 1: Game Startup & Wumpus Spawn

### Steps:
1. Run the game:
   ```powershell
   python main.py
   ```

### Expected Console Output:
```
[PrologEngine] Initialized successfully
✓ Prolog engine initialized
Map size: 80 x 70
Collision sprites: 114
Fall zones: 39
Found entrance at tile (50, 3) -> world position (1616, 212)

[Wumpus] Sprite sheet size: 512x320
[Wumpus] Loaded 4 animation states
  idle: X frames
  walk: X frames
  attack: X frames
  death: X frames
[Wumpus] Initialized at (X, Y), HP: 150/150

Collision boxes in Prolog: 114
Fall zones in Prolog: 39
```

### Checklist:
- [ ] ✅ No errors during startup
- [ ] ✅ `[Wumpus] Initialized` message appears
- [ ] ✅ Wumpus has HP: 150/150
- [ ] ✅ 4 animation states loaded (idle/walk/attack/death)
- [ ] ✅ Collision boxes = 114 (not 0)
- [ ] ✅ Fall zones = 39 (not 0)

### ❌ If Failed:
- Check console for error messages
- Verify `wumpus.py` exists
- Verify Wumpus sprite sheet exists at `assets/Wumpus/Shardsoul Slayer Sprite Sheet.png`

---

## 🧪 Test 2: Wumpus Rendering

### Steps:
1. Start the game
2. Navigate towards the bottom-right corner of the map (or wherever Wumpus spawned)
3. Look for the Wumpus sprite

### Expected Visual:
- Wumpus appears as a **red creature** sprite (or red circle placeholder if sprite failed to load)
- Wumpus should be visible and rendered correctly
- Wumpus should have idle animation playing

### Checklist:
- [ ] ✅ Wumpus is visible on screen
- [ ] ✅ Wumpus has sprite/placeholder rendered
- [ ] ✅ Wumpus idle animation is playing (sprite changes frames)

### ❌ If Failed:
- Press **F** to enable debug mode
- Check if Wumpus hitbox appears (green rectangle)
- If hitbox appears but no sprite, sprite loading failed (check assets folder)

---

## 🧪 Test 3: Debug Mode Visualization

### Steps:
1. Press **F** key to toggle debug mode
2. Look for debug overlays

### Expected Visual:
- **Player hitbox**: Green rectangle around player
- **Wumpus hitbox**: Green rectangle around Wumpus
- **AI State text**: Above Wumpus showing current state (e.g., "patrol", "chase", "attack")
- **Collision sprites**: Walls highlighted

### Checklist:
- [ ] ✅ Debug mode toggles on/off with F key
- [ ] ✅ Wumpus hitbox visible (green rectangle)
- [ ] ✅ AI state text visible above Wumpus
- [ ] ✅ Collision boxes visible (walls)

### ❌ If Failed:
- Check console for "Debug mode: True/False" message
- Verify draw_debug() method exists in main.py

---

## 🧪 Test 4: Wumpus Patrol Behavior

### Steps:
1. Start game and locate Wumpus
2. Stay far away from Wumpus (> 300 pixels)
3. Observe Wumpus movement for ~10 seconds
4. Press **F** to see AI state

### Expected Behavior:
- Wumpus should be in **"patrol"** state
- Wumpus should move between patrol points in a pattern
- Wumpus should change direction periodically
- Walk animation should play when moving

### Checklist:
- [ ] ✅ AI state shows "patrol"
- [ ] ✅ Wumpus moves autonomously
- [ ] ✅ Wumpus follows patrol path (not random movement)
- [ ] ✅ Walk animation plays when moving
- [ ] ✅ Wumpus stops at patrol points briefly

### ❌ If Failed:
- Check if `patrol_points` is empty (console message: "using fallback")
- Verify `patrol()` method in wumpus.py
- Check if Wumpus direction vector updates

---

## 🧪 Test 5: Wumpus Chase Behavior

### Steps:
1. Start game
2. Move player character towards Wumpus
3. Get within **300 pixels** of Wumpus (detection range)
4. Press **F** to check AI state
5. Move around - Wumpus should follow

### Expected Behavior:
- When player enters detection range (300px):
  - AI state changes to **"chase"**
  - Wumpus moves directly towards player
  - Wumpus follows player movements
  - Walk animation plays

### Checklist:
- [ ] ✅ AI state changes from "patrol" to "chase"
- [ ] ✅ Wumpus moves towards player position
- [ ] ✅ Wumpus follows player when player moves
- [ ] ✅ Walk animation plays during chase
- [ ] ✅ Wumpus direction updates to face player

### ❌ If Failed:
- Check `detection_range` value (should be 300)
- Verify distance calculation in `ai_update()`
- Check `chase()` method implementation

---

## 🧪 Test 6: Wumpus Attack Behavior

### Steps:
1. Start game
2. Get very close to Wumpus (within 50 pixels)
3. Press **F** to check AI state
4. Wait for attack animation

### Expected Behavior:
- When player enters attack range (50px):
  - AI state changes to **"attack"**
  - Attack animation plays
  - Wumpus stops moving (attack stance)

### Checklist:
- [ ] ✅ AI state changes to "attack" when very close
- [ ] ✅ Attack animation plays
- [ ] ✅ Wumpus stops moving during attack
- [ ] ❓ (Future) Player takes damage (not implemented yet)

### ❌ If Failed:
- Check `attack_range` value (should be 50)
- Verify `attack_player()` method
- Check animation switching in `animate()`

---

## 🧪 Test 7: Wumpus Collision Detection

### Steps:
1. Start game
2. Lure Wumpus to chase you
3. Lead Wumpus into a wall
4. Observe if Wumpus stops at wall

### Expected Behavior:
- Wumpus should **not pass through walls**
- Wumpus uses Prolog collision system (same as Player)
- Wumpus hitbox stops at wall boundaries

### Checklist:
- [ ] ✅ Wumpus cannot walk through walls
- [ ] ✅ Wumpus collision works horizontally
- [ ] ✅ Wumpus collision works vertically
- [ ] ✅ Wumpus uses Entity.move() (Prolog authoritative)

### ❌ If Failed:
- Verify Wumpus inherits from Entity
- Check if collision_sprites passed to Wumpus.__init__
- Verify Prolog collision_box facts exist (Collision boxes > 0)

---

## 🧪 Test 8: State Transitions

### Steps:
1. Enable debug mode (F key)
2. Test all state transitions:
   - **Far away** → Should be "patrol"
   - **Move close** (200px) → Should change to "chase"
   - **Move very close** (40px) → Should change to "attack"
   - **Move away** (400px) → Should return to "chase" or "patrol"

### Expected State Machine:
```
patrol ↔ chase ↔ attack
  ↑                ↓
  └────────────────┘
```

### Checklist:
- [ ] ✅ patrol → chase (when player in detection range)
- [ ] ✅ chase → attack (when player in attack range)
- [ ] ✅ attack → chase (when player moves away from attack range)
- [ ] ✅ chase → patrol (when player far away)
- [ ] ✅ State transitions are smooth (no flickering)

### ❌ If Failed:
- Check `ai_update()` state machine logic
- Verify distance calculations
- Check threshold values (detection_range, attack_range)

---

## 🧪 Test 9: Wumpus Animation System

### Steps:
1. Observe Wumpus in different states
2. Check if correct animations play:
   - Idle when stopped
   - Walk when moving
   - Attack when attacking

### Expected Animations:
- **Idle**: Plays when Wumpus stopped (at patrol point)
- **Walk**: Plays when Wumpus moving (patrol/chase)
- **Attack**: Plays when in attack state
- **Death**: (Future) Plays when HP reaches 0

### Checklist:
- [ ] ✅ Idle animation plays when stationary
- [ ] ✅ Walk animation plays when moving
- [ ] ✅ Attack animation plays when attacking
- [ ] ✅ Animations loop smoothly
- [ ] ✅ Animation speed is reasonable (not too fast/slow)

### ❌ If Failed:
- Check `animate()` method in wumpus.py
- Verify `current_animation` is set correctly
- Check animation frame count (console output)

---

## 🧪 Test 10: Performance Check

### Steps:
1. Run game for 2-3 minutes
2. Observe FPS and performance
3. Check console for errors

### Expected Performance:
- Game runs at stable 60 FPS
- No lag when Wumpus chases player
- No memory leaks
- No repeated error messages

### Checklist:
- [ ] ✅ Game runs smoothly (60 FPS)
- [ ] ✅ No lag during Wumpus AI updates
- [ ] ✅ No console errors during gameplay
- [ ] ✅ No repeated warning messages

### ❌ If Failed:
- Check for infinite loops in AI logic
- Verify dt (delta time) is being used correctly
- Look for memory leaks (sprites not being cleaned up)

---

## 📊 Summary Checklist

### Core Functionality:
- [ ] Wumpus spawns correctly
- [ ] Wumpus renders visually
- [ ] Wumpus has 4 animation states
- [ ] Debug mode shows AI state
- [ ] Patrol behavior works
- [ ] Chase behavior works
- [ ] Attack behavior works
- [ ] Collision detection works
- [ ] State transitions work
- [ ] Performance is acceptable

### Code Quality:
- [ ] No console errors during gameplay
- [ ] Wumpus inherits from Entity
- [ ] AI logic is in separate methods (ai_update, patrol, chase, attack_player)
- [ ] Proper separation of concerns

---

## 🎯 Phase 1.2 Status

If **ALL** tests pass:
- ✅ **Phase 1.2 COMPLETE** - Ready for Phase 1.3 (Move AI to Prolog)

If **SOME** tests fail:
- ⚠️ **Phase 1.2 INCOMPLETE** - Fix failing tests before proceeding

---

## 📝 Notes & Observations

Write any additional notes here:
- Wumpus spawn position: `_______________`
- Number of patrol points: `_______________`
- Any bugs found: `_______________`
- Performance issues: `_______________`

---

## 🔄 Next Phase Preview

**Phase 1.3: Move AI to Prolog**
- Create Prolog predicates for AI decision-making
- Implement `wumpus_decision/5` in game_logic.pl
- Connect Wumpus to Prolog AI interface
- Make Prolog authoritative for AI (like movement)
