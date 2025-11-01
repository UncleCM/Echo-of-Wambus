# Phase 1.3 Manual Testing Checklist

## 🎯 Goal
Verify that Wumpus AI now uses Prolog for decision-making instead of Python logic.

---

## ✅ Pre-requisites
- [ ] Phase 1.2 passed (Wumpus entity working)
- [ ] Prolog initialized successfully
- [ ] Game runs without errors

---

## 🧪 Test 1: Prolog AI Initialization

### Steps:
1. Run the game:
   ```powershell
   python main.py
   ```

### Expected Console Output:
```
[PrologEngine] Initialized successfully
✓ Prolog engine initialized
...
[Wumpus] Initialized at (X, Y), HP: 150/150
```

### Checklist:
- [ ] ✅ No Prolog errors during startup
- [ ] ✅ `[PrologEngine] Initialized successfully` appears
- [ ] ✅ Wumpus initialized message appears
- [ ] ✅ No errors about `wumpus_decision` or `init_wumpus`

### ❌ If Failed:
- Check game_logic.pl has Wumpus predicates (wumpus_decision/8)
- Verify prolog_interface.py has get_wumpus_decision() method
- Check console for Prolog syntax errors

---

## 🧪 Test 2: Patrol State (Far from Player)

### Steps:
1. Start game
2. Press **F** to enable debug mode
3. Stay far away from Wumpus (> 300 pixels)
4. Observe Wumpus AI state

### Expected Behavior:
- AI state shows **"patrol"**
- Wumpus moves between patrol points
- Wumpus does NOT chase player
- Walk animation plays

### Checklist:
- [ ] ✅ AI state is "patrol"
- [ ] ✅ Wumpus patrols autonomously
- [ ] ✅ Wumpus ignores distant player
- [ ] ✅ No console errors about Prolog queries

### ❌ If Failed:
- Check `wumpus_decision(_, _, _, _, patrol, NewState, _, _)` in game_logic.pl
- Verify `in_detection_range/4` predicate works
- Check console for Prolog query errors

---

## 🧪 Test 3: Patrol → Chase Transition

### Steps:
1. Start game with debug mode (F)
2. Start far from Wumpus (AI state = "patrol")
3. Move player within **300 pixels** of Wumpus
4. Watch AI state change

### Expected Behavior:
- AI state changes from **"patrol" → "chase"**
- Wumpus stops patrolling
- Wumpus turns towards player
- Wumpus starts moving towards player
- Walk animation plays

### Checklist:
- [ ] ✅ State transitions to "chase"
- [ ] ✅ Transition happens at ~300px distance
- [ ] ✅ Wumpus immediately changes direction towards player
- [ ] ✅ Console shows no Prolog errors

### Test Prolog Decision:
Add temporary debug print in wumpus.py after `get_wumpus_decision()`:
```python
print(f"[Wumpus AI] State: {new_state}, Direction: ({direction_x:.2f}, {direction_y:.2f})")
```

Should see:
```
[Wumpus AI] State: chase, Direction: (0.71, 0.71)  # or similar normalized vector
```

### ❌ If Failed:
- Verify `detection_range_squared(90000)` in game_logic.pl (300*300)
- Check `calculate_direction/6` predicate
- Verify `get_wumpus_decision()` returns correct values

---

## 🧪 Test 4: Chase → Attack Transition

### Steps:
1. Get Wumpus into chase state (approach within 300px)
2. Move very close to Wumpus (within **50 pixels**)
3. Watch AI state change

### Expected Behavior:
- AI state changes from **"chase" → "attack"**
- Wumpus stops moving (direction = 0, 0)
- Attack animation plays
- Wumpus stays in attack stance

### Checklist:
- [ ] ✅ State transitions to "attack"
- [ ] ✅ Transition happens at ~50px distance
- [ ] ✅ Wumpus stops moving
- [ ] ✅ Attack animation plays
- [ ] ✅ Console shows no errors

### ❌ If Failed:
- Verify `attack_range_squared(2500)` in game_logic.pl (50*50)
- Check `in_attack_range/4` predicate
- Verify attack state handled in wumpus.py ai_update()

---

## 🧪 Test 5: Attack → Chase Transition

### Steps:
1. Get Wumpus into attack state (very close)
2. Move player away (60-200 pixels away)
3. Watch AI state change

### Expected Behavior:
- AI state changes from **"attack" → "chase"**
- Wumpus starts moving again
- Wumpus follows player
- Walk animation resumes

### Checklist:
- [ ] ✅ State transitions to "chase"
- [ ] ✅ Wumpus resumes movement
- [ ] ✅ Wumpus direction points towards player
- [ ] ✅ Smooth transition (no stuttering)

### ❌ If Failed:
- Check attack→chase transition in game_logic.pl
- Verify `\+ in_attack_range(...)` condition works
- Check direction calculation

---

## 🧪 Test 6: Chase → Patrol Transition

### Steps:
1. Get Wumpus into chase state
2. Move player far away (> 300 pixels)
3. Watch AI state change

### Expected Behavior:
- AI state changes from **"chase" → "patrol"**
- Wumpus stops chasing
- Wumpus returns to patrol behavior
- Wumpus goes to nearest patrol point

### Checklist:
- [ ] ✅ State transitions to "patrol"
- [ ] ✅ Wumpus stops chasing
- [ ] ✅ Wumpus resumes patrol route
- [ ] ✅ Transition is smooth

### ❌ If Failed:
- Check chase→patrol transition in game_logic.pl
- Verify `\+ in_detection_range(...)` condition
- Check patrol() method in wumpus.py

---

## 🧪 Test 7: Direction Vector Accuracy

### Steps:
1. Enable debug mode (F)
2. Get Wumpus into chase state
3. Move around - watch Wumpus follow
4. Add temporary debug print to see direction vector

### Expected Behavior:
- Wumpus always moves towards player's current position
- Direction vector is **normalized** (length = 1.0)
- Direction updates smoothly as player moves
- No jerky or incorrect movements

### Test Directions:
Move player to different positions and verify Wumpus points correctly:
- [ ] ✅ Player to the **right** → Wumpus moves right (dx > 0, dy ≈ 0)
- [ ] ✅ Player **above** → Wumpus moves up (dx ≈ 0, dy < 0)
- [ ] ✅ Player **diagonal** → Wumpus moves diagonally (both dx, dy non-zero)
- [ ] ✅ Direction magnitude ≈ 1.0 (normalized)

### Debug Print:
Add to wumpus.py after Prolog query:
```python
print(f"Direction: ({direction_x:.3f}, {direction_y:.3f}), Magnitude: {math.sqrt(direction_x**2 + direction_y**2):.3f}")
```

Should see:
```
Direction: (0.707, 0.707), Magnitude: 1.000
```

### ❌ If Failed:
- Check `calculate_direction/6` in game_logic.pl
- Verify sqrt() calculation
- Check normalization formula (DX/Mag, DY/Mag)

---

## 🧪 Test 8: Prolog vs Python Fallback

### Steps:
1. **With Prolog**: Run game normally
   - Observe AI behavior
   - Check console for Prolog queries
   
2. **Simulate Prolog failure**: 
   - Temporarily break Prolog (rename game_logic.pl)
   - Run game
   - Observe fallback behavior

### Expected Behavior:

**With Prolog:**
- Console: No "falling back to Python" messages
- AI uses Prolog decisions
- State transitions are clean

**Without Prolog:**
- Console: `[Wumpus] Prolog AI query failed: ..., falling back to Python`
- AI uses Python fallback
- Basic chase/patrol still works
- Game doesn't crash

### Checklist:
- [ ] ✅ Prolog AI works when available
- [ ] ✅ Python fallback activates when Prolog fails
- [ ] ✅ No crashes in either mode
- [ ] ✅ Fallback message appears in console

### ❌ If Failed:
- Check try/except in wumpus.py ai_update()
- Verify fallback logic exists
- Check error handling in get_wumpus_decision()

---

## 🧪 Test 9: Performance & Stability

### Steps:
1. Run game for 3-5 minutes
2. Keep Wumpus transitioning between states:
   - Approach → chase
   - Get close → attack
   - Run away → patrol
   - Repeat
3. Monitor performance and console

### Expected Performance:
- Game runs at stable 60 FPS
- No slowdown during Prolog queries
- No memory leaks
- No repeated errors

### Checklist:
- [ ] ✅ FPS remains stable
- [ ] ✅ No performance degradation over time
- [ ] ✅ No console spam
- [ ] ✅ No memory issues

### ❌ If Failed:
- Check for infinite loops in Prolog predicates
- Verify Prolog queries are efficient (no backtracking issues)
- Check for memory leaks in Python/Prolog interface

---

## 🧪 Test 10: State Machine Completeness

### Test All Transitions:

| From State | To State | Condition | Working? |
|------------|----------|-----------|----------|
| patrol | chase | Player enters 300px | [ ] |
| chase | attack | Player enters 50px | [ ] |
| attack | chase | Player leaves 50px but in 300px | [ ] |
| chase | patrol | Player leaves 300px | [ ] |
| attack | patrol | Player leaves 300px entirely | [ ] |
| dead | dead | Always stays dead | [ ] |

### Checklist:
- [ ] ✅ All 6 transitions work correctly
- [ ] ✅ No invalid transitions occur
- [ ] ✅ Dead state is terminal (no escapes)
- [ ] ✅ State machine is deterministic (same input → same output)

### ❌ If Failed:
- Review wumpus_decision/8 clauses in game_logic.pl
- Check predicate order (earlier clauses have priority)
- Verify cut (!) operators prevent unintended backtracking

---

## 📊 Summary Checklist

### Prolog AI Core:
- [ ] Prolog predicates exist and load
- [ ] init_wumpus() works
- [ ] update_wumpus_position() works
- [ ] get_wumpus_decision() returns correct values
- [ ] Direction vectors are normalized

### State Transitions:
- [ ] patrol → chase (300px threshold)
- [ ] chase → attack (50px threshold)
- [ ] attack → chase (leave attack range)
- [ ] chase → patrol (leave detection range)
- [ ] All transitions smooth and correct

### Integration:
- [ ] Wumpus uses Prolog AI (not Python)
- [ ] Python fallback works when needed
- [ ] No console errors
- [ ] Performance is good

---

## 🎯 Phase 1.3 Status

If **ALL** tests pass:
- ✅ **Phase 1.3 COMPLETE** - Ready for Phase 1.4 (Combat System)

If **SOME** tests fail:
- ⚠️ **Phase 1.3 INCOMPLETE** - Fix failing tests before proceeding

---

## 📝 Test Results

**Date:** _______________

**Tester:** _______________

**Environment:**
- Python version: _______________
- Pygame version: _______________
- SWI-Prolog version: _______________

**Results:**
- Tests passed: _____ / 10
- Critical issues found: _______________
- Performance notes: _______________

**Notes:**
_______________________________________________
_______________________________________________
_______________________________________________

---

## 🔄 Next Phase Preview

**Phase 1.4: Combat System**
- Player can attack Wumpus
- Attack input (spacebar/click)
- Damage calculation
- Health system integration
- Attack cooldown
- Death handling
