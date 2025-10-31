# Phase 1.4 Manual Testing Checklist
**Combat System Verification**

## Prerequisites
- [ ] Game runs without errors (`python main.py`)
- [ ] Player spawns correctly
- [ ] Wumpus spawns correctly
- [ ] Debug mode enabled (F3 or in code)

---

## Test 1: Player Attack Input
**Objective:** Verify Spacebar triggers attack

**Steps:**
1. Start the game
2. Press Spacebar
3. Watch console for "[Debug] Player attacking!" message
4. Observe player character

**Expected Results:**
- ✅ Console shows attack message
- ✅ Player character stops moving briefly
- ✅ Yellow attack range circle appears (if debug mode enabled)

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 2: Attack Cooldown
**Objective:** Verify attacks have 0.5s cooldown

**Steps:**
1. Start the game
2. Press Spacebar rapidly multiple times
3. Watch console for attack messages
4. Count messages over 2 seconds

**Expected Results:**
- ✅ Attack messages appear maximum ~4 times in 2 seconds (0.5s cooldown)
- ✅ Rapid pressing doesn't spam attacks
- ✅ Cannot attack while previous attack is active

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 3: Attack Range Visualization
**Objective:** Verify attack range display in debug mode

**Steps:**
1. Enable debug mode (F3)
2. Press Spacebar to attack
3. Observe yellow circle around player

**Expected Results:**
- ✅ Yellow circle appears when attacking
- ✅ Circle radius is 80px (visually check against tiles)
- ✅ Circle disappears after attack ends
- ✅ Circle centered on player

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 4: Player Damages Wumpus
**Objective:** Verify player can damage Wumpus within range

**Steps:**
1. Navigate player close to Wumpus (<80px)
2. Press Spacebar to attack
3. Watch console for combat messages
4. Observe Wumpus health bar (debug mode)

**Expected Results:**
- ✅ Console shows "Player hit Wumpus for 50 damage!"
- ✅ Console shows "Wumpus HP: X/150"
- ✅ Wumpus health bar decreases (red bar shortens)
- ✅ No damage if player >80px away

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 5: Wumpus Damages Player
**Objective:** Verify Wumpus can damage player when in attack state

**Steps:**
1. Stay within Wumpus attack range (50px)
2. Wait for Wumpus AI to enter 'attack' state (red behavior)
3. Watch console for combat messages
4. Observe Player health bar (debug mode)

**Expected Results:**
- ✅ Console shows "Wumpus hit Player for 25 damage!"
- ✅ Console shows "Player HP: X/100"
- ✅ Player health bar decreases (green bar shortens)
- ✅ No damage if player >50px away

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 6: Health Bar Display
**Objective:** Verify health bars show correct information

**Steps:**
1. Enable debug mode (F3)
2. Look at Player and Wumpus
3. Damage both entities
4. Watch health bars update

**Expected Results:**
- ✅ Player has green health bar above sprite
- ✅ Wumpus has red health bar above sprite
- ✅ Health bars have white border
- ✅ Health bars shrink proportionally to damage (100px total width)
- ✅ Health text shows "HP: X/Max" above bar

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 7: Kill Wumpus (Victory)
**Objective:** Verify victory condition on Wumpus death

**Steps:**
1. Attack Wumpus repeatedly (need 3 hits: 150 HP / 50 damage)
2. Land final hit that kills Wumpus
3. Watch console and screen

**Expected Results:**
- ✅ Console shows "Wumpus defeated!"
- ✅ Victory message appears on screen
- ✅ Wumpus sprite changes (death animation/state)
- ✅ Wumpus health bar shows 0 HP

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 8: Player Death (Game Over)
**Objective:** Verify game over condition on player death

**Steps:**
1. Let Wumpus attack you repeatedly (need 4 hits: 100 HP / 25 damage)
2. Take final hit that kills player
3. Watch console and screen

**Expected Results:**
- ✅ Console shows "Player died!"
- ✅ Game over message appears on screen
- ✅ Player sprite changes (death animation/state)
- ✅ Player health bar shows 0 HP
- ✅ Game state changes (can't move/attack)

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 9: Combat Balance Feel
**Objective:** Verify combat feels fair and fun

**Steps:**
1. Fight Wumpus normally
2. Try different tactics (hit and run, circling, etc.)
3. Assess difficulty

**Expected Results:**
- ✅ Player has advantage (longer range, higher damage)
- ✅ Wumpus is challenging but beatable
- ✅ Combat doesn't feel too easy or impossible
- ✅ Attack cooldown feels responsive (not too slow)

**Subjective Rating:** ⬜ Too Easy / ⬜ Balanced / ⬜ Too Hard

---

## Test 10: Combat Edge Cases
**Objective:** Verify edge cases don't break combat

**Steps:**
1. Try attacking while moving
2. Try attacking at exactly 80px range (edge of range)
3. Try attacking dead Wumpus
4. Try moving while attacking

**Expected Results:**
- ✅ Can attack while moving (movement stops during attack)
- ✅ Edge-of-range attacks work correctly
- ✅ Can't damage already-dead entities
- ✅ Movement blocked during attack animation

**Status:** ⬜ Pass / ⬜ Fail

---

## Summary
- **Total Tests:** 10
- **Passed:** ___
- **Failed:** ___
- **Overall Status:** ⬜ Ready for Phase 1.5 / ⬜ Needs Fixes

## Notes
_Add any observations, bugs found, or balance feedback here:_

---

## Next Steps
- [ ] If all tests pass: Proceed to Phase 1.5 (Win/Lose Conditions)
- [ ] If tests fail: Fix issues and re-test
- [ ] Consider balance adjustments based on feel

---

**Tester:** ________________  
**Date:** ________________  
**Branch:** comlete-base-game
