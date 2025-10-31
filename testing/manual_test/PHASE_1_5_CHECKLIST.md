# Phase 1.5 Manual Testing Checklist
**Win/Lose Conditions Verification**

## Prerequisites
- [ ] Game runs without errors (`python main.py`)
- [ ] Player and Wumpus spawn correctly
- [ ] Debug mode available (F3)

---

## Test 1: Victory Screen - Kill Wumpus
**Objective:** Verify victory screen appears when Wumpus is defeated

**Steps:**
1. Start the game
2. Navigate to Wumpus
3. Attack and kill Wumpus (3 hits needed: 150 HP / 50 damage)
4. Observe victory screen

**Expected Results:**
- ✅ Screen transitions to victory screen
- ✅ "VICTORY!" title displayed in green/gold
- ✅ "Wumpus Defeated!" message shown
- ✅ Game time displayed (MM:SS format)
- ✅ Remaining health shown (e.g., "Health: 75/100")
- ✅ "Press R to Restart" prompt visible
- ✅ Game updates paused (player/wumpus don't move)
- ✅ Semi-transparent dark overlay visible

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 2: Game Over Screen - Defeated by Wumpus
**Objective:** Verify game over screen with correct message when killed by Wumpus

**Steps:**
1. Start the game
2. Find Wumpus and let it attack you
3. Take damage until death (4 hits needed: 100 HP / 25 damage)
4. Observe game over screen

**Expected Results:**
- ✅ Screen transitions to game over screen
- ✅ "GAME OVER" title displayed in red
- ✅ "Defeated by the Wumpus!" message shown
- ✅ Survival time displayed (MM:SS format)
- ✅ "Press R to Restart" prompt visible
- ✅ Game updates paused
- ✅ Semi-transparent dark overlay visible

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 3: Game Over Screen - Fell into Pit
**Objective:** Verify game over screen with correct message when falling

**Steps:**
1. Start the game
2. Navigate to a pit/hole
3. Walk into the pit to fall
4. Observe game over screen

**Expected Results:**
- ✅ Screen transitions to game over screen
- ✅ "GAME OVER" title displayed in red
- ✅ "Fell into a pit!" message shown (NOT "Defeated by Wumpus!")
- ✅ Survival time displayed (MM:SS format)
- ✅ "Press R to Restart" prompt visible
- ✅ Game updates paused
- ✅ Console shows "GAME OVER - Fell into a hole! (Prolog detected)"

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 4: Restart from Victory Screen
**Objective:** Verify restart works from victory screen

**Steps:**
1. Achieve victory (kill Wumpus)
2. Wait for victory screen
3. Press R key
4. Observe game state

**Expected Results:**
- ✅ Game restarts immediately
- ✅ Player spawns at entrance position
- ✅ Player health reset to 100/100
- ✅ Wumpus spawns at spawn position
- ✅ Wumpus health reset to 150/150
- ✅ Game timer resets (starts from 0)
- ✅ Game state returns to PLAYING
- ✅ Can move and play normally

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 5: Restart from Game Over Screen
**Objective:** Verify restart works from game over screen

**Steps:**
1. Trigger game over (fall or die to Wumpus)
2. Wait for game over screen
3. Press R key
4. Observe game state

**Expected Results:**
- ✅ Game restarts immediately
- ✅ Player spawns at entrance position
- ✅ Player health reset to 100/100
- ✅ Player is alive (is_alive = True)
- ✅ Wumpus spawns at spawn position
- ✅ Wumpus health reset to 150/150
- ✅ Wumpus is alive (is_alive = True)
- ✅ Game timer resets
- ✅ Death reason cleared
- ✅ Can move and play normally

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 6: Game Timer Accuracy
**Objective:** Verify game timer tracks time correctly

**Steps:**
1. Start the game
2. Play for approximately 1 minute
3. Trigger victory or game over
4. Check displayed time

**Expected Results:**
- ✅ Time displayed in MM:SS format
- ✅ Time is approximately correct (~60 seconds shows as 01:00)
- ✅ Seconds increment properly (00-59)
- ✅ Minutes increment at 60 seconds
- ✅ Time freezes at victory/game over (doesn't keep counting)

**Actual Time:** ________________

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 7: Game Pause on Victory
**Objective:** Verify game pauses when victory is achieved

**Steps:**
1. Kill Wumpus to achieve victory
2. Watch victory screen for 10 seconds
3. Observe player and environment

**Expected Results:**
- ✅ Player cannot move (input disabled)
- ✅ Wumpus doesn't move or update
- ✅ Animations may continue (idle animations OK)
- ✅ Camera doesn't move
- ✅ No combat possible
- ✅ Screen stays on victory display

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 8: Game Pause on Game Over
**Objective:** Verify game pauses when game over occurs

**Steps:**
1. Trigger game over (any method)
2. Watch game over screen for 10 seconds
3. Observe player and environment

**Expected Results:**
- ✅ Player cannot move (input disabled)
- ✅ Wumpus doesn't move or update
- ✅ No additional damage can occur
- ✅ Camera doesn't move
- ✅ Screen stays on game over display

**Status:** ⬜ Pass / ⬜ Fail

---

## Test 9: Victory Screen Visual Quality
**Objective:** Assess visual quality and readability of victory screen

**Steps:**
1. Achieve victory
2. Read all text on screen
3. Assess visual design

**Expected Results:**
- ✅ All text is readable
- ✅ Colors are appropriate (green/gold for victory)
- ✅ No text overlap
- ✅ Stats are accurate
- ✅ Screen looks polished
- ✅ Overlay doesn't obscure critical info

**Subjective Rating:** ⬜ Excellent / ⬜ Good / ⬜ Needs Improvement

**Notes:** _________________________________

---

## Test 10: Game Over Screen Visual Quality
**Objective:** Assess visual quality and readability of game over screen

**Steps:**
1. Trigger game over (both methods: Wumpus and fall)
2. Read all text on screen
3. Assess visual design

**Expected Results:**
- ✅ All text is readable
- ✅ Colors are appropriate (red for game over)
- ✅ No text overlap
- ✅ Death reason is clear and correct
- ✅ Stats are accurate
- ✅ Screen looks polished

**Subjective Rating:** ⬜ Excellent / ⬜ Good / ⬜ Needs Improvement

**Notes:** _________________________________

---

## Test 11: Multiple Restart Cycles
**Objective:** Verify game remains stable across multiple restarts

**Steps:**
1. Play game and trigger victory
2. Press R to restart
3. Play again and trigger game over
4. Press R to restart
5. Repeat 3 times

**Expected Results:**
- ✅ All restarts work correctly
- ✅ No memory leaks or slowdowns
- ✅ Game state resets properly each time
- ✅ No errors in console
- ✅ Player/Wumpus spawn correctly each time
- ✅ Health resets correctly each time

**Status:** ⬜ Pass / ⬜ Fail

**Cycles Completed:** _______________

---

## Test 12: Edge Case - Instant Death
**Objective:** Verify game over works if player dies immediately

**Steps:**
1. Start game
2. Immediately walk into pit (within 1 second)
3. Observe behavior

**Expected Results:**
- ✅ Game over triggers correctly
- ✅ Timer shows ~00:00 or 00:01
- ✅ No errors or crashes
- ✅ Can restart normally

**Status:** ⬜ Pass / ⬜ Fail

---

## Summary
- **Total Tests:** 12
- **Passed:** ___
- **Failed:** ___
- **Overall Status:** ⬜ Ready for Phase 2 / ⬜ Needs Fixes

## Critical Issues Found
_List any game-breaking bugs or critical problems:_

1. ___________________________________
2. ___________________________________
3. ___________________________________

## Minor Issues Found
_List any minor bugs or polish issues:_

1. ___________________________________
2. ___________________________________
3. ___________________________________

## Recommendations
_Suggestions for improvement:_

1. ___________________________________
2. ___________________________________
3. ___________________________________

---

## Next Steps
- [ ] If all tests pass: **Begin Phase 2 (Polish & Features)**
- [ ] If tests fail: **Fix issues and re-test**
- [ ] Consider adding: Victory music/sound effects, particle effects, better UI design

---

**Tester:** ________________  
**Date:** ________________  
**Branch:** comlete-base-game  
**Phase:** 1.5 - Win/Lose Conditions
