# Phase 1.5.1 Manual Testing Checklist
**Treasure Hunt Mechanics + Arrow Combat System Verification**

## Prerequisites
- [ ] Game runs without errors (`python main.py`)
- [ ] Player and Wumpus spawn correctly
- [ ] Treasure, Exit Portal, and Arrow Pickups spawn correctly
- [ ] Debug mode available (F3)

---

## Test 1: Arrow Combat Initialization
**Objective:** Verify arrow combat system initializes correctly

**Steps:**
1. Start the game
2. Check HUD for arrow count display (🏹 icon)
3. Press F3 to observe arrow pickup sprites
4. Count arrow pickups in the world

**Expected Results:**
- ✅ Player starts with 1 arrow (HUD shows: 🏹 x1/2)
- ✅ 3 arrow pickups spawn in the world (cyan glowing quivers)
- ✅ Arrow count displays in HUD at top-left
- ✅ Arrow count shows gray when arrows > 0, red when 0
- ✅ No errors in console

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 2: Arrow Shooting Mechanics
**Objective:** Verify arrow shooting works correctly with movement restriction

**Steps:**
1. Start the game
2. Try shooting while moving (press Space while walking)
3. Stop moving completely
4. Press Space to shoot arrow
5. Observe arrow projectile

**Expected Results:**
- ✅ Cannot shoot while moving (console shows "Must stop moving to shoot!")
- ✅ Can shoot when standing still
- ✅ Arrow count decreases by 1 after shooting
- ✅ Arrow projectile appears and flies in facing direction
- ✅ Arrow has proper rotation based on direction
- ✅ Arrow disappears after max distance (~450 pixels)
- ✅ Arrow disappears when hitting walls

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 3: Arrow Pickup Collection
**Objective:** Verify arrow pickups can be collected and respect limits

**Steps:**
1. Start game (1 arrow, max 2)
2. Shoot arrow to use it (now 0/2)
3. Find and walk into arrow pickup
4. Collect second arrow pickup
5. Try collecting third pickup

**Expected Results:**
- ✅ Arrow pickups have cyan glow animation
- ✅ Walking into pickup collects it instantly
- ✅ Pickup disappears permanently after collection
- ✅ Arrow count increases by 1 (max 2)
- ✅ Console shows pickup message with current count
- ✅ Cannot collect when at max (2/2) arrows
- ✅ HUD updates correctly

**Status:** ⬜ Pass / ⬜ Fail

**Arrow Count Journey:** Start: 1/2 → After use: _/2 → After pickup 1: _/2 → After pickup 2: _/2

---

## Test 4: Arrow Hit Detection
**Objective:** Verify arrows hit Wumpus reliably with improved detection

**Steps:**
1. Start game
2. Locate Wumpus
3. Stand facing Wumpus at various distances
4. Shoot arrows at Wumpus (close range, medium range)
5. Try shooting from different angles

**Expected Results:**
- ✅ Arrow hits Wumpus when aimed directly (hitbox collision)
- ✅ Arrow hits Wumpus within ~60 pixel radius (distance detection)
- ✅ Hit detection works at close range (face-to-face)
- ✅ Hit detection works at medium range
- ✅ Console shows hit message with distance
- ✅ Arrow disappears on hit
- ✅ Wumpus gets stunned on hit

**Status:** ⬜ Pass / ⬜ Fail

**Hit Rate:** Shots fired: ___ / Hits: ___ / Miss: ___

---

## Test 5: Wumpus Stun Mechanics
**Objective:** Verify Wumpus stun behavior works correctly

**Steps:**
1. Start game
2. Shoot arrow at Wumpus to trigger stun
3. Observe Wumpus behavior during stun
4. Wait 3 seconds
5. Observe Wumpus behavior after stun ends

**Expected Results:**
- ✅ Wumpus freezes completely when stunned
- ✅ Console shows "Wumpus stunned for 3 seconds!" message
- ✅ Wumpus does not move for 3 seconds
- ✅ Wumpus does not attack while stunned
- ✅ AI state shows 'stunned' (if debug visible)
- ✅ After 3 seconds, Wumpus returns to patrol
- ✅ Wumpus resumes normal behavior after recovery

**Status:** ⬜ Pass / ⬜ Fail

**Stun Duration Observed:** _____ seconds

---

## Test 6: Arrow Resource Management
**Objective:** Verify tactical gameplay with limited arrows

**Steps:**
1. Start game (1 arrow)
2. Shoot all starting arrows (1 shot)
3. Try shooting when out of arrows
4. Collect all 3 pickups
5. Use all arrows (should have used 4 total: 1 start + 3 pickups)
6. Verify no more arrows available

**Expected Results:**
- ✅ Cannot shoot when arrow count = 0
- ✅ Console feedback when trying to shoot without arrows
- ✅ Maximum total arrows available: 4 (1 start + 3 pickups)
- ✅ Can never carry more than 2 arrows at once
- ✅ Pickups do not respawn
- ✅ Strategy required to manage limited arrows

**Status:** ⬜ Pass / ⬜ Fail

**Total Arrows Used:** _____ (expected: 4 maximum)

---

## Test 7: Treasure Spawning and Collection
**Objective:** Verify treasure spawns and can be collected

**Steps:**
1. Start the game
2. Locate the treasure (should have glowing animation)
3. Walk into the treasure to collect it
4. Observe changes

**Expected Results:**
- ✅ Treasure spawns with glowing/sparkle effect
- ✅ Treasure disappears when player touches it
- ✅ Console shows "Treasure collected!" message
- ✅ HUD shows treasure icon changes (⬜ → ✓)
- ✅ Console shows "Exit unlocked!" message
- ✅ Console shows "The Wumpus is enraged!" message

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 2: Exit Portal States
**Objective:** Verify exit portal locked/unlocked behavior

**Steps:**
1. Start the game
2. Navigate to entrance (where exit portal is)
3. Observe exit portal appearance (should be locked)
4. Collect treasure
5. Return to exit portal
6. Observe exit portal appearance (should be unlocked/glowing)

**Expected Results:**
- ✅ Exit portal spawns at entrance position
- ✅ Exit portal shows locked state initially (🔒 icon in HUD)
- ✅ Cannot escape before collecting treasure
- ✅ Exit portal unlocks after treasure collected (🔓 icon in HUD)
- ✅ Exit portal glows/changes appearance when unlocked
- ✅ Can enter exit portal when unlocked

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 3: Wumpus Enrage Mechanic
**Objective:** Verify Wumpus speed increases after treasure collection

**Steps:**
1. Start the game
2. Observe Wumpus movement speed before treasure collection
3. Collect treasure
4. Observe Wumpus movement speed after collection

**Expected Results:**
- ✅ Wumpus moves at normal speed initially
- ✅ Console shows "The Wumpus is enraged!" when treasure collected
- ✅ Wumpus speed visibly increases by ~50% after treasure collected
- ✅ Wumpus becomes more difficult to evade
- ✅ Speed increase persists until game over/restart

**Status:** ⬜ Pass / ⬜ Fail

**Speed Estimate Before:** _____ 
**Speed Estimate After:** _____

---

## Test 4: Time Limit - Normal Play
**Objective:** Verify 3-minute time limit displays and counts down

**Steps:**
1. Start the game
2. Observe timer display at top center of screen
3. Play for 1-2 minutes
4. Watch timer countdown

**Expected Results:**
- ✅ Timer displays at top center in MM:SS format
- ✅ Timer starts at 03:00 (180 seconds)
- ✅ Timer counts down every second
- ✅ Timer turns red when below 00:30 (30 seconds)
- ✅ Timer is always visible during gameplay

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 5: Time Limit - Timeout Game Over
**Objective:** Verify timeout triggers game over screen

**Steps:**
1. Start the game
2. Wait for timer to reach 00:00 (or use debug to skip time)
3. Observe behavior

**Expected Results:**
- ✅ Game over screen appears when timer reaches 00:00
- ✅ "GAME OVER" title displayed in red
- ✅ "Time's up!" death reason shown
- ✅ Game over screen displays (NOT game closing)
- ✅ Survival time shown
- ✅ "Press R to Restart" prompt visible

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 6: Victory Screen - Escape with Treasure
**Objective:** Verify victory screen appears when escaping with treasure

**Steps:**
1. Start the game
2. Collect treasure
3. Return to entrance
4. Enter unlocked exit portal
5. Observe victory screen

**Expected Results:**
- ✅ Screen transitions to victory screen
- ✅ "VICTORY!" title displayed in green/gold
- ✅ "Escaped with the treasure!" message shown (NOT "Wumpus Defeated!")
- ✅ Escape time displayed (MM:SS format)
- ✅ Remaining health shown (e.g., "Health: 75/100")
- ✅ "Press R to Restart" prompt visible
- ✅ Game updates paused
- ✅ Semi-transparent dark overlay visible

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 7: Victory Blocked - No Treasure
**Objective:** Verify cannot escape without collecting treasure

**Steps:**
1. Start the game
2. Navigate to exit portal (entrance) WITHOUT collecting treasure
3. Try to enter exit portal
4. Observe behavior

**Expected Results:**
- ✅ Exit portal remains locked (🔒 icon)
- ✅ Cannot enter exit portal
- ✅ No victory screen triggered
- ✅ Player can still move freely

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 8: Game Over Screen - Defeated by Wumpus
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

## Test 9: Game Over Screen - Fell into Pit
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

## Test 10: Full Treasure Hunt Loop
**Objective:** Complete the full gameplay loop from start to victory

**Steps:**
1. Start the game
2. Explore and find treasure
3. Collect treasure (observe enrage)
4. Evade enraged Wumpus
5. Return to entrance
6. Escape through exit portal

**Expected Results:**
- ✅ Can complete full loop within 3 minutes
- ✅ All mechanics work smoothly together
- ✅ Wumpus chase is challenging but fair
- ✅ Victory feels earned

**Completion Time:** _____
**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 11: Restart from Victory Screen
**Objective:** Verify restart works from victory screen

**Steps:**
1. Achieve victory (escape with treasure)
2. Wait for victory screen
3. Press R key
4. Observe game state

**Expected Results:**
- ✅ Game restarts immediately
- ✅ Player spawns at entrance position
- ✅ Player health reset to 100/100
- ✅ Treasure respawns
- ✅ Exit portal relocked (🔒)
- ✅ has_treasure reset to False
- ✅ exit_unlocked reset to False
- ✅ Wumpus speed reset to normal
- ✅ Timer resets to 03:00
- ✅ Game state returns to PLAYING

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 12: Restart from Game Over Screen
**Objective:** Verify restart works from game over screen

**Steps:**
1. Trigger game over (fall, Wumpus, or timeout)
2. Wait for game over screen
3. Press R key
4. Observe game state

**Expected Results:**
- ✅ Game restarts immediately
- ✅ Player spawns at entrance position
- ✅ Player health reset to 100/100
- ✅ Player is alive (is_alive = True)
- ✅ Treasure respawns
- ✅ Exit portal relocked
- ✅ Wumpus spawns at spawn position
- ✅ Wumpus health reset to 150/150
- ✅ Wumpus is alive (is_alive = True)
- ✅ Timer resets to 03:00
- ✅ Death reason cleared
- ✅ Can move and play normally

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 13: HUD Display Accuracy
**Objective:** Verify HUD shows correct arrow count, treasure and exit status

**Steps:**
1. Start the game
2. Check HUD icons at start
3. Shoot an arrow
4. Collect treasure
5. Collect arrow pickup
6. Check HUD updates

**Expected Results:**
- ✅ Timer displays correctly (MM:SS format) at top center
- ✅ Arrow count shows 🏹 x1/2 at start (top-left)
- ✅ Arrow count turns red when 0 arrows
- ✅ Arrow count updates immediately after shooting
- ✅ Arrow count updates immediately after pickup
- ✅ Treasure icon starts as ⬜ (uncollected)
- ✅ Exit icon starts as 🔒 (locked)
- ✅ After collection, treasure icon becomes ✓
- ✅ After collection, exit icon becomes 🔓
- ✅ All icons are clearly visible and understandable

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 14: Multiple Restart Cycles
**Objective:** Verify game remains stable across multiple restarts

**Steps:**
1. Play game and trigger victory
2. Press R to restart
3. Play again and trigger game over (any method)
4. Press R to restart
5. Repeat 3 times

**Expected Results:**
- ✅ All restarts work correctly
- ✅ No memory leaks or slowdowns
- ✅ Game state resets properly each time
- ✅ No errors in console
- ✅ Player/Wumpus spawn correctly each time
- ✅ Treasure/Exit spawn correctly each time
- ✅ Health resets correctly each time
- ✅ Timer resets correctly each time

**Status:** ⬜ Pass / ⬜ Fail

**Cycles Completed:** _______________

---

## Test 15: Edge Case - Death After Treasure Collection
**Objective:** Verify game over works correctly after collecting treasure

**Steps:**
1. Start game
2. Collect treasure
3. Die to Wumpus or fall into pit
4. Observe behavior

**Expected Results:**
- ✅ Game over triggers correctly
- ✅ Death reason is accurate
- ✅ No errors or crashes
- ✅ Can restart normally
- ✅ After restart, treasure is back and uncollected

**Status:** ⬜ Pass / ⬜ Fail

**Notes:** _________________________________

---

## Test 16: Integrated Gameplay - Arrow Combat + Treasure Hunt
**Objective:** Verify complete gameplay loop with arrow combat

**Steps:**
1. Start game
2. Use starting arrow to stun Wumpus
3. Collect treasure while Wumpus is stunned
4. Collect arrow pickups
5. Use arrows to stun Wumpus when chased
6. Navigate to exit
7. Escape to win

**Expected Results:**
- ✅ Can stun Wumpus to create safe window
- ✅ Treasure collection triggers enrage during stun
- ✅ Enraged Wumpus is faster after recovering from stun
- ✅ Arrow pickups help survive enraged chase
- ✅ Strategic arrow use enables escape
- ✅ Victory achievable with smart arrow management
- ✅ Gameplay feels tactical and skill-based

**Status:** ⬜ Pass / ⬜ Fail

**Strategy Used:** _________________________________

---

## Summary
- **Total Tests:** 21 (6 new arrow combat tests + 15 original)
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

## Arrow Combat Feedback
_Specific feedback on arrow combat system:_

**Hit Detection:**
- Too easy / Just right / Too hard: _______________
- Suggested improvements: _______________

**Arrow Shooting:**
- Stop-to-shoot mechanic: Good / Frustrating: _______________
- Suggested changes: _______________

**Resource Management:**
- 4 total arrows: Too few / Just right / Too many: _______________
- Max 2 carrying: Good / Should be different: _______________

**Stun Duration:**
- 3 seconds: Too short / Just right / Too long: _______________
- Suggested duration: _______________

**Overall Feel:**
- Tactical gameplay: Yes / No: _______________
- Fun factor: ___/10
- Difficulty: Too easy / Balanced / Too hard: _______________

## Recommendations
_Suggestions for improvement:_

1. ___________________________________
2. ___________________________________
3. ___________________________________

---

## Next Steps
- [ ] If all tests pass: **Begin Phase 2 (Polish & Features)**
- [ ] If tests fail: **Fix issues and re-test**
- [ ] Consider adding: 
  - Visual stun effect on Wumpus (stars/dizzy animation)
  - Better arrow trail particles
  - Hit impact effect (spark/flash)
  - Arrow pickup sparkle enhancement
  - Sound effects for shooting/hit/stun
  - Charging/aiming indicator

---

**Tester:** ________________  
**Date:** ________________  
**Branch:** comlete-base-game  
**Phase:** 1.5.1 - Treasure Hunt Mechanics + Arrow Combat
