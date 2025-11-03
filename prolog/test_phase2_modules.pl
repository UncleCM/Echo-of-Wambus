% =============================================================================
% TEST FILE: test_phase2_modules.pl
% PURPOSE: Integration test for Phase 2 entity modules
% =============================================================================

:- use_module('prolog/core/geometry.pl').
:- use_module('prolog/core/movement.pl').
:- use_module('prolog/core/map_system.pl').
:- use_module('prolog/entities/player.pl').
:- use_module('prolog/entities/wumpus_ai.pl').
:- use_module('prolog/entities/treasure.pl').

% =============================================================================
% TEST HELPERS
% =============================================================================

test_pass(TestName) :-
    format('✓ PASS: ~w~n', [TestName]).

test_fail(TestName, Reason) :-
    format('✗ FAIL: ~w - ~w~n', [TestName, Reason]).

% =============================================================================
% PLAYER MODULE TESTS
% =============================================================================

test_player :-
    format('~n=== Testing entities/player.pl ===~n'),
    
    % Test 1: Player position
    ( player:update_player_position(100, 200),
      player:get_player_position(X, Y),
      X =:= 100, Y =:= 200 ->
        test_pass('Player position - update and get')
    ;   test_fail('Player position - update and get', 'position incorrect')
    ),
    
    % Test 2: Initialize inventory
    ( player:init_player_inventory,
      player:get_player_inventory(Arrows, Rocks),
      Arrows =:= 2, Rocks =:= 3 ->
        test_pass('Player inventory - initialization (2 arrows, 3 rocks)')
    ;   test_fail('Player inventory - initialization', 'wrong starting inventory')
    ),
    
    % Test 3: Use arrow
    ( player:use_arrow,
      player:get_player_inventory(NewArrows, _),
      NewArrows =:= 1 ->
        test_pass('Player inventory - use arrow')
    ;   test_fail('Player inventory - use arrow', 'arrow not decremented')
    ),
    
    % Test 4: Use rock
    ( player:use_rock,
      player:get_player_inventory(_, NewRocks),
      NewRocks =:= 2 ->
        test_pass('Player inventory - use rock')
    ;   test_fail('Player inventory - use rock', 'rock not decremented')
    ),
    
    % Test 5: Add arrows (with max limit)
    ( player:add_arrows(5),
      player:get_player_inventory(MaxedArrows, _),
      MaxedArrows =:= 2 ->  % Should cap at max (2)
        test_pass('Player inventory - add arrows (respects max limit)')
    ;   test_fail('Player inventory - add arrows', 'max limit not enforced')
    ),
    
    % Test 6: Health system
    ( player:set_player_health(100),
      player:damage_player(30),
      player:get_player_health(HP1),
      HP1 =:= 70 ->
        test_pass('Player health - damage')
    ;   test_fail('Player health - damage', 'damage not applied correctly')
    ),
    
    % Test 7: Healing
    ( player:heal_player(20),
      player:get_player_health(HP2),
      HP2 =:= 90 ->
        test_pass('Player health - healing')
    ;   test_fail('Player health - healing', 'healing not applied correctly')
    ),
    
    % Test 8: Item pickups
    ( player:add_arrow_pickup(1, 100, 100),
      player:can_pickup_arrow(105, 105, 32, 32, PickupID),
      PickupID =:= 1 ->
        test_pass('Player pickup - arrow detection')
    ;   test_fail('Player pickup - arrow detection', 'pickup not detected')
    ).

% =============================================================================
% WUMPUS AI MODULE TESTS
% =============================================================================

test_wumpus_ai :-
    format('~n=== Testing entities/wumpus_ai.pl ===~n'),
    
    % Test 1: AI initialization
    ( wumpus_ai:init_wumpus_ai(1, 300),
      wumpus_ai:get_wumpus_state(1, State),
      State = roaming ->
        test_pass('Wumpus AI - initialization (roaming state)')
    ;   test_fail('Wumpus AI - initialization', 'wrong initial state')
    ),
    
    % Test 2: State change
    ( wumpus_ai:set_wumpus_state(1, chasing),
      wumpus_ai:get_wumpus_state(1, NewState),
      NewState = chasing ->
        test_pass('Wumpus AI - state change')
    ;   test_fail('Wumpus AI - state change', 'state not updated')
    ),
    
    % Test 3: Target setting
    ( wumpus_ai:set_wumpus_target(1, 500, 600),
      wumpus_ai:get_wumpus_target(1, TX, TY),
      TX =:= 500, TY =:= 600 ->
        test_pass('Wumpus AI - target setting')
    ;   test_fail('Wumpus AI - target setting', 'target not set correctly')
    ),
    
    % Test 4: Patrol index
    ( wumpus_ai:set_patrol_index(1, 0),
      wumpus_ai:increment_patrol_index(1, 5),
      wumpus_ai:get_patrol_index(1, Index),
      Index =:= 1 ->
        test_pass('Wumpus AI - patrol index increment')
    ;   test_fail('Wumpus AI - patrol index increment', 'index not incremented')
    ),
    
    % Test 5: Search timer
    ( wumpus_ai:set_search_timer(1, 5.0),
      wumpus_ai:update_search_timer(1, 1.0),
      wumpus_ai:get_search_timer(1, Time),
      Time =:= 4.0 ->
        test_pass('Wumpus AI - search timer update')
    ;   test_fail('Wumpus AI - search timer update', 'timer not updated correctly')
    ),
    
    % Test 6: Hearing radius
    ( wumpus_ai:get_hearing_radius(1, BaseHearing),
      BaseHearing =:= 300 ->
        test_pass('Wumpus AI - hearing radius')
    ;   test_fail('Wumpus AI - hearing radius', 'wrong hearing value')
    ),
    
    % Test 7: Decision logic - sound detection
    ( wumpus_ai:decide_wumpus_action(1, walk, 200, 200, 50, roaming, 
                                      DecState, DecRoar, DecX, DecY),
      DecState = chasing,
      DecRoar = true,
      DecX =:= 200, DecY =:= 200 ->
        test_pass('Wumpus AI - decision logic (loud sound → chase)')
    ;   test_fail('Wumpus AI - decision logic', 'wrong decision made')
    ),
    
    % Test 8: Reached target
    ( wumpus_ai:reached_target(100, 100, 110, 110, 20) ->
        test_pass('Wumpus AI - reached target (within threshold)')
    ;   test_fail('Wumpus AI - reached target', 'should be within threshold')
    ).

% =============================================================================
% TREASURE MODULE TESTS
% =============================================================================

test_treasure :-
    format('~n=== Testing entities/treasure.pl ===~n'),
    
    % Test 1: Treasure system setup
    ( treasure:setup_treasure_system(100, 100, 200, 200, 300, 300),
      treasure:get_all_chests(Chests),
      length(Chests, Count),
      Count =:= 3 ->
        test_pass('Treasure - system setup (3 chests created)')
    ;   test_fail('Treasure - system setup', 'wrong number of chests')
    ),
    
    % Test 2: Chest types (1 treasure + 2 mimics)
    ( treasure:get_all_chests(AllChests),
      include(is_treasure_chest, AllChests, TreasureChests),
      include(is_mimic_chest, AllChests, MimicChests),
      length(TreasureChests, TC),
      length(MimicChests, MC),
      TC =:= 1, MC =:= 2 ->
        test_pass('Treasure - chest distribution (1 treasure, 2 mimics)')
    ;   test_fail('Treasure - chest distribution', 'wrong chest types')
    ),
    
    % Test 3: Distance calculation
    ( treasure:distance_to_chest(100, 100, 103, 104, Dist),
      Dist < 6 ->
        test_pass('Treasure - distance calculation')
    ;   test_fail('Treasure - distance calculation', 'wrong distance')
    ),
    
    % Test 4: Collect treasure
    ( treasure:collect_treasure,
      treasure:is_treasure_collected ->
        test_pass('Treasure - collection')
    ;   test_fail('Treasure - collection', 'treasure not marked collected')
    ),
    
    % Test 5: Exit system
    ( treasure:init_exit(800, 600),
      treasure:get_exit_position(EX, EY),
      EX =:= 800, EY =:= 600 ->
        test_pass('Treasure - exit initialization')
    ;   test_fail('Treasure - exit initialization', 'wrong exit position')
    ),
    
    % Test 6: Unlock exit
    ( treasure:unlock_exit,
      treasure:is_exit_unlocked ->
        test_pass('Treasure - exit unlock')
    ;   test_fail('Treasure - exit unlock', 'exit not unlocked')
    ),
    
    % Test 7: Can exit (with treasure, at exit)
    ( treasure:can_exit_game(800, 600, 32, 48) ->
        test_pass('Treasure - can exit game (all conditions met)')
    ;   test_fail('Treasure - can exit game', 'should allow exit')
    ).

% Helper predicates for treasure tests
is_treasure_chest([treasure|_]).
is_mimic_chest([mimic|_]).

% =============================================================================
% INTEGRATION TESTS
% =============================================================================

test_integration :-
    format('~n=== Integration Tests (Phase 2) ===~n'),
    
    % Test 1: Player + Map system
    ( map_system:init_map_system(1000, 800, 32, 32, 32),
      player:update_player_position(500, 500),
      player:get_player_position(PX, PY),
      PX =:= 500, PY =:= 500 ->
        test_pass('Integration - player position in map')
    ;   test_fail('Integration - player position in map', 'position not set')
    ),
    
    % Test 2: Player inventory + combat readiness
    ( player:init_player_inventory,
      player:can_use_arrow,
      player:can_use_rock ->
        test_pass('Integration - player combat ready')
    ;   test_fail('Integration - player combat ready', 'not ready for combat')
    ),
    
    % Test 3: Wumpus AI + player position (simulated chase)
    ( wumpus_ai:init_wumpus_ai(2, 300),
      player:get_player_position(PlayerX, PlayerY),
      wumpus_ai:set_wumpus_target(2, PlayerX, PlayerY),
      wumpus_ai:set_wumpus_state(2, chasing),
      wumpus_ai:get_wumpus_state(2, ChaseState),
      ChaseState = chasing ->
        test_pass('Integration - wumpus chasing player')
    ;   test_fail('Integration - wumpus chasing player', 'chase not working')
    ),
    
    % Test 4: Treasure + player position (victory condition)
    ( treasure:init_exit(500, 500),
      treasure:collect_treasure,
      treasure:unlock_exit,
      treasure:can_exit_game(500, 500, 32, 48) ->
        test_pass('Integration - victory condition check')
    ;   test_fail('Integration - victory condition check', 'victory not detected')
    ).

% =============================================================================
% MAIN TEST RUNNER
% =============================================================================

run_all_tests :-
    format('~n╔════════════════════════════════════════════╗~n'),
    format('║  PHASE 2 ENTITY MODULES TEST SUITE        ║~n'),
    format('╚════════════════════════════════════════════╝~n'),
    test_player,
    test_wumpus_ai,
    test_treasure,
    test_integration,
    format('~n╔════════════════════════════════════════════╗~n'),
    format('║  ALL PHASE 2 TESTS COMPLETED               ║~n'),
    format('╚════════════════════════════════════════════╝~n~n').

% Auto-run tests when file is consulted
:- initialization(run_all_tests).
