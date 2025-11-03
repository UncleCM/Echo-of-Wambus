% =============================================================================
% TEST FILE: test_phase3_modules.pl
% PURPOSE: Integration test for Phase 3 system modules
% =============================================================================

:- use_module('prolog/core/geometry.pl').
:- use_module('prolog/core/movement.pl').
:- use_module('prolog/core/map_system.pl').
:- use_module('prolog/entities/player.pl').
:- use_module('prolog/entities/wumpus_ai.pl').
:- use_module('prolog/entities/treasure.pl').
:- use_module('prolog/systems/combat.pl').
:- use_module('prolog/systems/spawning.pl').
:- use_module('prolog/systems/game_state.pl').

% =============================================================================
% TEST HELPERS
% =============================================================================

test_pass(TestName) :-
    format('✓ PASS: ~w~n', [TestName]).

test_fail(TestName, Reason) :-
    format('✗ FAIL: ~w - ~w~n', [TestName, Reason]).

% =============================================================================
% COMBAT MODULE TESTS
% =============================================================================

test_combat :-
    format('~n=== Testing systems/combat.pl ===~n'),
    
    % Test 1: Initialize projectile system
    ( combat:init_projectile_system,
      combat:count_active_arrows(ArrowCount),
      combat:count_active_rocks(RockCount),
      ArrowCount =:= 0, RockCount =:= 0 ->
        test_pass('Combat - projectile system initialization')
    ;   test_fail('Combat - projectile system initialization', 'counts not zero')
    ),
    
    % Test 2: Spawn arrow
    ( combat:spawn_arrow(100, 200, 1, 0, ArrowID),
      ArrowID =:= 1 ->
        test_pass('Combat - spawn arrow')
    ;   test_fail('Combat - spawn arrow', 'wrong arrow ID')
    ),
    
    % Test 3: Get arrow position
    ( combat:get_arrow_position(1, AX, AY),
      AX =:= 100, AY =:= 200 ->
        test_pass('Combat - get arrow position')
    ;   test_fail('Combat - get arrow position', 'position incorrect')
    ),
    
    % Test 4: Update arrow position
    ( combat:update_arrow_position(1, 150, 200),
      combat:get_arrow_position(1, NewAX, _),
      NewAX =:= 150 ->
        test_pass('Combat - update arrow position')
    ;   test_fail('Combat - update arrow position', 'position not updated')
    ),
    
    % Test 5: Spawn rock
    ( combat:spawn_rock(300, 400, 5, -3, RockID),
      RockID =:= 2 ->  % Should be ID 2 (after arrow)
        test_pass('Combat - spawn rock')
    ;   test_fail('Combat - spawn rock', 'wrong rock ID')
    ),
    
    % Test 6: Count projectiles
    ( combat:count_active_arrows(AC),
      combat:count_active_rocks(RC),
      AC =:= 1, RC =:= 1 ->
        test_pass('Combat - count projectiles')
    ;   test_fail('Combat - count projectiles', 'wrong counts')
    ),
    
    % Test 7: Arrow hit detection
    ( combat:arrow_hit_wumpus(150, 200, 160, 205, 15) ->
        test_pass('Combat - arrow hit detection (within radius)')
    ;   test_fail('Combat - arrow hit detection', 'should hit')
    ),
    
    % Test 8: Remove arrow
    ( combat:remove_arrow(1),
      combat:count_active_arrows(FinalAC),
      FinalAC =:= 0 ->
        test_pass('Combat - remove arrow')
    ;   test_fail('Combat - remove arrow', 'not removed')
    ),
    
    % Test 9: Damage calculation
    ( combat:calculate_damage(20, 1.5, Damage),
      Damage =:= 30.0 ->
        test_pass('Combat - damage calculation')
    ;   test_fail('Combat - damage calculation', 'wrong damage value')
    ).

% =============================================================================
% SPAWNING MODULE TESTS
% =============================================================================

test_spawning :-
    format('~n=== Testing systems/spawning.pl ===~n'),
    
    % Test 1: Initialize spawn config
    ( spawning:init_spawn_config,
      spawning:get_spawn_config(wumpus_count, WC),
      WC =:= 3 ->
        test_pass('Spawning - spawn config initialization')
    ;   test_fail('Spawning - spawn config initialization', 'wrong config')
    ),
    
    % Test 2: Get spawn config values
    ( spawning:get_spawn_config(wumpus_min_distance, MD),
      spawning:get_spawn_config(chest_count, CC),
      MD =:= 800, CC =:= 3 ->
        test_pass('Spawning - get spawn config values')
    ;   test_fail('Spawning - get spawn config values', 'wrong values')
    ),
    
    % Test 3: Initialize Wumpus system
    ( spawning:init_wumpus_system,
      spawning:spawn_wumpus(100, 200, WID1),
      WID1 =:= 1 ->
        test_pass('Spawning - Wumpus system initialization')
    ;   test_fail('Spawning - Wumpus system initialization', 'wrong ID')
    ),
    
    % Test 4: Spawn multiple Wumpus
    ( spawning:spawn_wumpus(300, 400, WID2),
      spawning:spawn_wumpus(500, 600, WID3),
      WID2 =:= 2, WID3 =:= 3 ->
        test_pass('Spawning - spawn multiple Wumpus')
    ;   test_fail('Spawning - spawn multiple Wumpus', 'wrong IDs')
    ),
    
    % Test 5: Get spawned Wumpus
    ( spawning:get_spawned_wumpus(1, WX, WY),
      WX =:= 100, WY =:= 200 ->
        test_pass('Spawning - get spawned Wumpus position')
    ;   test_fail('Spawning - get spawned Wumpus position', 'wrong position')
    ),
    
    % Test 6: Get all spawned Wumpus
    ( spawning:get_all_spawned_wumpus(AllW),
      length(AllW, WCount),
      WCount =:= 3 ->
        test_pass('Spawning - get all spawned Wumpus')
    ;   test_fail('Spawning - get all spawned Wumpus', 'wrong count')
    ),
    
    % Test 7: Hazard detection
    ( map_system:init_map_system(1000, 800, 32, 32, 32),
      map_system:add_fall_zone(100, 100, 64, 64),
      spawning:is_near_hazard(110, 110, 20) ->
        test_pass('Spawning - hazard detection (near pit)')
    ;   test_fail('Spawning - hazard detection', 'should detect hazard')
    ).

% =============================================================================
% GAME STATE MODULE TESTS
% =============================================================================

test_game_state :-
    format('~n=== Testing systems/game_state.pl ===~n'),
    
    % Test 1: Initialize game state
    ( game_state:init_game_state,
      game_state:get_game_state(State),
      State = playing ->
        test_pass('Game state - initialization')
    ;   test_fail('Game state - initialization', 'wrong initial state')
    ),
    
    % Test 2: Time remaining
    ( game_state:get_time_remaining(Time),
      Time =:= 180.0 ->
        test_pass('Game state - initial time (180 seconds)')
    ;   test_fail('Game state - initial time', 'wrong time value')
    ),
    
    % Test 3: Set game state
    ( game_state:set_game_state(paused),
      game_state:get_game_state(NewState),
      NewState = paused ->
        test_pass('Game state - set state to paused')
    ;   test_fail('Game state - set state', 'state not changed')
    ),
    
    % Test 4: Update time
    ( game_state:set_game_state(playing),
      game_state:set_time_remaining(180.0),
      game_state:update_time(1.5),
      game_state:get_time_remaining(UpdatedTime),
      UpdatedTime =:= 178.5 ->
        test_pass('Game state - update time (delta)')
    ;   test_fail('Game state - update time', 'time not updated correctly')
    ),
    
    % Test 5: Time timeout triggers game over
    ( game_state:set_time_remaining(0.5),
      game_state:update_time(1.0),
      game_state:get_game_state(TimeoutState),
      TimeoutState = game_over_timeout ->
        test_pass('Game state - timeout triggers game_over')
    ;   test_fail('Game state - timeout', 'should trigger game_over_timeout')
    ),
    
    % Test 6: Legacy game_over flag
    ( game_state:set_game_over(true),
      game_state:is_game_over(GameOver),
      GameOver = true ->
        test_pass('Game state - legacy game_over flag')
    ;   test_fail('Game state - legacy game_over', 'flag not set')
    ),
    
    % Test 7: Victory state
    ( game_state:set_game_state(victory),
      game_state:get_game_state(VictoryState),
      VictoryState = victory ->
        test_pass('Game state - victory state')
    ;   test_fail('Game state - victory state', 'not set correctly')
    ).

% =============================================================================
% INTEGRATION TESTS
% =============================================================================

test_integration :-
    format('~n=== Integration Tests (Phase 3) ===~n'),
    
    % Test 1: Combat + Game state (arrow shot during active game)
    ( game_state:init_game_state,
      combat:init_projectile_system,
      combat:spawn_arrow(500, 500, 1, 0, AID),
      game_state:get_game_state(GS),
      GS = playing, AID =:= 1 ->
        test_pass('Integration - combat during active game')
    ;   test_fail('Integration - combat during active game', 'not working')
    ),
    
    % Test 2: Spawning + Combat (Wumpus spawns and can be hit by arrow)
    ( spawning:init_wumpus_system,
      spawning:spawn_wumpus(600, 600, WID),
      combat:spawn_arrow(590, 595, 1, 0, AID2),
      combat:arrow_hits_wumpus(AID2, 600, 600, 20),
      WID =:= 1 ->
        test_pass('Integration - arrow can hit spawned Wumpus')
    ;   test_fail('Integration - arrow hit Wumpus', 'detection failed')
    ),
    
    % Test 3: Full game flow (spawn → combat → time → game over)
    ( game_state:init_game_state,
      spawning:init_wumpus_system,
      combat:init_projectile_system,
      game_state:set_time_remaining(1.0),
      game_state:update_time(2.0),
      game_state:get_game_state(FinalState),
      FinalState = game_over_timeout ->
        test_pass('Integration - full game flow (timeout)')
    ;   test_fail('Integration - full game flow', 'flow broken')
    ),
    
    % Test 4: All Phase 3 modules working together
    ( game_state:init_game_state,
      spawning:init_spawn_config,
      combat:init_projectile_system,
      spawning:get_spawn_config(wumpus_count, Count),
      combat:count_active_arrows(Arrows),
      game_state:get_time_remaining(Time),
      Count =:= 3, Arrows =:= 0, Time =:= 180.0 ->
        test_pass('Integration - all Phase 3 systems initialized')
    ;   test_fail('Integration - all Phase 3 systems', 'initialization failed')
    ).

% =============================================================================
% MAIN TEST RUNNER
% =============================================================================

run_all_tests :-
    format('~n╔════════════════════════════════════════════╗~n'),
    format('║  PHASE 3 SYSTEM MODULES TEST SUITE        ║~n'),
    format('╚════════════════════════════════════════════╝~n'),
    test_combat,
    test_spawning,
    test_game_state,
    test_integration,
    format('~n╔════════════════════════════════════════════╗~n'),
    format('║  ALL PHASE 3 TESTS COMPLETED               ║~n'),
    format('╚════════════════════════════════════════════╝~n~n').

% Auto-run tests when file is consulted
:- initialization(run_all_tests).
