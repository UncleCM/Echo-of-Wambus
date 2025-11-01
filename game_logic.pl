% game_logic.pl - Prolog rules for Echo of Wumpus game

% Facts about game state
:- dynamic player_position/2.
:- dynamic game_over/1.
:- dynamic collision_box/4.
:- dynamic fall_zone/4.

% Initialize game state
init_game :-
    retractall(player_position(_, _)),
    retractall(game_over(_)),
    retractall(collision_box(_, _, _, _)),
    retractall(fall_zone(_, _, _, _)),
    asserta(game_over(false)).

% Check if a point is inside a rectangle
point_in_rect(X, Y, RectX, RectY, RectW, RectH) :-
    X >= RectX,
    X =< RectX + RectW,
    Y >= RectY,
    Y =< RectY + RectH.

% Check if two rectangles collide
rects_collide(X1, Y1, W1, H1, X2, Y2, W2, H2) :-
    X1 < X2 + W2,
    X1 + W1 > X2,
    Y1 < Y2 + H2,
    Y1 + H1 > Y2.

% Check if position collides with any collision box
check_collision(PlayerX, PlayerY, PlayerW, PlayerH) :-
    collision_box(BoxX, BoxY, BoxW, BoxH),
    rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, BoxX, BoxY, BoxW, BoxH).

% Check if player feet collide with any fall zone
check_fall(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight) :-
    FeetY is PlayerY + PlayerH - FeetHeight,
    fall_zone(FallX, FallY, FallW, FallH),
    rects_collide(PlayerX, FeetY, PlayerW, FeetHeight, FallX, FallY, FallW, FallH).

% Predicates for managing dynamic facts
add_collision_box(X, Y, W, H) :-
    assertz(collision_box(X, Y, W, H)).

add_fall_zone(X, Y, W, H) :-
    assertz(fall_zone(X, Y, W, H)).

update_player_position(X, Y) :-
    retractall(player_position(_, _)),
    asserta(player_position(X, Y)).

player_position(X, Y) :-
    player_position(X, Y).

set_game_over(State) :-
    retractall(game_over(_)),
    asserta(game_over(State)).

is_game_over(State) :-
    game_over(State).

% Clear all collision boxes
clear_collision_boxes :-
    retractall(collision_box(_, _, _, _)).

% Clear all fall zones
clear_fall_zones :-
    retractall(fall_zone(_, _, _, _)).

% Query: Can player move to specific direction? (Kept for compatibility, but deprecated by resolve_movement)
validate_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, NewX, NewY) :-
    NewX is CurrentX + DeltaX,
    NewY is CurrentY + DeltaY,
    \+ check_collision(NewX, NewY, PlayerW, PlayerH). % can_move renamed to \+ check_collision

% Count total hazards in game
count_hazards(CollisionCount, FallCount) :-
    findall(1, collision_box(_, _, _, _), CollList),
    length(CollList, CollisionCount),
    findall(1, fall_zone(_, _, _, _), FallList),
    length(FallList, FallCount).

% Check if position is safe (no collision and no fall)
is_safe_position(X, Y, W, H, FeetHeight) :-
    \+ check_collision(X, Y, W, H),
    \+ check_fall(X, Y, W, H, FeetHeight).

% Find safe spawn position near entrance (simple proximity search)
find_safe_spawn(EntranceX, EntranceY, SafeX, SafeY, PlayerW, PlayerH, FeetHeight) :-
    % Try entrance position
    ( is_safe_position(EntranceX, EntranceY, PlayerW, PlayerH, FeetHeight) ->
        SafeX = EntranceX,
        SafeY = EntranceY
    ;
    % Fallback: Try a small offset from entrance (e.g., 50 pixels down)
      OffsetY is EntranceY + 50,
      ( is_safe_position(EntranceX, OffsetY, PlayerW, PlayerH, FeetHeight) ->
          SafeX = EntranceX,
          SafeY = OffsetY
      ;
      % Final Fallback: Use current player position (if it exists) or fail
          player_position(SafeX, SafeY)
      )
    ).

% =========================================================================
% NEW LOGIC: MOVEMENT RESOLUTION
% =========================================================================

% resolve_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, FinalX, FinalY)
% Determines the final position after attempting a move, resolving collisions separately for X and Y.
% This handles the core collision logic previously in player.py.
resolve_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, FinalX, FinalY) :-
    % --- Resolve X movement ---
    AttemptX is CurrentX + DeltaX,
    ( check_collision(AttemptX, CurrentY, PlayerW, PlayerH) ->
        % Collision detected on X-axis, revert X change
        ResolvedX = CurrentX
    ;
        % No X collision
        ResolvedX = AttemptX
    ),

    % --- Resolve Y movement ---
    AttemptY is CurrentY + DeltaY,
    % Check collision at the new Y position, using the Resolved X
    ( check_collision(ResolvedX, AttemptY, PlayerW, PlayerH) ->
        % Collision detected on Y-axis, revert Y change
        ResolvedY = CurrentY
    ;
        % No Y collision
        ResolvedY = AttemptY
    ),

    FinalX = ResolvedX,
    FinalY = ResolvedY,
    % Update the game state with the new resolved position
    update_player_position(FinalX, FinalY).

% ============================================================================
% WUMPUS AI LOGIC
% ============================================================================

% Dynamic facts for Wumpus state
:- dynamic wumpus_position/2.
:- dynamic wumpus_state/1.

% Initialize Wumpus state
init_wumpus(X, Y) :-
    retractall(wumpus_position(_, _)),
    retractall(wumpus_state(_)),
    assertz(wumpus_position(X, Y)),
    assertz(wumpus_state(patrol)).

% Update Wumpus position
update_wumpus_position(X, Y) :-
    retractall(wumpus_position(_, _)),
    assertz(wumpus_position(X, Y)).

% Update Wumpus state
update_wumpus_state(NewState) :-
    retractall(wumpus_state(_)),
    assertz(wumpus_state(NewState)).

% Calculate distance between two points (squared to avoid sqrt)
distance_squared(X1, Y1, X2, Y2, DistSq) :-
    DX is X2 - X1,
    DY is Y2 - Y1,
    DistSq is DX * DX + DY * DY.

% AI Parameters
detection_range_squared(90000).  % 300 * 300 = 90000
attack_range_squared(2500).      % 50 * 50 = 2500

% Check if player is in detection range
in_detection_range(WumpusX, WumpusY, PlayerX, PlayerY) :-
    distance_squared(WumpusX, WumpusY, PlayerX, PlayerY, DistSq),
    detection_range_squared(MaxDistSq),
    DistSq =< MaxDistSq.

% Check if player is in attack range
in_attack_range(WumpusX, WumpusY, PlayerX, PlayerY) :-
    distance_squared(WumpusX, WumpusY, PlayerX, PlayerY, DistSq),
    attack_range_squared(MaxDistSq),
    DistSq =< MaxDistSq.

% Wumpus AI Decision Making
% wumpus_decision(WumpusX, WumpusY, PlayerX, PlayerY, CurrentState, NewState, DirectionX, DirectionY)

% PATROL STATE: Check if should transition to chase
wumpus_decision(WX, WY, PX, PY, patrol, chase, DX, DY) :-
    in_detection_range(WX, WY, PX, PY),
    !,  % Cut to prevent backtracking
    calculate_direction(WX, WY, PX, PY, DX, DY).

% PATROL STATE: Stay in patrol (return no direction change)
wumpus_decision(_WX, _WY, _PX, _PY, patrol, patrol, 0, 0).

% CHASE STATE: Check if should transition to attack
wumpus_decision(WX, WY, PX, PY, chase, attack, 0, 0) :-
    in_attack_range(WX, WY, PX, PY),
    !.

% CHASE STATE: Check if player left detection range (return to patrol)
wumpus_decision(WX, WY, PX, PY, chase, patrol, 0, 0) :-
    \+ in_detection_range(WX, WY, PX, PY),
    !.

% CHASE STATE: Continue chasing
wumpus_decision(WX, WY, PX, PY, chase, chase, DX, DY) :-
    in_detection_range(WX, WY, PX, PY),
    calculate_direction(WX, WY, PX, PY, DX, DY).

% ATTACK STATE: Check if player moved out of attack range
wumpus_decision(WX, WY, PX, PY, attack, chase, DX, DY) :-
    \+ in_attack_range(WX, WY, PX, PY),
    in_detection_range(WX, WY, PX, PY),
    !,
    calculate_direction(WX, WY, PX, PY, DX, DY).

% ATTACK STATE: Player left detection range entirely
wumpus_decision(WX, WY, PX, PY, attack, patrol, 0, 0) :-
    \+ in_detection_range(WX, WY, PX, PY),
    !.

% ATTACK STATE: Stay attacking
wumpus_decision(_WX, _WY, _PX, _PY, attack, attack, 0, 0).

% DEAD STATE: Always stay dead
wumpus_decision(_WX, _WY, _PX, _PY, dead, dead, 0, 0).

% Calculate normalized direction vector from Wumpus to Player
% Returns direction as floats between -1.0 and 1.0
calculate_direction(WX, WY, PX, PY, DX, DY) :-
    DiffX is PX - WX,
    DiffY is PY - WY,
    % Calculate magnitude
    MagSquared is DiffX * DiffX + DiffY * DiffY,
    (MagSquared > 0 ->
        Mag is sqrt(MagSquared),
        DX is DiffX / Mag,
        DY is DiffY / Mag
    ;
        % Same position - no direction
        DX = 0,
        DY = 0
    ).

% ============================================================================
% TREASURE & MIMIC SYSTEM
% ============================================================================

:- dynamic treasure/3.        % treasure(ID, X, Y)
:- dynamic mimic/4.            % mimic(ID, X, Y, Activated)
:- dynamic treasure_collected/1.
:- dynamic next_wumpus_id/1.

% Setup 3 chests - 1 real treasure, 2 mimics (randomly assigned)
setup_treasure_system(X1, Y1, X2, Y2, X3, Y3) :-
    % Clear existing treasure/mimic data
    retractall(treasure(_, _, _)),
    retractall(mimic(_, _, _, _)),
    retractall(treasure_collected(_)),
    
    % Randomly assign which chest is the real treasure (1, 2, or 3)
    random_between(1, 3, TreasureChestID),
    
    % Create chests based on random assignment
    setup_chest(1, X1, Y1, TreasureChestID),
    setup_chest(2, X2, Y2, TreasureChestID),
    setup_chest(3, X3, Y3, TreasureChestID),
    
    asserta(treasure_collected(false)).

% Helper to create either treasure or mimic
setup_chest(ID, X, Y, TreasureID) :-
    (ID =:= TreasureID ->
        assertz(treasure(ID, X, Y))
    ;
        assertz(mimic(ID, X, Y, false))
    ).

% Open chest near player position (within 50 pixels)
open_chest(PlayerX, PlayerY, Result) :-
    % Try to find a treasure chest
    (treasure(_ID, ChestX, ChestY),
     distance_to_chest(PlayerX, PlayerY, ChestX, ChestY, Dist),
     Dist =< 50 ->
        Result = treasure_found
    ;
    % Try to find a mimic chest
     mimic(ID, ChestX, ChestY, Activated),
     distance_to_chest(PlayerX, PlayerY, ChestX, ChestY, Dist),
     Dist =< 50 ->
        (Activated = false ->
            % Activate the mimic
            retract(mimic(ID, ChestX, ChestY, false)),
            assertz(mimic(ID, ChestX, ChestY, true)),
            Result = mimic_activated
        ;
            Result = already_opened
        )
    ;
        Result = no_chest
    ).

% Calculate distance to chest
distance_to_chest(PlayerX, PlayerY, ChestX, ChestY, Distance) :-
    DX is ChestX - PlayerX,
    DY is ChestY - PlayerY,
    Distance is sqrt(DX * DX + DY * DY).

% Mark treasure as collected
collect_treasure :-
    retractall(treasure_collected(_)),
    assertz(treasure_collected(true)).

% ============================================================================
% WUMPUS SPAWNING SYSTEM
% ============================================================================

:- dynamic spawned_wumpus/3.  % spawned_wumpus(ID, X, Y)

% Initialize Wumpus ID counter
init_wumpus_system :-
    retractall(next_wumpus_id(_)),
    assertz(next_wumpus_id(1)).

% Spawn a new Wumpus and return its ID
spawn_wumpus(X, Y, WumpusID) :-
    % Get next ID
    (next_wumpus_id(ID) ->
        WumpusID = ID
    ;
        % If counter doesn't exist, initialize it
        init_wumpus_system,
        WumpusID = 1
    ),
    
    % Store the spawned Wumpus
    assertz(spawned_wumpus(WumpusID, X, Y)),
    
    % Increment counter
    NextID is WumpusID + 1,
    retractall(next_wumpus_id(_)),
    assertz(next_wumpus_id(NextID)).

% ============================================================================
% COMBAT SYSTEM
% ============================================================================

% Check if arrow hits Wumpus (distance-based collision)
arrow_hit_wumpus(ArrowX, ArrowY, WumpusX, WumpusY, HitRadius) :-
    distance_squared(ArrowX, ArrowY, WumpusX, WumpusY, DistSq),
    HitRadiusSq is HitRadius * HitRadius,
    DistSq =< HitRadiusSq.

% Check if Wumpus can attack player (distance and state check)
wumpus_can_attack_player(WumpusX, WumpusY, PlayerX, PlayerY, AttackRange, WumpusState) :-
    % Must be in attack state or chasing
    (WumpusState = attack ; WumpusState = chase),
    % Check if within attack range
    distance_squared(WumpusX, WumpusY, PlayerX, PlayerY, DistSq),
    AttackRangeSq is AttackRange * AttackRange,
    DistSq =< AttackRangeSq.

% Calculate damage dealt (could add modifiers here)
calculate_damage(BaseDamage, DamageModifier, ActualDamage) :-
    ActualDamage is BaseDamage * DamageModifier.

% ============================================================================
% VICTORY & EXIT SYSTEM
% ============================================================================

:- dynamic exit_position/2.
:- dynamic exit_unlocked/1.

% Initialize exit system
init_exit(ExitX, ExitY) :-
    retractall(exit_position(_, _)),
    retractall(exit_unlocked(_)),
    assertz(exit_position(ExitX, ExitY)),
    assertz(exit_unlocked(false)).

% Unlock the exit (after collecting treasure)
unlock_exit :-
    retractall(exit_unlocked(_)),
    assertz(exit_unlocked(true)).

% Check if player can exit the game (has treasure, exit unlocked, at exit position)
can_exit_game(PlayerX, PlayerY, PlayerW, PlayerH) :-
    % Must have collected treasure
    treasure_collected(true),
    % Exit must be unlocked
    exit_unlocked(true),
    % Must be at exit position (within collision range)
    exit_position(ExitX, ExitY),
    ExitW = 48,  % Exit portal size
    ExitH = 48,
    rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, ExitX, ExitY, ExitW, ExitH).

% ============================================================================
% ITEM PICKUP SYSTEM
% ============================================================================

:- dynamic arrow_pickup/3.    % arrow_pickup(ID, X, Y)
:- dynamic rock_pickup/3.      % rock_pickup(ID, X, Y)

% Add arrow pickup to world
add_arrow_pickup(ID, X, Y) :-
    assertz(arrow_pickup(ID, X, Y)).

% Add rock pickup to world
add_rock_pickup(ID, X, Y) :-
    assertz(rock_pickup(ID, X, Y)).

% Check if player can pickup arrow (returns PickupID if successful)
can_pickup_arrow(PlayerX, PlayerY, PlayerW, PlayerH, PickupID) :-
    arrow_pickup(PickupID, ItemX, ItemY),
    ItemW = 32,  % Pickup size
    ItemH = 32,
    rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, ItemX, ItemY, ItemW, ItemH).

% Check if player can pickup rock (returns PickupID if successful)
can_pickup_rock(PlayerX, PlayerY, PlayerW, PlayerH, PickupID) :-
    rock_pickup(PickupID, ItemX, ItemY),
    ItemW = 32,  % Pickup size
    ItemH = 32,
    rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, ItemX, ItemY, ItemW, ItemH).

% Remove arrow pickup after collection
remove_arrow_pickup(PickupID) :-
    retract(arrow_pickup(PickupID, _, _)).

% Remove rock pickup after collection
remove_rock_pickup(PickupID) :-
    retract(rock_pickup(PickupID, _, _)).

% Get all arrow pickups (for Python to query)
get_all_arrow_pickups(Pickups) :-
    findall([ID, X, Y], arrow_pickup(ID, X, Y), Pickups).

% Get all rock pickups (for Python to query)
get_all_rock_pickups(Pickups) :-
    findall([ID, X, Y], rock_pickup(ID, X, Y), Pickups).

% ============================================================================
% HELPER PREDICATES FOR GAME LOGIC
% ============================================================================

% Check if player is near chest (for interaction prompt)
near_chest(PlayerX, PlayerY, InteractionRadius) :-
    (treasure(_ID, ChestX, ChestY) ; mimic(_ID, ChestX, ChestY, _)),
    distance_to_chest(PlayerX, PlayerY, ChestX, ChestY, Distance),
    Distance =< InteractionRadius.