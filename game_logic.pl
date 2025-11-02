% game_logic.pl - Prolog rules for Echo of Wumpus game

% Facts about game state
:- dynamic player_position/2.
:- dynamic game_over/1.
:- dynamic collision_box/4.
:- dynamic fall_zone/4.
:- dynamic water_zone/4.

% Initialize game state
init_game :-
    retractall(player_position(_, _)),
    retractall(game_over(_)),
    retractall(collision_box(_, _, _, _)),
    retractall(fall_zone(_, _, _, _)),
    retractall(water_zone(_, _, _, _)),
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

% Check if player is standing in any water zone
check_water(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight) :-
    FeetY is PlayerY + PlayerH - FeetHeight,
    water_zone(WaterX, WaterY, WaterW, WaterH),
    rects_collide(PlayerX, FeetY, PlayerW, FeetHeight, WaterX, WaterY, WaterW, WaterH).

% Predicates for managing dynamic facts
add_collision_box(X, Y, W, H) :-
    assertz(collision_box(X, Y, W, H)).

add_fall_zone(X, Y, W, H) :-
    assertz(fall_zone(X, Y, W, H)).

add_water_zone(X, Y, W, H) :-
    assertz(water_zone(X, Y, W, H)).

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

% Clear all water zones
clear_water_zones :-
    retractall(water_zone(_, _, _, _)).

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