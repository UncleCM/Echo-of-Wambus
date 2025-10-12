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