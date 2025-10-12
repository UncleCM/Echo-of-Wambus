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

% Check if player feet collide with any fall zone
check_fall(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight) :-
    FeetY is PlayerY + PlayerH - FeetHeight,
    fall_zone(FallX, FallY, FallW, FallH),
    rects_collide(PlayerX, FeetY, PlayerW, FeetHeight, FallX, FallY, FallW, FallH).

% Check if player collides with any collision box
check_collision(PlayerX, PlayerY, PlayerW, PlayerH) :-
    collision_box(CollX, CollY, CollW, CollH),
    rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, CollX, CollY, CollW, CollH).

% Determine if movement is valid (no collision)
can_move(NewX, NewY, PlayerW, PlayerH) :-
    \+ check_collision(NewX, NewY, PlayerW, PlayerH).

% Check if player should fall
should_fall(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight) :-
    check_fall(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight),
    !.

% Update player position if valid
update_player_position(NewX, NewY) :-
    retractall(player_position(_, _)),
    asserta(player_position(NewX, NewY)).

% Set game over state
set_game_over(State) :-
    retractall(game_over(_)),
    asserta(game_over(State)).

% Get game over state
is_game_over(State) :-
    game_over(State).

% Add collision box to knowledge base
add_collision_box(X, Y, W, H) :-
    asserta(collision_box(X, Y, W, H)).

% Add fall zone to knowledge base
add_fall_zone(X, Y, W, H) :-
    asserta(fall_zone(X, Y, W, H)).

% Clear all collision boxes
clear_collision_boxes :-
    retractall(collision_box(_, _, _, _)).

% Clear all fall zones
clear_fall_zones :-
    retractall(fall_zone(_, _, _, _)).

% Query: Can player move to specific direction?
validate_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, NewX, NewY) :-
    NewX is CurrentX + DeltaX,
    NewY is CurrentY + DeltaY,
    can_move(NewX, NewY, PlayerW, PlayerH).

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

% Find safe spawn position near entrance
find_safe_spawn(EntranceX, EntranceY, SafeX, SafeY, PlayerW, PlayerH, FeetHeight) :-
    % Try entrance position
    is_safe_position(EntranceX, EntranceY, PlayerW, PlayerH, FeetHeight),
    SafeX = EntranceX,
    SafeY = EntranceY, !.

find_safe_spawn(EntranceX, EntranceY, SafeX, SafeY, PlayerW, PlayerH, FeetHeight) :-
    % Try positions around entrance
    Offset = 50,
    between(-2, 2, DX),
    between(-2, 2, DY),
    TestX is EntranceX + (DX * Offset),
    TestY is EntranceY + (DY * Offset),
    is_safe_position(TestX, TestY, PlayerW, PlayerH, FeetHeight),
    SafeX = TestX,
    SafeY = TestY, !.