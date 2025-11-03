% =============================================================================
% MODULE: core/movement.pl
% PURPOSE: Movement resolution and collision handling for all entities
% =============================================================================

:- module(movement, [
    resolve_movement/8,
    validate_movement/8,
    check_collision/4,
    check_fall/5,
    check_water/5,
    is_safe_position/5,
    find_safe_spawn/7,
    add_collision_box/4,
    add_fall_zone/4,
    add_water_zone/4,
    clear_collision_boxes/0,
    clear_fall_zones/0,
    clear_water_zones/0,
    % Dynamic facts (exported for external access)
    fall_zone/4,
    water_zone/4,
    collision_box/4
]).

:- use_module('geometry.pl').

% =============================================================================
% DYNAMIC FACTS
% =============================================================================

:- dynamic collision_box/4.  % collision_box(X, Y, W, H)
:- dynamic fall_zone/4.       % fall_zone(X, Y, W, H)
:- dynamic water_zone/4.      % water_zone(X, Y, W, H)

% =============================================================================
% COLLISION DETECTION
% =============================================================================

% Check if position collides with any collision box (walls, obstacles)
% check_collision(PlayerX, PlayerY, PlayerW, PlayerH)
check_collision(PlayerX, PlayerY, PlayerW, PlayerH) :-
    collision_box(BoxX, BoxY, BoxW, BoxH),
    geometry:rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, BoxX, BoxY, BoxW, BoxH).

% Check if player feet collide with any fall zone (pits)
% check_fall(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight)
check_fall(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight) :-
    FeetY is PlayerY + PlayerH - FeetHeight,
    fall_zone(FallX, FallY, FallW, FallH),
    geometry:rects_collide(PlayerX, FeetY, PlayerW, FeetHeight, FallX, FallY, FallW, FallH).

% Check if player is standing in any water zone (slows movement)
% check_water(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight)
check_water(PlayerX, PlayerY, PlayerW, PlayerH, FeetHeight) :-
    FeetY is PlayerY + PlayerH - FeetHeight,
    water_zone(WaterX, WaterY, WaterW, WaterH),
    geometry:rects_collide(PlayerX, FeetY, PlayerW, FeetHeight, WaterX, WaterY, WaterW, WaterH).

% =============================================================================
% MOVEMENT RESOLUTION
% =============================================================================

% Main movement resolution - handles collisions separately for X and Y axes
% This prevents "sticking" to walls when moving diagonally
% resolve_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, FinalX, FinalY)
resolve_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, FinalX, FinalY) :-
    % --- Resolve X movement first ---
    AttemptX is CurrentX + DeltaX,
    ( check_collision(AttemptX, CurrentY, PlayerW, PlayerH) ->
        % Collision detected on X-axis, revert X change
        ResolvedX = CurrentX
    ;
        % No X collision, accept new X position
        ResolvedX = AttemptX
    ),

    % --- Then resolve Y movement ---
    AttemptY is CurrentY + DeltaY,
    % Check collision at new Y position using the resolved X
    ( check_collision(ResolvedX, AttemptY, PlayerW, PlayerH) ->
        % Collision detected on Y-axis, revert Y change
        ResolvedY = CurrentY
    ;
        % No Y collision, accept new Y position
        ResolvedY = AttemptY
    ),

    FinalX = ResolvedX,
    FinalY = ResolvedY.

% Legacy movement validation (kept for compatibility)
% validate_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, NewX, NewY)
validate_movement(CurrentX, CurrentY, DeltaX, DeltaY, PlayerW, PlayerH, NewX, NewY) :-
    NewX is CurrentX + DeltaX,
    NewY is CurrentY + DeltaY,
    \+ check_collision(NewX, NewY, PlayerW, PlayerH).

% =============================================================================
% SPAWN POSITION UTILITIES
% =============================================================================

% Check if position is safe (no collision and no fall)
% is_safe_position(X, Y, W, H, FeetHeight)
is_safe_position(X, Y, W, H, FeetHeight) :-
    \+ check_collision(X, Y, W, H),
    \+ check_fall(X, Y, W, H, FeetHeight).

% Find safe spawn position near entrance
% find_safe_spawn(EntranceX, EntranceY, SafeX, SafeY, PlayerW, PlayerH, FeetHeight)
find_safe_spawn(EntranceX, EntranceY, SafeX, SafeY, PlayerW, PlayerH, FeetHeight) :-
    % Try entrance position first
    ( is_safe_position(EntranceX, EntranceY, PlayerW, PlayerH, FeetHeight) ->
        SafeX = EntranceX,
        SafeY = EntranceY
    ;
    % Fallback: Try offset from entrance (50 pixels down)
      OffsetY is EntranceY + 50,
      ( is_safe_position(EntranceX, OffsetY, PlayerW, PlayerH, FeetHeight) ->
          SafeX = EntranceX,
          SafeY = OffsetY
      ;
      % Final fallback: Use default position
          SafeX = EntranceX,
          SafeY = EntranceY
      )
    ).

% =============================================================================
% FACT MANAGEMENT
% =============================================================================

% Add collision box (wall, obstacle)
add_collision_box(X, Y, W, H) :-
    assertz(collision_box(X, Y, W, H)).

% Add fall zone (pit, hole)
add_fall_zone(X, Y, W, H) :-
    assertz(fall_zone(X, Y, W, H)).

% Add water zone (slowing area)
add_water_zone(X, Y, W, H) :-
    assertz(water_zone(X, Y, W, H)).

% Clear all collision boxes
clear_collision_boxes :-
    retractall(collision_box(_, _, _, _)).

% Clear all fall zones
clear_fall_zones :-
    retractall(fall_zone(_, _, _, _)).

% Clear all water zones
clear_water_zones :-
    retractall(water_zone(_, _, _, _)).
