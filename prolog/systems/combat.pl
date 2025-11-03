% =============================================================================
% MODULE: systems/combat.pl
% PURPOSE: Combat system including projectiles, collision detection, and damage
% =============================================================================

:- module(combat, [
    % Projectile system initialization
    init_projectile_system/0,
    get_next_projectile_id/1,
    
    % Arrow projectile management
    spawn_arrow/5,
    update_arrow_position/3,
    remove_arrow/1,
    get_active_arrows/1,
    count_active_arrows/1,
    arrow_exists/1,
    get_arrow_position/3,
    
    % Rock projectile management
    spawn_rock/5,
    update_rock/5,
    remove_rock/1,
    get_active_rocks/1,
    count_active_rocks/1,
    rock_exists/1,
    get_rock_position/3,
    
    % Combat queries
    arrow_near_position/3,
    rock_near_position/3,
    nearest_arrow_to_position/4,
    
    % Collision detection
    arrow_hit_wumpus/5,
    arrow_hits_wumpus/4,
    wumpus_can_attack_player/6,
    calculate_damage/3
]).

:- use_module('../core/geometry.pl').

% =============================================================================
% PROJECTILE TRACKING SYSTEM
% =============================================================================

:- dynamic active_arrow/4.          % active_arrow(ID, X, Y, Direction)
:- dynamic active_rock/5.           % active_rock(ID, X, Y, VelX, VelY)
:- dynamic projectile_id_counter/1. % Counter for unique projectile IDs

% Initialize projectile tracking system
% init_projectile_system
init_projectile_system :-
    retractall(active_arrow(_, _, _, _)),
    retractall(active_rock(_, _, _, _, _)),
    retractall(projectile_id_counter(_)),
    assertz(projectile_id_counter(0)).

% Get next projectile ID
% get_next_projectile_id(ID)
get_next_projectile_id(ID) :-
    projectile_id_counter(CurrentID),
    ID is CurrentID + 1,
    retract(projectile_id_counter(CurrentID)),
    assertz(projectile_id_counter(ID)).

% =============================================================================
% ARROW PROJECTILE MANAGEMENT
% =============================================================================

% Register new arrow projectile
% spawn_arrow(X, Y, DirX, DirY, ArrowID)
spawn_arrow(X, Y, DirX, DirY, ArrowID) :-
    get_next_projectile_id(ArrowID),
    assertz(active_arrow(ArrowID, X, Y, (DirX, DirY))).

% Update arrow position
% update_arrow_position(ArrowID, NewX, NewY)
update_arrow_position(ArrowID, NewX, NewY) :-
    active_arrow(ArrowID, _, _, Dir),
    retract(active_arrow(ArrowID, _, _, Dir)),
    assertz(active_arrow(ArrowID, NewX, NewY, Dir)).

% Remove arrow (hit target or expired)
% remove_arrow(ArrowID)
remove_arrow(ArrowID) :-
    retract(active_arrow(ArrowID, _, _, _)).

% Get all active arrows
% get_active_arrows(Arrows)
get_active_arrows(Arrows) :-
    findall((ID, X, Y, Dir), active_arrow(ID, X, Y, Dir), Arrows).

% Count active arrows
% count_active_arrows(Count)
count_active_arrows(Count) :-
    findall(ID, active_arrow(ID, _, _, _), IDs),
    length(IDs, Count).

% Check if arrow exists
% arrow_exists(ArrowID)
arrow_exists(ArrowID) :-
    active_arrow(ArrowID, _, _, _).

% Get arrow position
% get_arrow_position(ArrowID, X, Y)
get_arrow_position(ArrowID, X, Y) :-
    active_arrow(ArrowID, X, Y, _).

% =============================================================================
% ROCK PROJECTILE MANAGEMENT
% =============================================================================

% Register new rock projectile
% spawn_rock(X, Y, VelX, VelY, RockID)
spawn_rock(X, Y, VelX, VelY, RockID) :-
    get_next_projectile_id(RockID),
    assertz(active_rock(RockID, X, Y, VelX, VelY)).

% Update rock position and velocity
% update_rock(RockID, NewX, NewY, NewVelX, NewVelY)
update_rock(RockID, NewX, NewY, NewVelX, NewVelY) :-
    retract(active_rock(RockID, _, _, _, _)),
    assertz(active_rock(RockID, NewX, NewY, NewVelX, NewVelY)).

% Remove rock (expired or stopped)
% remove_rock(RockID)
remove_rock(RockID) :-
    retract(active_rock(RockID, _, _, _, _)).

% Get all active rocks
% get_active_rocks(Rocks)
get_active_rocks(Rocks) :-
    findall((ID, X, Y, VelX, VelY), active_rock(ID, X, Y, VelX, VelY), Rocks).

% Count active rocks
% count_active_rocks(Count)
count_active_rocks(Count) :-
    findall(ID, active_rock(ID, _, _, _, _), IDs),
    length(IDs, Count).

% Check if rock exists
% rock_exists(RockID)
rock_exists(RockID) :-
    active_rock(RockID, _, _, _, _).

% Get rock position
% get_rock_position(RockID, X, Y)
get_rock_position(RockID, X, Y) :-
    active_rock(RockID, X, Y, _, _).

% =============================================================================
% COMBAT QUERIES
% =============================================================================

% Check if any arrow is near a position (for Wumpus awareness)
% arrow_near_position(X, Y, Radius)
arrow_near_position(X, Y, Radius) :-
    active_arrow(_, ArrowX, ArrowY, _),
    geometry:distance_squared(X, Y, ArrowX, ArrowY, DistSq),
    RadiusSq is Radius * Radius,
    DistSq =< RadiusSq.

% Check if any rock is near a position (for sound detection)
% rock_near_position(X, Y, Radius)
rock_near_position(X, Y, Radius) :-
    active_rock(_, RockX, RockY, _, _),
    geometry:distance_squared(X, Y, RockX, RockY, DistSq),
    RadiusSq is Radius * Radius,
    DistSq =< RadiusSq.

% Get nearest arrow to a position
% nearest_arrow_to_position(X, Y, ArrowID, Distance)
nearest_arrow_to_position(X, Y, ArrowID, Distance) :-
    findall(
        (Dist, ID),
        (
            active_arrow(ID, AX, AY, _),
            geometry:distance_squared(X, Y, AX, AY, DistSq),
            Dist is sqrt(DistSq)
        ),
        Pairs
    ),
    Pairs \= [],
    sort(Pairs, [(Distance, ArrowID)|_]).

% =============================================================================
% COLLISION DETECTION
% =============================================================================

% Check if arrow hits Wumpus (distance-based collision)
% arrow_hit_wumpus(ArrowX, ArrowY, WumpusX, WumpusY, HitRadius)
arrow_hit_wumpus(ArrowX, ArrowY, WumpusX, WumpusY, HitRadius) :-
    geometry:distance_squared(ArrowX, ArrowY, WumpusX, WumpusY, DistSq),
    HitRadiusSq is HitRadius * HitRadius,
    DistSq =< HitRadiusSq.

% Check if specific arrow hits any Wumpus position
% arrow_hits_wumpus(ArrowID, WumpusX, WumpusY, HitRadius)
arrow_hits_wumpus(ArrowID, WumpusX, WumpusY, HitRadius) :-
    active_arrow(ArrowID, ArrowX, ArrowY, _),
    arrow_hit_wumpus(ArrowX, ArrowY, WumpusX, WumpusY, HitRadius).

% Check if Wumpus can attack player (distance and state check)
% wumpus_can_attack_player(WumpusX, WumpusY, PlayerX, PlayerY, AttackRange, WumpusState)
wumpus_can_attack_player(WumpusX, WumpusY, PlayerX, PlayerY, AttackRange, WumpusState) :-
    % Must be in attack state or chasing
    (WumpusState = attack ; WumpusState = chase),
    % Check if within attack range
    geometry:distance_squared(WumpusX, WumpusY, PlayerX, PlayerY, DistSq),
    AttackRangeSq is AttackRange * AttackRange,
    DistSq =< AttackRangeSq.

% Calculate damage dealt (could add modifiers here)
% calculate_damage(BaseDamage, DamageModifier, ActualDamage)
calculate_damage(BaseDamage, DamageModifier, ActualDamage) :-
    ActualDamage is BaseDamage * DamageModifier.
