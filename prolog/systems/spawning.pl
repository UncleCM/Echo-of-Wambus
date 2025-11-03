% =============================================================================
% MODULE: systems/spawning.pl
% PURPOSE: Entity spawning system including configuration and position generation
% =============================================================================

:- module(spawning, [
    % Spawn configuration
    init_spawn_config/0,
    get_spawn_config/2,
    
    % Wumpus spawning
    init_wumpus_system/0,
    spawn_wumpus/3,
    get_spawned_wumpus/3,
    get_all_spawned_wumpus/1,
    clear_spawned_wumpus/0,
    
    % Position generation
    generate_wumpus_spawns/3,
    generate_pickup_spawns/2,
    
    % Helper predicates
    get_safe_position_far_from_list/4,
    is_near_hazard/3
]).

:- use_module('../core/geometry.pl').
:- use_module('../core/movement.pl').
:- use_module('../core/map_system.pl').

% =============================================================================
% SPAWN CONFIGURATION
% =============================================================================

:- dynamic spawn_config/2.

% Initialize spawn config (call at game start)
% init_spawn_config
init_spawn_config :-
    retractall(spawn_config(_, _)),
    assertz(spawn_config(wumpus_count, 3)),           % 3-4 Wumpus
    assertz(spawn_config(wumpus_min_distance, 800)),  % Min distance between Wumpus
    assertz(spawn_config(chest_count, 3)),             % 1 real + 2 mimics
    assertz(spawn_config(arrow_pickup_count, 3)),      % 3 arrow pickups
    assertz(spawn_config(rock_pickup_count, 5)).       % 5 rock pickups

% Get spawn configuration
% get_spawn_config(Key, Value)
get_spawn_config(Key, Value) :-
    spawn_config(Key, Value), !.
% Default fallbacks
get_spawn_config(wumpus_count, 3).
get_spawn_config(wumpus_min_distance, 800).
get_spawn_config(chest_count, 3).
get_spawn_config(arrow_pickup_count, 3).
get_spawn_config(rock_pickup_count, 5).

% =============================================================================
% WUMPUS SPAWNING SYSTEM
% =============================================================================

:- dynamic spawned_wumpus/3.      % spawned_wumpus(ID, X, Y)
:- dynamic next_wumpus_id/1.      % Counter for Wumpus IDs

% Initialize Wumpus ID counter
% init_wumpus_system
init_wumpus_system :-
    retractall(next_wumpus_id(_)),
    retractall(spawned_wumpus(_, _, _)),
    assertz(next_wumpus_id(1)).

% Spawn a new Wumpus and return its ID
% spawn_wumpus(X, Y, WumpusID)
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

% Get spawned Wumpus position
% get_spawned_wumpus(WumpusID, X, Y)
get_spawned_wumpus(WumpusID, X, Y) :-
    spawned_wumpus(WumpusID, X, Y).

% Get all spawned Wumpus
% get_all_spawned_wumpus(List)
get_all_spawned_wumpus(List) :-
    findall((ID, X, Y), spawned_wumpus(ID, X, Y), List).

% Clear all spawned Wumpus
% clear_spawned_wumpus
clear_spawned_wumpus :-
    retractall(spawned_wumpus(_, _, _)).

% =============================================================================
% POSITION GENERATION
% =============================================================================

% Generate Wumpus spawn positions (far apart from each other)
% generate_wumpus_spawns(Count, MinDistance, Positions)
% Returns list of (X, Y) positions
generate_wumpus_spawns(Count, MinDistance, Positions) :-
    get_spawn_config(wumpus_count, MaxCount),
    ActualCount is min(Count, MaxCount),
    generate_wumpus_spawns_helper(ActualCount, MinDistance, [], Positions).

generate_wumpus_spawns_helper(0, _, Acc, Acc) :- !.
generate_wumpus_spawns_helper(N, MinDist, Acc, Result) :-
    N > 0,
    % Get safe position far from already spawned
    get_safe_position_far_from_list(Acc, MinDist, X, Y),
    N1 is N - 1,
    generate_wumpus_spawns_helper(N1, MinDist, [(X, Y)|Acc], Result).

% Helper: get safe position far from list of positions
% get_safe_position_far_from_list(ExistingPositions, MinDist, X, Y)
get_safe_position_far_from_list([], _, X, Y) :-
    % No existing positions, just get any Wumpus-safe position
    map_system:random_wumpus_safe_position(X, Y), !.
get_safe_position_far_from_list(ExistingPositions, MinDist, X, Y) :-
    % Try with requested MinDist first
    try_get_position_with_distance(ExistingPositions, MinDist, X, Y), !.
get_safe_position_far_from_list(ExistingPositions, MinDist, X, Y) :-
    % Fallback: try with reduced distance (75% of original)
    ReducedDist is MinDist * 0.75,
    ReducedDist > 100,  % Don't reduce below 100px
    try_get_position_with_distance(ExistingPositions, ReducedDist, X, Y), !.
get_safe_position_far_from_list(ExistingPositions, _, X, Y) :-
    % Last resort: try with minimum distance 100px
    try_get_position_with_distance(ExistingPositions, 100, X, Y), !.
get_safe_position_far_from_list(_, _, X, Y) :-
    % Absolute fallback: just get any safe position (no distance constraint)
    map_system:random_wumpus_safe_position(X, Y).

% Helper: try to find position with specific distance
% try_get_position_with_distance(ExistingPositions, MinDist, X, Y)
try_get_position_with_distance(ExistingPositions, MinDist, X, Y) :-
    between(1, 100, _),
    map_system:random_wumpus_safe_position(X, Y),
    \+ too_close_to_any(X, Y, ExistingPositions, MinDist).

% Check if position is too close to any in list
% too_close_to_any(X, Y, Positions, MinDist)
too_close_to_any(X, Y, [(EX, EY)|_], MinDist) :-
    DX is X - EX, 
    DY is Y - EY,
    DistSq is DX * DX + DY * DY,
    MinDistSq is MinDist * MinDist,
    DistSq < MinDistSq, !.
too_close_to_any(X, Y, [_|Rest], MinDist) :-
    too_close_to_any(X, Y, Rest, MinDist).

% Generate pickup spawn positions (arrows/rocks)
% generate_pickup_spawns(Count, Positions)
generate_pickup_spawns(Count, Positions) :-
    generate_pickup_spawns_helper(Count, [], Positions).

generate_pickup_spawns_helper(0, Acc, Acc) :- !.
generate_pickup_spawns_helper(N, Acc, Result) :-
    N > 0,
    map_system:random_safe_position(X, Y),
    % Ensure not near pits
    \+ is_near_hazard(X, Y, 50),
    N1 is N - 1,
    generate_pickup_spawns_helper(N1, [(X, Y)|Acc], Result).

% Check if position is near any hazard (pit/water)
% is_near_hazard(X, Y, Radius)
is_near_hazard(X, Y, Radius) :-
    movement:fall_zone(FX, FY, FW, FH),
    X >= FX - Radius, 
    X =< FX + FW + Radius,
    Y >= FY - Radius, 
    Y =< FY + FH + Radius, !.
is_near_hazard(X, Y, Radius) :-
    movement:water_zone(WX, WY, WW, WH),
    X >= WX - Radius,
    X =< WX + WW + Radius,
    Y >= WY - Radius,
    Y =< WY + WH + Radius, !.
