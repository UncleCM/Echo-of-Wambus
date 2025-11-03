% =============================================================================
% MODULE: entities/player.pl
% PURPOSE: Player state management, inventory, and actions
% =============================================================================

:- module(player, [
    % Player position
    update_player_position/2,
    get_player_position/2,
    
    % Inventory management
    init_player_inventory/0,
    get_player_inventory/2,
    use_arrow/0,
    use_rock/0,
    add_arrows/1,
    add_rocks/1,
    can_use_arrow/0,
    can_use_rock/0,
    
    % Health management
    get_player_health/1,
    damage_player/1,
    heal_player/1,
    set_player_health/1,
    
    % Item pickup
    can_pickup_arrow/5,
    can_pickup_rock/5,
    add_arrow_pickup/3,
    add_rock_pickup/3,
    remove_arrow_pickup/1,
    remove_rock_pickup/1,
    get_all_arrow_pickups/1,
    get_all_rock_pickups/1
]).

:- use_module('../core/geometry.pl').

% =============================================================================
% DYNAMIC FACTS
% =============================================================================

:- dynamic player_position/2.       % player_position(X, Y)
:- dynamic player_inventory/2.      % player_inventory(Arrows, Rocks)
:- dynamic player_max_inventory/2.  % player_max_inventory(MaxArrows, MaxRocks)
:- dynamic player_health/1.         % player_health(HP) - 0 to 100
:- dynamic arrow_pickup/3.          % arrow_pickup(ID, X, Y)
:- dynamic rock_pickup/3.           % rock_pickup(ID, X, Y)

% =============================================================================
% PLAYER POSITION
% =============================================================================

% Update player position
% update_player_position(X, Y)
update_player_position(X, Y) :-
    retractall(player_position(_, _)),
    asserta(player_position(X, Y)).

% Get current player position
% get_player_position(X, Y)
get_player_position(X, Y) :-
    player_position(X, Y).

% =============================================================================
% INVENTORY MANAGEMENT
% =============================================================================

% Initialize player inventory with default values
% Matches Settings.py: MAX_ARROWS=2, MAX_ROCKS=3
init_player_inventory :-
    retractall(player_inventory(_, _)),
    retractall(player_max_inventory(_, _)),
    asserta(player_inventory(2, 3)),      % Start: 2 arrows, 3 rocks
    asserta(player_max_inventory(2, 3)).  % Max: 2 arrows, 3 rocks

% Get current inventory
% get_player_inventory(Arrows, Rocks)
get_player_inventory(Arrows, Rocks) :-
    player_inventory(Arrows, Rocks).

% Use one arrow (returns true if had arrow, false if no arrows)
use_arrow :-
    player_inventory(Arrows, Rocks),
    Arrows > 0,
    NewArrows is Arrows - 1,
    retract(player_inventory(Arrows, Rocks)),
    asserta(player_inventory(NewArrows, Rocks)).

% Use one rock (returns true if had rock, false if no rocks)
use_rock :-
    player_inventory(Arrows, Rocks),
    Rocks > 0,
    NewRocks is Rocks - 1,
    retract(player_inventory(Arrows, Rocks)),
    asserta(player_inventory(Arrows, NewRocks)).

% Add arrows to inventory (respects max limit)
% add_arrows(Amount)
add_arrows(Amount) :-
    player_inventory(Arrows, Rocks),
    player_max_inventory(MaxArrows, _),
    NewArrows is min(MaxArrows, Arrows + Amount),
    retract(player_inventory(Arrows, Rocks)),
    asserta(player_inventory(NewArrows, Rocks)).

% Add rocks to inventory (respects max limit)
% add_rocks(Amount)
add_rocks(Amount) :-
    player_inventory(Arrows, Rocks),
    player_max_inventory(_, MaxRocks),
    NewRocks is min(MaxRocks, Rocks + Amount),
    retract(player_inventory(Arrows, Rocks)),
    asserta(player_inventory(Arrows, NewRocks)).

% Check if player can use arrow
can_use_arrow :-
    player_inventory(Arrows, _),
    Arrows > 0.

% Check if player can use rock
can_use_rock :-
    player_inventory(_, Rocks),
    Rocks > 0.

% =============================================================================
% HEALTH MANAGEMENT
% =============================================================================

% Get current player health (0-100)
% get_player_health(HP)
get_player_health(HP) :-
    (player_health(HP) -> true ; HP = 100).  % Default to 100 if not set

% Damage player by amount (clamps to 0)
% damage_player(Amount)
damage_player(Amount) :-
    (player_health(Current) -> true ; Current = 100),
    New is max(0, Current - Amount),
    retractall(player_health(_)),
    asserta(player_health(New)).

% Heal player by amount (clamps to 100)
% heal_player(Amount)
heal_player(Amount) :-
    (player_health(Current) -> true ; Current = 100),
    New is min(100, Current + Amount),
    retractall(player_health(_)),
    asserta(player_health(New)).

% Set player health directly (clamps 0-100)
% set_player_health(HP)
set_player_health(HP) :-
    retractall(player_health(_)),
    Clamped is max(0, min(100, HP)),
    asserta(player_health(Clamped)).

% =============================================================================
% ITEM PICKUP SYSTEM
% =============================================================================

% Add arrow pickup to world
% add_arrow_pickup(ID, X, Y)
add_arrow_pickup(ID, X, Y) :-
    assertz(arrow_pickup(ID, X, Y)).

% Add rock pickup to world
% add_rock_pickup(ID, X, Y)
add_rock_pickup(ID, X, Y) :-
    assertz(rock_pickup(ID, X, Y)).

% Check if player can pickup arrow (returns PickupID if successful)
% can_pickup_arrow(PlayerX, PlayerY, PlayerW, PlayerH, PickupID)
can_pickup_arrow(PlayerX, PlayerY, PlayerW, PlayerH, PickupID) :-
    arrow_pickup(PickupID, ItemX, ItemY),
    ItemW = 32,  % Pickup size
    ItemH = 32,
    geometry:rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, ItemX, ItemY, ItemW, ItemH).

% Check if player can pickup rock (returns PickupID if successful)
% can_pickup_rock(PlayerX, PlayerY, PlayerW, PlayerH, PickupID)
can_pickup_rock(PlayerX, PlayerY, PlayerW, PlayerH, PickupID) :-
    rock_pickup(PickupID, ItemX, ItemY),
    ItemW = 32,  % Pickup size
    ItemH = 32,
    geometry:rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, ItemX, ItemY, ItemW, ItemH).

% Remove arrow pickup after collection
% remove_arrow_pickup(PickupID)
remove_arrow_pickup(PickupID) :-
    retract(arrow_pickup(PickupID, _, _)).

% Remove rock pickup after collection
% remove_rock_pickup(PickupID)
remove_rock_pickup(PickupID) :-
    retract(rock_pickup(PickupID, _, _)).

% Get all arrow pickups (for Python to query)
% get_all_arrow_pickups(Pickups)
get_all_arrow_pickups(Pickups) :-
    findall([ID, X, Y], arrow_pickup(ID, X, Y), Pickups).

% Get all rock pickups (for Python to query)
% get_all_rock_pickups(Pickups)
get_all_rock_pickups(Pickups) :-
    findall([ID, X, Y], rock_pickup(ID, X, Y), Pickups).
