% =============================================================================
% MODULE: entities/treasure.pl
% PURPOSE: Treasure and mimic chest system management
% =============================================================================

:- module(treasure, [
    % Treasure system setup
    setup_treasure_system/6,
    setup_chest/4,
    
    % Treasure queries
    get_all_chests/1,
    is_treasure/3,
    is_mimic/4,
    
    % Chest interaction
    open_chest/3,
    distance_to_chest/5,
    collect_treasure/0,
    is_treasure_collected/0,
    
    % Exit system
    init_exit/2,
    unlock_exit/0,
    can_exit_game/4,
    get_exit_position/2,
    is_exit_unlocked/0
]).

:- use_module('../core/geometry.pl').

% =============================================================================
% DYNAMIC FACTS
% =============================================================================

:- dynamic treasure/3.           % treasure(ID, X, Y)
:- dynamic mimic/4.              % mimic(ID, X, Y, Activated)
:- dynamic treasure_collected/1. % treasure_collected(Bool)
:- dynamic exit_position/2.      % exit_position(X, Y)
:- dynamic exit_unlocked/1.      % exit_unlocked(Bool)

% =============================================================================
% TREASURE SYSTEM SETUP
% =============================================================================

% Setup 3 chests - 1 real treasure, 2 mimics (randomly assigned)
% setup_treasure_system(X1, Y1, X2, Y2, X3, Y3)
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
% setup_chest(ID, X, Y, TreasureID)
setup_chest(ID, X, Y, TreasureID) :-
    (ID =:= TreasureID ->
        assertz(treasure(ID, X, Y))
    ;
        assertz(mimic(ID, X, Y, false))
    ).

% =============================================================================
% TREASURE QUERIES
% =============================================================================

% Get all chests (treasures + mimics) for Python
% get_all_chests(Chests)
get_all_chests(Chests) :-
    findall([treasure, ID, X, Y], treasure(ID, X, Y), Treasures),
    findall([mimic, ID, X, Y, Activated], mimic(ID, X, Y, Activated), Mimics),
    append(Treasures, Mimics, Chests).

% Check if there's a treasure at position
% is_treasure(ID, X, Y)
is_treasure(ID, X, Y) :-
    treasure(ID, X, Y).

% Check if there's a mimic at position
% is_mimic(ID, X, Y, Activated)
is_mimic(ID, X, Y, Activated) :-
    mimic(ID, X, Y, Activated).

% =============================================================================
% CHEST INTERACTION
% =============================================================================

% Open chest near player position (within 50 pixels)
% open_chest(PlayerX, PlayerY, Result)
% Result can be: treasure_found, mimic_activated, already_opened, no_chest
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
% distance_to_chest(PlayerX, PlayerY, ChestX, ChestY, Distance)
distance_to_chest(PlayerX, PlayerY, ChestX, ChestY, Distance) :-
    DX is ChestX - PlayerX,
    DY is ChestY - PlayerY,
    Distance is sqrt(DX * DX + DY * DY).

% Mark treasure as collected
collect_treasure :-
    retractall(treasure_collected(_)),
    assertz(treasure_collected(true)).

% Check if treasure has been collected
is_treasure_collected :-
    treasure_collected(true).

% =============================================================================
% EXIT SYSTEM
% =============================================================================

% Initialize exit system
% init_exit(ExitX, ExitY)
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
% can_exit_game(PlayerX, PlayerY, PlayerW, PlayerH)
can_exit_game(PlayerX, PlayerY, PlayerW, PlayerH) :-
    % Must have collected treasure
    treasure_collected(true),
    % Exit must be unlocked
    exit_unlocked(true),
    % Must be at exit position (within collision range)
    exit_position(ExitX, ExitY),
    ExitW = 48,  % Exit portal size
    ExitH = 48,
    geometry:rects_collide(PlayerX, PlayerY, PlayerW, PlayerH, ExitX, ExitY, ExitW, ExitH).

% Get exit position
% get_exit_position(X, Y)
get_exit_position(X, Y) :-
    exit_position(X, Y).

% Check if exit is unlocked
is_exit_unlocked :-
    exit_unlocked(true).
