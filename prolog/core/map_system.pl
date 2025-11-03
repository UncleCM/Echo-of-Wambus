% =============================================================================
% MODULE: core/map_system.pl
% PURPOSE: Map data structures, grid system, navigation, and pathfinding
% =============================================================================

:- module(map_system, [
    init_map_system/5,
    build_navigation_grid/0,
    classify_and_add_grid_cell/3,
    generate_safe_positions/2,
    get_safe_positions/1,
    is_safe_grid_position/4,
    nearest_pit_distance/3,
    is_near_pit/3,
    count_grid_cells/2,
    % Dynamic facts (exported for external access)
    map_size/2,
    tile_size/2,
    grid_cell/3,
    grid_size/2,
    safe_position/2
]).

:- use_module('geometry.pl').
:- use_module('movement.pl').

% =============================================================================
% DYNAMIC FACTS
% =============================================================================

:- dynamic map_size/2.           % map_size(Width, Height)
:- dynamic tile_size/2.          % tile_size(TileW, TileH)
:- dynamic grid_cell/3.          % grid_cell(GridX, GridY, Type) - Type: safe, wall, pit, water
:- dynamic grid_size/2.          % grid_size(CellSize, [GridCountX, GridCountY])
:- dynamic safe_position/2.      % safe_position(X, Y) - cached safe positions
:- dynamic cached_path/3.        % cached_path(StartPos, GoalPos, Path) - pathfinding cache

% =============================================================================
% MAP INITIALIZATION
% =============================================================================

% Initialize map metadata and grid system
% init_map_system(MapW, MapH, TileW, TileH, CellSize)
init_map_system(MapW, MapH, TileW, TileH, CellSize) :-
    retractall(map_size(_, _)),
    retractall(tile_size(_, _)),
    retractall(grid_size(_, _)),
    retractall(grid_cell(_, _, _)),
    retractall(safe_position(_, _)),
    retractall(cached_path(_, _, _)),
    asserta(map_size(MapW, MapH)),
    asserta(tile_size(TileW, TileH)),
    GridCountX is ceiling(MapW / CellSize),
    GridCountY is ceiling(MapH / CellSize),
    asserta(grid_size(CellSize, [GridCountX, GridCountY])).

% =============================================================================
% NAVIGATION GRID
% =============================================================================

% Build complete navigation grid from collision/fall/water data
% This creates a grid representation of the map for AI pathfinding
build_navigation_grid :-
    grid_size(CellSize, [MaxX, MaxY]),
    MaxGX is MaxX - 1,
    MaxGY is MaxY - 1,
    forall(
        (
            between(0, MaxGX, GX),
            between(0, MaxGY, GY)
        ),
        classify_and_add_grid_cell(GX, GY, CellSize)
    ).

% Classify and add a single grid cell to the navigation grid
% classify_and_add_grid_cell(GridX, GridY, CellSize)
classify_and_add_grid_cell(GridX, GridY, CellSize) :-
    WorldX is GridX * CellSize,
    WorldY is GridY * CellSize,
    HalfCell is CellSize // 2,
    CenterX is WorldX + HalfCell,
    CenterY is WorldY + HalfCell,
    
    % Classify cell type based on collision/fall/water (priority order)
    (   movement:check_collision(CenterX, CenterY, CellSize, CellSize) ->
        Type = wall
    ;   movement:check_fall(CenterX, CenterY, CellSize, CellSize, CellSize) ->
        Type = pit
    ;   movement:check_water(CenterX, CenterY, CellSize, CellSize, CellSize) ->
        Type = water
    ;   Type = safe
    ),
    assertz(grid_cell(GridX, GridY, Type)).

% Count cells of specific type in grid
% count_grid_cells(Type, Count)
count_grid_cells(Type, Count) :-
    findall(1, grid_cell(_, _, Type), Cells),
    length(Cells, Count).

% =============================================================================
% SAFE POSITION MANAGEMENT
% =============================================================================

% Generate all safe positions and cache them for spawning
% generate_safe_positions(EntityWidth, EntityHeight)
generate_safe_positions(EntityWidth, EntityHeight) :-
    retractall(safe_position(_, _)),
    map_size(MapW, MapH),
    grid_size(CellSize, _),
    MaxX is MapW - EntityWidth,
    MaxY is MapH - EntityHeight,
    forall(
        (
            between(0, MaxX, CellSize, X),
            between(0, MaxY, CellSize, Y),
            is_safe_grid_position(X, Y, EntityWidth, EntityHeight)
        ),
        assertz(safe_position(X, Y))
    ).

% Helper: Generate values from Min to Max in steps of Step
between(Min, Max, Step, Value) :-
    Min =< Max,
    between_step(Max, Step, Min, Value).

between_step(Max, _Step, Current, Current) :-
    Current =< Max.
between_step(Max, Step, Current, Value) :-
    Current =< Max,
    Next is Current + Step,
    Next =< Max,
    between_step(Max, Step, Next, Value).

% Check if position is safe (no collision, no pit)
% is_safe_grid_position(X, Y, Width, Height)
is_safe_grid_position(X, Y, Width, Height) :-
    \+ movement:check_collision(X, Y, Width, Height),
    \+ movement:check_fall(X, Y, Width, Height, 10).

% Get all cached safe positions
% get_safe_positions(Positions)
get_safe_positions(Positions) :-
    findall([X, Y], safe_position(X, Y), Positions).

% =============================================================================
% PIT DETECTION & DANGER ZONES
% =============================================================================

% Calculate distance to nearest pit
% nearest_pit_distance(X, Y, Distance)
nearest_pit_distance(X, Y, Distance) :-
    findall(D, (
        movement:fall_zone(PitX, PitY, PitW, PitH),
        CenterX is PitX + PitW/2,
        CenterY is PitY + PitH/2,
        DX is X - CenterX,
        DY is Y - CenterY,
        D is sqrt(DX*DX + DY*DY)
    ), Distances),
    (   Distances = [] ->
        Distance = 999999  % No pits in map
    ;   min_list(Distances, Distance)
    ).

% Check if position is dangerously close to a pit
% is_near_pit(X, Y, DangerRadius)
is_near_pit(X, Y, DangerRadius) :-
    nearest_pit_distance(X, Y, Distance),
    Distance < DangerRadius.
