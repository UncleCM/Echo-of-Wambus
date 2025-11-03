% game_logic.pl - Prolog rules for Echo of Wumpus game

% Facts about game state
:- dynamic player_position/2.
:- dynamic game_over/1.
:- dynamic collision_box/4.
:- dynamic fall_zone/4.
:- dynamic water_zone/4.

% ======= NEW: Map System Facts =======
:- dynamic map_size/2.           % map_size(Width, Height)
:- dynamic tile_size/2.          % tile_size(TileW, TileH)
:- dynamic grid_cell/3.          % grid_cell(GridX, GridY, Type) - Type: safe, wall, pit, water
:- dynamic grid_size/2.          % grid_size(CellSize, [GridCountX, GridCountY])
:- dynamic safe_position/2.      % safe_position(X, Y) - cached safe positions
:- dynamic cached_path/3.        % cached_path(StartPos, GoalPos, Path) - pathfinding cache

% Initialize game state
init_game :-
    retractall(player_position(_, _)),
    retractall(game_over(_)),
    retractall(collision_box(_, _, _, _)),
    retractall(fall_zone(_, _, _, _)),
    retractall(water_zone(_, _, _, _)),
    % Clear map system facts
    retractall(map_size(_, _)),
    retractall(tile_size(_, _)),
    retractall(grid_cell(_, _, _)),
    retractall(grid_size(_, _)),
    retractall(safe_position(_, _)),
    retractall(cached_path(_, _, _)),
    % Clear projectile system
    init_projectile_system,
    % Clear Wumpus AI system
    retractall(wumpus_ai_state(_, _)),
    retractall(wumpus_target(_, _, _)),
    retractall(wumpus_patrol_index(_, _)),
    retractall(wumpus_search_timer(_, _)),
    retractall(wumpus_hearing_radius(_, _)),
    retractall(wumpus_is_roaring(_, _)),
    retractall(wumpus_last_roar_time(_, _)),
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

% PROJECTILE TRACKING
% ----------------------------------------------------------------------------
:- dynamic active_arrow/4.     % active_arrow(ID, X, Y, Direction)
:- dynamic active_rock/5.      % active_rock(ID, X, Y, VelX, VelY)
:- dynamic projectile_id_counter/1.

% Initialize projectile tracking system
init_projectile_system :-
    retractall(active_arrow(_, _, _, _)),
    retractall(active_rock(_, _, _, _, _)),
    retractall(projectile_id_counter(_)),
    assertz(projectile_id_counter(0)).

% Get next projectile ID
get_next_projectile_id(ID) :-
    projectile_id_counter(CurrentID),
    ID is CurrentID + 1,
    retract(projectile_id_counter(CurrentID)),
    assertz(projectile_id_counter(ID)).

% ARROW PROJECTILE MANAGEMENT
% ----------------------------------------------------------------------------

% Register new arrow projectile
spawn_arrow(X, Y, DirX, DirY, ArrowID) :-
    get_next_projectile_id(ArrowID),
    assertz(active_arrow(ArrowID, X, Y, (DirX, DirY))).

% Update arrow position
update_arrow_position(ArrowID, NewX, NewY) :-
    active_arrow(ArrowID, _, _, Dir),
    retract(active_arrow(ArrowID, _, _, Dir)),
    assertz(active_arrow(ArrowID, NewX, NewY, Dir)).

% Remove arrow (hit target or expired)
remove_arrow(ArrowID) :-
    retract(active_arrow(ArrowID, _, _, _)).

% Get all active arrows
get_active_arrows(Arrows) :-
    findall((ID, X, Y, Dir), active_arrow(ID, X, Y, Dir), Arrows).

% Count active arrows
count_active_arrows(Count) :-
    findall(ID, active_arrow(ID, _, _, _), IDs),
    length(IDs, Count).

% Check if arrow exists
arrow_exists(ArrowID) :-
    active_arrow(ArrowID, _, _, _).

% Get arrow position
get_arrow_position(ArrowID, X, Y) :-
    active_arrow(ArrowID, X, Y, _).

% ROCK PROJECTILE MANAGEMENT
% ----------------------------------------------------------------------------

% Register new rock projectile
spawn_rock(X, Y, VelX, VelY, RockID) :-
    get_next_projectile_id(RockID),
    assertz(active_rock(RockID, X, Y, VelX, VelY)).

% Update rock position and velocity
update_rock(RockID, NewX, NewY, NewVelX, NewVelY) :-
    retract(active_rock(RockID, _, _, _, _)),
    assertz(active_rock(RockID, NewX, NewY, NewVelX, NewVelY)).

% Remove rock (expired or stopped)
remove_rock(RockID) :-
    retract(active_rock(RockID, _, _, _, _)).

% Get all active rocks
get_active_rocks(Rocks) :-
    findall((ID, X, Y, VelX, VelY), active_rock(ID, X, Y, VelX, VelY), Rocks).

% Count active rocks
count_active_rocks(Count) :-
    findall(ID, active_rock(ID, _, _, _, _), IDs),
    length(IDs, Count).

% Check if rock exists
rock_exists(RockID) :-
    active_rock(RockID, _, _, _, _).

% Get rock position
get_rock_position(RockID, X, Y) :-
    active_rock(RockID, X, Y, _, _).

% COMBAT QUERIES
% ----------------------------------------------------------------------------

% Check if any arrow is near a position (for Wumpus awareness)
arrow_near_position(X, Y, Radius) :-
    active_arrow(_, ArrowX, ArrowY, _),
    distance_squared(X, Y, ArrowX, ArrowY, DistSq),
    RadiusSq is Radius * Radius,
    DistSq =< RadiusSq.

% Check if any rock is near a position (for sound detection)
rock_near_position(X, Y, Radius) :-
    active_rock(_, RockX, RockY, _, _),
    distance_squared(X, Y, RockX, RockY, DistSq),
    RadiusSq is Radius * Radius,
    DistSq =< RadiusSq.

% Get nearest arrow to a position
nearest_arrow_to_position(X, Y, ArrowID, Distance) :-
    findall(
        (Dist, ID),
        (
            active_arrow(ID, AX, AY, _),
            distance_squared(X, Y, AX, AY, DistSq),
            Dist is sqrt(DistSq)
        ),
        Pairs
    ),
    Pairs \= [],
    sort(Pairs, [(Distance, ArrowID)|_]).

% COLLISION DETECTION
% ----------------------------------------------------------------------------

% Check if arrow hits Wumpus (distance-based collision)
arrow_hit_wumpus(ArrowX, ArrowY, WumpusX, WumpusY, HitRadius) :-
    distance_squared(ArrowX, ArrowY, WumpusX, WumpusY, DistSq),
    HitRadiusSq is HitRadius * HitRadius,
    DistSq =< HitRadiusSq.

% Check if specific arrow hits any Wumpus position
arrow_hits_wumpus(ArrowID, WumpusX, WumpusY, HitRadius) :-
    active_arrow(ArrowID, ArrowX, ArrowY, _),
    arrow_hit_wumpus(ArrowX, ArrowY, WumpusX, WumpusY, HitRadius).

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
% MAP SYSTEM - Enhanced map queries and navigation
% ============================================================================

% Initialize map metadata
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

% Build navigation grid from collision/fall data
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

% Classify and add a single grid cell
classify_and_add_grid_cell(GridX, GridY, CellSize) :-
    WorldX is GridX * CellSize,
    WorldY is GridY * CellSize,
    HalfCell is CellSize // 2,
    CenterX is WorldX + HalfCell,
    CenterY is WorldY + HalfCell,
    
    % Classify cell type based on collision/fall/water
    (   check_collision(CenterX, CenterY, CellSize, CellSize) ->
        Type = wall
    ;   check_fall(CenterX, CenterY, CellSize, CellSize, CellSize) ->
        Type = pit
    ;   check_water(CenterX, CenterY, CellSize, CellSize, CellSize) ->
        Type = water
    ;   Type = safe
    ),
    asserta(grid_cell(GridX, GridY, Type)).

% Check if position is safe (no collision, no pit)
is_safe_position(X, Y, Width, Height) :-
    \+ check_collision(X, Y, Width, Height),
    \+ check_fall(X, Y, Width, Height, 10).

% Calculate nearest pit distance
nearest_pit_distance(X, Y, Distance) :-
    findall(D, (
        fall_zone(PitX, PitY, PitW, PitH),
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

% Check if position is near a pit
is_near_pit(X, Y, DangerRadius) :-
    nearest_pit_distance(X, Y, Distance),
    Distance < DangerRadius.

% Generate all safe positions and cache them
generate_safe_positions(EntityWidth, EntityHeight) :-
    retractall(safe_position(_, _)),
    map_size(MapW, MapH),
    grid_size(CellSize, _),
    MaxX is MapW - EntityWidth,
    MaxY is MapH - EntityHeight,
    Step is CellSize,
    forall(
        (
            between(0, MaxX, X),
            0 is X mod Step,
            between(0, MaxY, Y),
            0 is Y mod Step,
            is_safe_position(X, Y, EntityWidth, EntityHeight),
            \+ is_near_pit(X, Y, 60)
        ),
        asserta(safe_position(X, Y))
    ).

% Get random safe position from cache
random_safe_position(X, Y) :-
    findall([PX, PY], safe_position(PX, PY), Positions),
    length(Positions, Count),
    Count > 0,
    random(0, Count, Index),
    nth0(Index, Positions, [X, Y]).

% Get safe position that's far from other positions
safe_position_far_from(X, Y, OtherPositions, MinDistance) :-
    safe_position(X, Y),
    forall(
        member([OtherX, OtherY], OtherPositions),
        (
            DX is X - OtherX,
            DY is Y - OtherY,
            Distance is sqrt(DX*DX + DY*DY),
            Distance >= MinDistance
        )
    ).

% Count grid cells by type
count_grid_cells(Type, Count) :-
    findall(_, grid_cell(_, _, Type), Cells),
    length(Cells, Count).

% Count safe positions
count_safe_positions(Count) :-
    findall(_, safe_position(_, _), Positions),
    length(Positions, Count).

% ============================================================================
% GAME STATE MANAGEMENT SYSTEM
% ============================================================================

:- dynamic game_state/1.          % playing, paused, game_over_death, game_over_timeout, victory
:- dynamic time_remaining/1.       % เวลาที่เหลือ (seconds)
:- dynamic player_health/1.        % HP ของ player (0-100)
:- dynamic player_inventory/2.     % inventory(Arrows, Rocks)
:- dynamic player_max_inventory/2. % max_inventory(MaxArrows, MaxRocks)

% Initialize complete game state
init_game_state :-
    retractall(game_state(_)),
    retractall(time_remaining(_)),
    retractall(player_health(_)),
    retractall(player_inventory(_, _)),
    retractall(player_max_inventory(_, _)),
    asserta(game_state(playing)),
    asserta(time_remaining(180.0)),      % 3 minutes
    asserta(player_health(100)),
    asserta(player_inventory(3, 3)),      % Start: 3 arrows, 3 rocks
    asserta(player_max_inventory(10, 10)). % Max: 10 arrows, 10 rocks

% Get current game state
get_game_state(State) :-
    game_state(State).

% Set game state
set_game_state(NewState) :-
    retractall(game_state(_)),
    asserta(game_state(NewState)).

% Update time (called every frame with delta time)
update_time(DeltaTime) :-
    time_remaining(Current),
    New is max(0, Current - DeltaTime),
    retract(time_remaining(Current)),
    asserta(time_remaining(New)),
    % Check timeout
    (New =< 0 -> set_game_state(game_over_timeout) ; true).

% Get time remaining
get_time_remaining(Time) :-
    time_remaining(Time).

% Get player health
get_player_health(HP) :-
    player_health(HP).

% Damage player
damage_player(Amount) :-
    player_health(Current),
    New is max(0, Current - Amount),
    retract(player_health(Current)),
    asserta(player_health(New)),
    % Check death
    (New =< 0 -> set_game_state(game_over_death) ; true).

% Heal player
heal_player(Amount) :-
    player_health(Current),
    New is min(100, Current + Amount),
    retract(player_health(Current)),
    asserta(player_health(New)).

% Set player health directly
set_player_health(HP) :-
    retractall(player_health(_)),
    Clamped is max(0, min(100, HP)),
    asserta(player_health(Clamped)).

% Get inventory
get_player_inventory(Arrows, Rocks) :-
    player_inventory(Arrows, Rocks).

% Use arrow (returns true if had arrow, false if no arrows)
use_arrow :-
    player_inventory(Arrows, Rocks),
    Arrows > 0,
    NewArrows is Arrows - 1,
    retract(player_inventory(Arrows, Rocks)),
    asserta(player_inventory(NewArrows, Rocks)).

% Use rock (returns true if had rock, false if no rocks)
use_rock :-
    player_inventory(Arrows, Rocks),
    Rocks > 0,
    NewRocks is Rocks - 1,
    retract(player_inventory(Arrows, Rocks)),
    asserta(player_inventory(Arrows, NewRocks)).

% Add arrows (respects max limit)
add_arrows(Amount) :-
    player_inventory(Arrows, Rocks),
    player_max_inventory(MaxArrows, _),
    NewArrows is min(MaxArrows, Arrows + Amount),
    retract(player_inventory(Arrows, Rocks)),
    asserta(player_inventory(NewArrows, Rocks)).

% Add rocks (respects max limit)
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

% Check victory condition (has treasure + at exit)
% Check victory condition (has treasure + at exit)
check_victory_condition :-
    treasure_collected(true),
    exit_unlocked(true),
    player_position(PX, PY),
    exit_position(EX, EY),
    PlayerW = 32,
    PlayerH = 48,
    rects_collide(PX, PY, PlayerW, PlayerH, EX, EY, 48, 48).

% ============================================================================
% WUMPUS AI SYSTEM
% ============================================================================

% AI State Constants
% roaming - Patrolling/wandering
% investigating - Moving to sound source
% chasing - Actively pursuing player
% searching - Lost player, searching area
% stunned - Hit by arrow
% attack - In attack range
% dead - Defeated

:- dynamic wumpus_ai_state/2.        % wumpus_ai_state(WumpusID, State)
:- dynamic wumpus_target/3.          % wumpus_target(WumpusID, X, Y)
:- dynamic wumpus_patrol_index/2.    % wumpus_patrol_index(WumpusID, Index)
:- dynamic wumpus_search_timer/2.    % wumpus_search_timer(WumpusID, TimeLeft)
:- dynamic wumpus_hearing_radius/2.  % wumpus_hearing_radius(WumpusID, Radius)
:- dynamic wumpus_is_roaring/2.      % wumpus_is_roaring(WumpusID, Bool)
:- dynamic wumpus_last_roar_time/2.  % wumpus_last_roar_time(WumpusID, Time)

% Initialize Wumpus AI state
init_wumpus_ai(WumpusID, BaseHearing) :-
    assertz(wumpus_ai_state(WumpusID, roaming)),
    assertz(wumpus_target(WumpusID, 0, 0)),
    assertz(wumpus_patrol_index(WumpusID, 0)),
    assertz(wumpus_search_timer(WumpusID, 0)),
    assertz(wumpus_hearing_radius(WumpusID, BaseHearing)),
    assertz(wumpus_is_roaring(WumpusID, false)),
    assertz(wumpus_last_roar_time(WumpusID, 0)).

% Get Wumpus AI state
get_wumpus_state(WumpusID, State) :-
    wumpus_ai_state(WumpusID, State).

% Set Wumpus AI state
set_wumpus_state(WumpusID, NewState) :-
    retractall(wumpus_ai_state(WumpusID, _)),
    assertz(wumpus_ai_state(WumpusID, NewState)).

% Get Wumpus target position
get_wumpus_target(WumpusID, X, Y) :-
    wumpus_target(WumpusID, X, Y).

% Set Wumpus target position
set_wumpus_target(WumpusID, X, Y) :-
    retractall(wumpus_target(WumpusID, _, _)),
    assertz(wumpus_target(WumpusID, X, Y)).

% Get patrol waypoint index
get_patrol_index(WumpusID, Index) :-
    wumpus_patrol_index(WumpusID, Index).

% Set patrol waypoint index
set_patrol_index(WumpusID, Index) :-
    retractall(wumpus_patrol_index(WumpusID, _)),
    assertz(wumpus_patrol_index(WumpusID, Index)).

% Increment patrol index (with wrap-around)
increment_patrol_index(WumpusID, MaxWaypoints) :-
    wumpus_patrol_index(WumpusID, Current),
    Next is (Current + 1) mod MaxWaypoints,
    set_patrol_index(WumpusID, Next).

% Get search timer
get_search_timer(WumpusID, Time) :-
    wumpus_search_timer(WumpusID, Time).

% Set search timer
set_search_timer(WumpusID, Time) :-
    retractall(wumpus_search_timer(WumpusID, _)),
    assertz(wumpus_search_timer(WumpusID, Time)).

% Update search timer (decrease by dt)
update_search_timer(WumpusID, DT) :-
    wumpus_search_timer(WumpusID, Time),
    NewTime is max(0, Time - DT),
    set_search_timer(WumpusID, NewTime).

% Get hearing radius
get_hearing_radius(WumpusID, Radius) :-
    wumpus_hearing_radius(WumpusID, Radius).

% Set hearing radius
set_hearing_radius(WumpusID, Radius) :-
    retractall(wumpus_hearing_radius(WumpusID, _)),
    assertz(wumpus_hearing_radius(WumpusID, Radius)).

% Check if Wumpus is roaring
is_roaring(WumpusID) :-
    wumpus_is_roaring(WumpusID, true).

% Set roaring state
set_roaring(WumpusID, Bool) :-
    retractall(wumpus_is_roaring(WumpusID, _)),
    assertz(wumpus_is_roaring(WumpusID, Bool)).

% Get last roar time
get_last_roar_time(WumpusID, Time) :-
    wumpus_last_roar_time(WumpusID, Time).

% Set last roar time
set_last_roar_time(WumpusID, Time) :-
    retractall(wumpus_last_roar_time(WumpusID, _)),
    assertz(wumpus_last_roar_time(WumpusID, Time)).

% ============================================================================
% WUMPUS AI DECISION LOGIC
% ============================================================================

% Decide action based on sound detection
% Returns: NewState, ShouldRoar, NewTarget(X,Y)
decide_wumpus_action(WumpusID, SoundType, SoundX, SoundY, Loudness, CurrentState, 
                     NewState, ShouldRoar, TargetX, TargetY) :-
    % Rock impact - LOUD distraction
    (SoundType = rock_impact ->
        NewState = investigating,
        ShouldRoar = false,
        TargetX = SoundX,
        TargetY = SoundY
    ;
    % Player sounds (walk, dash, arrow)
    (member(SoundType, [walk, dash, arrow_shot]) ->
        (Loudness > 30 ->
            % Loud enough to chase
            NewState = chasing,
            ShouldRoar = true,
            TargetX = SoundX,
            TargetY = SoundY
        ;
            % Faint - just investigate
            NewState = investigating,
            ShouldRoar = false,
            TargetX = SoundX,
            TargetY = SoundY
        )
    ;
        % No relevant sound - keep current state
        NewState = CurrentState,
        ShouldRoar = false,
        TargetX = 0,
        TargetY = 0
    )).

% Decide action when no sound detected
decide_no_sound_action(WumpusID, CurrentState, NewState, SearchTime) :-
    % If chasing but lost sound, switch to searching
    (CurrentState = chasing ->
        NewState = searching,
        SearchTime = 8.0
    ;
    % If investigating and lost sound, also search
    (CurrentState = investigating ->
        NewState = searching,
        SearchTime = 5.0
    ;
        % Otherwise keep current state
        NewState = CurrentState,
        SearchTime = 0
    )).

% Check if Wumpus reached target position
reached_target(WumpusX, WumpusY, TargetX, TargetY, Threshold) :-
    distance_squared(WumpusX, WumpusY, TargetX, TargetY, DistSq),
    ThresholdSq is Threshold * Threshold,
    DistSq =< ThresholdSq.

% Decide behavior when target reached
decide_target_reached(WumpusID, CurrentState, NewState, SearchTime) :-
    % Investigating - arrived at sound location, start searching
    (CurrentState = investigating ->
        NewState = searching,
        SearchTime = 5.0
    ;
    % Chasing - lost player at last position, start searching
    (CurrentState = chasing ->
        NewState = searching,
        SearchTime = 8.0
    ;
        % Other states - return to roaming
        NewState = roaming,
        SearchTime = 0
    )).

% Check if search timer expired
search_timeout(WumpusID) :-
    wumpus_search_timer(WumpusID, Time),
    Time =< 0.

% Check if Wumpus can roar (cooldown check)
can_roar(WumpusID, CurrentTime, Cooldown) :-
    wumpus_last_roar_time(WumpusID, LastTime),
    TimeSince is CurrentTime - LastTime,
    TimeSince >= Cooldown.

% Check if Wumpus should transition to attack state
should_attack(WumpusX, WumpusY, PlayerX, PlayerY, AttackRange, CurrentState) :-
    % Must be chasing
    CurrentState = chasing,
    % Must be within attack range
    distance_squared(WumpusX, WumpusY, PlayerX, PlayerY, DistSq),
    RangeSq is AttackRange * AttackRange,
    DistSq =< RangeSq.

% Calculate enhanced hearing during chase
calculate_hearing_radius(WumpusID, BaseHearing, ChaseBonus, Radius) :-
    wumpus_ai_state(WumpusID, State),
    (State = chasing ->
        Radius is BaseHearing + ChaseBonus
    ;
        Radius = BaseHearing
    ).

% Check all game over conditions
check_game_over_conditions :-
    % Check death
    (player_health(HP), HP =< 0 -> 
        set_game_state(game_over_death)
    ;
    % Check timeout
     time_remaining(Time), Time =< 0 ->
        set_game_state(game_over_timeout)
    ;
    % Check victory
     treasure_collected(true), exit_unlocked(true),
     player_position(PX, PY), exit_position(EX, EY),
     DX is PX - EX, DY is PY - EY,
     DistSq is DX * DX + DY * DY,
     DistSq < 2500 ->
        set_game_state(victory)
    ;
        true  % Game continues
    ).

% Check if game is over (any end state)
is_game_over :-
    game_state(State),
    member(State, [game_over_death, game_over_timeout, victory]).

% Get game over reason
get_game_over_reason(Reason) :-
    game_state(State),
    (State = game_over_death -> Reason = death
    ; State = game_over_timeout -> Reason = timeout
    ; State = victory -> Reason = victory
    ; Reason = none).

% ============================================================================
% PHASE 4: SPAWNING SYSTEM
% ============================================================================
% Entity spawning configuration and logic

% Spawning configuration
:- dynamic spawn_config/2.

% Initialize spawn config (call at game start)
init_spawn_config :-
    retractall(spawn_config(_, _)),
    assertz(spawn_config(wumpus_count, 3)),      % 3-4 Wumpus
    assertz(spawn_config(wumpus_min_distance, 800)),
    assertz(spawn_config(chest_count, 3)),        % 1 real + 2 mimics
    assertz(spawn_config(arrow_pickup_count, 3)),
    assertz(spawn_config(rock_pickup_count, 5)).

% Get spawn configuration
get_spawn_config(Key, Value) :-
    spawn_config(Key, Value), !.
get_spawn_config(wumpus_count, 3).  % Default fallback
get_spawn_config(wumpus_min_distance, 800).
get_spawn_config(chest_count, 3).
get_spawn_config(arrow_pickup_count, 3).
get_spawn_config(rock_pickup_count, 5).

% Generate Wumpus spawn positions (far apart from each other)
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
get_safe_position_far_from_list([], _, X, Y) :-
    % No existing positions, just get any safe position
    random_safe_position(X, Y), !.
get_safe_position_far_from_list(ExistingPositions, MinDist, X, Y) :-
    % Try multiple times to find position far from all existing
    between(1, 100, _),
    random_safe_position(X, Y),
    \+ too_close_to_any(X, Y, ExistingPositions, MinDist),
    !.

% Check if position is too close to any in list
too_close_to_any(X, Y, [(EX, EY)|_], MinDist) :-
    DX is X - EX, DY is Y - EY,
    DistSq is DX * DX + DY * DY,
    MinDistSq is MinDist * MinDist,
    DistSq < MinDistSq, !.
too_close_to_any(X, Y, [_|Rest], MinDist) :-
    too_close_to_any(X, Y, Rest, MinDist).

% Generate pickup spawn positions (arrows/rocks)
generate_pickup_spawns(Count, Positions) :-
    generate_pickup_spawns_helper(Count, [], Positions).

generate_pickup_spawns_helper(0, Acc, Acc) :- !.
generate_pickup_spawns_helper(N, Acc, Result) :-
    N > 0,
    random_safe_position(X, Y),
    % Ensure not near pits
    \+ is_near_hazard(X, Y, 50),
    N1 is N - 1,
    generate_pickup_spawns_helper(N1, [(X, Y)|Acc], Result).

% Check if position is near any hazard (pit/water)
is_near_hazard(X, Y, Radius) :-
    fall_zone(FX, FY, FW, FH),
    X >= FX - Radius, X =< FX + FW + Radius,
    Y >= FY - Radius, Y =< FY + FH + Radius, !.

% ============================================================================
% HELPER PREDICATES FOR GAME LOGIC
% ============================================================================

% Check if player is near chest (for interaction prompt)
near_chest(PlayerX, PlayerY, InteractionRadius) :-
    (treasure(_ID, ChestX, ChestY) ; mimic(_ID, ChestX, ChestY, _)),
    distance_to_chest(PlayerX, PlayerY, ChestX, ChestY, Distance),
    Distance =< InteractionRadius.