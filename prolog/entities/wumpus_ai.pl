% =============================================================================
% MODULE: entities/wumpus_ai.pl
% PURPOSE: Wumpus AI state machine and decision-making logic
% =============================================================================

:- module(wumpus_ai, [
    % AI state initialization
    init_wumpus_ai/2,
    
    % State management
    get_wumpus_state/2,
    set_wumpus_state/2,
    
    % Target management
    get_wumpus_target/3,
    set_wumpus_target/3,
    
    % Patrol management
    get_patrol_index/2,
    set_patrol_index/2,
    increment_patrol_index/2,
    
    % Search timer
    get_search_timer/2,
    set_search_timer/2,
    update_search_timer/2,
    search_timeout/1,
    
    % Hearing system
    get_hearing_radius/2,
    set_hearing_radius/2,
    calculate_hearing_radius/4,
    
    % Roaring system
    is_roaring/1,
    set_roaring/2,
    get_last_roar_time/2,
    set_last_roar_time/2,
    can_roar/3,
    
    % Decision logic
    decide_wumpus_action/10,
    decide_no_sound_action/4,
    decide_target_reached/4,
    should_attack/6,
    
    % Position checks
    reached_target/5
]).

:- use_module('../core/geometry.pl').

% =============================================================================
% DYNAMIC FACTS
% =============================================================================

:- dynamic wumpus_ai_state/2.       % wumpus_ai_state(WumpusID, State)
:- dynamic wumpus_target/3.         % wumpus_target(WumpusID, X, Y)
:- dynamic wumpus_patrol_index/2.   % wumpus_patrol_index(WumpusID, Index)
:- dynamic wumpus_search_timer/2.   % wumpus_search_timer(WumpusID, TimeLeft)
:- dynamic wumpus_hearing_radius/2. % wumpus_hearing_radius(WumpusID, Radius)
:- dynamic wumpus_is_roaring/2.     % wumpus_is_roaring(WumpusID, Bool)
:- dynamic wumpus_last_roar_time/2. % wumpus_last_roar_time(WumpusID, Time)

% =============================================================================
% AI INITIALIZATION
% =============================================================================

% Initialize Wumpus AI state
% init_wumpus_ai(WumpusID, BaseHearing)
init_wumpus_ai(WumpusID, BaseHearing) :-
    assertz(wumpus_ai_state(WumpusID, roaming)),
    assertz(wumpus_target(WumpusID, 0, 0)),
    assertz(wumpus_patrol_index(WumpusID, 0)),
    assertz(wumpus_search_timer(WumpusID, 0)),
    assertz(wumpus_hearing_radius(WumpusID, BaseHearing)),
    assertz(wumpus_is_roaring(WumpusID, false)),
    assertz(wumpus_last_roar_time(WumpusID, 0)).

% =============================================================================
% STATE MANAGEMENT
% =============================================================================

% Get Wumpus AI state
% get_wumpus_state(WumpusID, State)
% States: roaming, investigating, chasing, searching, stunned, attack, dead
get_wumpus_state(WumpusID, State) :-
    wumpus_ai_state(WumpusID, State).

% Set Wumpus AI state
% set_wumpus_state(WumpusID, NewState)
set_wumpus_state(WumpusID, NewState) :-
    retractall(wumpus_ai_state(WumpusID, _)),
    assertz(wumpus_ai_state(WumpusID, NewState)).

% =============================================================================
% TARGET MANAGEMENT
% =============================================================================

% Get Wumpus target position
% get_wumpus_target(WumpusID, X, Y)
get_wumpus_target(WumpusID, X, Y) :-
    wumpus_target(WumpusID, X, Y).

% Set Wumpus target position
% set_wumpus_target(WumpusID, X, Y)
set_wumpus_target(WumpusID, X, Y) :-
    retractall(wumpus_target(WumpusID, _, _)),
    assertz(wumpus_target(WumpusID, X, Y)).

% =============================================================================
% PATROL MANAGEMENT
% =============================================================================

% Get patrol waypoint index
% get_patrol_index(WumpusID, Index)
get_patrol_index(WumpusID, Index) :-
    wumpus_patrol_index(WumpusID, Index).

% Set patrol waypoint index
% set_patrol_index(WumpusID, Index)
set_patrol_index(WumpusID, Index) :-
    retractall(wumpus_patrol_index(WumpusID, _)),
    assertz(wumpus_patrol_index(WumpusID, Index)).

% Increment patrol index (with wrap-around)
% increment_patrol_index(WumpusID, MaxWaypoints)
increment_patrol_index(WumpusID, MaxWaypoints) :-
    wumpus_patrol_index(WumpusID, Current),
    Next is (Current + 1) mod MaxWaypoints,
    set_patrol_index(WumpusID, Next).

% =============================================================================
% SEARCH TIMER
% =============================================================================

% Get search timer
% get_search_timer(WumpusID, Time)
get_search_timer(WumpusID, Time) :-
    wumpus_search_timer(WumpusID, Time).

% Set search timer
% set_search_timer(WumpusID, Time)
set_search_timer(WumpusID, Time) :-
    retractall(wumpus_search_timer(WumpusID, _)),
    assertz(wumpus_search_timer(WumpusID, Time)).

% Update search timer (decrease by dt)
% update_search_timer(WumpusID, DT)
update_search_timer(WumpusID, DT) :-
    wumpus_search_timer(WumpusID, Time),
    NewTime is max(0, Time - DT),
    set_search_timer(WumpusID, NewTime).

% Check if search timer expired
% search_timeout(WumpusID)
search_timeout(WumpusID) :-
    wumpus_search_timer(WumpusID, Time),
    Time =< 0.

% =============================================================================
% HEARING SYSTEM
% =============================================================================

% Get hearing radius
% get_hearing_radius(WumpusID, Radius)
get_hearing_radius(WumpusID, Radius) :-
    wumpus_hearing_radius(WumpusID, Radius).

% Set hearing radius
% set_hearing_radius(WumpusID, Radius)
set_hearing_radius(WumpusID, Radius) :-
    retractall(wumpus_hearing_radius(WumpusID, _)),
    assertz(wumpus_hearing_radius(WumpusID, Radius)).

% Calculate enhanced hearing during chase
% calculate_hearing_radius(WumpusID, BaseHearing, ChaseBonus, Radius)
calculate_hearing_radius(WumpusID, BaseHearing, ChaseBonus, Radius) :-
    wumpus_ai_state(WumpusID, State),
    (State = chasing ->
        Radius is BaseHearing + ChaseBonus
    ;
        Radius = BaseHearing
    ).

% =============================================================================
% ROARING SYSTEM
% =============================================================================

% Check if Wumpus is roaring
% is_roaring(WumpusID)
is_roaring(WumpusID) :-
    wumpus_is_roaring(WumpusID, true).

% Set roaring state
% set_roaring(WumpusID, Bool)
set_roaring(WumpusID, Bool) :-
    retractall(wumpus_is_roaring(WumpusID, _)),
    assertz(wumpus_is_roaring(WumpusID, Bool)).

% Get last roar time
% get_last_roar_time(WumpusID, Time)
get_last_roar_time(WumpusID, Time) :-
    wumpus_last_roar_time(WumpusID, Time).

% Set last roar time
% set_last_roar_time(WumpusID, Time)
set_last_roar_time(WumpusID, Time) :-
    retractall(wumpus_last_roar_time(WumpusID, _)),
    assertz(wumpus_last_roar_time(WumpusID, Time)).

% Check if Wumpus can roar (cooldown check)
% can_roar(WumpusID, CurrentTime, Cooldown)
can_roar(WumpusID, CurrentTime, Cooldown) :-
    wumpus_last_roar_time(WumpusID, LastTime),
    TimeSince is CurrentTime - LastTime,
    TimeSince >= Cooldown.

% =============================================================================
% DECISION LOGIC
% =============================================================================

% Decide action based on sound detection
% Returns: NewState, ShouldRoar, NewTarget(X,Y)
% decide_wumpus_action(WumpusID, SoundType, SoundX, SoundY, Loudness, CurrentState, 
%                      NewState, ShouldRoar, TargetX, TargetY)
decide_wumpus_action(_WumpusID, SoundType, SoundX, SoundY, Loudness, CurrentState, 
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
            % Only roar if transitioning FROM patrol/investigating/searching/roaming TO chasing
            % Don't roar if already chasing or attacking (attack state means currently in combat)
            (member(CurrentState, [patrol, investigating, searching, roaming]) ->
                ShouldRoar = true
            ;
                ShouldRoar = false
            ),
            TargetX = SoundX,
            TargetY = SoundY
        ;
            % Faint - just investigate
            NewState = investigating,
            % Roar if transitioning FROM roaming/patrol TO investigating (first detection)
            % Don't roar if already investigating, chasing, searching, or attacking
            (member(CurrentState, [roaming, patrol]) ->
                ShouldRoar = true
            ;
                ShouldRoar = false
            ),
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
% decide_no_sound_action(WumpusID, CurrentState, NewState, SearchTime)
decide_no_sound_action(_WumpusID, CurrentState, NewState, SearchTime) :-
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
% reached_target(WumpusX, WumpusY, TargetX, TargetY, Threshold)
reached_target(WumpusX, WumpusY, TargetX, TargetY, Threshold) :-
    geometry:distance_squared(WumpusX, WumpusY, TargetX, TargetY, DistSq),
    ThresholdSq is Threshold * Threshold,
    DistSq =< ThresholdSq.

% Decide behavior when target reached
% decide_target_reached(WumpusID, CurrentState, NewState, SearchTime)
decide_target_reached(_WumpusID, CurrentState, NewState, SearchTime) :-
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

% Check if Wumpus should transition to attack state
% should_attack(WumpusX, WumpusY, PlayerX, PlayerY, AttackRange, CurrentState)
should_attack(WumpusX, WumpusY, PlayerX, PlayerY, AttackRange, CurrentState) :-
    % Must be chasing
    CurrentState = chasing,
    % Must be within attack range
    geometry:distance_squared(WumpusX, WumpusY, PlayerX, PlayerY, DistSq),
    RangeSq is AttackRange * AttackRange,
    DistSq =< RangeSq.
