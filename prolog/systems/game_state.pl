% =============================================================================
% MODULE: systems/game_state.pl
% PURPOSE: Game state management including time, game flow, and win/loss conditions
% =============================================================================

:- module(game_state, [
    % Game state initialization
    init_game_state/0,
    
    % State management
    get_game_state/1,
    set_game_state/1,
    
    % Time management
    update_time/1,
    get_time_remaining/1,
    set_time_remaining/1,
    
    % Game over predicates (legacy)
    set_game_over/1,
    is_game_over/1
]).

% =============================================================================
% GAME STATE TRACKING
% =============================================================================

:- dynamic game_state/1.       % playing, paused, game_over_death, game_over_timeout, victory
:- dynamic time_remaining/1.   % Time remaining (seconds)
:- dynamic game_over/1.        % Legacy: true/false

% Initialize complete game state
% init_game_state
init_game_state :-
    % Randomize seed for random number generation
    get_time(T),
    Seed is round(T * 1000000) mod 2147483647,
    set_random(seed(Seed)),
    
    % Clear previous state
    retractall(game_state(_)),
    retractall(time_remaining(_)),
    retractall(game_over(_)),
    
    % Initialize new state
    assertz(game_state(playing)),
    assertz(time_remaining(180.0)),  % 3 minutes
    assertz(game_over(false)).

% =============================================================================
% STATE MANAGEMENT
% =============================================================================

% Get current game state
% get_game_state(State)
get_game_state(State) :-
    game_state(State).

% Set game state
% set_game_state(NewState)
set_game_state(NewState) :-
    retractall(game_state(_)),
    assertz(game_state(NewState)),
    % Update legacy game_over flag if needed
    (member(NewState, [game_over_death, game_over_timeout]) ->
        set_game_over(true)
    ;   true
    ),
    (NewState = victory ->
        set_game_over(false)  % Victory is not "game over" in the failure sense
    ;   true
    ).

% =============================================================================
% TIME MANAGEMENT
% =============================================================================

% Update time (called every frame with delta time)
% update_time(DeltaTime)
update_time(DeltaTime) :-
    time_remaining(Current),
    New is max(0, Current - DeltaTime),
    retract(time_remaining(Current)),
    assertz(time_remaining(New)),
    % Check timeout
    (New =< 0 -> set_game_state(game_over_timeout) ; true).

% Get time remaining
% get_time_remaining(Time)
get_time_remaining(Time) :-
    time_remaining(Time).

% Set time remaining
% set_time_remaining(Time)
set_time_remaining(Time) :-
    retractall(time_remaining(_)),
    assertz(time_remaining(Time)).

% =============================================================================
% LEGACY GAME OVER PREDICATES
% =============================================================================
% These are kept for backward compatibility with existing Python code

% Set game over state (legacy)
% set_game_over(State)
set_game_over(State) :-
    retractall(game_over(_)),
    assertz(game_over(State)).

% Check if game is over (legacy)
% is_game_over(State)
is_game_over(State) :-
    game_over(State).
