% =============================================================================
% game_logic_modular.pl - Bridge file using new modular Prolog architecture
% This file loads the new modules and provides backward compatibility
% =============================================================================

% Load core modules (Phase 1 - Complete)
:- use_module('prolog/core/geometry.pl').
:- use_module('prolog/core/movement.pl').
:- use_module('prolog/core/map_system.pl').

% Load entity modules (Phase 2 - Complete)
:- use_module('prolog/entities/player.pl').
:- use_module('prolog/entities/wumpus_ai.pl').
:- use_module('prolog/entities/treasure.pl').

% Load system modules (Phase 3 - Complete)
:- use_module('prolog/systems/combat.pl').
:- use_module('prolog/systems/spawning.pl').
:- use_module('prolog/systems/game_state.pl').

% Load the original game_logic.pl for predicates not yet modularized
% This ensures all game features still work during the refactoring process
:- include('game_logic.pl').

% The predicates from modules will override the ones in game_logic.pl
% This allows for gradual migration without breaking existing functionality
