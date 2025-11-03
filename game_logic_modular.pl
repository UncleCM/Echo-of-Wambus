% =============================================================================
% game_logic_modular.pl - Bridge file using new modular Prolog architecture
% This file loads the new modules and provides backward compatibility
% =============================================================================

% Load core modules (Phase 1 - Complete)
:- use_module('prolog/core/geometry.pl').
:- use_module('prolog/core/movement.pl').
:- use_module('prolog/core/map_system.pl').

% Load the original game_logic.pl for predicates not yet modularized
% This ensures all game features still work during the refactoring process
:- include('game_logic.pl').

% The predicates from modules will override the ones in game_logic.pl
% This allows for gradual migration without breaking existing functionality
