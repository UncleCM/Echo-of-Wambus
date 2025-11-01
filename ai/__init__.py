"""AI and Prolog interface for Echo-of-Wambus game"""
from ai.prolog_engine import PrologEngine
from ai.physics_queries import PhysicsQueries
from ai.entity_queries import EntityQueries
from ai.game_queries import GameQueries
from ai.prolog_interface import PrologInterface

__all__ = [
    'PrologEngine',
    'PhysicsQueries',
    'EntityQueries', 
    'GameQueries',
    'PrologInterface'
]
