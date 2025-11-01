"""
Complete Prolog interface combining all query modules

This module provides the PrologInterface class which combines:
1. Physics and collision queries (PhysicsQueries)
2. Entity AI queries (EntityQueries)
3. Game mechanics queries (GameQueries)
"""
from ai.physics_queries import PhysicsQueries
from ai.entity_queries import EntityQueries
from ai.game_queries import GameQueries


class PrologInterface(PhysicsQueries, EntityQueries, GameQueries):
    """
    Complete Prolog interface for the game
    
    Inherits from:
    - PhysicsQueries: Collision detection, movement resolution
    - EntityQueries: Player, Wumpus AI
    - GameQueries: Treasure, combat, items, victory
    """
    
    def __init__(self, knowledge_base_file="game_logic.pl"):
        """
        Initialize the complete Prolog interface
        
        Args:
            knowledge_base_file: Path to .pl file to consult
        """
        # Initialize the base PrologEngine (will be called once via MRO)
        super().__init__(knowledge_base_file)
        
        if self.available:
            print("[PrologInterface] ✅ All query modules ready")
        else:
            print("[PrologInterface] ⚠️ Running in fallback mode (no Prolog)")
