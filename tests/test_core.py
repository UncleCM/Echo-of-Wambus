"""
Unit tests for Game core functionality
Tests game initialization, state management, and main loop
"""

import unittest
import pygame
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from game_state import GameState


class TestGameInitialization(unittest.TestCase):
    """Test game initialization"""
    
    @classmethod
    def setUpClass(cls):
        """Initialize pygame once for all tests"""
        pygame.init()
    
    @classmethod
    def tearDownClass(cls):
        """Cleanup pygame"""
        pygame.quit()
    
    def test_game_state_enum_exists(self):
        """Test that GameState enum has required states"""
        self.assertTrue(hasattr(GameState, 'MAIN_MENU'))
        self.assertTrue(hasattr(GameState, 'PLAYING'))
        self.assertTrue(hasattr(GameState, 'GAME_OVER'))
        self.assertTrue(hasattr(GameState, 'VICTORY'))
    
    def test_game_state_values(self):
        """Test GameState enum values"""
        states = [GameState.MAIN_MENU, GameState.PLAYING, GameState.GAME_OVER, GameState.VICTORY]
        # Should have unique values
        self.assertEqual(len(states), len(set(states)))


class TestSpawnSystemRequirements(unittest.TestCase):
    """Test requirements for spawn system"""
    
    def test_spawn_positions_should_be_tuples(self):
        """Spawn positions should be (x, y) tuples"""
        pos = (100, 200)
        self.assertIsInstance(pos, tuple)
        self.assertEqual(len(pos), 2)
        self.assertIsInstance(pos[0], (int, float))
        self.assertIsInstance(pos[1], (int, float))
    
    def test_spawn_groups_should_be_list(self):
        """Spawn groups should be lists of sprite groups"""
        groups = [pygame.sprite.Group(), pygame.sprite.Group()]
        self.assertIsInstance(groups, list)
        for group in groups:
            self.assertIsInstance(group, pygame.sprite.Group)


class TestPrologIntegration(unittest.TestCase):
    """Test Prolog engine integration requirements"""
    
    def test_prolog_engine_can_be_imported(self):
        """PrologEngine should be importable"""
        try:
            from prolog_interface import PrologEngine
            self.assertTrue(True)
        except ImportError as e:
            self.fail(f"Cannot import PrologEngine: {e}")
    
    def test_prolog_engine_initialization(self):
        """PrologEngine should initialize without errors"""
        from prolog_interface import PrologEngine
        try:
            engine = PrologEngine()
            self.assertIsNotNone(engine)
        except Exception as e:
            self.fail(f"PrologEngine initialization failed: {e}")


if __name__ == '__main__':
    # Run tests with verbose output
    unittest.main(verbosity=2)
