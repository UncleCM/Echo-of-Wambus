"""
Unit tests for SpawnSystem
Tests entity spawning logic
"""

import unittest
import pygame
import sys
import os

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from systems.spawn_system import SpawnSystem


class MockTMXMap:
    """Mock TMX map for testing"""
    def __init__(self):
        self.width = 80
        self.height = 70
        self.tilewidth = 16
        self.tileheight = 16
        self.layers = []


class TestSpawnSystem(unittest.TestCase):
    """Test spawn system functionality"""
    
    @classmethod
    def setUpClass(cls):
        """Initialize pygame once for all tests"""
        pygame.init()
    
    @classmethod
    def tearDownClass(cls):
        """Cleanup pygame"""
        pygame.quit()
    
    def setUp(self):
        """Setup for each test"""
        self.mock_map = MockTMXMap()
        self.spawn_system = SpawnSystem(self.mock_map, map_scale=2)
    
    def test_spawn_system_initialization(self):
        """Test SpawnSystem initializes correctly"""
        self.assertIsNotNone(self.spawn_system)
        self.assertEqual(self.spawn_system.map_scale, 2)
        self.assertIsNotNone(self.spawn_system.tmx_map)
    
    def test_spawn_system_has_required_methods(self):
        """Test SpawnSystem has all required spawn methods"""
        required_methods = [
            'find_player_spawn',
            'find_wumpus_spawn',
            'spawn_treasure',
            'spawn_exit',
            'spawn_arrow_pickups',
            'spawn_rock_pickups'
        ]
        for method in required_methods:
            self.assertTrue(hasattr(self.spawn_system, method))
            self.assertTrue(callable(getattr(self.spawn_system, method)))
    
    def test_find_player_spawn_returns_tuple(self):
        """Player spawn should return (x, y) tuple"""
        pos = self.spawn_system.find_player_spawn()
        self.assertIsInstance(pos, tuple)
        self.assertEqual(len(pos), 2)
        self.assertIsInstance(pos[0], (int, float))
        self.assertIsInstance(pos[1], (int, float))
    
    def test_find_wumpus_spawn_returns_tuple(self):
        """Wumpus spawn should return (x, y) tuple"""
        pos = self.spawn_system.find_wumpus_spawn()
        self.assertIsInstance(pos, tuple)
        self.assertEqual(len(pos), 2)
        self.assertIsInstance(pos[0], (int, float))
        self.assertIsInstance(pos[1], (int, float))
    
    def test_map_dimensions_calculated(self):
        """Map dimensions should be calculated correctly"""
        expected_width = 80 * 16 * 2  # width * tilewidth * scale
        expected_height = 70 * 16 * 2  # height * tileheight * scale
        self.assertEqual(self.spawn_system.map_width, expected_width)
        self.assertEqual(self.spawn_system.map_height, expected_height)
    
    def test_spawn_exit_returns_position(self):
        """Spawn exit should return exit position"""
        sprite_groups = [pygame.sprite.Group(), pygame.sprite.Group()]
        pos = self.spawn_system.spawn_exit(sprite_groups)
        self.assertIsInstance(pos, tuple)
        self.assertEqual(len(pos), 2)


if __name__ == '__main__':
    unittest.main(verbosity=2)
