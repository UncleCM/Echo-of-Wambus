"""Unit tests for enemy module (Phase 3)"""
import pytest
import pygame
from unittest.mock import Mock, MagicMock
from enemies import Wumpus, WumpusAI, WumpusAIState


class TestWumpusModule:
    """Test Wumpus enemy module structure"""
    
    def test_wumpus_ai_state_constants(self):
        """Test AI state constants exist"""
        assert hasattr(WumpusAIState, 'ROAMING')
        assert hasattr(WumpusAIState, 'INVESTIGATING')
        assert hasattr(WumpusAIState, 'CHASING')
        assert hasattr(WumpusAIState, 'SEARCHING')
        assert hasattr(WumpusAIState, 'STUNNED')
        assert hasattr(WumpusAIState, 'DEAD')
    
    def test_wumpus_ai_initialization(self):
        """Test WumpusAI can be initialized"""
        pygame.init()
        
        # Mock Wumpus object
        mock_wumpus = Mock()
        mock_wumpus.hearing_radius = 350
        mock_wumpus.chase_hearing_bonus = 200
        mock_wumpus.roar_cooldown = 5000
        
        ai = WumpusAI(mock_wumpus)
        
        assert ai.state == WumpusAIState.ROAMING
        assert ai.hearing_radius == 350
        assert ai.current_hearing_radius == 350
        
        pygame.quit()
    
    def test_wumpus_initialization(self):
        """Test Wumpus can be initialized with mocked dependencies"""
        pygame.init()
        
        # Mock required objects
        mock_groups = Mock()
        mock_collision_sprites = Mock()
        mock_prolog = Mock()
        mock_map_knowledge = Mock()
        mock_sound_manager = Mock()
        
        # Create Wumpus (should not crash)
        wumpus = Wumpus(
            pos=(100, 100),
            groups=mock_groups,
            collision_sprites=mock_collision_sprites,
            prolog_engine=mock_prolog,
            map_knowledge=mock_map_knowledge,
            sound_manager=mock_sound_manager
        )
        
        assert wumpus is not None
        assert wumpus.ai is not None
        assert isinstance(wumpus.ai, WumpusAI)
        assert wumpus.speed == 250
        assert wumpus.chase_speed == 450
        assert wumpus.max_health == 150
        
        pygame.quit()
    
    def test_wumpus_has_ai_controller(self):
        """Test Wumpus has AI controller instance"""
        pygame.init()
        
        mock_groups = Mock()
        mock_collision_sprites = Mock()
        
        wumpus = Wumpus(
            pos=(100, 100),
            groups=mock_groups,
            collision_sprites=mock_collision_sprites
        )
        
        # Check AI controller exists
        assert hasattr(wumpus, 'ai')
        assert isinstance(wumpus.ai, WumpusAI)
        
        pygame.quit()
    
    def test_wumpus_ai_state_transitions(self):
        """Test AI state can transition"""
        pygame.init()
        
        mock_wumpus = Mock()
        mock_wumpus.hearing_radius = 350
        mock_wumpus.chase_hearing_bonus = 200
        mock_wumpus.roar_cooldown = 5000
        mock_wumpus.is_alive = True
        mock_wumpus.is_stunned = False
        
        ai = WumpusAI(mock_wumpus)
        
        # Test state transitions
        assert ai.state == WumpusAIState.ROAMING
        
        ai.state = WumpusAIState.INVESTIGATING
        assert ai.state == WumpusAIState.INVESTIGATING
        
        ai.state = WumpusAIState.CHASING
        assert ai.state == WumpusAIState.CHASING
        
        pygame.quit()
    
    def test_wumpus_apply_stun(self):
        """Test stun applies to both Wumpus and AI"""
        pygame.init()
        
        mock_groups = Mock()
        mock_collision_sprites = Mock()
        
        wumpus = Wumpus(
            pos=(100, 100),
            groups=mock_groups,
            collision_sprites=mock_collision_sprites
        )
        
        # Apply stun
        wumpus.apply_stun(duration=2.0)
        
        assert wumpus.is_stunned == True
        assert wumpus.stun_timer == 2.0
        assert wumpus.ai.state == WumpusAIState.STUNNED
        
        pygame.quit()
