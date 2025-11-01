"""Unit tests for player module (Phase 4)"""
import pytest
import pygame
from unittest.mock import Mock
from player import Player, PlayerInventory, PlayerInput, PlayerAnimations


class TestPlayerModule:
    """Test Player module structure"""
    
    def test_player_inventory_initialization(self):
        """Test PlayerInventory initializes correctly"""
        inventory = PlayerInventory()
        
        assert inventory.arrows == 1  # STARTING_ARROWS
        assert inventory.max_arrows == 2  # MAX_ARROWS
        assert inventory.rocks == 0
        assert inventory.max_rocks == 3  # MAX_ROCKS
    
    def test_player_inventory_add_arrows(self):
        """Test adding arrows to inventory"""
        inventory = PlayerInventory()
        
        # Can add arrows
        result = inventory.add_arrows(1)
        assert result == True
        assert inventory.arrows == 2  # Already at max
        
        # Can't exceed max
        inventory.arrows = 0
        inventory.add_arrows(3)
        assert inventory.arrows == 2  # Capped at max
    
    def test_player_inventory_add_rocks(self):
        """Test adding rocks to inventory"""
        inventory = PlayerInventory()
        
        # Can add rocks
        result = inventory.add_rocks(2)
        assert result == True
        assert inventory.rocks == 2
        
        # Can't exceed max
        inventory.add_rocks(5)
        assert inventory.rocks == 3  # Capped at max
    
    def test_player_input_handler(self):
        """Test PlayerInput can be initialized"""
        pygame.init()
        
        mock_player = Mock()
        mock_player.is_attacking = False
        mock_player.direction = pygame.math.Vector2(0, 0)
        mock_player.facing = "down"
        
        input_handler = PlayerInput(mock_player)
        assert input_handler.player == mock_player
        
        pygame.quit()
    
    def test_player_animations_loading(self):
        """Test PlayerAnimations can be initialized"""
        pygame.init()
        
        # PlayerAnimations now requires player reference
        # Just test it can be created
        animations = PlayerAnimations()
        
        # Check initial state
        assert animations.current_animation == "idle_down"
        assert animations.animation_speed == 12
        assert animations.animations == {}  # Empty until initialized
        
        pygame.quit()
    
    def test_player_initialization(self):
        """Test Player can be initialized"""
        pygame.init()
        
        mock_groups = Mock()
        mock_collision_sprites = Mock()
        mock_prolog = Mock()
        mock_sound_manager = Mock()
        
        player = Player(
            pos=(100, 100),
            groups=mock_groups,
            collision_sprites=mock_collision_sprites,
            prolog_engine=mock_prolog,
            sound_manager=mock_sound_manager
        )
        
        assert player is not None
        assert player.speed == 200
        assert hasattr(player, 'inventory')
        assert hasattr(player, 'input_handler')
        assert hasattr(player, 'animator')
        
        pygame.quit()
    
    def test_player_has_subsystems(self):
        """Test Player has inventory, input, and animation subsystems"""
        pygame.init()
        
        mock_groups = Mock()
        mock_collision_sprites = Mock()
        
        player = Player(
            pos=(100, 100),
            groups=mock_groups,
            collision_sprites=mock_collision_sprites
        )
        
        # Check subsystems exist
        assert isinstance(player.inventory, PlayerInventory)
        assert isinstance(player.input_handler, PlayerInput)
        assert isinstance(player.animator, PlayerAnimations)
        
        pygame.quit()
    
    def test_player_inventory_properties(self):
        """Test Player exposes inventory properties"""
        pygame.init()
        
        mock_groups = Mock()
        mock_collision_sprites = Mock()
        
        player = Player(
            pos=(100, 100),
            groups=mock_groups,
            collision_sprites=mock_collision_sprites
        )
        
        # Check property access
        assert player.arrows == player.inventory.arrows
        assert player.max_arrows == player.inventory.max_arrows
        assert player.rocks == player.inventory.rocks
        assert player.max_rocks == player.inventory.max_rocks
        
        pygame.quit()
