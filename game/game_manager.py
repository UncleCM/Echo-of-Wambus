"""
Game Manager - Main controller for the Wumpus game
Handles game state, entities, and coordination between systems
"""

import pygame
from typing import Dict, Tuple
from .enhanced_map import EnhancedGameMap
from .camera import Camera2D5
from entities.player import Player
from entities.enemy import EnemySpawner

class GameManager:
    """Main game controller"""
    
    def __init__(self, screen: pygame.Surface, screen_width: int, screen_height: int):
        """Initialize the game manager"""
        self.screen = screen
        self.screen_width = screen_width
        self.screen_height = screen_height
        
        # 2.5D Camera system
        self.camera = Camera2D5(screen_width, screen_height)
        
        # Initialize game systems
        self.map = EnhancedGameMap("assets/Map/MainLev2.0", prefer_psd=True)
        self.player = Player(400, 400)  # Start player at center of map
        
        # Set camera bounds based on map size
        map_width, map_height = self.map.get_map_size()
        self.camera.set_bounds(0, 0, map_width, map_height)
        
        # Initialize enemy spawner
        self.enemy_spawner = EnemySpawner(map_width, map_height)
        
        # Game state
        self.running = True
        
        # Input tracking
        self.keys_pressed = {}
        
        # Set up camera to follow player
        self.camera.follow_target(self.player.x, self.player.y)
        
    def handle_event(self, event: pygame.event.Event):
        """Handle pygame events"""
        if event.type == pygame.KEYDOWN:
            self.keys_pressed[event.key] = True
            
            # Handle zoom controls
            if event.key == pygame.K_PLUS or event.key == pygame.K_EQUALS:
                self.camera.zoom_in()
            elif event.key == pygame.K_MINUS:
                self.camera.zoom_out()
            
        elif event.type == pygame.KEYUP:
            self.keys_pressed[event.key] = False
    
    def update(self, dt: float):
        """Update game state"""
        # Update player
        self.player.update(dt, self.keys_pressed)
        
        # Check collision between player and map
        self._check_player_collision()
        
        # Update enemies
        player_pos = (self.player.x, self.player.y)
        self.enemy_spawner.update(dt, player_pos)
        
        # Handle weapon system
        self._update_weapon_system(dt)
        
        # Update camera to follow player
        self.camera.follow_target(self.player.x, self.player.y)
        self.camera.update(dt)
    
    def _check_player_collision(self):
        """Check and resolve collisions between player and map"""
        player_rect = self.player.get_rect()
        camera_offset = self.camera.get_camera_offset()
        
        # Check collision with map
        if self.map.check_collision(player_rect, camera_offset):
            # Simple collision resolution - push player back
            # This is a basic implementation; more sophisticated physics would be better
            
            # Check horizontal collision
            test_rect = player_rect.copy()
            test_rect.x = int(self.player.x - 1)
            if not self.map.check_collision(test_rect, camera_offset):
                self.player.x -= 1
                self.player.velocity_x = 0
            
            test_rect.x = int(self.player.x + 1)
            if not self.map.check_collision(test_rect, camera_offset):
                self.player.x += 1
                self.player.velocity_x = 0
            
            # Check vertical collision
            test_rect = player_rect.copy()
            test_rect.y = int(self.player.y - 1)
            if not self.map.check_collision(test_rect, camera_offset):
                self.player.y -= 1
                self.player.velocity_y = 0
            
            test_rect.y = int(self.player.y + 1)
            if not self.map.check_collision(test_rect, camera_offset):
                self.player.y += 1
                self.player.velocity_y = 0
    
    def _update_weapon_system(self, dt: float):
        """Update weapon system and handle collisions"""
        import time
        current_time = time.time()
        
        # Auto-fire weapon at enemies
        enemies = self.enemy_spawner.enemies
        self.player.auto_fire_weapon(enemies, current_time)
        
        # Check projectile collisions with enemies
        collisions = self.player.weapon_system.check_collisions(enemies)
        
        # Handle collisions
        for projectile, enemy in collisions:
            # Apply damage to enemy
            enemy.take_damage(projectile.damage)
            
            # Apply camera shake for hit effect
            self.camera.apply_shake(5.0, 0.1)
    
    
    def render(self):
        """Render the game"""
        # Clear screen with darker background for 2.5D feel
        self.screen.fill((20, 20, 30))  # Dark blue-gray background
        
        # Get camera offset for rendering
        camera_offset = self.camera.get_camera_offset()
        
        # Render map
        self.map.render(self.screen, camera_offset)
        
        # Render player
        self.player.render(self.screen, camera_offset)
        
        # Render enemies
        self.enemy_spawner.render(self.screen, camera_offset)
        
        # Render weapon projectiles
        self.player.render_weapons(self.screen, camera_offset)
        
        # Render UI elements
        self._render_ui()
    
    def _render_ui(self):
        """Render user interface elements"""
        # Render player position info
        font = pygame.font.Font(None, 24)
        
        # Player position
        pos_text = font.render(f"Position: ({int(self.player.x)}, {int(self.player.y)})", True, (255, 255, 255))
        self.screen.blit(pos_text, (10, 10))
        
        # Camera info
        camera_text = font.render(f"Camera: ({int(self.camera.x):.1f}, {int(self.camera.y):.1f}) Zoom: {self.camera.zoom:.1f}x", True, (255, 255, 255))
        self.screen.blit(camera_text, (10, 35))
        
        # Player state
        state_text = font.render(f"State: {self.player.current_state.value}", True, (255, 255, 255))
        self.screen.blit(state_text, (10, 60))
        
        # Player direction
        direction_text = font.render(f"Direction: {self.player.current_direction.value}", True, (255, 255, 255))
        self.screen.blit(direction_text, (10, 85))
        
        # Enemy info
        enemy_text = font.render(f"Enemies: {self.enemy_spawner.get_enemy_count()} | Wave: {self.enemy_spawner.get_current_wave()}", True, (255, 255, 255))
        self.screen.blit(enemy_text, (10, 110))
        
        # Weapon info
        current_weapon = self.player.weapon_system.get_current_weapon()
        if current_weapon:
            weapon_text = font.render(f"Weapon: {current_weapon.weapon_type.value} | Projectiles: {len(current_weapon.projectiles)}", True, (255, 255, 255))
            self.screen.blit(weapon_text, (10, 135))
        
        # Controls help
        controls_text = font.render("Controls: WASD=Move, Space=Dash, Q/E=Weapon, +/-=Zoom", True, (255, 255, 255))
        self.screen.blit(controls_text, (10, self.screen_height - 25))
