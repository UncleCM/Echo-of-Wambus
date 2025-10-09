"""
Enemy entity system for Vampire Survivors style gameplay
"""

import pygame
import math
import random
from typing import Tuple, List, Optional
from enum import Enum

class EnemyType(Enum):
    """Types of enemies"""
    BASIC = "basic"
    FAST = "fast"
    HEAVY = "heavy"

class EnemyState(Enum):
    """Enemy states"""
    SPAWNING = "spawning"
    MOVING = "moving"
    ATTACKING = "attacking"
    DEAD = "dead"

class Enemy:
    """Basic enemy class for 2.5D top-down gameplay"""
    
    def __init__(self, x: float, y: float, enemy_type: EnemyType = EnemyType.BASIC):
        """Initialize enemy"""
        self.x = x
        self.y = y
        self.enemy_type = enemy_type
        self.state = EnemyState.SPAWNING
        
        # Enemy properties based on type
        if enemy_type == EnemyType.BASIC:
            self.speed = 80.0
            self.health = 50
            self.max_health = 50
            self.size = 24
            self.color = (255, 100, 100)  # Red
        elif enemy_type == EnemyType.FAST:
            self.speed = 150.0
            self.health = 30
            self.max_health = 30
            self.size = 20
            self.color = (255, 200, 100)  # Orange
        elif enemy_type == EnemyType.HEAVY:
            self.speed = 50.0
            self.health = 100
            self.max_health = 100
            self.size = 32
            self.color = (100, 100, 255)  # Blue
        
        # Movement properties
        self.velocity_x = 0.0
        self.velocity_y = 0.0
        self.target_x = x
        self.target_y = y
        
        # AI properties
        self.ai_timer = 0.0
        self.ai_update_interval = 0.5  # Update AI every 0.5 seconds
        self.detection_range = 200.0
        self.attack_range = 30.0
        
        # Spawn animation
        self.spawn_timer = 1.0
        self.spawn_duration = 1.0
        
    def update(self, dt: float, player_pos: Tuple[float, float]):
        """Update enemy state"""
        if self.state == EnemyState.SPAWNING:
            self.spawn_timer -= dt
            if self.spawn_timer <= 0:
                self.state = EnemyState.MOVING
        
        elif self.state == EnemyState.MOVING:
            self._update_ai(dt, player_pos)
            self._update_movement(dt)
        
        elif self.state == EnemyState.ATTACKING:
            # Simple attack behavior - just move towards player
            self._move_towards_player(player_pos, dt)
        
        elif self.state == EnemyState.DEAD:
            # Dead enemies don't move
            pass
    
    def _update_ai(self, dt: float, player_pos: Tuple[float, float]):
        """Update enemy AI"""
        self.ai_timer += dt
        
        if self.ai_timer >= self.ai_update_interval:
            self.ai_timer = 0.0
            
            # Calculate distance to player
            dx = player_pos[0] - self.x
            dy = player_pos[1] - self.y
            distance = math.sqrt(dx * dx + dy * dy)
            
            if distance <= self.detection_range:
                # Player detected - move towards player
                self.state = EnemyState.ATTACKING
                self.target_x = player_pos[0]
                self.target_y = player_pos[1]
            else:
                # Random movement
                self._random_movement()
    
    def _update_movement(self, dt: float):
        """Update enemy movement"""
        # Move towards target
        dx = self.target_x - self.x
        dy = self.target_y - self.y
        distance = math.sqrt(dx * dx + dy * dy)
        
        if distance > 5:  # If not at target
            # Normalize direction and apply speed
            self.velocity_x = (dx / distance) * self.speed
            self.velocity_y = (dy / distance) * self.speed
            
            # Update position
            self.x += self.velocity_x * dt
            self.y += self.velocity_y * dt
        else:
            # At target, stop moving
            self.velocity_x = 0
            self.velocity_y = 0
    
    def _move_towards_player(self, player_pos: Tuple[float, float], dt: float):
        """Move directly towards player"""
        dx = player_pos[0] - self.x
        dy = player_pos[1] - self.y
        distance = math.sqrt(dx * dx + dy * dy)
        
        if distance > 0:
            # Normalize direction and apply speed
            self.velocity_x = (dx / distance) * self.speed
            self.velocity_y = (dy / distance) * self.speed
            
            # Update position
            self.x += self.velocity_x * dt
            self.y += self.velocity_y * dt
    
    def _random_movement(self):
        """Set random movement target"""
        # Random direction within detection range
        angle = random.uniform(0, 2 * math.pi)
        distance = random.uniform(50, 150)
        
        self.target_x = self.x + math.cos(angle) * distance
        self.target_y = self.y + math.sin(angle) * distance
    
    def take_damage(self, damage: int) -> bool:
        """Take damage and return True if enemy dies"""
        self.health -= damage
        if self.health <= 0:
            self.health = 0
            self.state = EnemyState.DEAD
            return True
        return False
    
    def get_rect(self) -> pygame.Rect:
        """Get enemy collision rectangle"""
        return pygame.Rect(int(self.x - self.size/2), int(self.y - self.size/2), self.size, self.size)
    
    def get_distance_to_player(self, player_pos: Tuple[float, float]) -> float:
        """Get distance to player"""
        dx = player_pos[0] - self.x
        dy = player_pos[1] - self.y
        return math.sqrt(dx * dx + dy * dy)
    
    def render(self, screen: pygame.Surface, camera_offset: Tuple[float, float]):
        """Render the enemy"""
        if self.state == EnemyState.DEAD:
            return
        
        # Calculate render position
        render_x = int(self.x - camera_offset[0] - self.size/2)
        render_y = int(self.y - camera_offset[1] - self.size/2)
        
        # Apply spawn animation
        if self.state == EnemyState.SPAWNING:
            alpha = 1.0 - (self.spawn_timer / self.spawn_duration)
            size = int(self.size * alpha)
            if size > 0:
                render_x += (self.size - size) // 2
                render_y += (self.size - size) // 2
            else:
                return
        else:
            size = self.size
        
        # Draw enemy
        pygame.draw.circle(screen, self.color, (render_x + size//2, render_y + size//2), size//2)
        
        # Draw health bar if damaged
        if self.health < self.max_health:
            health_bar_width = size
            health_bar_height = 4
            health_percentage = self.health / self.max_health
            
            # Background (red)
            pygame.draw.rect(screen, (255, 0, 0), 
                           (render_x, render_y - 8, health_bar_width, health_bar_height))
            
            # Health (green)
            pygame.draw.rect(screen, (0, 255, 0), 
                           (render_x, render_y - 8, int(health_bar_width * health_percentage), health_bar_height))
        
        # Draw detection range for debugging (optional)
        if False:  # Set to True for debugging
            detection_radius = int(self.detection_range)
            pygame.draw.circle(screen, (255, 255, 0, 100), (render_x + size//2, render_y + size//2), detection_radius, 1)

class EnemySpawner:
    """Manages enemy spawning"""
    
    def __init__(self, map_width: int, map_height: int):
        """Initialize enemy spawner"""
        self.map_width = map_width
        self.map_height = map_height
        self.enemies: List[Enemy] = []
        
        # Spawning properties
        self.spawn_timer = 0.0
        self.spawn_interval = 2.0  # Spawn every 2 seconds
        self.max_enemies = 50
        self.spawn_distance_from_player = 300.0
        
        # Wave management
        self.wave_timer = 0.0
        self.wave_duration = 30.0  # 30 second waves
        self.current_wave = 1
        self.enemies_per_wave = 5
        
    def update(self, dt: float, player_pos: Tuple[float, float]):
        """Update enemy spawner"""
        self.spawn_timer += dt
        self.wave_timer += dt
        
        # Check for wave completion
        if self.wave_timer >= self.wave_duration:
            self.wave_timer = 0.0
            self.current_wave += 1
            self.enemies_per_wave = min(20, self.enemies_per_wave + 2)  # Increase difficulty
        
        # Spawn enemies
        if (self.spawn_timer >= self.spawn_interval and 
            len(self.enemies) < self.max_enemies and
            self.wave_timer < self.wave_duration):
            
            self.spawn_timer = 0.0
            
            # Spawn multiple enemies based on wave
            for _ in range(min(3, self.enemies_per_wave - len(self.enemies))):
                self._spawn_enemy(player_pos)
        
        # Update all enemies
        for enemy in self.enemies[:]:  # Use slice to avoid modification during iteration
            enemy.update(dt, player_pos)
            
            # Remove dead enemies
            if enemy.state == EnemyState.DEAD:
                self.enemies.remove(enemy)
    
    def _spawn_enemy(self, player_pos: Tuple[float, float]):
        """Spawn a new enemy"""
        # Calculate spawn position (away from player)
        angle = random.uniform(0, 2 * math.pi)
        distance = random.uniform(self.spawn_distance_from_player, self.spawn_distance_from_player + 100)
        
        spawn_x = player_pos[0] + math.cos(angle) * distance
        spawn_y = player_pos[1] + math.sin(angle) * distance
        
        # Clamp to map bounds
        spawn_x = max(50, min(self.map_width - 50, spawn_x))
        spawn_y = max(50, min(self.map_height - 50, spawn_y))
        
        # Choose enemy type based on wave
        if self.current_wave <= 2:
            enemy_type = EnemyType.BASIC
        elif self.current_wave <= 4:
            enemy_type = random.choice([EnemyType.BASIC, EnemyType.FAST])
        else:
            enemy_type = random.choice(list(EnemyType))
        
        # Create and add enemy
        enemy = Enemy(spawn_x, spawn_y, enemy_type)
        self.enemies.append(enemy)
    
    def get_enemies_in_range(self, center: Tuple[float, float], radius: float) -> List[Enemy]:
        """Get all enemies within a certain range"""
        enemies_in_range = []
        for enemy in self.enemies:
            if enemy.get_distance_to_player(center) <= radius:
                enemies_in_range.append(enemy)
        return enemies_in_range
    
    def render(self, screen: pygame.Surface, camera_offset: Tuple[float, float]):
        """Render all enemies"""
        for enemy in self.enemies:
            enemy.render(screen, camera_offset)
    
    def get_enemy_count(self) -> int:
        """Get current enemy count"""
        return len(self.enemies)
    
    def get_current_wave(self) -> int:
        """Get current wave number"""
        return self.current_wave
