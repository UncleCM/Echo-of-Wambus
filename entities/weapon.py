"""
Weapon system for Vampire Survivors style gameplay
"""

import pygame
import math
import random
from typing import List, Tuple, Optional
from enum import Enum

class WeaponType(Enum):
    """Types of weapons"""
    BASIC_GUN = "basic_gun"
    SHOTGUN = "shotgun"
    LASER = "laser"

class Projectile:
    """Projectile class for weapons"""
    
    def __init__(self, x: float, y: float, velocity_x: float, velocity_y: float, 
                 damage: int = 10, lifetime: float = 3.0, size: int = 4):
        """Initialize projectile"""
        self.x = x
        self.y = y
        self.velocity_x = velocity_x
        self.velocity_y = velocity_y
        self.damage = damage
        self.lifetime = lifetime
        self.max_lifetime = lifetime
        self.size = size
        self.active = True
        
        # Visual properties
        self.color = (255, 255, 0)  # Yellow
        
    def update(self, dt: float):
        """Update projectile"""
        if not self.active:
            return
            
        # Update position
        self.x += self.velocity_x * dt
        self.y += self.velocity_y * dt
        
        # Update lifetime
        self.lifetime -= dt
        if self.lifetime <= 0:
            self.active = False
    
    def get_rect(self) -> pygame.Rect:
        """Get projectile collision rectangle"""
        return pygame.Rect(int(self.x - self.size/2), int(self.y - self.size/2), self.size, self.size)
    
    def render(self, screen: pygame.Surface, camera_offset: Tuple[float, float]):
        """Render projectile"""
        if not self.active:
            return
            
        # Calculate render position
        render_x = int(self.x - camera_offset[0] - self.size/2)
        render_y = int(self.y - camera_offset[1] - self.size/2)
        
        # Fade out based on lifetime
        alpha = self.lifetime / self.max_lifetime
        color = tuple(int(c * alpha) for c in self.color)
        
        # Draw projectile
        pygame.draw.circle(screen, color, (render_x + self.size//2, render_y + self.size//2), self.size//2)

class Weapon:
    """Base weapon class"""
    
    def __init__(self, weapon_type: WeaponType):
        """Initialize weapon"""
        self.weapon_type = weapon_type
        self.projectiles: List[Projectile] = []
        
        # Weapon properties
        self.damage = 10
        self.fire_rate = 1.0  # Shots per second
        self.range = 300.0
        self.projectile_speed = 400.0
        
        # Firing state
        self.last_shot_time = 0.0
        self.auto_fire = True
        
        # Weapon-specific properties
        if weapon_type == WeaponType.BASIC_GUN:
            self.damage = 10
            self.fire_rate = 2.0
            self.projectile_speed = 500.0
        elif weapon_type == WeaponType.SHOTGUN:
            self.damage = 8
            self.fire_rate = 0.8
            self.projectile_speed = 400.0
            self.pellets = 5
        elif weapon_type == WeaponType.LASER:
            self.damage = 15
            self.fire_rate = 3.0
            self.projectile_speed = 800.0
    
    def can_fire(self, current_time: float) -> bool:
        """Check if weapon can fire"""
        return (current_time - self.last_shot_time) >= (1.0 / self.fire_rate)
    
    def fire(self, x: float, y: float, target_x: float, target_y: float, current_time: float):
        """Fire weapon towards target"""
        if not self.can_fire(current_time):
            return
            
        self.last_shot_time = current_time
        
        # Calculate direction to target
        dx = target_x - x
        dy = target_y - y
        distance = math.sqrt(dx * dx + dy * dy)
        
        if distance == 0:
            return
            
        # Normalize direction
        dir_x = dx / distance
        dir_y = dy / distance
        
        if self.weapon_type == WeaponType.SHOTGUN:
            # Fire multiple pellets with spread
            for i in range(self.pellets):
                # Add random spread
                spread_angle = random.uniform(-0.3, 0.3)  # ±17 degrees
                cos_spread = math.cos(spread_angle)
                sin_spread = math.sin(spread_angle)
                
                # Rotate direction vector
                pellet_dir_x = dir_x * cos_spread - dir_y * sin_spread
                pellet_dir_y = dir_x * sin_spread + dir_y * cos_spread
                
                # Create projectile
                velocity_x = pellet_dir_x * self.projectile_speed
                velocity_y = pellet_dir_y * self.projectile_speed
                
                projectile = Projectile(x, y, velocity_x, velocity_y, self.damage, 
                                      self.range / self.projectile_speed, 3)
                projectile.color = (255, 100, 0)  # Orange for shotgun
                self.projectiles.append(projectile)
        else:
            # Single projectile
            velocity_x = dir_x * self.projectile_speed
            velocity_y = dir_y * self.projectile_speed
            
            projectile = Projectile(x, y, velocity_x, velocity_y, self.damage,
                                  self.range / self.projectile_speed, 4)
            
            # Set color based on weapon type
            if self.weapon_type == WeaponType.LASER:
                projectile.color = (0, 255, 255)  # Cyan for laser
                projectile.size = 6
            
            self.projectiles.append(projectile)
    
    def update(self, dt: float):
        """Update weapon and projectiles"""
        # Update all projectiles
        for projectile in self.projectiles[:]:  # Use slice to avoid modification during iteration
            projectile.update(dt)
            if not projectile.active:
                self.projectiles.remove(projectile)
    
    def render(self, screen: pygame.Surface, camera_offset: Tuple[float, float]):
        """Render all projectiles"""
        for projectile in self.projectiles:
            projectile.render(screen, camera_offset)
    
    def get_active_projectiles(self) -> List[Projectile]:
        """Get all active projectiles"""
        return [p for p in self.projectiles if p.active]

class WeaponSystem:
    """Manages all weapons for the player"""
    
    def __init__(self):
        """Initialize weapon system"""
        self.weapons: List[Weapon] = []
        self.current_weapon_index = 0
        self.auto_target = True
        
        # Add starting weapon
        self.add_weapon(WeaponType.BASIC_GUN)
        
        # Targeting
        self.target_x = 0.0
        self.target_y = 0.0
        
    def add_weapon(self, weapon_type: WeaponType):
        """Add a new weapon"""
        weapon = Weapon(weapon_type)
        self.weapons.append(weapon)
    
    def switch_weapon(self, direction: int = 1):
        """Switch to next/previous weapon"""
        if len(self.weapons) <= 1:
            return
            
        self.current_weapon_index = (self.current_weapon_index + direction) % len(self.weapons)
    
    def get_current_weapon(self) -> Optional[Weapon]:
        """Get current weapon"""
        if self.weapons:
            return self.weapons[self.current_weapon_index]
        return None
    
    def update_target(self, player_x: float, player_y: float, mouse_pos: Tuple[int, int]):
        """Update weapon target based on mouse position"""
        # Convert mouse position to world coordinates
        # This is a simplified version - in a real game you'd use proper screen-to-world conversion
        self.target_x = mouse_pos[0] + player_x - 512  # Approximate conversion
        self.target_y = mouse_pos[1] + player_y - 384
    
    def auto_target_nearest_enemy(self, player_x: float, player_y: float, enemies):
        """Automatically target nearest enemy"""
        if not enemies:
            return
            
        nearest_enemy = None
        nearest_distance = float('inf')
        
        for enemy in enemies:
            if enemy.state.value != "dead":
                distance = enemy.get_distance_to_player((player_x, player_y))
                if distance < nearest_distance and distance <= 400:  # Within targeting range
                    nearest_distance = distance
                    nearest_enemy = enemy
        
        if nearest_enemy:
            self.target_x = nearest_enemy.x
            self.target_y = nearest_enemy.y
    
    def fire_current_weapon(self, player_x: float, player_y: float, current_time: float):
        """Fire current weapon"""
        current_weapon = self.get_current_weapon()
        if current_weapon:
            current_weapon.fire(player_x, player_y, self.target_x, self.target_y, current_time)
    
    def update(self, dt: float):
        """Update all weapons"""
        for weapon in self.weapons:
            weapon.update(dt)
    
    def render(self, screen: pygame.Surface, camera_offset: Tuple[float, float]):
        """Render all weapons"""
        for weapon in self.weapons:
            weapon.render(screen, camera_offset)
    
    def check_collisions(self, enemies) -> List[Tuple[Projectile, 'Enemy']]:
        """Check collisions between projectiles and enemies"""
        collisions = []
        
        for weapon in self.weapons:
            for projectile in weapon.get_active_projectiles():
                for enemy in enemies:
                    if enemy.state.value != "dead":
                        # Check collision
                        if projectile.get_rect().colliderect(enemy.get_rect()):
                            collisions.append((projectile, enemy))
                            projectile.active = False
                            break  # Projectile can only hit one enemy
        
        return collisions
