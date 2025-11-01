"""Health and damage system for entities"""
import pygame


class HealthSystem:
    """Manages entity health, damage, and death"""
    
    def __init__(self, max_health=100):
        """
        Initialize health system
        
        Args:
            max_health: Maximum health points
        """
        self.max_health = max_health
        self.health = self.max_health
        self.is_alive = True
    
    def take_damage(self, amount):
        """
        Apply damage to entity
        
        Args:
            amount: Damage amount
            
        Returns:
            int: Actual damage dealt
        """
        if not self.is_alive:
            return 0
        
        actual_damage = min(amount, self.health)
        self.health -= amount
        
        if self.health <= 0:
            self.health = 0
            self.is_alive = False
        
        return actual_damage
    
    def heal(self, amount):
        """
        Heal entity
        
        Args:
            amount: Healing amount
        """
        if not self.is_alive:
            return
        
        self.health = min(self.health + amount, self.max_health)
    
    def get_health_percentage(self):
        """Get health as percentage (0.0 to 1.0)"""
        return self.health / self.max_health if self.max_health > 0 else 0.0
    
    def is_at_full_health(self):
        """Check if entity is at full health"""
        return self.health >= self.max_health
    
    def is_dead(self):
        """Check if entity is dead"""
        return not self.is_alive
