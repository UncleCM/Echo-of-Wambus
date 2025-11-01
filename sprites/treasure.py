"""
Treasure and chest sprites
"""

from Settings import *
import math


class Treasure(pygame.sprite.Sprite):
    """Gold treasure collectible - main objective"""
    def __init__(self, pos, groups):
        super().__init__(groups)
        
        # Create treasure visual (golden circle with glow effect)
        size = 32
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        
        # Draw golden treasure
        pygame.draw.circle(self.image, (255, 215, 0), (size//2, size//2), size//3)  # Gold
        pygame.draw.circle(self.image, (255, 255, 100), (size//2, size//2), size//4)  # Shine
        
        self.rect = self.image.get_rect(center=pos)
        self.hitbox_rect = self.rect.inflate(-8, -8)  # Smaller hitbox
        
        # Animation
        self.collected = False
        self.glow_timer = 0
        self.glow_speed = 3
    
    def update(self, dt):
        """Animate glowing effect"""
        if not self.collected:
            self.glow_timer += dt * self.glow_speed
            
            # Pulsing glow effect
            glow_alpha = int(128 + 127 * abs(math.sin(self.glow_timer)))
            
            # Redraw with pulsing glow
            size = 32
            self.image = pygame.Surface((size, size), pygame.SRCALPHA)
            
            # Outer glow
            glow_surf = pygame.Surface((size, size), pygame.SRCALPHA)
            pygame.draw.circle(glow_surf, (255, 215, 0, glow_alpha), (size//2, size//2), size//2)
            self.image.blit(glow_surf, (0, 0))
            
            # Core treasure
            pygame.draw.circle(self.image, (255, 215, 0), (size//2, size//2), size//3)
            pygame.draw.circle(self.image, (255, 255, 100), (size//2, size//2), size//4)
    
    def collect(self):
        """Collect treasure and remove from world"""
        self.collected = True
        self.kill()
        print("[Treasure] Collected! Find the exit!")


class TreasureChest(pygame.sprite.Sprite):
    """Treasure chest - can be real treasure or mimic"""
    def __init__(self, pos, groups, chest_id, is_mimic=False, prolog_engine=None):
        super().__init__(groups)
        
        self.chest_id = chest_id
        self.is_mimic = is_mimic
        self.prolog = prolog_engine
        self.opened = False
        self.pos = pygame.math.Vector2(pos)
        
        # Animation - initialize before creating image
        self.glow_timer = 0
        self.glow_speed = 2
        self.shake_amount = 0  # For mimic shaking
        
        # Create chest visual
        self.create_chest_image()
        
        self.rect = self.image.get_rect(center=pos)
        self.hitbox_rect = self.rect.inflate(-10, -10)
    
    def create_chest_image(self):
        """Draw a treasure chest"""
        size = 48
        self.image = pygame.Surface((size, size), pygame.SRCALPHA)
        
        if self.opened:
            # Opened chest (empty or with gold)
            if not self.is_mimic:
                # Show gold inside
                pygame.draw.rect(self.image, (139, 69, 19), (8, 24, 32, 20))  # Chest body
                pygame.draw.rect(self.image, (101, 67, 33), (8, 20, 32, 4))   # Lock area
                pygame.draw.rect(self.image, (255, 215, 0), (18, 28, 12, 12)) # Gold!
            else:
                # Empty (was mimic)
                pygame.draw.rect(self.image, (139, 69, 19), (8, 24, 32, 20))
                pygame.draw.rect(self.image, (101, 67, 33), (8, 20, 32, 4))
        else:
            # Closed chest
            pygame.draw.rect(self.image, (139, 69, 19), (8, 24, 32, 20))  # Chest body
            pygame.draw.rect(self.image, (101, 67, 33), (8, 16, 32, 12))  # Chest lid
            pygame.draw.rect(self.image, (218, 165, 32), (22, 26, 4, 6))  # Lock
            
            # Add glow for unopened chests
            if not self.opened:
                glow_alpha = int(100 + 50 * abs(math.sin(self.glow_timer)))
                glow_color = (255, 215, 0, glow_alpha) if not self.is_mimic else (255, 100, 100, glow_alpha)
                pygame.draw.circle(self.image, glow_color, (size//2, size//2), size//2 + 4, 2)
    
    def update(self, dt):
        """Animate chest"""
        if not self.opened:
            self.glow_timer += dt * self.glow_speed
            
            # Mimics shake slightly
            if self.is_mimic:
                self.shake_amount = math.sin(self.glow_timer * 10) * 1
            
            self.create_chest_image()
            
            # Apply shake offset
            if self.shake_amount != 0:
                self.rect.x = int(self.pos.x + self.shake_amount)
    
    def open(self, game_instance):
        """
        Open the chest
        Returns: 'treasure' if real, 'mimic' if fake
        """
        if self.opened:
            return 'already_opened'
        
        self.opened = True
        
        if self.prolog and self.prolog.available:
            # Use Prolog to handle chest opening
            result = self.prolog.open_chest(self.pos.x, self.pos.y)
            
            if result == 'treasure_found':
                print(f"[Chest {self.chest_id}] Real treasure found!")
                self.create_chest_image()
                return 'treasure'
            elif result == 'mimic_activated':
                print(f"[Chest {self.chest_id}] MIMIC! Spawning Wumpus!")
                self.create_chest_image()
                # Prolog already spawned Wumpus
                return 'mimic'
        else:
            # Fallback without Prolog
            if not self.is_mimic:
                print(f"[Chest {self.chest_id}] Real treasure!")
                self.create_chest_image()
                return 'treasure'
            else:
                print(f"[Chest {self.chest_id}] MIMIC!")
                self.create_chest_image()
                return 'mimic'
        
        return 'no_chest'
