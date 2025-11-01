"""Player inventory system - Arrows and Rocks"""
from Settings import STARTING_ARROWS, MAX_ARROWS, MAX_ROCKS


class PlayerInventory:
    """Manages player's inventory (arrows and rocks)"""
    
    def __init__(self):
        """Initialize inventory with starting items"""
        # Arrow system
        self.arrows = STARTING_ARROWS
        self.max_arrows = MAX_ARROWS
        
        # Rock system
        self.rocks = 0
        self.max_rocks = MAX_ROCKS
    
    def add_arrows(self, amount=1):
        """
        Add arrows to inventory
        
        Args:
            amount: Number of arrows to add
            
        Returns:
            bool: True if arrows were added, False if inventory full
        """
        if self.arrows < self.max_arrows:
            self.arrows = min(self.arrows + amount, self.max_arrows)
            print(f"[Player] Picked up {amount} arrow(s)! Total: {self.arrows}/{self.max_arrows}")
            return True
        else:
            print(f"[Player] Arrow capacity full! ({self.max_arrows}/{self.max_arrows})")
            return False
    
    def add_rocks(self, amount=1):
        """
        Add rocks to inventory
        
        Args:
            amount: Number of rocks to add
            
        Returns:
            bool: True if rocks were added, False if inventory full
        """
        if self.rocks < self.max_rocks:
            self.rocks = min(self.rocks + amount, self.max_rocks)
            print(f"[Player] Picked up {amount} rock(s)! Total: {self.rocks}/{self.max_rocks}")
            return True
        else:
            print(f"[Player] Rock capacity full! ({self.max_rocks}/{self.max_rocks})")
            return False
    
    def use_arrow(self):
        """
        Use one arrow from inventory
        
        Returns:
            bool: True if arrow was used, False if no arrows available
        """
        if self.arrows > 0:
            self.arrows -= 1
            print(f"[Player] Shot arrow! Arrows remaining: {self.arrows}/{self.max_arrows}")
            return True
        else:
            print("[Player] No arrows left!")
            return False
    
    def use_rock(self):
        """
        Use one rock from inventory
        
        Returns:
            bool: True if rock was used, False if no rocks available
        """
        if self.rocks > 0:
            self.rocks -= 1
            print(f"[Player] Threw rock! Rocks remaining: {self.rocks}/{self.max_rocks}")
            return True
        else:
            print("[Player] No rocks left!")
            return False
