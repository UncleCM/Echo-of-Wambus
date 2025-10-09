#!/usr/bin/env python3
"""
Echo of Wambus - A real-time thinking Wumpus game
Main game file
"""

import pygame
import sys
import os
from game.game_manager import GameManager

def main():
    """Main entry point for the game"""
    pygame.init()
    
    # Game constants
    SCREEN_WIDTH = 1024
    SCREEN_HEIGHT = 768
    FPS = 60
    
    # Create the game window
    screen = pygame.display.set_mode((SCREEN_WIDTH, SCREEN_HEIGHT))
    pygame.display.set_caption("Echo of Wambus - Real-time Wumpus")
    
    # Create game manager
    game_manager = GameManager(screen, SCREEN_WIDTH, SCREEN_HEIGHT)
    
    # Game clock
    clock = pygame.time.Clock()
    
    # Main game loop
    running = True
    while running:
        dt = clock.tick(FPS) / 1000.0  # Delta time in seconds
        
        # Handle events
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
            else:
                game_manager.handle_event(event)
        
        # Update game state
        game_manager.update(dt)
        
        # Render everything
        game_manager.render()
        
        # Update display
        pygame.display.flip()
    
    pygame.quit()
    sys.exit()

if __name__ == "__main__":
    main()
