"""Test script to verify R key detection in game over state"""
import pygame
from main_menu import GameState

pygame.init()
screen = pygame.display.set_mode((800, 600))
pygame.display.set_caption("R Key Test")
clock = pygame.time.Clock()

game_state = GameState.GAME_OVER
running = True
font = pygame.font.Font(None, 36)

print("Game started in GAME_OVER state")
print("Press R to test restart, ESC to quit")

while running:
    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            running = False
        
        if event.type == pygame.KEYDOWN:
            print(f"Key pressed: {event.key} (pygame.K_r = {pygame.K_r})")
            
            if game_state in (GameState.GAME_OVER, GameState.VICTORY):
                print(f"In GAME_OVER/VICTORY state, checking key...")
                if event.key == pygame.K_r:
                    print("✅ R KEY DETECTED! Restarting...")
                    game_state = GameState.PLAYING
                    print(f"New state: {game_state}")
                    pygame.time.wait(1000)
                    game_state = GameState.GAME_OVER  # Reset for testing
                elif event.key == pygame.K_ESCAPE:
                    print("ESC detected, quitting...")
                    running = False
    
    # Draw
    screen.fill((50, 50, 50))
    
    if game_state == GameState.GAME_OVER:
        text = font.render("GAME OVER - Press R to Restart", True, (255, 255, 255))
        screen.blit(text, (200, 250))
    elif game_state == GameState.PLAYING:
        text = font.render("PLAYING!", True, (0, 255, 0))
        screen.blit(text, (300, 250))
    
    pygame.display.flip()
    clock.tick(60)

pygame.quit()
print("Test ended")
