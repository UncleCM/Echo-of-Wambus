"""
Player entity for the Wumpus game
Handles player movement, animation, and rendering
"""

import pygame
import os
from typing import Dict, List, Optional, Tuple
from enum import Enum
from .weapon import WeaponSystem
from utils.sprite_sheet import PlayerSpriteSheet, AnimationManager

class PlayerState(Enum):
    """Player animation states"""
    IDLE = "idle"
    WALK = "walk"
    DASH = "dash"
    JUMP = "jump"
    DEATH = "death"

class Direction(Enum):
    """Player facing directions for 2.5D top-down view"""
    DOWN = "Down"
    LEFT = "Left"
    RIGHT = "Right"
    UP = "Up"
    LEFT_DOWN = "Left_Down"
    LEFT_UP = "Left_Up"
    RIGHT_DOWN = "Right_Down"
    RIGHT_UP = "Right_Up"

class Player:
    """Player character with sprite animation support"""
    
    def __init__(self, x: float, y: float, sprite_path: str = "assets/Player"):
        """Initialize the player at given coordinates"""
        self.x = x
        self.y = y
        self.width = 48
        self.height = 64
        
        # Movement properties
        self.speed = 200.0  # pixels per second
        self.dash_speed = 400.0
        self.velocity_x = 0.0
        self.velocity_y = 0.0
        
        # Animation properties
        self.current_state = PlayerState.IDLE
        self.current_direction = Direction.DOWN
        self.animation_timer = 0.0
        self.animation_speed = 8.0  # frames per second
        self.current_frame = 0
        
        # Sprite sheet loading
        self.sprite_path = sprite_path
        self.animation_manager = AnimationManager()
        self.load_sprite_sheets()
        
        # Player state
        self.is_dashing = False
        self.dash_cooldown = 0.0
        self.dash_duration = 0.2  # seconds
        self.dash_timer = 0.0
        
        # Weapon system
        self.weapon_system = WeaponSystem()
        self.auto_fire = True
        
    def load_sprite_sheets(self):
        """Load player sprite sheets"""
        try:
            # Load ALL available sprite sheet files (384x64 = 8 frames of 48x64)
            animation_paths = {
                # Idle animations - all directions
                "idle_down": "assets/Player/Idle/Idle_Down.png",
                "idle_up": "assets/Player/Idle/Idle_Up.png", 
                "idle_left_down": "assets/Player/Idle/Idle_Left_Down.png",
                "idle_left_up": "assets/Player/Idle/Idle_Left_Up.png",
                "idle_right_down": "assets/Player/Idle/Idle_Right_Down.png",
                "idle_right_up": "assets/Player/Idle/Idle_Right_Up.png",
                
                # Walk animations - all directions
                "walk_down": "assets/Player/Walk/walk_Down.png",
                "walk_up": "assets/Player/Walk/walk_Up.png",
                "walk_left_down": "assets/Player/Walk/walk_Left_Down.png",
                "walk_left_up": "assets/Player/Walk/walk_Left_Up.png",
                "walk_right_down": "assets/Player/Walk/walk_Right_Down.png",
                "walk_right_up": "assets/Player/Walk/walk_Right_Up.png",
                
                # Dash animations - all directions
                "dash_down": "assets/Player/Dash/Dash_Down.png",
                "dash_up": "assets/Player/Dash/Dash_Up.png",
                "dash_left_down": "assets/Player/Dash/Dash_Left_Down.png",
                "dash_left_up": "assets/Player/Dash/Dash_Left_Up.png",
                "dash_right_down": "assets/Player/Dash/Dash_Right_Down.png",
                "dash_right_up": "assets/Player/Dash/Dash_Right_Up.png",
                
                # Jump animations - all directions
                "jump_down": "assets/Player/Jump - NEW/Normal/Jump_Down.png",
                "jump_up": "assets/Player/Jump - NEW/Normal/Jump_up.png",
                "jump_left_down": "assets/Player/Jump - NEW/Normal/Jump_Left_Down.png",
                "jump_left_up": "assets/Player/Jump - NEW/Normal/Jump_Left_Up.png",
                "jump_right_down": "assets/Player/Jump - NEW/Normal/Jump_Right_Down.png",
                "jump_right_up": "assets/Player/Jump - NEW/Normal/Jump_Right_Up.png",
                
                # Death animations - all directions
                "death_down": "assets/Player/Death/death_Down.png",
                "death_up": "assets/Player/Death/death_Up.png",
                "death_left_down": "assets/Player/Death/death_Left_Down.png",
                "death_left_up": "assets/Player/Death/death_Left_Up.png",
                "death_right_down": "assets/Player/Death/death_Right_Down.png",
                "death_right_up": "assets/Player/Death/death_Right_Up.png",
            }
            
            # Load each animation sprite sheet
            sheets_loaded = 0
            for animation_name, sheet_path in animation_paths.items():
                if os.path.exists(sheet_path):
                    print(f"Loading animation: {animation_name} from {sheet_path}")
                    # Create sprite sheet with 48x64 frames, 8 frames horizontally (384/48 = 8)
                    sprite_sheet = PlayerSpriteSheet(sheet_path)
                    sprite_sheet.frame_width = 48
                    sprite_sheet.frame_height = 64
                    sprite_sheet.cols = 8  # 384/48 = 8 frames
                    sprite_sheet.rows = 1
                    sprite_sheet.frame_count = 8
                    
                    # Re-extract frames with correct dimensions
                    sprite_sheet.frames = []
                    for col in range(8):
                        x = col * 48
                        y = 0
                        frame_rect = pygame.Rect(x, y, 48, 64)
                        frame = sprite_sheet.sheet.subsurface(frame_rect)
                        sprite_sheet.frames.append(frame)
                    
                    self.animation_manager.add_animation(animation_name, sprite_sheet)
                    sheets_loaded += 1
            
            if sheets_loaded > 0:
                print(f"Loaded {sheets_loaded} animation sprite sheets")
                print("Available animations:", list(self.animation_manager.animations.keys()))
                # Set default animation
                self.animation_manager.set_animation("idle_down")
            else:
                print("No sprite sheets found, using fallback sprites")
                self._create_fallback_sprites()
                
        except Exception as e:
            print(f"Error loading sprite sheets: {e}")
            self._create_fallback_sprites()
    
    def _create_fallback_sprites(self):
        """Create simple colored rectangles as fallback sprites"""
        # Create a simple fallback animation
        fallback_frames = []
        colors = [(100, 255, 100), (120, 255, 120), (100, 255, 100), (80, 255, 80)]  # Pulsing green
        
        for color in colors:
            sprite = pygame.Surface((self.width, self.height))
            sprite.fill(color)
            pygame.draw.rect(sprite, (255, 255, 255), (0, 0, self.width, self.height), 2)
            fallback_frames.append(sprite)
        
        # Add to animation manager
        from utils.sprite_sheet import PlayerSpriteSheet
        fallback_sheet = PlayerSpriteSheet.__new__(PlayerSpriteSheet)
        fallback_sheet.frames = fallback_frames
        fallback_sheet.frame_count = len(fallback_frames)
        self.animation_manager.add_animation("main", fallback_sheet)
        self.animation_manager.set_animation("main")
    
    def update(self, dt: float, keys_pressed: Dict[int, bool]):
        """Update player state and position"""
        self.animation_timer += dt
        
        # Handle dash cooldown
        if self.dash_cooldown > 0:
            self.dash_cooldown -= dt
        
        # Handle dash duration
        if self.is_dashing:
            self.dash_timer -= dt
            if self.dash_timer <= 0:
                self.is_dashing = False
        
        # Handle input
        self._handle_input(keys_pressed, dt)
        
        # Update position
        self.x += self.velocity_x * dt
        self.y += self.velocity_y * dt
        
        # Update animation
        self._update_animation(dt)
        self.animation_manager.update(dt)
        
        # Update weapon system
        self.weapon_system.update(dt)
    
    def _handle_input(self, keys_pressed: Dict[int, bool], dt: float):
        """Handle player input for 2.5D top-down movement"""
        # Reset velocity
        self.velocity_x = 0
        self.velocity_y = 0
        
        # Movement input - 8-directional movement
        move_speed = self.dash_speed if self.is_dashing else self.speed
        
        # Check for movement keys
        left = keys_pressed.get(pygame.K_LEFT) or keys_pressed.get(pygame.K_a)
        right = keys_pressed.get(pygame.K_RIGHT) or keys_pressed.get(pygame.K_d)
        up = keys_pressed.get(pygame.K_UP) or keys_pressed.get(pygame.K_w)
        down = keys_pressed.get(pygame.K_DOWN) or keys_pressed.get(pygame.K_s)
        
        # Calculate movement vectors
        if left:
            self.velocity_x = -move_speed
        if right:
            self.velocity_x = move_speed
        if up:
            self.velocity_y = -move_speed
        if down:
            self.velocity_y = move_speed
        
        # Normalize diagonal movement (so diagonal speed = horizontal/vertical speed)
        if self.velocity_x != 0 and self.velocity_y != 0:
            self.velocity_x *= 0.707  # 1/sqrt(2)
            self.velocity_y *= 0.707
        
        # Determine facing direction based on movement
        if self.velocity_x != 0 or self.velocity_y != 0:
            if not self.is_dashing:
                self.current_state = PlayerState.WALK
            
            # Set direction based on primary movement
            if abs(self.velocity_x) > abs(self.velocity_y):
                # Horizontal movement is primary
                if self.velocity_x > 0:
                    self.current_direction = Direction.RIGHT
                else:
                    self.current_direction = Direction.LEFT
            elif abs(self.velocity_y) > abs(self.velocity_x):
                # Vertical movement is primary
                if self.velocity_y > 0:
                    self.current_direction = Direction.DOWN
                else:
                    self.current_direction = Direction.UP
            else:
                # Diagonal movement
                if self.velocity_x > 0 and self.velocity_y > 0:
                    self.current_direction = Direction.RIGHT_DOWN
                elif self.velocity_x > 0 and self.velocity_y < 0:
                    self.current_direction = Direction.RIGHT_UP
                elif self.velocity_x < 0 and self.velocity_y > 0:
                    self.current_direction = Direction.LEFT_DOWN
                elif self.velocity_x < 0 and self.velocity_y < 0:
                    self.current_direction = Direction.LEFT_UP
        
        # Dash input
        if keys_pressed.get(pygame.K_SPACE) and self.dash_cooldown <= 0 and not self.is_dashing:
            self.is_dashing = True
            self.dash_timer = self.dash_duration
            self.dash_cooldown = 1.0  # 1 second cooldown
            self.current_state = PlayerState.DASH
        
        # If no movement input and not dashing, set to idle
        if (self.velocity_x == 0 and self.velocity_y == 0 and 
            not self.is_dashing):
            self.current_state = PlayerState.IDLE
        
        # Weapon controls
        if keys_pressed.get(pygame.K_q):
            self.weapon_system.switch_weapon(-1)  # Previous weapon
        if keys_pressed.get(pygame.K_e):
            self.weapon_system.switch_weapon(1)   # Next weapon
    
    def _update_animation(self, dt: float):
        """Update animation based on player state and direction"""
        # Create animation name based on state and direction
        state_name = self.current_state.value
        direction_name = self.current_direction.value.lower()
        
        # Map direction enum to animation names
        direction_map = {
            "down": "down",
            "up": "up", 
            "left": "left",  # Use left_down as default left
            "right": "right",  # Use right_down as default right
            "left_down": "left_down",
            "left_up": "left_up",
            "right_down": "right_down", 
            "right_up": "right_up"
        }
        
        direction_key = direction_map.get(direction_name, "down")
        animation_name = f"{state_name}_{direction_key}"
        
        # Debug: Print animation changes (only when switching)
        if not hasattr(self, '_last_animation') or self._last_animation != animation_name:
            print(f"Animation: {animation_name} (State: {state_name}, Direction: {direction_name})")
            self._last_animation = animation_name
        
        # Set the animation
        self.animation_manager.set_animation(animation_name)
        
        # For continuous looping animations (both idle and moving)
        # The animation manager will automatically loop frames 1-8
        # No need to reset frame counter - let it loop continuously
    
    def get_rect(self) -> pygame.Rect:
        """Get player collision rectangle"""
        return pygame.Rect(int(self.x), int(self.y), self.width, self.height)
    
    def render(self, screen: pygame.Surface, camera_offset: Tuple[int, int] = (0, 0)):
        """Render the player to the screen"""
        # Get current sprite from animation manager
        sprite = self.animation_manager.get_current_frame()
        
        if sprite:
            # Scale sprite to player size if needed
            if sprite.get_width() != self.width or sprite.get_height() != self.height:
                sprite = pygame.transform.scale(sprite, (self.width, self.height))
            
            # Calculate render position
            render_x = int(self.x - camera_offset[0])
            render_y = int(self.y - camera_offset[1])
            
            # Draw the sprite
            screen.blit(sprite, (render_x, render_y))
            
            # Debug: Draw collision rectangle
            if False:  # Set to True for debugging
                debug_rect = self.get_rect()
                debug_rect.x -= camera_offset[0]
                debug_rect.y -= camera_offset[1]
                pygame.draw.rect(screen, (255, 0, 0), debug_rect, 2)
    
    def fire_weapon(self, target_x: float, target_y: float, current_time: float):
        """Fire weapon towards target"""
        self.weapon_system.fire_current_weapon(self.x, self.y, current_time)
    
    def auto_fire_weapon(self, enemies, current_time: float):
        """Auto-fire weapon at nearest enemy"""
        if self.auto_fire:
            self.weapon_system.auto_target_nearest_enemy(self.x, self.y, enemies)
            self.weapon_system.fire_current_weapon(self.x, self.y, current_time)
    
    def render_weapons(self, screen: pygame.Surface, camera_offset: Tuple[float, float]):
        """Render weapon projectiles"""
        self.weapon_system.render(screen, camera_offset)
    
    def _get_current_sprite(self) -> Optional[pygame.Surface]:
        """Get the current sprite frame"""
        return self.animation_manager.get_current_frame()
    
    def set_position(self, x: float, y: float):
        """Set player position"""
        self.x = x
        self.y = y
