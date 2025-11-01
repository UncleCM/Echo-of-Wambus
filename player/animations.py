"""Player animation system"""
import pygame
from os.path import join


class PlayerAnimations:
    """Manages player sprite animations"""
    
    def __init__(self, player=None):
        """
        Initialize and load all animations
        
        Args:
            player: Reference to parent Player entity (for load_sprite_strip)
        """
        self.player = player
        self.animations = {}
        self.current_animation = "idle_down"
        self.previous_animation = "idle_down"
        self.animation_timer = 0
        self.animation_speed = 12  # FPS for animations
        
        # Load all animations (defer if player not set yet)
        if player:
            self._load_all_animations()
    
    def initialize(self, player):
        """
        Initialize animations after player is set
        
        Args:
            player: Reference to parent Player entity
        """
        self.player = player
        self._load_all_animations()
    
    def _load_all_animations(self):
        """Load all animation frames from sprite strip files"""
        # Idle animations
        self.animations["idle_down"] = self._load_sprite_strip("Player/Idle/Idle_Down.png")
        self.animations["idle_up"] = self._load_sprite_strip("Player/Idle/Idle_Up.png")
        self.animations["idle_left"] = self._load_sprite_strip("Player/Idle/Idle_Left_Down.png")
        self.animations["idle_right"] = self._load_sprite_strip("Player/Idle/Idle_Right_Down.png")
        self.animations["idle_left_up"] = self._load_sprite_strip("Player/Idle/Idle_Left_Up.png")
        self.animations["idle_right_up"] = self._load_sprite_strip("Player/Idle/Idle_Right_Up.png")

        # Walk animations
        self.animations["walk_down"] = self._load_sprite_strip("Player/Walk/walk_Down.png")
        self.animations["walk_up"] = self._load_sprite_strip("Player/Walk/walk_Up.png")
        self.animations["walk_left_down"] = self._load_sprite_strip("Player/Walk/walk_Left_Down.png")
        self.animations["walk_right_down"] = self._load_sprite_strip("Player/Walk/walk_Right_Down.png")
        self.animations["walk_left_up"] = self._load_sprite_strip("Player/Walk/walk_Left_Up.png")
        self.animations["walk_right_up"] = self._load_sprite_strip("Player/Walk/walk_Right_Up.png")

        # Aliases for simpler left/right
        self.animations["walk_left"] = self.animations["walk_left_down"]
        self.animations["walk_right"] = self.animations["walk_right_down"]

        # Dash animations (optional)
        try:
            self.animations["dash"] = self._load_sprite_strip("Player/Dash/Dash.png")
        except Exception:
            self.animations["dash"] = self.animations["idle_down"]

        # Death animations (optional)
        try:
            self.animations["death"] = self._load_sprite_strip("Player/Death/Death.png")
        except Exception:
            self.animations["death"] = self.animations["idle_down"]

        print(f"Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")
    
    def _load_sprite_strip(self, path):
        """
        Load a horizontal sprite strip using player's Entity method
        
        Args:
            path: Relative path from assets folder
            
        Returns:
            list: List of pygame.Surface frames
        """
        if self.player and hasattr(self.player, 'load_sprite_strip'):
            try:
                # Use Entity's load_sprite_strip method (handles frame size correctly)
                return self.player.load_sprite_strip(path)
            except Exception as e:
                # Handle errors (e.g., no video mode in tests)
                print(f"[PlayerAnimations] Error loading {path}: {e}")
                return []
        else:
            # Fallback: return empty list
            print(f"[PlayerAnimations] Cannot load {path}: player not set")
            return []
    
    def get_animation_for_state(self, is_moving, direction, facing):
        """
        Determine which animation to play based on player state
        
        Args:
            is_moving: bool - Is player moving?
            direction: Vector2 - Movement direction
            facing: str - Direction player is facing
            
        Returns:
            str: Animation name
        """
        if is_moving:
            # Moving - determine animation from direction
            if direction.y > 0 and direction.x < 0:
                return "walk_left_down"
            elif direction.y > 0 and direction.x > 0:
                return "walk_right_down"
            elif direction.y < 0 and direction.x < 0:
                return "walk_left_up"
            elif direction.y < 0 and direction.x > 0:
                return "walk_right_up"
            elif direction.x < 0:
                return "walk_left_down"
            elif direction.x > 0:
                return "walk_right_down"
            elif direction.y < 0:
                return "walk_up"
            else:
                return "walk_down"
        else:
            # Idle - match facing direction
            idle_map = {
                "left": "idle_left",
                "right": "idle_right",
                "up": "idle_up",
                "down": "idle_down",
                "left_up": "idle_left_up",
                "right_up": "idle_right_up",
                "left_down": "idle_left",
                "right_down": "idle_right",
            }
            return idle_map.get(facing, "idle_down")
    
    def update(self, dt, direction, facing):
        """
        Update animation based on movement
        
        Args:
            dt: Delta time
            direction: Vector2 - Current movement direction
            facing: str - Direction player is facing
            
        Returns:
            pygame.Surface: Current animation frame
        """
        # Determine which animation to use
        is_moving = direction.magnitude() > 0
        self.current_animation = self.get_animation_for_state(is_moving, direction, facing)

        # Reset timer if animation changed
        if self.current_animation != self.previous_animation:
            self.animation_timer = 0
            self.previous_animation = self.current_animation

        # Get current animation frames
        animation_frames = self.animations.get(self.current_animation, [])
        
        if not animation_frames:
            # Return placeholder if no frames
            placeholder = pygame.Surface((96, 128), pygame.SRCALPHA)
            pygame.draw.circle(placeholder, (100, 150, 255), (48, 64), 30)
            return placeholder

        # Update animation timer
        self.animation_timer += dt

        # Calculate frame index
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)

        return animation_frames[frame_index]
