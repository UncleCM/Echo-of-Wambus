"""Wumpus enemy - Sound-based AI hunter"""
import pygame
from os.path import join
from Settings import *
from entity import Entity
from enemies.base import WumpusAIState
from enemies.wumpus_ai import WumpusAI


class Wumpus(Entity):
    """Wumpus enemy - Sound-based AI hunter with complete map knowledge"""

    def __init__(self, pos, groups, collision_sprites, prolog_engine=None, map_knowledge=None, sound_manager=None):
        # Initialize base Entity
        super().__init__(
            pos, groups, collision_sprites, prolog_engine, entity_type="wumpus"
        )

        # Core systems
        self.map_knowledge = map_knowledge  # Complete map awareness
        self.sound_manager = sound_manager  # Sound detection system

        # Wumpus-specific attributes
        self.speed = 250  # เดินเร็วกว่า player walk (200), ช้ากว่า player dash (400)
        self.chase_speed = 450  # Chase เร็วกว่า player dash!
        self.damage = 25  # Damage dealt to player
        self.max_health = 150  # More HP than player
        self.health = self.max_health

        # Combat ranges
        self.attack_range = 50  # Pixels within which Wumpus can attack

        # Hearing system (replaces vision)
        self.hearing_radius = WUMPUS_BASE_HEARING  # Base hearing (350px)
        self.chase_hearing_bonus = WUMPUS_CHASE_BONUS  # Bonus when chasing (+200px)

        # Stun mechanics (arrow combat)
        self.is_stunned = False
        self.stun_timer = 0
        self.stun_duration = ARROW_STUN_DURATION  # From Settings.py

        # Roar system
        self.roar_cooldown = WUMPUS_ROAR_COOLDOWN  # 5000ms

        # Initialize AI controller
        self.ai = WumpusAI(self)

        # Load Wumpus animations
        self.animations = self.load_animations()

        print(f"[Wumpus] Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")

        # Setup sprite and hitbox
        self.current_animation = "idle"
        initial_image = self.animations[self.current_animation][0]
        self.setup_sprite(initial_image, hitbox_inflate=(-80, -80))

        print(f"[Wumpus] Initialized at {pos}, HP: {self.health}/{self.max_health}")
        print(f"[Wumpus] Hearing: {self.hearing_radius}px (base), +{self.chase_hearing_bonus}px (chase)")
        print(f"[Wumpus] AI: Sound-based detection, map knowledge enabled")
    
    @property
    def pos(self):
        """Get current position as Vector2"""
        return pygame.math.Vector2(self.hitbox_rect.center)

    def load_animations(self):
        """Load Wumpus animation frames from sprite sheet"""
        animations = {}

        try:
            # Load the sprite sheet
            sprite_sheet = pygame.image.load(
                join("assets", "Wumpus", "Shardsoul Slayer Sprite Sheet.png")
            ).convert_alpha()
            sheet_width = sprite_sheet.get_width()
            sheet_height = sprite_sheet.get_height()

            print(f"[Wumpus] Sprite sheet size: {sheet_width}x{sheet_height}")

            # Extract frames from sprite sheet
            frames_per_row = 8
            frame_width = sheet_width // frames_per_row
            estimated_rows = 4
            frame_height = (sheet_height // estimated_rows) - 5

            print(f"[Wumpus] Calculated frame size: {frame_width}x{frame_height}")

            # Scale factor for display
            scale_factor = 2

            # Idle animation from first row (frames 0-7)
            idle_frames = []
            for i in range(frames_per_row):
                frame_rect = pygame.Rect(i * frame_width, 0, frame_width, frame_height)
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                idle_frames.append(scaled_frame)
            animations["idle"] = (
                idle_frames if idle_frames else [self.create_placeholder()]
            )

            # Walk animation from second row (frames 8-15)
            walk_frames = []
            for i in range(frames_per_row):
                frame_rect = pygame.Rect(
                    i * frame_width, frame_height, frame_width, frame_height
                )
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                walk_frames.append(scaled_frame)
            animations["walk"] = walk_frames if walk_frames else animations["idle"]

            # Attack animation from third row (frames 16-20, only 5 frames)
            attack_frames = []
            for i in range(5):  # Only 5 attack frames
                frame_rect = pygame.Rect(
                    i * frame_width, frame_height * 2, frame_width, frame_height
                )
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                attack_frames.append(scaled_frame)
            animations["attack"] = (
                attack_frames if attack_frames else animations["idle"]
            )

            # Death animation - 6 frames starting from row 4
            death_frames = []
            for i in range(6):
                # Calculate which row and column this frame is in
                row = 3 + (i // frames_per_row)
                col = i % frames_per_row
                frame_rect = pygame.Rect(
                    col * frame_width, row * frame_height, frame_width, frame_height
                )
                frame = sprite_sheet.subsurface(frame_rect).copy()
                scaled_frame = pygame.transform.scale(
                    frame, (frame_width * scale_factor, frame_height * scale_factor)
                )
                death_frames.append(scaled_frame)
            animations["death"] = death_frames if death_frames else animations["idle"]

        except Exception as e:
            print(f"[Wumpus] Warning: Could not load sprite sheet: {e}")
            import traceback
            traceback.print_exc()
            animations["idle"] = [self.create_placeholder()]
            animations["walk"] = animations["idle"]
            animations["attack"] = animations["idle"]
            animations["death"] = animations["idle"]

        return animations

    def create_placeholder(self):
        """Create a placeholder sprite for Wumpus"""
        placeholder = pygame.Surface((128, 128), pygame.SRCALPHA)
        # Red circle for Wumpus
        pygame.draw.circle(placeholder, (200, 50, 50), (64, 64), 50)
        # Eyes
        pygame.draw.circle(placeholder, (255, 255, 0), (50, 50), 8)
        pygame.draw.circle(placeholder, (255, 255, 0), (78, 50), 8)
        # Pupils
        pygame.draw.circle(placeholder, (0, 0, 0), (50, 50), 4)
        pygame.draw.circle(placeholder, (0, 0, 0), (78, 50), 4)
        return placeholder

    def ai_update(self, player_pos, dt):
        """
        Delegate AI logic to AI controller
        
        Args:
            player_pos: Player position
            dt: Delta time
        """
        self.ai.update(player_pos, dt)

    def apply_stun(self, duration=None):
        """
        Apply stun effect to Wumpus (from arrow hit).
        Wumpus freezes in place and cannot move or attack.
        """
        if duration is None:
            duration = self.stun_duration

        self.is_stunned = True
        self.stun_timer = duration
        self.direction = pygame.math.Vector2(0, 0)
        
        # Notify AI
        self.ai.apply_stun(duration)

    def take_damage(self, amount):
        """Override to handle Wumpus death"""
        damage_taken = super().take_damage(amount)

        if self.health <= 0 and self.is_alive:
            self.on_death()

        return damage_taken

    def on_death(self):
        """Handle Wumpus death"""
        super().on_death()
        self.direction = pygame.math.Vector2(0, 0)
        
        # Notify AI
        self.ai.on_death()

    def animate(self, dt):
        """Update animation frames"""
        # Determine animation state
        if self.ai.state == WumpusAIState.DEAD or not self.is_alive:
            self.current_animation = "death"
        elif self.ai.state == "attack":
            self.current_animation = "attack"
        elif self.direction.magnitude() > 0:
            self.current_animation = "walk"
        else:
            self.current_animation = "idle"

        # Reset timer if animation changed
        if self.current_animation != self.previous_animation:
            self.animation_timer = 0
            self.previous_animation = self.current_animation

        # Get current animation frames
        animation_frames = self.animations.get(self.current_animation, [])

        if not animation_frames:
            return

        # Update animation timer
        self.animation_timer += dt

        # Calculate frame index
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)

        # Update image
        self.image = animation_frames[frame_index]

        # Flip sprite based on direction
        if self.direction.x < 0:
            self.image = pygame.transform.flip(self.image, True, False)

    def update(self, dt):
        """Update Wumpus every frame with pit avoidance"""
        # Update stun timer
        if self.is_stunned:
            self.stun_timer -= dt
            if self.stun_timer <= 0:
                self.is_stunned = False
                self.ai.state = WumpusAIState.ROAMING
                print("[Wumpus] Recovered from stun!")

        # Handle death state
        if not self.is_alive:
            self.animate(dt)
            return

        # Can't move while stunned
        if self.is_stunned:
            self.direction = pygame.math.Vector2(0, 0)

        # Move with pit avoidance (use chase speed when chasing)
        current_speed = self.chase_speed if self.ai.state == WumpusAIState.CHASING else self.speed
        self._move_with_pit_avoidance(dt, current_speed)
        self.animate(dt)
    
    def _move_with_pit_avoidance(self, dt, speed_override=None):
        """Move while avoiding pits using map knowledge"""
        if self.direction.length() == 0:
            return
        
        # Use speed override for chasing
        movement_speed = speed_override if speed_override is not None else self.speed
        
        # Check if current direction leads to danger
        if self.map_knowledge:
            # Look ahead
            look_ahead_distance = 50
            next_pos = self.pos + self.direction * look_ahead_distance
            
            # Check if near pit
            if self.map_knowledge.is_near_pit(next_pos.x, next_pos.y, danger_radius=50):
                # Find safe alternative direction
                safe_directions = self.map_knowledge.find_safe_alternative_directions(
                    self.pos,
                    self.direction
                )
                
                if safe_directions:
                    # Use first safe direction
                    self.direction = safe_directions[0]
                    print("[Wumpus] Avoiding pit, changing direction")
                else:
                    # No safe direction found - stop
                    self.direction = pygame.math.Vector2(0, 0)
                    # Switch to roaming to find new path
                    if self.ai.state != WumpusAIState.ROAMING:
                        self.ai.state = WumpusAIState.ROAMING
                        self.ai.roaming_target = None
                    return
        
        # Normal movement with custom speed
        self.move(dt, speed_override=movement_speed)
