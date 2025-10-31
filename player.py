from Settings import *
from entity import Entity

class Player(Entity):
    """Player character - controlled by user input"""
    def __init__(self, pos, groups, collision_sprites, prolog_engine=None):
        # Initialize base Entity
        super().__init__(pos, groups, collision_sprites, prolog_engine, entity_type='player')
    
        # Player-specific attributes
        self.speed = 200  # Player movement speed
        
        # Combat attributes
        self.attack_damage = 50  # Damage dealt to enemies
        self.attack_range = 80  # Attack range in pixels
        self.attack_cooldown = 0.5  # Seconds between attacks
        self.attack_timer = 0  # Time since last attack
        self.is_attacking = False  # Currently in attack animation
        
        # Load player animations
        self.animations = self.load_animations()
        
        print(f"Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")
        
        # Setup sprite and hitbox
        self.current_animation = 'idle_down'
        initial_image = self.animations[self.current_animation][0]
        self.setup_sprite(initial_image, hitbox_inflate=(-75, -75))
        
        print(f"First frame size: {self.image.get_size()}")
    
        # ... rest of the load_animations, load_sprite_strip, input, and animate methods stay the same ...
    
    def load_animations(self):
        """Load all animation frames from sprite strip files"""
        animations = {}
        
        # Idle animations
        animations['idle_down'] = self.load_sprite_strip('Player/Idle/Idle_Down.png')
        animations['idle_up'] = self.load_sprite_strip('Player/Idle/Idle_Up.png')
        animations['idle_left'] = self.load_sprite_strip('Player/Idle/Idle_Left_Down.png')
        animations['idle_right'] = self.load_sprite_strip('Player/Idle/Idle_Right_Down.png')
        animations['idle_left_up'] = self.load_sprite_strip('Player/Idle/Idle_Left_Up.png')
        animations['idle_right_up'] = self.load_sprite_strip('Player/Idle/Idle_Right_Up.png')
        
        # Walk animations
        animations['walk_down'] = self.load_sprite_strip('Player/Walk/walk_Down.png')
        animations['walk_up'] = self.load_sprite_strip('Player/Walk/walk_Up.png')
        animations['walk_left_down'] = self.load_sprite_strip('Player/Walk/walk_Left_Down.png')
        animations['walk_right_down'] = self.load_sprite_strip('Player/Walk/walk_Right_Down.png')
        animations['walk_left_up'] = self.load_sprite_strip('Player/Walk/walk_Left_Up.png')
        animations['walk_right_up'] = self.load_sprite_strip('Player/Walk/walk_Right_Up.png')
        
        # Aliases for simpler left/right (use left_down and right_down as defaults)
        animations['walk_left'] = animations['walk_left_down']
        animations['walk_right'] = animations['walk_right_down']
        
        # Optional: Dash animations
        try:
            animations['dash'] = self.load_sprite_strip('Player/Dash/Dash.png')
        except:
            animations['dash'] = animations['idle_down']  # Fallback
        
        # Optional: Death animations
        try:
            animations['death'] = self.load_sprite_strip('Player/Death/Death.png')
        except:
            animations['death'] = animations['idle_down']  # Fallback
        
        return animations
    
    def input(self):
        """Handle player input"""
        keys = pygame.key.get_pressed()
        
        # Attack input (Spacebar)
        if keys[pygame.K_SPACE] and not self.is_attacking and self.attack_timer <= 0:
            self.attack()
        
        # Can't move while attacking
        if self.is_attacking:
            self.direction.x = 0
            self.direction.y = 0
            return
        
        # Reset direction
        self.direction.x = 0
        self.direction.y = 0
        
        # Movement input
        if keys[pygame.K_UP] or keys[pygame.K_w]:
            self.direction.y = -1
            self.facing = 'up'
        elif keys[pygame.K_DOWN] or keys[pygame.K_s]:
            self.direction.y = 1
            self.facing = 'down'
            
        if keys[pygame.K_LEFT] or keys[pygame.K_a]:
            self.direction.x = -1
            self.facing = 'left'
        elif keys[pygame.K_RIGHT] or keys[pygame.K_d]:
            self.direction.x = 1
            self.facing = 'right'
        
        # Normalize diagonal movement
        if self.direction.magnitude() > 0:
            self.direction = self.direction.normalize()
    
    def attack(self):
        """Trigger attack action"""
        self.is_attacking = True
        self.attack_timer = self.attack_cooldown
        # Animation will be handled in animate()
        print(f"[Player] Attack! Range: {self.attack_range}, Damage: {self.attack_damage}")
    
    def animate(self, dt):
        """Update animation frames"""
        # Determine animation state based on movement
        if self.direction.magnitude() > 0:
            # Moving - determine direction including diagonals
            if self.direction.y > 0 and self.direction.x < 0:
                # Moving down-left
                self.current_animation = 'walk_left_down'
                self.facing = 'left'
            elif self.direction.y > 0 and self.direction.x > 0:
                # Moving down-right
                self.current_animation = 'walk_right_down'
                self.facing = 'right'
            elif self.direction.y < 0 and self.direction.x < 0:
                self.current_animation = 'walk_left_up'
                self.facing = 'left'
            elif self.direction.y < 0 and self.direction.x > 0:
                self.current_animation = 'walk_right_up'
                self.facing = 'right'
            elif self.direction.x < 0:
                self.current_animation = 'walk_left_down'
                self.facing = 'left'
            elif self.direction.x > 0:
                self.current_animation = 'walk_right_down'
                self.facing = 'right'
            elif self.direction.y < 0:
                self.current_animation = 'walk_up'
                self.facing = 'up'
            else:
                self.current_animation = 'walk_down'
                self.facing = 'down'
        else:
            # Idle - match the last direction
            if self.facing == 'left':
                self.current_animation = 'idle_left'
            elif self.facing == 'right':
                self.current_animation = 'idle_right'
            elif self.facing == 'up':
                self.current_animation = 'idle_up'
            else:
                self.current_animation = 'idle_down'
        
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
        
        # Calculate frame index using total time (like your old model)
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)
        
        # Update image
        self.image = animation_frames[frame_index]
    
    def update(self, dt):
        """Update player every frame"""
        # Update attack cooldown timer
        if self.attack_timer > 0:
            self.attack_timer -= dt
        
        self.input()
        self.move(dt)
        self.animate(dt)
        
        # Check if attack animation finished
        if self.is_attacking:
            # Attack animation duration (assume ~0.3 seconds for attack)
            if self.attack_timer <= self.attack_cooldown - 0.3:
                self.is_attacking = False