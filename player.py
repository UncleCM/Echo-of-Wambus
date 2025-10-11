from Settings import *

class Player(pygame.sprite.Sprite):
    def __init__(self, pos, groups):
        super().__init__(groups)
        
        # Frame settings - adjust these based on your sprite size
        self.frame_width = 48  # Width of each individual frame (384/8 = 48)
        self.frame_height = 64  # Height of each frame
        self.scale_factor = 2  # Scale up 2x for better visibility
        
        # Animation settings
        self.animation_timer = 0
        self.animation_speed = 12  # Frames per second
        self.previous_animation = 'idle_down'
        
        # Load animations from sprite strip files
        self.animations = self.load_animations()
        
        print(f"Loaded {len(self.animations)} animation states")
        for anim_name, frames in self.animations.items():
            print(f"  {anim_name}: {len(frames)} frames")
        
        # Current animation state
        self.current_animation = 'idle_down'
        self.image = self.animations[self.current_animation][0]
        self.rect = self.image.get_rect(center=pos)  # Use center instead of topleft
        
        print(f"First frame size: {self.image.get_size()}")
        
        # Movement
        self.direction = pygame.math.Vector2()
        self.speed = 200
        self.facing = 'down'
        
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
    
    def load_sprite_strip(self, filepath):
        """Extract individual frames from a horizontal sprite strip"""
        try:
            # Load the sprite strip
            sprite_strip = pygame.image.load(join('assets', filepath)).convert_alpha()
            strip_width = sprite_strip.get_width()
            strip_height = sprite_strip.get_height()
            
            print(f"Loading {filepath}: {strip_width}x{strip_height}")
            
            # Calculate number of frames (should be 8 for 384px wide sprites)
            num_frames = strip_width // self.frame_width
            
            print(f"  Extracting {num_frames} frames of {self.frame_width}x{self.frame_height}")
            
            # Extract frames - USE ALL FRAMES
            frames = []
            
            for i in range(num_frames):  # Use ALL 8 frames
                # Extract the full height frame
                frame_rect = pygame.Rect(i * self.frame_width, 0, self.frame_width, self.frame_height)
                frame = sprite_strip.subsurface(frame_rect).copy()
                
                # Scale if needed
                if self.scale_factor != 1:
                    scaled_frame = pygame.transform.scale(frame, 
                        (self.frame_width * self.scale_factor, self.frame_height * self.scale_factor))
                    frames.append(scaled_frame)
                else:
                    frames.append(frame)
            
            if frames:
                print(f"Successfully loaded {len(frames)} frames from {filepath} (sampled from {num_frames} total)")
                return frames
            else:
                # Create placeholder with scaled size
                placeholder = pygame.Surface((self.frame_width * self.scale_factor, 
                                            self.frame_height * self.scale_factor), pygame.SRCALPHA)
                pygame.draw.rect(placeholder, (255, 0, 255), placeholder.get_rect(), 2)
                return [placeholder]
            
        except FileNotFoundError:
            print(f"Warning: Could not load sprite strip: {filepath}")
            # Create placeholder with scaled size
            placeholder = pygame.Surface((self.frame_width * self.scale_factor, 
                                        self.frame_height * self.scale_factor), pygame.SRCALPHA)
            pygame.draw.rect(placeholder, (255, 0, 255), placeholder.get_rect(), 2)
            return [placeholder]
    
    def input(self):
        """Handle player input"""
        keys = pygame.key.get_pressed()
        
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
    
    def move(self, dt):
        """Move the player"""
        self.rect.x += self.direction.x * self.speed * dt
        self.rect.y += self.direction.y * self.speed * dt
    
    def update(self, dt):
        """Update player every frame"""
        self.input()
        self.move(dt)
        self.animate(dt)