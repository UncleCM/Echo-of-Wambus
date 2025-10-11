from Settings import *

class Player(pygame.sprite.Sprite):
    def __init__(self, pos, groups):
        super().__init__(groups)
        
        # Frame settings - adjust these based on your sprite size
        self.frame_width = 16  # Width of each individual frame
        self.frame_height = 64  # Height of each frame (sprites are tall!)
        self.scale_factor = 3  # Scale up the sprite for visibility
        
        # Animation settings
        self.frame_index = 0
        self.animation_timer = 0
        self.frame_duration = 0.05  # Fast frame rate since we have 24 frames
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
        animations['walk_left'] = self.load_sprite_strip('Player/Walk/walk_Left_Down.png')
        animations['walk_right'] = self.load_sprite_strip('Player/Walk/walk_Right_Down.png')
        animations['walk_left_up'] = self.load_sprite_strip('Player/Walk/walk_Left_Up.png')
        animations['walk_right_up'] = self.load_sprite_strip('Player/Walk/walk_Right_Up.png')
        
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
            
            # Calculate number of frames
            num_frames = strip_width // self.frame_width
            
            # Extract frames - USE ALL FRAMES
            frames = []
            
            for i in range(num_frames):  # Use ALL frames
                # Extract the full height frame
                frame_rect = pygame.Rect(i * self.frame_width, 0, self.frame_width, self.frame_height)
                frame = sprite_strip.subsurface(frame_rect).copy()
                
                # Scale up the frame for visibility  
                scaled_frame = pygame.transform.scale(frame, 
                    (self.frame_width * self.scale_factor, self.frame_height * self.scale_factor))
                
                frames.append(scaled_frame)
            
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
            # Moving
            if self.direction.y < 0 and self.direction.x < 0:
                self.current_animation = 'walk_left_up'
            elif self.direction.y < 0 and self.direction.x > 0:
                self.current_animation = 'walk_right_up'
            elif self.direction.x < 0:
                self.current_animation = 'walk_left'
            elif self.direction.x > 0:
                self.current_animation = 'walk_right'
            elif self.direction.y < 0:
                self.current_animation = 'walk_up'
            else:
                self.current_animation = 'walk_down'
        else:
            # Idle
            if self.facing == 'left':
                self.current_animation = 'idle_left'
            elif self.facing == 'right':
                self.current_animation = 'idle_right'
            elif self.facing == 'up':
                self.current_animation = 'idle_up'
            else:
                self.current_animation = 'idle_down'
        
        # Reset animation if state changed
        if self.current_animation != self.previous_animation:
            self.frame_index = 0
            self.animation_timer = 0
            self.previous_animation = self.current_animation
        
        # Get current animation frames
        animation_frames = self.animations.get(self.current_animation, [])
        
        if not animation_frames:
            return
        
        # Update animation timer
        self.animation_timer += dt
        
        # Check if it's time to advance to next frame
        if self.animation_timer >= self.frame_duration:
            self.animation_timer = 0
            self.frame_index += 1
            if self.frame_index >= len(animation_frames):
                self.frame_index = 0
        
        # Always update the image to current frame
        old_center = self.rect.center
        self.image = animation_frames[self.frame_index]
        self.rect = self.image.get_rect(center=old_center)
    
    def move(self, dt):
        """Move the player"""
        self.rect.x += self.direction.x * self.speed * dt
        self.rect.y += self.direction.y * self.speed * dt
    
    def update(self, dt):
        """Update player every frame"""
        self.input()
        self.move(dt)
        self.animate(dt)