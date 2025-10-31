from Settings import *

class Entity(pygame.sprite.Sprite):
    """
    Base class for all moving entities (Player, Wumpus, etc.)
    Handles common functionality: movement, collision, animations, health
    """
    def __init__(self, pos, groups, collision_sprites, prolog_engine=None, entity_type='entity'):
        super().__init__(groups)
        
        self.entity_type = entity_type  # 'player', 'wumpus', etc.
        self.prolog = prolog_engine
        self.collision_sprites = collision_sprites
        
        # Position and movement
        self.position = pygame.math.Vector2(pos)
        self.direction = pygame.math.Vector2()
        self.speed = 200  # Default speed (can be overridden)
        self.facing = 'down'
        
        # Fractional movement remainder (to prevent stuttering from rounding small deltas)
        self._rem_x = 0.0
        self._rem_y = 0.0
        
        # Health system
        self.max_health = 100
        self.health = self.max_health
        self.is_alive = True
        
        # Animation settings
        self.animation_timer = 0
        self.animation_speed = 12  # Frames per second
        self.previous_animation = 'idle_down'
        self.current_animation = 'idle_down'
        
        # Frame settings (to be set by child classes if needed)
        self.frame_width = 48
        self.frame_height = 64
        self.scale_factor = 2
        
        # Sprite & hitbox (to be set by child classes)
        self.animations = {}
        self.image = None
        self.rect = None
        self.hitbox_rect = None
        
        # Z-order for rendering (entities on top of tiles)
        self.z = 1
    
    def setup_sprite(self, initial_image, hitbox_inflate=(-75, -75)):
        """
        Setup sprite rect and hitbox (to be called by child classes after loading animations)
        """
        self.image = initial_image
        self.rect = self.image.get_rect(center=self.position)
        self.hitbox_rect = self.rect.inflate(*hitbox_inflate)
    
    def move(self, dt):
        """
        Move the entity using Prolog for authoritative collision resolution.
        The core collision logic is in game_logic.pl.
        """
        if self.direction.length() == 0:
            # Keep Prolog's authoritative position in sync when idle
            if self.prolog and getattr(self.prolog, 'available', False):
                try:
                    self.prolog.update_player_position(int(self.hitbox_rect.x), int(self.hitbox_rect.y))
                except Exception:
                    pass
            return

        # Calculate movement deltas
        delta_x = self.direction.x * self.speed * dt
        delta_y = self.direction.y * self.speed * dt
        
        # =========================================================================
        # PROLOG AUTHORITATIVE MOVEMENT RESOLUTION
        # =========================================================================
        if self.prolog and getattr(self.prolog, 'available', False):
            # Get current hitbox position (must be integers for Prolog)
            current_x = int(self.hitbox_rect.x)
            current_y = int(self.hitbox_rect.y)
            
            # Accumulate fractional movement to avoid losing small deltas (prevent stutter)
            self._rem_x += delta_x
            self._rem_y += delta_y
            
            # Convert accumulated movement to integers for Prolog call
            int_delta_x = int(round(self._rem_x))
            int_delta_y = int(round(self._rem_y))
            
            # Subtract sent integer portion from remainder (keep fractional part)
            self._rem_x -= int_delta_x
            self._rem_y -= int_delta_y
            
            player_w = self.hitbox_rect.width
            player_h = self.hitbox_rect.height
            
            # Prolog resolves collision and updates its internal position fact
            try:
                resolved_x, resolved_y = self.prolog.resolve_movement(
                    current_x, current_y, 
                    int_delta_x, int_delta_y, 
                    player_w, player_h
                )
                # Update Python's hitbox with the authoritative resolved position
                self.hitbox_rect.x = resolved_x
                self.hitbox_rect.y = resolved_y
            except Exception as e:
                print(f"[{self.entity_type}] Prolog resolve_movement failed: {e}")
                # On error, keep current position (don't crash the game)

        else:
            # Fallback to original collision system (only if Prolog unavailable)
            new_x = self.hitbox_rect.x + delta_x
            new_y = self.hitbox_rect.y + delta_y
            
            self.hitbox_rect.x = new_x
            self.collision('horizontal')
            self.hitbox_rect.y = new_y
            self.collision('vertical')

        # Sync sprite rect with hitbox
        self.rect.center = self.hitbox_rect.center
        self.position.x = self.rect.centerx
        self.position.y = self.rect.centery
    
    def collision(self, direction):
        """
        Handle collision with collision sprites (fallback method when Prolog unavailable)
        """
        for sprite in self.collision_sprites:
            if sprite.rect.colliderect(self.hitbox_rect):
                if direction == 'horizontal':
                    # Moving right - hit left side of wall
                    if self.direction.x > 0:
                        self.hitbox_rect.right = sprite.rect.left
                    # Moving left - hit right side of wall
                    if self.direction.x < 0:
                        self.hitbox_rect.left = sprite.rect.right
                
                if direction == 'vertical':
                    # Moving down - hit top of wall
                    if self.direction.y > 0:
                        self.hitbox_rect.bottom = sprite.rect.top
                    # Moving up - hit bottom of wall
                    if self.direction.y < 0:
                        self.hitbox_rect.top = sprite.rect.bottom
    
    def animate(self, dt):
        """
        Update animation frames based on current state.
        To be overridden by child classes for specific animation logic.
        """
        # Get current animation frames
        animation_frames = self.animations.get(self.current_animation, [])
        
        if not animation_frames:
            return
        
        # Update animation timer
        self.animation_timer += dt
        
        # Calculate frame index using total time
        frame_duration = 1.0 / self.animation_speed
        frame_index = int(self.animation_timer / frame_duration) % len(animation_frames)
        
        # Update image
        self.image = animation_frames[frame_index]
    
    def take_damage(self, amount):
        """
        Apply damage to entity
        """
        if not self.is_alive:
            return
        
        self.health -= amount
        
        if self.health <= 0:
            self.health = 0
            self.is_alive = False
            self.on_death()
    
    def heal(self, amount):
        """
        Heal entity
        """
        if not self.is_alive:
            return
        
        self.health = min(self.health + amount, self.max_health)
    
    def on_death(self):
        """
        Called when entity dies (to be overridden by child classes)
        """
        print(f"{self.entity_type} has died!")
    
    def update(self, dt):
        """
        Update entity (to be overridden by child classes)
        Default: just move and animate
        """
        self.move(dt)
        self.animate(dt)
    
    def load_sprite_strip(self, filepath):
        """
        Extract individual frames from a horizontal sprite strip.
        Can be used by child classes.
        """
        try:
            # Load the sprite strip
            sprite_strip = pygame.image.load(join('assets', filepath)).convert_alpha()
            strip_width = sprite_strip.get_width()
            strip_height = sprite_strip.get_height()
            
            # Calculate number of frames
            num_frames = strip_width // self.frame_width
            
            # Extract frames
            frames = []
            for i in range(num_frames):
                frame_rect = pygame.Rect(i * self.frame_width, 0, self.frame_width, self.frame_height)
                frame = sprite_strip.subsurface(frame_rect).copy()
                
                # Scale if needed
                if self.scale_factor != 1:
                    scaled_frame = pygame.transform.scale(frame,
                                                         (self.frame_width * self.scale_factor,
                                                          self.frame_height * self.scale_factor))
                    frames.append(scaled_frame)
                else:
                    frames.append(frame)
            
            return frames if frames else self._create_placeholder()
            
        except FileNotFoundError:
            print(f"Warning: Could not load sprite strip: {filepath}")
            return self._create_placeholder()
    
    def _create_placeholder(self):
        """
        Create a placeholder sprite when asset loading fails
        """
        placeholder = pygame.Surface((self.frame_width * self.scale_factor,
                                      self.frame_height * self.scale_factor), pygame.SRCALPHA)
        pygame.draw.rect(placeholder, (255, 0, 255), placeholder.get_rect(), 2)
        return [placeholder]
