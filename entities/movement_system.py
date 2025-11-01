"""Movement and collision system for entities"""
import pygame


class MovementSystem:
    """Handles entity movement and collision detection"""
    
    def __init__(self, prolog_engine=None, collision_sprites=None, speed=200):
        """
        Initialize movement system
        
        Args:
            prolog_engine: Prolog engine for authoritative collision
            collision_sprites: Pygame sprite group for fallback collision
            speed: Movement speed in pixels per second
        """
        self.prolog = prolog_engine
        self.collision_sprites = collision_sprites
        self.speed = speed
        
        # Movement state
        self.direction = pygame.math.Vector2()
        self.facing = 'down'
        
        # Fractional movement remainder (prevents stuttering from rounding)
        self._rem_x = 0.0
        self._rem_y = 0.0
    
    def move(self, hitbox_rect, dt, speed_override=None):
        """
        Move entity using Prolog for authoritative collision resolution
        
        Args:
            hitbox_rect: Entity's hitbox rectangle
            dt: Delta time
            speed_override: Optional speed to use instead of self.speed
            
        Returns:
            pygame.Rect: Updated hitbox rectangle
        """
        # Use overridden speed or default
        movement_speed = speed_override if speed_override is not None else self.speed
        
        if self.direction.length() == 0:
            # Keep Prolog's authoritative position in sync when idle
            if self.prolog and getattr(self.prolog, 'available', False):
                try:
                    self.prolog.update_player_position(int(hitbox_rect.x), int(hitbox_rect.y))
                except Exception:
                    pass
            return hitbox_rect

        # Calculate movement deltas
        delta_x = self.direction.x * movement_speed * dt
        delta_y = self.direction.y * movement_speed * dt
        
        # =========================================================================
        # PROLOG AUTHORITATIVE MOVEMENT RESOLUTION
        # =========================================================================
        if self.prolog and getattr(self.prolog, 'available', False):
            # Get current hitbox position (must be integers for Prolog)
            current_x = int(hitbox_rect.x)
            current_y = int(hitbox_rect.y)
            
            # Accumulate fractional movement to avoid losing small deltas
            self._rem_x += delta_x
            self._rem_y += delta_y
            
            # Convert accumulated movement to integers for Prolog call
            int_delta_x = int(round(self._rem_x))
            int_delta_y = int(round(self._rem_y))
            
            # Subtract sent integer portion from remainder (keep fractional part)
            self._rem_x -= int_delta_x
            self._rem_y -= int_delta_y
            
            player_w = hitbox_rect.width
            player_h = hitbox_rect.height
            
            # Prolog resolves collision and updates its internal position fact
            try:
                resolved_x, resolved_y = self.prolog.resolve_movement(
                    current_x, current_y, 
                    int_delta_x, int_delta_y, 
                    player_w, player_h
                )
                # Update hitbox with the authoritative resolved position
                hitbox_rect.x = resolved_x
                hitbox_rect.y = resolved_y
            except Exception as e:
                print(f"[MovementSystem] Prolog resolve_movement failed: {e}")
                # On error, keep current position (don't crash the game)

        else:
            # Fallback to original collision system (only if Prolog unavailable)
            new_x = hitbox_rect.x + delta_x
            new_y = hitbox_rect.y + delta_y
            
            hitbox_rect.x = new_x
            self._collision(hitbox_rect, 'horizontal')
            hitbox_rect.y = new_y
            self._collision(hitbox_rect, 'vertical')
        
        return hitbox_rect
    
    def _collision(self, hitbox_rect, direction):
        """
        Handle collision with collision sprites (fallback method)
        
        Args:
            hitbox_rect: Entity's hitbox rectangle
            direction: 'horizontal' or 'vertical'
        """
        if not self.collision_sprites:
            return
        
        for sprite in self.collision_sprites:
            if sprite.rect.colliderect(hitbox_rect):
                if direction == 'horizontal':
                    # Moving right - hit left side of wall
                    if self.direction.x > 0:
                        hitbox_rect.right = sprite.rect.left
                    # Moving left - hit right side of wall
                    if self.direction.x < 0:
                        hitbox_rect.left = sprite.rect.right
                
                if direction == 'vertical':
                    # Moving down - hit top of wall
                    if self.direction.y > 0:
                        hitbox_rect.bottom = sprite.rect.top
                    # Moving up - hit bottom of wall
                    if self.direction.y < 0:
                        hitbox_rect.top = sprite.rect.bottom
    
    def set_direction(self, x, y):
        """Set movement direction"""
        self.direction.x = x
        self.direction.y = y
    
    def stop(self):
        """Stop all movement"""
        self.direction.x = 0
        self.direction.y = 0
