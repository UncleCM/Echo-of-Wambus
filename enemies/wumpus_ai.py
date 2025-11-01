"""Wumpus AI behavior system - Sound-based hunting logic"""
import pygame
import random
from enemies.base import WumpusAIState
from Settings import SOUND_LEVELS, SOUND_DURATIONS


class WumpusAI:
    """
    Sound-based AI controller for Wumpus enemy.
    Handles state machine and behavior logic.
    """
    
    def __init__(self, wumpus):
        """
        Initialize AI controller
        
        Args:
            wumpus: Reference to parent Wumpus entity
        """
        self.wumpus = wumpus
        
        # AI state
        self.state = WumpusAIState.ROAMING
        self.target_position = None
        self.last_heard_sound = None
        self.roaming_target = None
        self.search_timer = 0
        self.search_wander_timer = 0
        
        # Hearing system
        self.hearing_radius = wumpus.hearing_radius
        self.chase_hearing_bonus = wumpus.chase_hearing_bonus
        self.current_hearing_radius = self.hearing_radius
        
        # Roar system
        self.is_roaring = False
        self.roar_cooldown = wumpus.roar_cooldown
        self.last_roar_time = 0
    
    def update(self, player_pos, dt):
        """
        Main AI update - Sound-based decision making
        
        Args:
            player_pos: Player position (for attack range check only)
            dt: Delta time
        """
        if not self.wumpus.is_alive or self.wumpus.is_stunned:
            return
        
        # Listen for sounds
        if self.wumpus.sound_manager:
            loudest_sound, loudness = self.wumpus.sound_manager.get_loudest_sound(
                self.wumpus.pos,
                min_threshold=10.0
            )
            
            # React to detected sound
            if loudest_sound and loudness > 0:
                self._handle_sound_detected(loudest_sound, loudness)
            else:
                self._handle_no_sound()
        
        # Execute behavior based on current state
        if self.state == WumpusAIState.ROAMING:
            self._behavior_roaming()
        elif self.state == WumpusAIState.INVESTIGATING:
            self._behavior_investigating()
        elif self.state == WumpusAIState.CHASING:
            self._behavior_chasing()
        elif self.state == WumpusAIState.SEARCHING:
            self._behavior_searching(dt)
        elif self.state == WumpusAIState.STUNNED:
            # Just stay still
            self.wumpus.direction = pygame.math.Vector2(0, 0)
        
        # Check if in attack range (proximity-based)
        distance_to_player = self.wumpus.pos.distance_to(pygame.math.Vector2(player_pos))
        if distance_to_player <= self.wumpus.attack_range and self.state == WumpusAIState.CHASING:
            # Player is very close - attack!
            self.state = "attack"
            self.wumpus.direction = pygame.math.Vector2(0, 0)
    
    def _handle_sound_detected(self, sound, loudness):
        """React to detected sound"""
        if sound.source_type == 'rock_impact':
            # LOUD distraction - investigate immediately!
            print(f"[Wumpus] Heard rock impact (loudness: {loudness:.1f})! Investigating...")
            self.state = WumpusAIState.INVESTIGATING
            self.target_position = sound.position.copy()
            self.last_heard_sound = sound
            
        elif sound.source_type in ['walk', 'dash', 'arrow_shot']:
            # Player sounds detected
            if loudness > 30:  # Loud enough to chase
                print(f"[Wumpus] Heard player (loudness: {loudness:.1f})! Chasing...")
                self.state = WumpusAIState.CHASING
                self.target_position = sound.position.copy()
                self._trigger_roar()
            else:
                # Faint sound - just investigate
                self.state = WumpusAIState.INVESTIGATING
                self.target_position = sound.position.copy()
    
    def _handle_no_sound(self):
        """No sound detected - continue current behavior"""
        # If chasing but lost sound, switch to searching
        if self.state == WumpusAIState.CHASING:
            self.state = WumpusAIState.SEARCHING
            self.search_timer = 8.0  # Search for 8 seconds
            print("[Wumpus] Lost sound... searching area")
    
    def _behavior_roaming(self):
        """Wander the map randomly using map knowledge"""
        if self.roaming_target is None or self._reached_target():
            # Pick new random safe position
            if self.wumpus.map_knowledge:
                new_target = self.wumpus.map_knowledge.get_safe_random_position()
                if new_target:
                    self.roaming_target = new_target
                    self.target_position = pygame.math.Vector2(new_target)
        
        # Move towards target with pit avoidance
        if self.target_position:
            direction = (self.target_position - self.wumpus.pos)
            if direction.length() > 0:
                self.wumpus.direction = direction.normalize()
    
    def _behavior_investigating(self):
        """Move to sound source location"""
        if self._reached_target():
            # Reached source, nothing here - start searching
            self.state = WumpusAIState.SEARCHING
            self.search_timer = 5.0
            print("[Wumpus] Reached sound location, nothing here... searching")
        else:
            # Move towards sound
            direction = (self.target_position - self.wumpus.pos)
            if direction.length() > 0:
                self.wumpus.direction = direction.normalize()
    
    def _behavior_chasing(self):
        """Chase last heard player sound"""
        if self._reached_target():
            # Lost player at last known position
            self.state = WumpusAIState.SEARCHING
            self.search_timer = 8.0
            self.is_roaring = False
            self.current_hearing_radius = self.hearing_radius  # Reset hearing
            print("[Wumpus] Lost player... searching")
        else:
            # Continue to last heard position
            direction = (self.target_position - self.wumpus.pos)
            if direction.length() > 0:
                self.wumpus.direction = direction.normalize()
    
    def _behavior_searching(self, dt):
        """Search around last known position"""
        self.search_timer -= dt
        
        if self.search_timer <= 0:
            # Give up, return to roaming
            self.state = WumpusAIState.ROAMING
            self.current_hearing_radius = self.hearing_radius
            self.is_roaring = False
            print("[Wumpus] Search timeout... resuming roaming")
        else:
            # Circle/wander around search area
            self.search_wander_timer += dt
            if self.search_wander_timer > 2.0:  # Change direction every 2 seconds
                self.search_wander_timer = 0
                # Random direction
                angle = random.uniform(0, 360)
                self.wumpus.direction = pygame.math.Vector2(1, 0).rotate(angle)
    
    def _trigger_roar(self):
        """Roar when starting chase - increases hearing radius"""
        current_time = pygame.time.get_ticks()
        if current_time - self.last_roar_time > self.roar_cooldown:
            self.is_roaring = True
            self.current_hearing_radius = self.hearing_radius + self.chase_hearing_bonus
            self.last_roar_time = current_time
            
            # Emit roar sound
            if self.wumpus.sound_manager:
                self.wumpus.sound_manager.play_sound("roar", volume=0.1)
                self.wumpus.sound_manager.emit_sound(
                    self.wumpus.pos,
                    SOUND_LEVELS['wumpus_roar'],
                    SOUND_DURATIONS['wumpus_roar'],
                    'wumpus_roar'
                )
            
            print(f"[Wumpus] ROAR! Hearing increased to {self.current_hearing_radius}px")
    
    def _reached_target(self, threshold=30):
        """Check if reached target position"""
        if self.target_position is None:
            return True
        return self.wumpus.pos.distance_to(self.target_position) < threshold
    
    def apply_stun(self, duration):
        """Apply stun effect - freeze in place"""
        self.state = WumpusAIState.STUNNED
        print(f"[Wumpus] Stunned for {duration} seconds!")
    
    def on_death(self):
        """Handle AI state on death"""
        self.state = WumpusAIState.DEAD
        print("[Wumpus] AI disabled - Wumpus defeated!")
