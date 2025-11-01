"""
Lighting System Module

Handles all lighting and darkness effects for the game, including:
- Directional flashlight with wall occlusion
- Raycasting for realistic light blocking
- Player glow effects
"""

import pygame
import math
from Settings import WINDOW_WIDTH, WINDOW_HEIGHT


class LightingConfig:
    """Configuration constants for the lighting system."""

    # Darkness overlay
    DARKNESS_ALPHA = 240  # 0-255, higher = darker

    # Flashlight beam settings
    BEAM_LENGTH = 450  # Maximum distance the light reaches
    BEAM_ANGLE = 50  # Cone width in degrees
    BEAM_COLOR = (255, 255, 200)  # Warm yellowish light
    BEAM_ALPHA = 220  # Transparency of the light

    # Raycasting settings
    NUM_RAYS = 50  # Number of rays cast (more = smoother but slower)
    RAY_STEP_SIZE = (
        8  # Distance between ray checks (smaller = more accurate but slower)
    )
    RAY_CHECK_SIZE = 6  # Size of collision check rect

    # Player glow settings
    GLOW_RADIUS = 60  # Radius of the ambient glow around player
    GLOW_BASE_ALPHA = 180  # Base alpha for glow effect
    GLOW_COLOR = (255, 255, 200)  # Color of the glow


class FlashlightSystem:
    """
    Handles the directional flashlight rendering with wall occlusion.

    Uses raycasting to create realistic lighting that respects wall geometry,
    preventing light from passing through solid objects.
    """

    # Mapping of player facing directions to angles in degrees
    # 0° = right, 90° = down, 180° = left, 270° = up (pygame coordinate system)
    DIRECTION_ANGLES = {
        "right": 0,
        "right_down": 45,
        "down": 90,
        "left_down": 135,
        "left": 180,
        "left_up": 225,
        "up": 270,
        "right_up": 315,
    }

    def __init__(self, collision_sprites):
        """
        Initialize the flashlight system.

        Args:
            collision_sprites: Pygame sprite group containing wall collision boxes
        """
        self.collision_sprites = collision_sprites
        self.config = LightingConfig()

    def cast_ray_to_wall(self, start_world_pos, angle_degrees, max_distance):
        """
        Cast a ray from start position and find distance to nearest wall.

        Uses stepped raycasting to check for wall collisions along the ray path.

        Args:
            start_world_pos: (x, y) tuple in world coordinates
            angle_degrees: angle in degrees (0 = right, 90 = down in pygame coords)
            max_distance: maximum distance to check

        Returns:
            float: distance to wall, or max_distance if no wall hit
        """
        # Convert angle to radians for trigonometry
        angle_rad = math.radians(angle_degrees)
        dx = math.cos(angle_rad)
        dy = math.sin(angle_rad)

        # Calculate number of steps based on step size
        steps = int(max_distance / self.config.RAY_STEP_SIZE)

        # Step along the ray checking for collisions
        for step in range(1, steps + 1):
            distance = step * self.config.RAY_STEP_SIZE
            check_x = start_world_pos[0] + dx * distance
            check_y = start_world_pos[1] + dy * distance

            # Create a small rect to check collision at this point
            half_size = self.config.RAY_CHECK_SIZE // 2
            check_rect = pygame.Rect(
                check_x - half_size,
                check_y - half_size,
                self.config.RAY_CHECK_SIZE,
                self.config.RAY_CHECK_SIZE,
            )

            # Check against all wall sprites
            for wall in self.collision_sprites:
                if check_rect.colliderect(wall.rect):
                    return distance

        # No wall hit, return maximum distance
        return max_distance

    def get_facing_angle(self, facing_direction):
        """
        Get the angle in degrees for a given facing direction.

        Args:
            facing_direction: String direction like "up", "right_down", etc.

        Returns:
            float: Angle in degrees, defaults to 0 (right) if direction unknown
        """
        return self.DIRECTION_ANGLES.get(facing_direction, 0)

    def create_light_polygon(
        self, player_screen_pos, player_world_pos, facing_direction
    ):
        """
        Create a polygon representing the flashlight cone with wall occlusion.

        Casts multiple rays to build a polygon that stops at walls, creating
        realistic shadows and light blocking.

        Args:
            player_screen_pos: (x, y) player position in screen coordinates
            player_world_pos: (x, y) player position in world coordinates
            facing_direction: String indicating which way player is facing

        Returns:
            list: List of (x, y) tuples defining the light polygon in screen space
        """
        base_angle = self.get_facing_angle(facing_direction)
        light_points = [player_screen_pos]  # Start polygon at player

        # Cast rays across the cone angle
        for i in range(self.config.NUM_RAYS + 1):
            # Calculate angle for this ray within the cone
            angle_ratio = i / self.config.NUM_RAYS  # 0.0 to 1.0
            angle_offset = (angle_ratio - 0.5) * self.config.BEAM_ANGLE
            ray_angle = base_angle + angle_offset

            # Cast ray to find wall distance (in world coordinates)
            distance = self.cast_ray_to_wall(
                player_world_pos, ray_angle, self.config.BEAM_LENGTH
            )

            # Convert to screen coordinates
            angle_rad = math.radians(ray_angle)
            end_x = player_screen_pos[0] + math.cos(angle_rad) * distance
            end_y = player_screen_pos[1] + math.sin(angle_rad) * distance
            light_points.append((end_x, end_y))

        return light_points

    def draw_player_glow(self, surface, player_screen_pos):
        """
        Draw a subtle ambient glow around the player.

        Creates concentric circles with fading alpha for a soft glow effect.

        Args:
            surface: Pygame surface to draw on (will be subtracted from darkness)
            player_screen_pos: (x, y) player position in screen coordinates
        """
        glow_radius = self.config.GLOW_RADIUS
        glow_surface = pygame.Surface(
            (glow_radius * 2, glow_radius * 2), pygame.SRCALPHA
        )

        # Draw concentric circles with decreasing alpha
        for r in range(glow_radius, 0, -4):
            alpha_ratio = r / glow_radius
            alpha = max(
                0,
                self.config.GLOW_BASE_ALPHA
                - (alpha_ratio * self.config.GLOW_BASE_ALPHA),
            )
            pygame.draw.circle(
                glow_surface,
                (*self.config.GLOW_COLOR, int(alpha / 2)),
                (glow_radius, glow_radius),
                r,
            )

        # Blit glow at player position
        surface.blit(
            glow_surface,
            (player_screen_pos[0] - glow_radius, player_screen_pos[1] - glow_radius),
            special_flags=pygame.BLEND_RGBA_SUB,
        )

    def render(self, screen, player_screen_pos, player_world_pos, facing_direction):
        """
        Render the complete lighting system including darkness, flashlight, and glow.

        Args:
            screen: Main game screen surface to render to
            player_screen_pos: (x, y) player position in screen coordinates
            player_world_pos: (x, y) player position in world coordinates
            facing_direction: String indicating which way player is facing
        """
        # Create darkness overlay
        darkness = pygame.Surface((WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA)
        darkness.fill((0, 0, 0, self.config.DARKNESS_ALPHA))

        # Create light polygon with raycasting
        light_points = self.create_light_polygon(
            player_screen_pos, player_world_pos, facing_direction
        )

        # Draw the flashlight cone if we have enough points
        if len(light_points) > 2:
            light_surface = pygame.Surface(
                (WINDOW_WIDTH, WINDOW_HEIGHT), pygame.SRCALPHA
            )
            pygame.draw.polygon(
                light_surface,
                (*self.config.BEAM_COLOR, self.config.BEAM_ALPHA),
                light_points,
                0,  # Filled polygon
            )

            # Subtract light from darkness
            darkness.blit(light_surface, (0, 0), special_flags=pygame.BLEND_RGBA_SUB)

        # Add player glow
        self.draw_player_glow(darkness, player_screen_pos)

        # Apply final darkness overlay to screen
        screen.blit(darkness, (0, 0))
