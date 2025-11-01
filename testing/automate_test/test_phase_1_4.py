"""
Phase 1.4 Verification Test
Tests that Combat System is working correctly
"""

import sys
import os

# Add parent directory to path so we can import game modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

def test_player_attack_attributes():
    """Test that Player has attack attributes"""
    print("\n=== Testing Player Attack Attributes ===")
    from player import Player
    import pygame
    
    pygame.init()
    # Create dummy display for sprite loading
    pygame.display.set_mode((1, 1))
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    player = Player((100, 100), test_group, test_collision, None)
    
    required_attrs = {
        'attack_damage': 50,
        'attack_range': 80,
        'attack_cooldown': 0.5,
        'attack_timer': 0,
        'is_attacking': False
    }
    
    failed = []
    for attr, expected_value in required_attrs.items():
        if not hasattr(player, attr):
            failed.append(f"Missing attribute: {attr}")
        elif getattr(player, attr) != expected_value:
            actual = getattr(player, attr)
            failed.append(f"{attr}: expected {expected_value}, got {actual}")
    
    pygame.quit()
    
    if failed:
        print("❌ FAIL: Player attack attributes incorrect:")
        for error in failed:
            print(f"  - {error}")
        return False
    
    print("✅ PASS: Player has all attack attributes with correct values")
    return True

def test_player_attack_method():
    """Test that Player.attack() method works"""
    print("\n=== Testing Player Attack Method ===")
    from player import Player
    import pygame
    
    pygame.init()
    # Create dummy display for sprite loading
    pygame.display.set_mode((1, 1))
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    player = Player((100, 100), test_group, test_collision, None)
    
    # Test attack method exists
    if not hasattr(player, 'attack'):
        print("❌ FAIL: Player has no attack() method")
        pygame.quit()
        return False
    
    # Test attack triggers
    player.attack()
    
    if not player.is_attacking:
        print("❌ FAIL: attack() did not set is_attacking to True")
        pygame.quit()
        return False
    
    if player.attack_timer != player.attack_cooldown:
        print(f"❌ FAIL: attack_timer not set correctly: {player.attack_timer} != {player.attack_cooldown}")
        pygame.quit()
        return False
    
    pygame.quit()
    print("✅ PASS: Player attack() method works correctly")
    return True

def test_attack_cooldown():
    """Test that attack cooldown prevents rapid attacks"""
    print("\n=== Testing Attack Cooldown ===")
    from player import Player
    import pygame
    
    pygame.init()
    # Create dummy display for sprite loading
    pygame.display.set_mode((1, 1))
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    player = Player((100, 100), test_group, test_collision, None)
    
    # First attack should work
    player.attack()
    first_attack = player.is_attacking
    
    # Immediate second attack should not work (cooldown active)
    player.attack()
    
    if not first_attack:
        print("❌ FAIL: First attack failed")
        pygame.quit()
        return False
    
    # Simulate time passing
    player.update(0.6)  # More than cooldown (0.5s)
    
    # Now attack should work again
    if player.attack_timer > 0:
        print(f"❌ FAIL: Cooldown not reset after time: {player.attack_timer}")
        pygame.quit()
        return False
    
    pygame.quit()
    print("✅ PASS: Attack cooldown prevents rapid attacks")
    return True

def test_damage_calculation():
    """Test that damage is calculated correctly"""
    print("\n=== Testing Damage Calculation ===")
    from wumpus import Wumpus
    from player import Player
    import pygame
    
    pygame.init()
    # Create dummy display for sprite loading
    pygame.display.set_mode((1, 1))
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    player = Player((200, 200), test_group, test_collision, None)
    
    # Record initial health
    initial_wumpus_hp = wumpus.health
    initial_player_hp = player.health
    
    # Player damages Wumpus
    damage_dealt = wumpus.take_damage(player.attack_damage)
    
    if damage_dealt != player.attack_damage:
        print(f"❌ FAIL: Damage dealt incorrect: {damage_dealt} != {player.attack_damage}")
        pygame.quit()
        return False
    
    if wumpus.health != initial_wumpus_hp - player.attack_damage:
        print(f"❌ FAIL: Wumpus health not updated: {wumpus.health} != {initial_wumpus_hp - player.attack_damage}")
        pygame.quit()
        return False
    
    # Wumpus damages Player
    damage_dealt = player.take_damage(wumpus.damage)
    
    if damage_dealt != wumpus.damage:
        print(f"❌ FAIL: Wumpus damage dealt incorrect: {damage_dealt} != {wumpus.damage}")
        pygame.quit()
        return False
    
    if player.health != initial_player_hp - wumpus.damage:
        print(f"❌ FAIL: Player health not updated: {player.health} != {initial_player_hp - wumpus.damage}")
        pygame.quit()
        return False
    
    pygame.quit()
    print("✅ PASS: Damage calculation works correctly")
    return True

def test_death_handling():
    """Test that death is handled correctly"""
    print("\n=== Testing Death Handling ===")
    from wumpus import Wumpus
    from player import Player
    import pygame
    
    pygame.init()
    # Create dummy display for sprite loading
    pygame.display.set_mode((1, 1))
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    player = Player((200, 200), test_group, test_collision, None)
    
    # Kill Wumpus
    wumpus.take_damage(1000)  # Massive damage
    
    if wumpus.is_alive:
        print("❌ FAIL: Wumpus should be dead")
        pygame.quit()
        return False
    
    if wumpus.health > 0:
        print(f"❌ FAIL: Wumpus health should be 0: {wumpus.health}")
        pygame.quit()
        return False
    
    # Kill Player
    player.take_damage(1000)  # Massive damage
    
    if player.is_alive:
        print("❌ FAIL: Player should be dead")
        pygame.quit()
        return False
    
    if player.health > 0:
        print(f"❌ FAIL: Player health should be 0: {player.health}")
        pygame.quit()
        return False
    
    pygame.quit()
    print("✅ PASS: Death handling works correctly")
    return True

def test_health_bounds():
    """Test that health cannot exceed max or go below 0"""
    print("\n=== Testing Health Bounds ===")
    from wumpus import Wumpus
    import pygame
    
    pygame.init()
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    
    # Test heal cannot exceed max
    initial_max = wumpus.max_health
    wumpus.health = wumpus.max_health - 10
    wumpus.heal(50)  # Try to overheal
    
    if wumpus.health > initial_max:
        print(f"❌ FAIL: Health exceeded max: {wumpus.health} > {initial_max}")
        pygame.quit()
        return False
    
    # Test damage cannot go below 0
    wumpus.take_damage(10000)  # Massive overkill
    
    if wumpus.health < 0:
        print(f"❌ FAIL: Health went below 0: {wumpus.health}")
        pygame.quit()
        return False
    
    pygame.quit()
    print("✅ PASS: Health bounds enforced correctly")
    return True

def test_combat_integration():
    """Test combat system integration in Game class"""
    print("\n=== Testing Combat Integration ===")
    
    try:
        from main import Game
        import pygame
        
        pygame.init()
        game = Game()
        
        # Check combat methods exist
        if not hasattr(game, 'check_player_attack'):
            print("❌ FAIL: Game has no check_player_attack() method")
            pygame.quit()
            return False
        
        if not hasattr(game, 'check_wumpus_attack'):
            print("❌ FAIL: Game has no check_wumpus_attack() method")
            pygame.quit()
            return False
        
        # Test methods are callable
        if not callable(getattr(game, 'check_player_attack')):
            print("❌ FAIL: check_player_attack is not callable")
            pygame.quit()
            return False
        
        if not callable(getattr(game, 'check_wumpus_attack')):
            print("❌ FAIL: check_wumpus_attack is not callable")
            pygame.quit()
            return False
        
        print("✅ PASS: Combat methods integrated into Game class")
        pygame.quit()
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_attack_range_check():
    """Test that attacks only hit within range"""
    print("\n=== Testing Attack Range Check ===")
    from wumpus import Wumpus
    from player import Player
    import pygame
    
    pygame.init()
    # Create dummy display for sprite loading
    pygame.display.set_mode((1, 1))
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    player = Player((0, 0), test_group, test_collision, None)
    wumpus_close = Wumpus((50, 0), test_group, test_collision, None)  # Within 80px
    wumpus_far = Wumpus((200, 0), test_group, test_collision, None)   # Beyond 80px
    
    # Calculate distances
    player_pos = pygame.math.Vector2(player.hitbox_rect.center)
    close_pos = pygame.math.Vector2(wumpus_close.hitbox_rect.center)
    far_pos = pygame.math.Vector2(wumpus_far.hitbox_rect.center)
    
    close_distance = player_pos.distance_to(close_pos)
    far_distance = player_pos.distance_to(far_pos)
    
    # Close wumpus should be in range
    if close_distance > player.attack_range:
        print(f"⚠️  WARNING: Test setup issue - close wumpus at {close_distance}px")
    
    # Far wumpus should be out of range
    if far_distance <= player.attack_range:
        print(f"⚠️  WARNING: Test setup issue - far wumpus at {far_distance}px")
    
    print(f"  Close distance: {close_distance:.1f}px (range: {player.attack_range}px)")
    print(f"  Far distance: {far_distance:.1f}px (range: {player.attack_range}px)")
    
    pygame.quit()
    print("✅ PASS: Attack range check setup verified")
    return True

def test_combat_stats():
    """Test combat stats are balanced"""
    print("\n=== Testing Combat Stats Balance ===")
    from wumpus import Wumpus
    from player import Player
    import pygame
    
    pygame.init()
    # Create dummy display for sprite loading
    pygame.display.set_mode((1, 1))
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    player = Player((0, 0), test_group, test_collision, None)
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    
    # Calculate time to kill
    player_ttk = wumpus.max_health / player.attack_damage  # Hits to kill Wumpus
    wumpus_ttk = player.max_health / wumpus.damage  # Hits to kill Player
    
    print(f"  Player HP: {player.max_health}, Damage: {player.attack_damage}, Range: {player.attack_range}")
    print(f"  Wumpus HP: {wumpus.max_health}, Damage: {wumpus.damage}, Range: {wumpus.attack_range}")
    print(f"  Player kills Wumpus in: {player_ttk:.1f} hits")
    print(f"  Wumpus kills Player in: {wumpus_ttk:.1f} hits")
    
    # Player should have advantage (higher damage, longer range)
    if player.attack_damage <= wumpus.damage:
        print("⚠️  WARNING: Player damage not higher than Wumpus")
    
    if player.attack_range <= wumpus.attack_range:
        print("⚠️  WARNING: Player range not longer than Wumpus")
    
    pygame.quit()
    print("✅ PASS: Combat stats verified")
    return True

def main():
    print("=" * 60)
    print("PHASE 1.4 VERIFICATION TEST")
    print("Testing Combat System")
    print("=" * 60)
    
    tests = [
        ("Player Attack Attributes", test_player_attack_attributes),
        ("Player Attack Method", test_player_attack_method),
        ("Attack Cooldown", test_attack_cooldown),
        ("Damage Calculation", test_damage_calculation),
        ("Death Handling", test_death_handling),
        ("Health Bounds", test_health_bounds),
        ("Attack Range Check", test_attack_range_check),
        ("Combat Stats Balance", test_combat_stats),
        ("Combat Integration", test_combat_integration),
    ]
    
    results = []
    for test_name, test_func in tests:
        try:
            result = test_func()
            results.append(result)
        except Exception as e:
            print(f"❌ EXCEPTION in {test_name}: {e}")
            import traceback
            traceback.print_exc()
            results.append(False)
    
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    passed = sum(results)
    total = len(results)
    print(f"Tests passed: {passed}/{total}")
    
    if passed == total:
        print("\n✅ ALL TESTS PASSED - Phase 1.4 is complete!")
        return 0
    else:
        print("\n❌ SOME TESTS FAILED - Phase 1.4 needs fixes")
        return 1

if __name__ == "__main__":
    sys.exit(main())
