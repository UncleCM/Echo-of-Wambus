"""
Phase 1.2 Verification Test
Tests that Wumpus entity is properly created and integrated
"""

import sys
import os

# Add parent directory to path so we can import game modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

def test_wumpus_class():
    """Test that Wumpus class exists and inherits from Entity"""
    print("\n=== Testing Wumpus Class ===")
    from wumpus import Wumpus
    from entity import Entity
    
    # Check inheritance
    if not issubclass(Wumpus, Entity):
        print("❌ FAIL: Wumpus does not inherit from Entity")
        return False
    
    print("✅ PASS: Wumpus inherits from Entity")
    return True

def test_wumpus_attributes():
    """Test that Wumpus has required attributes"""
    print("\n=== Testing Wumpus Attributes ===")
    from wumpus import Wumpus
    import pygame
    
    # Create minimal test setup
    pygame.init()
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    # Create Wumpus instance
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    
    required_attrs = {
        'speed': 150,
        'damage': 25,
        'max_health': 150,
        'health': 150,
        'attack_range': 50,
        'detection_range': 300,
        'ai_state': 'patrol',
        'is_alive': True
    }
    
    failed = []
    for attr, expected_value in required_attrs.items():
        if not hasattr(wumpus, attr):
            failed.append(f"Missing attribute: {attr}")
        elif getattr(wumpus, attr) != expected_value:
            actual = getattr(wumpus, attr)
            failed.append(f"{attr}: expected {expected_value}, got {actual}")
    
    if failed:
        print("❌ FAIL: Wumpus attributes incorrect:")
        for error in failed:
            print(f"  - {error}")
        return False
    
    print("✅ PASS: Wumpus has all required attributes with correct values")
    return True

def test_wumpus_methods():
    """Test that Wumpus has required AI methods"""
    print("\n=== Testing Wumpus AI Methods ===")
    from wumpus import Wumpus
    import pygame
    
    # Create minimal test setup
    pygame.init()
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    
    required_methods = [
        'ai_update', 'patrol', 'chase', 'attack_player', 
        'load_animations', 'take_damage', 'on_death'
    ]
    
    missing = []
    for method_name in required_methods:
        if not hasattr(wumpus, method_name):
            missing.append(method_name)
        elif not callable(getattr(wumpus, method_name)):
            missing.append(f"{method_name} (not callable)")
    
    if missing:
        print(f"❌ FAIL: Wumpus missing methods: {', '.join(missing)}")
        return False
    
    print("✅ PASS: Wumpus has all required AI methods")
    return True

def test_wumpus_animations():
    """Test that Wumpus animations are loaded"""
    print("\n=== Testing Wumpus Animations ===")
    from wumpus import Wumpus
    import pygame
    
    # Create minimal test setup
    pygame.init()
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    
    required_animations = ['idle', 'walk', 'attack', 'death']
    
    if not hasattr(wumpus, 'animations'):
        print("❌ FAIL: Wumpus has no animations attribute")
        return False
    
    missing = []
    for anim_name in required_animations:
        if anim_name not in wumpus.animations:
            missing.append(anim_name)
        elif not wumpus.animations[anim_name]:  # Check if list is not empty
            missing.append(f"{anim_name} (empty)")
    
    if missing:
        print(f"❌ FAIL: Missing animations: {', '.join(missing)}")
        return False
    
    print("✅ PASS: Wumpus has all required animations")
    print(f"  Loaded {len(wumpus.animations)} animation states:")
    for anim_name, frames in wumpus.animations.items():
        print(f"    - {anim_name}: {len(frames)} frames")
    return True

def test_wumpus_spawn_integration():
    """Test that Wumpus can be spawned via main.py integration"""
    print("\n=== Testing Wumpus Spawn Integration ===")
    
    try:
        from main import Game
        import pygame
        
        pygame.init()
        game = Game()
        
        # Check if wumpus exists
        if not hasattr(game, 'wumpus'):
            print("❌ FAIL: Game has no wumpus attribute")
            return False
        
        if game.wumpus is None:
            print("❌ FAIL: Wumpus is None")
            return False
        
        # Check if wumpus has patrol points
        if not hasattr(game.wumpus, 'patrol_points'):
            print("❌ FAIL: Wumpus has no patrol_points")
            return False
        
        if not game.wumpus.patrol_points:
            print("⚠️  WARNING: Wumpus patrol_points is empty (using fallback)")
        else:
            print(f"  Patrol points: {len(game.wumpus.patrol_points)} points")
        
        print("✅ PASS: Wumpus spawned and integrated into game")
        print(f"  Position: {game.wumpus.hitbox_rect.center}")
        print(f"  AI State: {game.wumpus.ai_state}")
        
        # Cleanup
        pygame.quit()
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_wumpus_ai_state_machine():
    """Test that Wumpus AI state transitions work"""
    print("\n=== Testing Wumpus AI State Machine ===")
    from wumpus import Wumpus
    import pygame
    
    # Create minimal test setup
    pygame.init()
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    wumpus = Wumpus((100, 100), test_group, test_collision, None)
    wumpus.patrol_points = [(200, 200), (300, 300)]
    
    # Test patrol state
    wumpus.ai_state = 'patrol'
    player_far = pygame.math.Vector2(1000, 1000)  # Far away
    wumpus.ai_update(player_far, 0.016)
    
    if wumpus.ai_state != 'patrol':
        print(f"❌ FAIL: Should stay in patrol state when player far, got {wumpus.ai_state}")
        return False
    
    # Test chase state
    player_near = pygame.math.Vector2(200, 200)  # Within detection range
    wumpus.ai_update(player_near, 0.016)
    
    if wumpus.ai_state != 'chase':
        print(f"❌ FAIL: Should transition to chase when player near, got {wumpus.ai_state}")
        return False
    
    # Test attack state
    player_very_near = pygame.math.Vector2(110, 110)  # Within attack range
    wumpus.ai_update(player_very_near, 0.016)
    
    if wumpus.ai_state != 'attack':
        print(f"❌ FAIL: Should transition to attack when player very near, got {wumpus.ai_state}")
        return False
    
    print("✅ PASS: Wumpus AI state machine transitions correctly")
    print("  patrol → chase → attack")
    return True

def main():
    print("=" * 60)
    print("PHASE 1.2 VERIFICATION TEST")
    print("Checking Wumpus entity implementation")
    print("=" * 60)
    
    tests = [
        ("File Structure", lambda: test_file_structure()),
        ("Wumpus Class", test_wumpus_class),
        ("Wumpus Attributes", test_wumpus_attributes),
        ("Wumpus Methods", test_wumpus_methods),
        ("Wumpus Animations", test_wumpus_animations),
        ("Wumpus AI State Machine", test_wumpus_ai_state_machine),
        ("Wumpus Spawn Integration", test_wumpus_spawn_integration),
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
        print("\n✅ ALL TESTS PASSED - Phase 1.2 is complete!")
        return 0
    else:
        print("\n❌ SOME TESTS FAILED - Phase 1.2 needs fixes")
        return 1

def test_file_structure():
    """Test that required files exist"""
    print("\n=== Testing File Structure ===")
    
    required_files = [
        'wumpus.py',
        'entity.py',
        'main.py',
        'prolog_interface.py',
        'game_logic.pl'
    ]
    
    missing = []
    for filepath in required_files:
        if not os.path.exists(filepath):
            missing.append(filepath)
    
    if missing:
        print(f"❌ FAIL: Missing files: {', '.join(missing)}")
        return False
    
    print("✅ PASS: All required files exist")
    return True

if __name__ == "__main__":
    sys.exit(main())
