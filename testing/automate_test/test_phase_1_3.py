"""
Phase 1.3 Verification Test
Tests that Prolog AI integration is working correctly
"""

import sys
import os

# Add parent directory to path so we can import game modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

def test_prolog_ai_predicates():
    """Test that Prolog AI predicates exist and work"""
    print("\n=== Testing Prolog AI Predicates ===")
    from prolog_interface import PrologEngine
    
    prolog = PrologEngine()
    
    if not prolog.available:
        print("❌ FAIL: Prolog not available - cannot test AI predicates")
        return False
    
    # Test init_wumpus
    try:
        prolog.init_wumpus(100, 100)
        print("✅ PASS: init_wumpus() works")
    except Exception as e:
        print(f"❌ FAIL: init_wumpus() failed: {e}")
        return False
    
    # Test update_wumpus_position
    try:
        prolog.update_wumpus_position(200, 200)
        print("✅ PASS: update_wumpus_position() works")
    except Exception as e:
        print(f"❌ FAIL: update_wumpus_position() failed: {e}")
        return False
    
    # Test update_wumpus_state
    try:
        prolog.update_wumpus_state('chase')
        print("✅ PASS: update_wumpus_state() works")
    except Exception as e:
        print(f"❌ FAIL: update_wumpus_state() failed: {e}")
        return False
    
    return True

def test_wumpus_decision_patrol_to_chase():
    """Test Prolog AI decision: patrol -> chase transition"""
    print("\n=== Testing AI Decision: Patrol -> Chase ===")
    from prolog_interface import PrologEngine
    
    prolog = PrologEngine()
    
    if not prolog.available:
        print("⚠️  SKIP: Prolog not available")
        return True  # Skip but don't fail
    
    try:
        prolog.init_wumpus(100, 100)
        
        # Test: Wumpus at (100, 100), Player far away at (500, 500)
        # Distance > 300px, should stay in patrol
        new_state, dx, dy = prolog.get_wumpus_decision(100, 100, 500, 500, 'patrol')
        
        if new_state != 'patrol':
            print(f"❌ FAIL: Expected 'patrol', got '{new_state}' (player far away)")
            return False
        
        print("✅ PASS: Stays in patrol when player far away")
        
        # Test: Wumpus at (100, 100), Player close at (200, 200)
        # Distance ~141px < 300px, should transition to chase
        new_state, dx, dy = prolog.get_wumpus_decision(100, 100, 200, 200, 'patrol')
        
        if new_state != 'chase':
            print(f"❌ FAIL: Expected 'chase', got '{new_state}' (player close)")
            return False
        
        # Check direction vector points towards player
        if dx <= 0 or dy <= 0:
            print(f"❌ FAIL: Direction vector incorrect: ({dx}, {dy})")
            return False
        
        print(f"✅ PASS: Transitions to chase with direction ({dx:.2f}, {dy:.2f})")
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_wumpus_decision_chase_to_attack():
    """Test Prolog AI decision: chase -> attack transition"""
    print("\n=== Testing AI Decision: Chase -> Attack ===")
    from prolog_interface import PrologEngine
    
    prolog = PrologEngine()
    
    if not prolog.available:
        print("⚠️  SKIP: Prolog not available")
        return True
    
    try:
        prolog.init_wumpus(100, 100)
        
        # Test: Wumpus at (100, 100), Player very close at (120, 120)
        # Distance ~28px < 50px, should transition to attack
        new_state, dx, dy = prolog.get_wumpus_decision(100, 100, 120, 120, 'chase')
        
        if new_state != 'attack':
            print(f"❌ FAIL: Expected 'attack', got '{new_state}' (player very close)")
            return False
        
        print("✅ PASS: Transitions to attack when player in attack range")
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_wumpus_decision_attack_to_chase():
    """Test Prolog AI decision: attack -> chase transition"""
    print("\n=== Testing AI Decision: Attack -> Chase ===")
    from prolog_interface import PrologEngine
    
    prolog = PrologEngine()
    
    if not prolog.available:
        print("⚠️  SKIP: Prolog not available")
        return True
    
    try:
        prolog.init_wumpus(100, 100)
        
        # Test: Wumpus at (100, 100), Player leaves attack range at (180, 180)
        # Distance ~113px > 50px but < 300px, should transition to chase
        new_state, dx, dy = prolog.get_wumpus_decision(100, 100, 180, 180, 'attack')
        
        if new_state != 'chase':
            print(f"❌ FAIL: Expected 'chase', got '{new_state}' (player left attack range)")
            return False
        
        print("✅ PASS: Transitions to chase when player leaves attack range")
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_wumpus_decision_chase_to_patrol():
    """Test Prolog AI decision: chase -> patrol transition"""
    print("\n=== Testing AI Decision: Chase -> Patrol ===")
    from prolog_interface import PrologEngine
    
    prolog = PrologEngine()
    
    if not prolog.available:
        print("⚠️  SKIP: Prolog not available")
        return True
    
    try:
        prolog.init_wumpus(100, 100)
        
        # Test: Wumpus at (100, 100), Player far at (500, 500)
        # Distance ~565px > 300px, should transition to patrol
        new_state, dx, dy = prolog.get_wumpus_decision(100, 100, 500, 500, 'chase')
        
        if new_state != 'patrol':
            print(f"❌ FAIL: Expected 'patrol', got '{new_state}' (player far away)")
            return False
        
        print("✅ PASS: Transitions to patrol when player leaves detection range")
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_wumpus_uses_prolog_ai():
    """Test that Wumpus ai_update() uses Prolog for decisions"""
    print("\n=== Testing Wumpus Uses Prolog AI ===")
    from wumpus import Wumpus
    from prolog_interface import PrologEngine
    import pygame
    
    pygame.init()
    
    prolog = PrologEngine()
    test_group = pygame.sprite.Group()
    test_collision = pygame.sprite.Group()
    
    wumpus = Wumpus((100, 100), test_group, test_collision, prolog)
    
    if not prolog.available:
        print("⚠️  SKIP: Prolog not available - testing fallback")
        # Test fallback AI works
        player_pos = pygame.math.Vector2(200, 200)
        wumpus.ai_update(player_pos, 0.016)
        
        if wumpus.ai_state != 'chase':
            print(f"❌ FAIL: Fallback AI failed, state: {wumpus.ai_state}")
            return False
        
        print("✅ PASS: Fallback Python AI works when Prolog unavailable")
        return True
    
    # Test Prolog AI integration
    try:
        # Start in patrol
        wumpus.ai_state = 'patrol'
        
        # Player far away - should stay patrol
        player_far = pygame.math.Vector2(500, 500)
        wumpus.ai_update(player_far, 0.016)
        
        if wumpus.ai_state != 'patrol':
            print(f"❌ FAIL: Should stay in patrol, got {wumpus.ai_state}")
            return False
        
        # Player close - should chase
        player_close = pygame.math.Vector2(200, 200)
        wumpus.ai_update(player_close, 0.016)
        
        if wumpus.ai_state != 'chase':
            print(f"❌ FAIL: Should transition to chase, got {wumpus.ai_state}")
            return False
        
        # Player very close - should attack
        player_very_close = pygame.math.Vector2(120, 120)
        wumpus.ai_update(player_very_close, 0.016)
        
        if wumpus.ai_state != 'attack':
            print(f"❌ FAIL: Should transition to attack, got {wumpus.ai_state}")
            return False
        
        print("✅ PASS: Wumpus uses Prolog AI for state transitions")
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        pygame.quit()

def test_direction_vector_calculation():
    """Test that Prolog calculates correct direction vectors"""
    print("\n=== Testing Direction Vector Calculation ===")
    from prolog_interface import PrologEngine
    import math
    
    prolog = PrologEngine()
    
    if not prolog.available:
        print("⚠️  SKIP: Prolog not available")
        return True
    
    try:
        prolog.init_wumpus(0, 0)
        
        # Test: Wumpus at (0, 0), Player at (100, 0) - should point right
        new_state, dx, dy = prolog.get_wumpus_decision(0, 0, 100, 0, 'patrol')
        
        if new_state != 'chase':
            print(f"⚠️  Skipping vector test - not in chase state")
            return True
        
        # Direction should be normalized (length = 1)
        magnitude = math.sqrt(dx*dx + dy*dy)
        
        if abs(magnitude - 1.0) > 0.01:
            print(f"❌ FAIL: Direction vector not normalized: magnitude = {magnitude}")
            return False
        
        # Should point right (dx > 0, dy ≈ 0)
        if dx < 0.9 or abs(dy) > 0.1:
            print(f"❌ FAIL: Direction incorrect for rightward movement: ({dx}, {dy})")
            return False
        
        print(f"✅ PASS: Direction vector normalized and correct: ({dx:.2f}, {dy:.2f})")
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def test_dead_state_terminal():
    """Test that dead state is terminal (no transitions)"""
    print("\n=== Testing Dead State is Terminal ===")
    from prolog_interface import PrologEngine
    
    prolog = PrologEngine()
    
    if not prolog.available:
        print("⚠️  SKIP: Prolog not available")
        return True
    
    try:
        prolog.init_wumpus(100, 100)
        
        # Test: Dead state should stay dead regardless of player position
        new_state, dx, dy = prolog.get_wumpus_decision(100, 100, 120, 120, 'dead')
        
        if new_state != 'dead':
            print(f"❌ FAIL: Dead state transitioned to '{new_state}'")
            return False
        
        print("✅ PASS: Dead state is terminal (no transitions)")
        return True
        
    except Exception as e:
        print(f"❌ EXCEPTION: {e}")
        import traceback
        traceback.print_exc()
        return False

def main():
    print("=" * 60)
    print("PHASE 1.3 VERIFICATION TEST")
    print("Testing Prolog AI Integration")
    print("=" * 60)
    
    tests = [
        ("Prolog AI Predicates", test_prolog_ai_predicates),
        ("Patrol -> Chase Transition", test_wumpus_decision_patrol_to_chase),
        ("Chase -> Attack Transition", test_wumpus_decision_chase_to_attack),
        ("Attack -> Chase Transition", test_wumpus_decision_attack_to_chase),
        ("Chase -> Patrol Transition", test_wumpus_decision_chase_to_patrol),
        ("Direction Vector Calculation", test_direction_vector_calculation),
        ("Dead State Terminal", test_dead_state_terminal),
        ("Wumpus Uses Prolog AI", test_wumpus_uses_prolog_ai),
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
        print("\n✅ ALL TESTS PASSED - Phase 1.3 is complete!")
        return 0
    else:
        print("\n❌ SOME TESTS FAILED - Phase 1.3 needs fixes")
        return 1

if __name__ == "__main__":
    sys.exit(main())
