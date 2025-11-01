"""
Phase 1.1 Verification Test
Tests that Entity base class refactoring is complete and working
"""

import sys
import os

# Add parent directory to path so we can import game modules
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '../..')))

def test_entity_class():
    """Test that Entity class has all required methods"""
    print("\n=== Testing Entity Base Class ===")
    from entity import Entity
    
    required_methods = [
        '__init__', 'setup_sprite', 'move', 'collision', 
        'animate', 'take_damage', 'heal', 'on_death', 
        'update', 'load_sprite_strip'
    ]
    
    missing = []
    for method in required_methods:
        if not hasattr(Entity, method):
            missing.append(method)
    
    if missing:
        print(f"❌ FAIL: Entity missing methods: {missing}")
        return False
    else:
        print("✅ PASS: Entity has all required methods")
        return True

def test_player_inheritance():
    """Test that Player properly inherits from Entity"""
    print("\n=== Testing Player Inheritance ===")
    from player import Player
    from entity import Entity
    
    # Check inheritance
    if not issubclass(Player, Entity):
        print("❌ FAIL: Player does not inherit from Entity")
        return False
    print("✅ PASS: Player inherits from Entity")
    
    # Check Player-specific methods exist
    player_methods = ['load_animations', 'input', 'animate', 'update']
    missing = []
    for method in player_methods:
        if not hasattr(Player, method):
            missing.append(method)
    
    if missing:
        print(f"❌ FAIL: Player missing methods: {missing}")
        return False
    print("✅ PASS: Player has all player-specific methods")
    
    # Check that duplicate methods were removed (they should be inherited from Entity)
    # Player should NOT have move() or collision() unless it's overriding
    player_dict = Player.__dict__
    
    # These should NOT be in Player's own __dict__ (should be inherited)
    should_not_have = []
    if 'move' in player_dict and player_dict['move'].__qualname__.startswith('Player'):
        should_not_have.append('move')
    if 'collision' in player_dict and player_dict['collision'].__qualname__.startswith('Player'):
        should_not_have.append('collision')
    if 'load_sprite_strip' in player_dict:
        should_not_have.append('load_sprite_strip')
    
    if should_not_have:
        print(f"⚠️  WARNING: Player still has duplicate methods: {should_not_have}")
        print("   These should be inherited from Entity, not redefined in Player")
        return False
    
    print("✅ PASS: Player uses inherited Entity methods (no duplicates)")
    return True

def test_prolog_initialization():
    """Test that PrologEngine initializes correctly"""
    print("\n=== Testing Prolog Initialization ===")
    from prolog_interface import PrologEngine
    
    # Create engine
    engine = PrologEngine()
    
    # Check attributes exist
    if not hasattr(engine, 'available'):
        print("❌ FAIL: PrologEngine missing 'available' attribute")
        return False
    
    if not hasattr(engine, 'prolog'):
        print("❌ FAIL: PrologEngine missing 'prolog' attribute")
        return False
    
    print(f"✅ PASS: PrologEngine has required attributes")
    print(f"   - available: {engine.available}")
    print(f"   - prolog: {type(engine.prolog)}")
    
    return True

def test_file_structure():
    """Test that all required files exist"""
    print("\n=== Testing File Structure ===")
    
    required_files = [
        'entity.py',
        'player.py', 
        'main.py',
        'prolog_interface.py',
        'game_logic.pl',
        'Settings.py',
        'sprites.py',
        'groups.py'
    ]
    
    missing = []
    for filename in required_files:
        if not os.path.exists(filename):
            missing.append(filename)
    
    if missing:
        print(f"❌ FAIL: Missing files: {missing}")
        return False
    
    print("✅ PASS: All required files exist")
    return True

def main():
    print("=" * 60)
    print("PHASE 1.1 VERIFICATION TEST")
    print("Checking Entity base class refactoring")
    print("=" * 60)
    
    tests = [
        test_file_structure,
        test_entity_class,
        test_player_inheritance,
        test_prolog_initialization
    ]
    
    results = []
    for test in tests:
        try:
            result = test()
            results.append(result)
        except Exception as e:
            print(f"❌ EXCEPTION in {test.__name__}: {e}")
            import traceback
            traceback.print_exc()
            results.append(False)
    
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    passed = sum(results)
    total = len(results)
    
    print(f"Tests passed: {passed}/{total}")
    
    if all(results):
        print("\n✅ ALL TESTS PASSED - Phase 1.1 is COMPLETE!")
        print("\nReady for Phase 1.2: Create Wumpus class")
        return 0
    else:
        print("\n❌ SOME TESTS FAILED - Phase 1.1 needs fixes")
        return 1

if __name__ == '__main__':
    sys.exit(main())
