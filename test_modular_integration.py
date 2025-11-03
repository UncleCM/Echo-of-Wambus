#!/usr/bin/env python3
"""
Test script to verify modular Prolog works with the game.
This loads the modular architecture and runs basic integration tests.
"""

from pyswip import Prolog
import sys


def test_modular_prolog():
    print("=" * 60)
    print("Testing Modular Prolog Integration with Game")
    print("=" * 60)

    try:
        prolog = Prolog()
        print("\n✓ Prolog engine initialized")

        # Load modular game logic
        print("\nLoading modular game logic...")
        prolog.consult("game_logic_modular.pl")
        print("✓ Modular game logic loaded")

        # Test 1: Initialize game
        print("\n[Test 1] Initializing game...")
        result = list(prolog.query("init_game"))
        print("✓ Game initialized")

        # Test 2: Add collision boxes (movement module)
        print("\n[Test 2] Adding collision boxes...")
        list(prolog.query("add_collision_box(100, 100, 50, 50)"))
        list(prolog.query("add_collision_box(200, 200, 100, 100)"))
        print("✓ Collision boxes added")

        # Test 3: Check collision (should detect)
        print("\n[Test 3] Testing collision detection...")
        result = list(prolog.query("check_collision(110, 110, 20, 20)"))
        if result:
            print("✓ Collision detected correctly at (110, 110)")
        else:
            print("✗ FAIL: Should detect collision")
            return False

        # Test 4: Check no collision (should not detect)
        result = list(prolog.query("check_collision(10, 10, 20, 20)"))
        if not result:
            print("✓ No collision correctly detected at (10, 10)")
        else:
            print("✗ FAIL: Should not detect collision")
            return False

        # Test 5: Add fall zones
        print("\n[Test 4] Adding fall zones...")
        list(prolog.query("add_fall_zone(300, 300, 100, 100)"))
        print("✓ Fall zones added")

        # Test 6: Check fall detection
        print("\n[Test 5] Testing fall detection...")
        result = list(prolog.query("check_fall(320, 320, 32, 32, 10)"))
        if result:
            print("✓ Fall detected correctly at (320, 320)")
        else:
            print("✗ FAIL: Should detect fall")
            return False

        # Test 7: Add water zones
        print("\n[Test 6] Adding water zones...")
        list(prolog.query("add_water_zone(400, 400, 100, 100)"))
        print("✓ Water zones added")

        # Test 8: Check water detection
        print("\n[Test 7] Testing water detection...")
        result = list(prolog.query("check_water(420, 420, 32, 32, 10)"))
        if result:
            print("✓ Water detected correctly at (420, 420)")
        else:
            print("✗ FAIL: Should detect water")
            return False

        # Test 9: Movement resolution
        print("\n[Test 8] Testing movement resolution...")
        result = list(prolog.query("resolve_movement(0, 0, 10, 10, 32, 32, X, Y)"))
        if result and result[0]["X"] == 10 and result[0]["Y"] == 10:
            print(
                f"✓ Movement resolved correctly: (0,0) + (10,10) = ({result[0]['X']}, {result[0]['Y']})"
            )
        else:
            print("✗ FAIL: Movement resolution incorrect")
            return False

        # Test 10: Map system initialization
        print("\n[Test 9] Testing map system initialization...")
        list(prolog.query("init_map_system(800, 600, 32, 32, 32)"))
        result = list(prolog.query("map_system:map_size(W, H)"))
        if result and result[0]["W"] == 800 and result[0]["H"] == 600:
            print(f"✓ Map initialized: {result[0]['W']}x{result[0]['H']}")
        else:
            print("✗ FAIL: Map initialization incorrect")
            return False

        # Test 11: Build navigation grid
        print("\n[Test 10] Building navigation grid...")
        list(prolog.query("build_navigation_grid"))
        result = list(prolog.query("map_system:grid_cell(0, 0, Type)"))
        if result:
            print(f"✓ Navigation grid built (cell 0,0 type: {result[0]['Type']})")
        else:
            print("✗ FAIL: Navigation grid not built")
            return False

        # Test 12: Geometry module functions
        print("\n[Test 11] Testing geometry module...")
        result = list(
            prolog.query("geometry:rects_collide(0, 0, 50, 50, 25, 25, 50, 50)")
        )
        if result:
            print("✓ Geometry module working (rectangle collision)")
        else:
            print("✗ FAIL: Geometry module not working")
            return False

        print("\n" + "=" * 60)
        print("✓ ALL TESTS PASSED - Modular Prolog is working!")
        print("=" * 60)
        return True

    except Exception as e:
        print(f"\n✗ ERROR: {e}")
        import traceback

        traceback.print_exc()
        return False


if __name__ == "__main__":
    success = test_modular_prolog()
    sys.exit(0 if success else 1)
