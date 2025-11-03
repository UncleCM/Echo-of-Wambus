#!/usr/bin/env python3
"""Quick test to verify game can load modular Prolog without errors."""

import sys

sys.path.insert(0, "/Users/tanakrit/Documents/Areas/GitHub/Echo-of-Wambus")

print("=" * 60)
print("Testing Game with Modular Prolog")
print("=" * 60)

# Import settings
from Settings import USE_MODULAR_PROLOG

print(f"\nUSE_MODULAR_PROLOG = {USE_MODULAR_PROLOG}")

# Import and initialize Prolog engine
print("\nInitializing PrologEngine...")
from prolog_interface import PrologEngine

engine = PrologEngine()

if engine.available:
    print("✓ PrologEngine initialized successfully!")
    print(f"  Prolog available: {engine.available}")

    # Test basic functions
    print("\nTesting basic Prolog functions:")

    # Test collision
    engine.add_collision_box(100, 100, 50, 50)
    collision = engine.check_collision(110, 110, 20, 20)
    print(f"  ✓ Collision detection: {collision}")

    # Test movement
    result = engine.resolve_movement(0, 0, 10, 10, 32, 32)
    print(f"  ✓ Movement resolution: {result}")

    # Test safe position
    safe = engine.is_safe_position(500, 500, 32, 32, 10)
    print(f"  ✓ Safe position check: {safe}")

    print("\n" + "=" * 60)
    print("✓ Game can use modular Prolog successfully!")
    print("=" * 60)
else:
    print("✗ PrologEngine failed to initialize")
    sys.exit(1)
