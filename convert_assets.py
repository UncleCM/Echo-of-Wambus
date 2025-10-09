#!/usr/bin/env python3
"""
Asset conversion script
Converts PSD files to PNG format for use in the game
"""

import os
import sys
from utils.psd_converter import convert_all_psd_files

def main():
    """Convert all PSD assets to PNG format"""
    print("Converting PSD assets to PNG format...")
    print("=" * 50)
    
    # Convert map PSD files
    map_psd_dir = "assets/Map/_PSD"
    if os.path.exists(map_psd_dir):
        print(f"Converting map PSD files from: {map_psd_dir}")
        convert_all_psd_files(map_psd_dir, "assets/Map")
    else:
        print(f"Warning: Map PSD directory not found: {map_psd_dir}")
    
    print("\nAsset conversion complete!")
    print("The game can now use the converted PNG files.")

if __name__ == "__main__":
    main()
