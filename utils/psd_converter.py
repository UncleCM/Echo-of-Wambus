"""
PSD to PNG converter utility
Converts Photoshop PSD files to PNG format for use in Pygame
"""

import os
from typing import Optional
from PIL import Image
from psd_tools import PSDImage

def convert_psd_to_png(psd_path: str, output_path: Optional[str] = None) -> str:
    """
    Convert a PSD file to PNG format
    
    Args:
        psd_path: Path to the input PSD file
        output_path: Optional output path. If None, uses same name with .png extension
        
    Returns:
        Path to the converted PNG file
    """
    try:
        # Load the PSD file
        psd = PSDImage.open(psd_path)
        
        # Convert to PIL Image
        pil_image = psd.composite()
        
        # Determine output path
        if output_path is None:
            output_path = os.path.splitext(psd_path)[0] + ".png"
        
        # Ensure output directory exists
        os.makedirs(os.path.dirname(output_path), exist_ok=True)
        
        # Save as PNG
        pil_image.save(output_path, "PNG")
        
        print(f"Successfully converted: {psd_path} -> {output_path}")
        return output_path
        
    except Exception as e:
        print(f"Error converting {psd_path}: {e}")
        return None

def convert_all_psd_files(directory: str, output_directory: Optional[str] = None):
    """
    Convert all PSD files in a directory to PNG format
    
    Args:
        directory: Directory containing PSD files
        output_directory: Optional output directory. If None, saves in same location
    """
    if output_directory is None:
        output_directory = directory
    
    converted_files = []
    
    # Find all PSD files recursively
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.lower().endswith('.psd'):
                psd_path = os.path.join(root, file)
                
                # Calculate relative path for output
                rel_path = os.path.relpath(psd_path, directory)
                rel_dir = os.path.dirname(rel_path)
                output_name = os.path.splitext(file)[0] + ".png"
                
                if rel_dir:
                    output_path = os.path.join(output_directory, rel_dir, output_name)
                else:
                    output_path = os.path.join(output_directory, output_name)
                
                # Convert the file
                result = convert_psd_to_png(psd_path, output_path)
                if result:
                    converted_files.append(result)
    
    print(f"Converted {len(converted_files)} PSD files to PNG")
    return converted_files

def main():
    """Main function for command-line usage"""
    import sys
    
    if len(sys.argv) < 2:
        print("Usage: python psd_converter.py <psd_file_or_directory> [output_directory]")
        print("Example: python psd_converter.py assets/Map/_PSD")
        return
    
    input_path = sys.argv[1]
    output_dir = sys.argv[2] if len(sys.argv) > 2 else None
    
    if os.path.isfile(input_path):
        # Single file conversion
        convert_psd_to_png(input_path, output_dir)
    elif os.path.isdir(input_path):
        # Directory conversion
        convert_all_psd_files(input_path, output_dir)
    else:
        print(f"Error: {input_path} is not a valid file or directory")

if __name__ == "__main__":
    main()
