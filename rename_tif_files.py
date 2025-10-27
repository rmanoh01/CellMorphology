import csv
import os
import sys
from pathlib import Path

def load_treatment_mapping(csv_path):
    """Load the Surgery ID to Treatment Group mapping from CSV."""
    mapping = {}
    with open(csv_path, 'r', encoding='utf-8-sig') as f:
        reader = csv.DictReader(f)
        for row in reader:
            surgery_id = row['Surgery ID'].strip()
            treatment = row['Treatment Group'].strip()
            # Store both with and without leading zeros for flexible matching
            mapping[surgery_id] = treatment
            mapping[surgery_id.lstrip('0')] = treatment  # version without leading zeros
    return mapping

def extract_surgery_id(filename):
    """Extract surgery ID from filename like 'processed_03042025003_caudate_1.tif'."""
    # Remove 'processed_' prefix if present
    name = filename
    if name.startswith('processed_'):
        name = name[len('processed_'):]
    
    # Extract the first part before the first underscore (the surgery ID)
    parts = name.split('_')
    if parts:
        # Strip leading zeros for consistent matching
        return parts[0].lstrip('0')
    return None

def rename_files(folder_path, csv_path, dry_run=True):
    """Rename .tif files in the folder by adding treatment codes."""
    # Load the treatment mapping
    treatment_map = load_treatment_mapping(csv_path)
    
    print(f"Loaded {len(treatment_map)} treatment mappings from CSV")
    print(f"\nScanning folder: {folder_path}")
    
    # Find all .tif files
    folder = Path(folder_path)
    tif_files = list(folder.glob('*.tif')) + list(folder.glob('*.tiff'))
    
    if not tif_files:
        print("No .tif files found in the folder!")
        return
    
    print(f"Found {len(tif_files)} .tif files\n")
    
    renamed_count = 0
    not_found_count = 0
    
    for tif_file in tif_files:
        original_name = tif_file.name
        surgery_id = extract_surgery_id(original_name)
        
        if surgery_id and surgery_id in treatment_map:
            treatment = treatment_map[surgery_id]
            
            # Create new filename: insert treatment code before .tif
            name_without_ext = tif_file.stem
            extension = tif_file.suffix
            new_name = f"{name_without_ext}_{treatment}{extension}"
            new_path = tif_file.parent / new_name
            
            if dry_run:
                print(f"Would rename:")
                print(f"  {original_name}")
                print(f"  -> {new_name}")
                print()
            else:
                tif_file.rename(new_path)
                print(f"Renamed: {original_name} -> {new_name}")
            
            renamed_count += 1
        else:
            print(f"⚠ No treatment found for: {original_name} (Surgery ID: {surgery_id})")
            not_found_count += 1
    
    print(f"\n{'DRY RUN - ' if dry_run else ''}Summary:")
    print(f"  Files to rename: {renamed_count}")
    print(f"  Files without matching treatment: {not_found_count}")
    
    if dry_run:
        print("\nThis was a DRY RUN. No files were actually renamed.")
        print("Run with --execute to perform the actual renaming.")

if __name__ == "__main__":
    # Default paths
    csv_path = input("Enter the path to your CSV file containing treatment mappings: ").strip()
    folder_path = input("Enter the path to your folder containing .tif files: ").strip()
    
    if not folder_path:
        print("No folder path provided!")
        sys.exit(1)
    
    # Check if user wants to execute or just preview
    execute = '--execute' in sys.argv
    
    rename_files(folder_path, csv_path, dry_run=not execute)