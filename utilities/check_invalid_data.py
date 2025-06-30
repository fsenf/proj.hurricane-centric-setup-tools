#!/usr/bin/env python3
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/check_invalid_data.py

"""
Check NetCDF files for invalid data points (NaN, Inf, missing values)

Usage:
    check_invalid_data.py <netcdf_file> [--threshold PERCENT]

Arguments:
    netcdf_file     Path to NetCDF file to check
    --threshold     Optional threshold percentage for warnings (default: 0.1%)
"""

import sys
import argparse
import numpy as np
import xarray as xr
from tqdm import tqdm


def count_invalid_values(var_data, fill_value=None):
    """Count NaN, Inf, and fill values in a variable."""
    total_count = np.prod(var_data.shape)
    
    # Count NaN values
    nan_count = np.count_nonzero(np.isnan(var_data))
    
    # Count Inf values
    inf_count = np.count_nonzero(np.isinf(var_data))
    
    # Count fill values if specified
    fill_count = 0
    if fill_value is not None:
        # For floating point fill values, use isclose to handle precision issues
        if np.issubdtype(var_data.dtype, np.floating):
            fill_count = np.count_nonzero(np.isclose(var_data, fill_value))
        else:
            fill_count = np.count_nonzero(var_data == fill_value)
    
    # Total invalid
    invalid_count = nan_count + inf_count + fill_count
    
    # Remove double counting (a NaN might also be counted as fill value)
    invalid_count = min(invalid_count, total_count)
    
    return {
        "total": total_count,
        "nan": nan_count,
        "inf": inf_count,
        "fill": fill_count,
        "invalid": invalid_count,
        "valid": total_count - invalid_count,
        "percent_invalid": 100 * invalid_count / total_count if total_count > 0 else 0
    }


def check_netcdf_file(file_path, threshold=0.1):
    """Check a NetCDF file for invalid data points."""
    print(f"Analyzing file: {file_path}")
    
    try:
        # Open the dataset
        with xr.open_dataset(file_path) as ds:
            var_results = {}
            total_invalid = 0
            total_points = 0
            
            # Print metadata
            print("\nFile Attributes:")
            for key, value in ds.attrs.items():
                print(f"  {key}: {value}")
            
            # Process each variable
            print("\nChecking variables for invalid data:")
            for var_name, var in tqdm(ds.variables.items(), desc="Variables"):
                # Skip coordinate variables
                if var_name in ds.coords:
                    continue
                
                # Get fill value if it exists
                fill_value = None
                if hasattr(var, "_FillValue"):
                    fill_value = var._FillValue
                elif hasattr(var, "missing_value"):
                    fill_value = var.missing_value
                
                # Get the data (load into memory)
                var_data = var.values
                
                # Count invalid values
                counts = count_invalid_values(var_data, fill_value)
                var_results[var_name] = counts
                
                total_invalid += counts["invalid"]
                total_points += counts["total"]
            
            # Print results
            print("\nResults:")
            print(f"{'Variable':<30} {'Total Points':>12} {'Invalid Points':>15} {'% Invalid':>10}")
            print("-" * 70)
            
            # Sort variables by percentage of invalid values (descending)
            sorted_vars = sorted(
                var_results.items(), 
                key=lambda x: x[1]["percent_invalid"],
                reverse=True
            )
            
            for var_name, counts in sorted_vars:
                print(f"{var_name:<30} {counts['total']:>12,d} {counts['invalid']:>15,d} {counts['percent_invalid']:>9.2f}%")
                
                # Show detailed breakdown if there are invalid values
                if counts["invalid"] > 0:
                    print(f"  - NaN: {counts['nan']:,d}, Inf: {counts['inf']:,d}, Fill value: {counts['fill']:,d}")
                    
                    # Warning for high percentage of invalid data
                    if counts["percent_invalid"] > threshold:
                        print(f"  ⚠️  WARNING: More than {threshold}% of data points are invalid!")
            
            # Print summary
            print("\nSummary:")
            total_percent = 100 * total_invalid / total_points if total_points > 0 else 0
            print(f"Total variables checked: {len(var_results)}")
            print(f"Total data points: {total_points:,d}")
            print(f"Total invalid points: {total_invalid:,d} ({total_percent:.2f}%)")
            
            # Final verdict
            if total_invalid > 0:
                if total_percent > threshold:
                    print(f"\n⚠️  WARNING: File contains {total_percent:.2f}% invalid data (threshold: {threshold}%)")
                    return 1
                else:
                    print("\n✓ File contains some invalid data but below threshold")
                    return 0
            else:
                print("\n✅ File contains no invalid data points")
                return 0
    
    except Exception as e:
        print(f"\n❌ Error analyzing file: {e}", file=sys.stderr)
        return 2


def main():
    parser = argparse.ArgumentParser(description="Check NetCDF files for invalid data")
    parser.add_argument("netcdf_file", help="Path to NetCDF file to analyze")
    parser.add_argument("--threshold", type=float, default=0.1,
                      help="Warning threshold percentage (default: 0.1%%)")
    args = parser.parse_args()
    
    return check_netcdf_file(args.netcdf_file, args.threshold)


if __name__ == "__main__":
    sys.exit(main())