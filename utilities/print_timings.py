#!/usr/bin/env python3
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/utilities/find_icbc_file.py
"""
Find IC/BC file(s) based on segment number and configuration.

Usage:
    python find_icbc_file.py <config_file> <segment_number> <time_type> [-t]
    
Arguments:
    config_file: Path to hurricane_config.toml
    segment_number: Segment number (iseg)
    time_type: Either 'START' or 'END'
    -t: Optional flag to enable test mode
    
'
"""

import sys
import argparse
from helpers import print_timings


def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Find IC/BC files based on segment number')
    parser.add_argument('config_file', help='Path to hurricane_config.toml')
    parser.add_argument('segment_number', type=int, help='Segment number (iseg)')
    parser.add_argument('time_type', help="Either 'START' or 'END'")
    parser.add_argument('-t', '--test', action='store_true', help='Enable test mode')
    
    # Parse arguments
    args = parser.parse_args()
    
    # Set test_mode based on flag
    test_mode = args.test
    
    # Find file(s)
    times = print_timings(args.config_file, args.segment_number, args.time_type, test_mode)
    print(times)

if __name__ == "__main__":
    main()