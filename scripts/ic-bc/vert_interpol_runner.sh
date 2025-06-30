#!/bin/bash
# filepath: /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/vert_interpol_runner.sh
#=============================================================================
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=vinterpol_runner
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=32
#SBATCH --exclusive
#SBATCH --time=02:00:00
#SBATCH --chdir=/scratch/b/b380352/icontools
#SBATCH --mem=0
#=============================================================================

# Check if experiment directory is provided
if [ $# -lt 1 ]; then
    echo "Error: Experiment directory is required"
    echo "Usage: $0 [experiment_directory]"
    echo "  experiment_directory: Full path to experiment directory"
    exit 1
fi

experiment_dir="$1"

# Validate experiment directory
if [ ! -d "$experiment_dir" ]; then
    echo "Error: Experiment directory does not exist: $experiment_dir"
    exit 1
fi

cmd=/home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/calculate_vertical_interpolation.sh

echo "Starting vertical interpolation for all domains in: ${experiment_dir}"

for idom in {1..3}; do
    echo "Launching domain ${idom} processing..."
    bash $cmd "${experiment_dir}" ${idom}
done

# Wait for all background processes to complete
# wait

echo "All domains have been processed"
