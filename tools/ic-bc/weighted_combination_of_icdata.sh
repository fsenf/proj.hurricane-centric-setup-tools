#!/bin/bash
#=============================================================================
#
# Levante cpu batch job parameters
#
#SBATCH --account=bb1376
#SBATCH --job-name=weighted_ic_combi
#SBATCH --partition=compute
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --exclusive
#SBATCH --time=00:10:00
#SBATCH --chdir=/scratch/b/b380352/icontools
#SBATCH --mem=0
#=============================================================================


module load python3

iseg=$*

script_path=/work/bb1376/user/fabian/analysis/2023-03_ifces2-scalexa/scripts/

#script=${script_path}/53-Method-to-Combine-Grids-from-Different-Segments.py
script=${script_path}/58-Updated-Method-to-Combine-Center-Grids-from-Different-Segments.py

for dom in {1..3}; do

    python $script $iseg $dom &

done

wait

