#!/bin/bash
#=============================================================================
# DESCRIPTION:
#   Generate platform-specific post-processing template
#   Creates a post.TEMPLATE with correct SBATCH headers for current platform
#
# USAGE:
#   utilities/create_post_template.sh <output_template_path>
#
# ARGUMENTS:
#   output_template_path - Where to create the platform-specific template
#
# EXAMPLES:
#   utilities/create_post_template.sh scripts/runscripts/templates/post.TEMPLATE_for_segment_runscript
#
#=============================================================================

output_template="$1"

if [[ -z "$output_template" ]]; then
    echo "Error: Output template path required"
    echo "Usage: $0 <output_template_path>"
    exit 1
fi

# Get script directory and detect platform
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PLATFORM=${PLATFORM:-$("${SCRIPT_DIR}/detect_platform.sh")}

echo "Creating post-processing template for platform: $PLATFORM"

# Load platform-specific SBATCH settings for post-processing
source "${SCRIPT_DIR}/../config/${PLATFORM}/sbatch_env_setter.sh" "postproc"

# Create output directory if it doesn't exist
output_dir="$(dirname "$output_template")"
mkdir -p "$output_dir"

# Generate platform-specific template
cat << 'EOF' > "$output_template"
#!/bin/bash
EOF

# Add platform-specific SBATCH headers
cat << EOF >> "$output_template"
#SBATCH --job-name=${SBATCH_JOB_NAME}
#SBATCH --partition=${SBATCH_PARTITION}
EOF

# Add either --ntasks or --nodes depending on what's configured
if [[ -n "${SBATCH_NTASKS:-}" ]]; then
    echo "#SBATCH --ntasks=${SBATCH_NTASKS}" >> "$output_template"
elif [[ -n "${SBATCH_NODES:-}" ]]; then
    echo "#SBATCH --nodes=${SBATCH_NODES}" >> "$output_template"
fi

# Add remaining SBATCH parameters
cat << EOF >> "$output_template"
#SBATCH --cpus-per-task=${SBATCH_CPUS_PER_TASK}
#SBATCH --time=${SBATCH_TIME}
#SBATCH --account=${SBATCH_ACCOUNT}
#SBATCH --output=LOG.slurm-post_run-%j.o
#SBATCH --mem=${SBATCH_MEM}
EOF


# Make the template executable
chmod +x "$output_template"

