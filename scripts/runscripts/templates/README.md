# Hurricane-Centric ICON Templates

## Template Overview

### Base Template
- `exp.TEMPLATE_for_segment_runscript` - Standard hurricane-centric configuration
  - Default: No custom microphysics parameters (uses ICON defaults)

### Derived Templates (Microphysics Variants)

All variants add `&twomom_mcrph_nml` namelist block after radiation namelist (~line 299).

#### CCN (Cloud Condensation Nuclei) Variants
- `exp.TEMPLATE_for_segment_runscript_lowCCN` 
  - **Change**: `ccn_type = 6` (low aerosol scenario)

- `exp.TEMPLATE_for_segment_runscript_highCCN`
  - **Change**: `ccn_type = 9` (high aerosol scenario)

#### Ice Terminal Velocity Variants
Base value: 27.7. Slow/fast are ±30% from base, xslow/xfast are ±60% from base.

- `exp.TEMPLATE_for_segment_runscript_xslowICE`
  - **Changes**: `ccn_type = 7`, `avel_i = 11.08`

- `exp.TEMPLATE_for_segment_runscript_slowICE`
  - **Changes**: `ccn_type = 7`, `avel_i = 19.39`

- `exp.TEMPLATE_for_segment_runscript_fastICE`
  - **Changes**: `ccn_type = 7`, `avel_i = 36.01`

- `exp.TEMPLATE_for_segment_runscript_xfastICE`
  - **Changes**: `ccn_type = 7`, `avel_i = 44.32`

#### Snow Terminal Velocity Variants
Base value: 400. Slow/fast are ±30% from base (wrong direction, kept as is). **Note: Snow physics is buggy.** xslow/xfast use correct ±60% direction.

- `exp.TEMPLATE_for_segment_runscript_xslowSNOW`
  - **Changes**: `ccn_type = 7`, `avel_s = 160.0`

- `exp.TEMPLATE_for_segment_runscript_slowSNOW`
  - **Changes**: `ccn_type = 7`, `avel_s = 520.0`

- `exp.TEMPLATE_for_segment_runscript_fastSNOW`
  - **Changes**: `ccn_type = 7`, `avel_s = 280.0`

- `exp.TEMPLATE_for_segment_runscript_xfastSNOW`
  - **Changes**: `ccn_type = 7`, `avel_s = 640.0`

#### Graupel Terminal Velocity Variants
Base value: 100. Slow/fast are ±30% from base, xslow/xfast are ±60% from base.

- `exp.TEMPLATE_for_segment_runscript_xslowGRAUPEL`
  - **Changes**: `ccn_type = 7`, `avel_g = 40.0`

- `exp.TEMPLATE_for_segment_runscript_slowGRAUPEL`
  - **Changes**: `ccn_type = 7`, `avel_g = 70.0`

- `exp.TEMPLATE_for_segment_runscript_fastGRAUPEL`
  - **Changes**: `ccn_type = 7`, `avel_g = 130.0`

- `exp.TEMPLATE_for_segment_runscript_xfastGRAUPEL`
  - **Changes**: `ccn_type = 7`, `avel_g = 160.0`

#### Rimed Particle Variants
Controls transition from ice to graupel via degree of void filling (alpha_spacefilling).

- `exp.TEMPLATE_for_segment_runscript_softRIMED`
  - Soft riming threshold - particles stay ice longer
  - **Changes**: `ccn_type = 7`, `alpha_spacefilling = 0.02`

- `exp.TEMPLATE_for_segment_runscript_denseRIMED`
  - Dense riming threshold - particles convert to graupel sooner  
  - **Changes**: `ccn_type = 7`, `alpha_spacefilling = 0.005`

#### Ice Particle Size Variants
Controls ice-to-snow conversion via diameter threshold (D_conv_ii).

- `exp.TEMPLATE_for_segment_runscript_smallICE`
  - Small ice threshold - earlier conversion to snow
  - **Changes**: `ccn_type = 7`, `D_conv_ii = 50.e-6`

- `exp.TEMPLATE_for_segment_runscript_largeICE`
  - Large ice threshold - later conversion to snow
  - **Changes**: `ccn_type = 7`, `D_conv_ii = 100.e-6`


## Usage
Templates are processed by segment workflow. Microphysics variants enable sensitivity testing of precipitation processes in hurricane simulations.