# Current Processing Status

## Solid Part

1. run pre-processing chain: including testrun

```bash
cd scripts/processing-chains

bash run_hurricane_segments_preproc_chain.sh 1 -c ../../config/hurricane_config_width100km_reinit24h.toml

cd ../..

```

## Hacky Part
3. vertical interpolation

- first: manually check if test run was successful --> remember test run experiment dir
```bash
cd scripts/ic-bc
sbatch vert_interpol_runner.sh /work/bb1376/user/fabian/model/icon/icon-builds/icon-release-2024.07/experiments/hurricane-paulette2020-segments-width200km_reinit12h-segment2-20200909-exp108
```

*dirty hook for 1st segment*
- finally get `IC_vertically_interp*nc` files from <experiment_dir>
```
cd <experiment_dir>
bash /home/b/b380352/proj/2025-05_hurricane-centric-setup-tools/scripts/ic-bc/refwarm_mover.sh
cd -
```

*dirty hook for later segments*
```
cd test
for i in 1 2 3;do  bash test_remap_and_merge.sh2 2 $i; done
cd ..
```
- `project_width_config` is hard coded



## BUGS

### init_date bug
- wrong init_date is shown in experiment_dir if reinit12h is used.
- we might be limited to run only x=7 segments

### grid for seg2 width=200km and reinit24h crashed
- too many neigbors error in gridgen
- chain is blocked


## Old

### 2025-06-30


2. combined check and testrun starter utility

```bash
cd scripts/processing-chains

bash run_hurricane_testrun_chain.sh -c ../../config/hurricane_config_width100km_reinit12h.toml 2

cd ../..

```


### 2025-06-28

3. vertical interpolation

```bash 
cd scripts/ic-bc/

bash extract_heights_from_geoz.sh
```
- everthing hardcoded, need experiment directory
- final output is written to `target_ic_final_DOM0${k}.XXXXXX` under `~/scratch/icontools`
- files need to be moved to repsective `bc-init` folder

4. add missing vn
```bash
i=2

bash replace_uv_with_vn_in_icfile.sh /work/bb1376/data/icon/grids-extpar/hurricane-paulette2020-segments/seg1_width200km_reinit24h/hurricane-paulette2020-segments-seg1_dom${i}_DOM01.nc /work/bb1376/data/icon/bc-init/hurricane-paulette2020-segments/seg1_width200km_reinit24h/ifces2-atlanXL-ML_hurricane-paulette2020-segments_seg1_width200km_reinit24h_DOM0${i}_warmini.novn.nc
```
- `warmini` needs to be renamed to `warmini.novn`
- final output is written to `warmini.nonv.nc.new` and needs to be renamed
