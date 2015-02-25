#!/bin/tcsh 
#PBS -S /bin/csh 
#PBS -l nodes=1:ppn=1,walltime=6:00:00 
#PBS -j oe 
#PBS -r y 
#PBS -o /work/Carolyn.Whitlock/fudge/stdout//ind.${MOAB_JOBNAME}.${MOAB_JOBID} 
#PBS -N fudge_run 
#PBS -E 
#PBS -q batch 
source /home/cew/Code/fudge2014//utils/bin/init 
mkdir -p /nbhome/cew/Code/testing//scripts/RR/RRprp1-CDFt-B38ap-testL01K00//log/
mkdir -p $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/geomasks/red_river_0p1/OneD/
mkdir -p $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks
gcp --sync -cd /archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/
mkdir -p $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks
gcp --sync -cd /archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_19610101-20051231.nc $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/
mkdir -p $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks
gcp --sync -cd /archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/maskdays_bymonth_20060101-20991231.nc $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/timemasks/
mkdir -p $TMPDIR/archive/esd/PROJECTS/DOWNSCALING///OBS_DATA/GRIDDED_OBS//livneh/historical//atmos/day/r0i0p0/v1p2/pr/SCCSC0p1/OneD//
mkdir -p $TMPDIR/archive/esd/PROJECTS/DOWNSCALING///GCM_DATA/CMIP5//MPI-ESM-LR/historical//atmos/day/r1i1p1/v20111006/pr/SCCSC0p1/OneD//
mkdir -p $TMPDIR/archive/esd/PROJECTS/DOWNSCALING///GCM_DATA/CMIP5//MPI-ESM-LR/rcp85//atmos/day/r1i1p1/v20111014/pr/SCCSC0p1/OneD//
gcp --sync -cd /archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/geomasks/red_river_0p1/OneD//*I181_J31-170.nc $TMPDIR/archive/esd/PROJECTS/DOWNSCALING/3ToThe5th/masks/geomasks/red_river_0p1/OneD//
gcp -cd /archive/esd/PROJECTS/DOWNSCALING///OBS_DATA/GRIDDED_OBS//livneh/historical//atmos/day/r0i0p0/v1p2/pr/SCCSC0p1/OneD///*19610101-20051231.I181_J31-170.nc $TMPDIR/archive/esd/PROJECTS/DOWNSCALING///OBS_DATA/GRIDDED_OBS//livneh/historical//atmos/day/r0i0p0/v1p2/pr/SCCSC0p1/OneD///
gcp -cd /archive/esd/PROJECTS/DOWNSCALING///GCM_DATA/CMIP5//MPI-ESM-LR/historical//atmos/day/r1i1p1/v20111006/pr/SCCSC0p1/OneD///*19610101-20051231.I181_J31-170.nc $TMPDIR/archive/esd/PROJECTS/DOWNSCALING///GCM_DATA/CMIP5//MPI-ESM-LR/historical//atmos/day/r1i1p1/v20111006/pr/SCCSC0p1/OneD///
gcp -cd /archive/esd/PROJECTS/DOWNSCALING///GCM_DATA/CMIP5//MPI-ESM-LR/rcp85//atmos/day/r1i1p1/v20111014/pr/SCCSC0p1/OneD///*20060101-20991231.I181_J31-170.nc $TMPDIR/archive/esd/PROJECTS/DOWNSCALING///GCM_DATA/CMIP5//MPI-ESM-LR/rcp85//atmos/day/r1i1p1/v20111014/pr/SCCSC0p1/OneD///
mkdir -p /work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108/
mkdir -p /work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108//pr_qcmask
mkdir -p $TMPDIR/work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108//  
mkdir -p $TMPDIR/work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108//pr_qcmask/  
R --vanilla < /nbhome/cew/Code/testing//scripts/RR/RRprp1-CDFt-B38ap-testL01K00//runcode/code.pr.RRprp1-CDFt-B38ap-testL01K00.I181_J31-170.2015-02-05.09:43:42.442345 > /nbhome/cew/Code/testing//scripts/RR/RRprp1-CDFt-B38ap-testL01K00//log//out.RRprp1-CDFt-B38ap-testL01K00.I181_J31-170.2015-02-05.09:43:42.442345 &
wait 
gcp -cd $TMPDIR/work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108//*.I181_J31-170.nc /work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108// 
gcp -cd $TMPDIR/work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108//pr_qcmask/*.I181_J31-170.nc /work/cew//downscaled/NOAA-GFDL/MPI-ESM-LR/rcp85/day/atmos/day/r1i1p1/v20111014/RRprp1-CDFt-B38ap-testL01K00/pr/RR/OneD/v20140108//pr_qcmask/ 
