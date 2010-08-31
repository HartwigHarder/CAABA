#! /bin/tcsh -f
# Time-stamp: <2010-08-05 20:00:11 sander>

##############################################################################

set outputdir = "/mnt/modeloutput/`date +'%Y_%m_%d_%H-%M-%S'`"
mkdir $outputdir
mkdir $outputdir/runs
if (! -d $outputdir/montecarlo) mkdir $outputdir/montecarlo
set basedir = `pwd`

# delete output from old runs:
rm caaba_*.nc

set logfile = "$outputdir/montecarlo.log"

# Define the number of Monte-Carlo runs here. Seeds are taken from
# mcexp_seed.txt which can for example be created with the number
# generator messy/mbm/rnd/rnd.f90-mecca. Note that this number must not
# be larger than the number of lines in mcexp_seed.txt.

#set maxline=5
set maxline=5
#set maxline=1000

set line=0
while ($line<$maxline)
  @ line++ 
  set mcexp_seed = `head -n$line montecarlo/mcexp_seed.txt | tail -n1`
  set mcrun = `printf "%4.4d" $line`
  mkdir $outputdir/runs/$mcrun
  # test if mecca.nml is suitable for Monte-Carlo runs:
  set test = `grep mcexp_seed mecca.nml`
  if ( "$test" == "" ) then
    echo "ERROR: mecca.nml does not contain mcexp_seed"
    exit
  endif
  # copy to a temporary file. In-place editing with "sed -i" is not
  # possible because it converts the link mecca.nml into a real file
  cp mecca.nml tmp_mecca.nml
  sed "s|.*mcexp_seed.*| mcexp_seed = $mcexp_seed|" tmp_mecca.nml > mecca.nml
  rm tmp_mecca.nml
  printf "Simulation $mcrun with seed = %10d" $mcexp_seed | tee -a $logfile
  ./caaba.exe > caaba.log
  set MaxTime = (`ncks -M caaba_mecca.nc | awk '/name = time, size =/ {print $8}'`)
  printf " has finished (MaxTime=%d)\n" $MaxTime | tee -a $logfile
  # check if MaxTime is different from previous run:
  if ( "$line" != "1" ) then
    if ( "$MaxTime" != "$MaxTime0" ) set DifferentMaxTime
  endif
  set MaxTime0 = $MaxTime
  cp  caaba.log mecca.nml *.nc $outputdir/runs/$mcrun
end

##############################################################################

echo "Creating zip file of caaba model code. Please wait..."
gmake zip >> $logfile
echo "now moving the zip file"
mv `basename $PWD`.zip $outputdir

##############################################################################

# merge netcdf files, based on example from:
# http://nco.sourceforge.net/nco.html#Averaging-vs_002e-Concatenating

cd $outputdir

if (${?DifferentMaxTime}) then
  set fullfilenames = (caaba_mecca_c_end.nc caaba_mecca_k_end.nc)
else
  set fullfilenames = (runs/0001/*.nc)
endif
echo "\nMerging the netcdf files:"

foreach fullfilename ($fullfilenames)

  set ncfile = `basename $fullfilename`
  echo "Working on $ncfile"
  echo -n "Monte-Carlo run"
  foreach mcrun (runs/*)
    set basemcrun = `basename $mcrun`
    echo -n " $basemcrun"
    set oldnc = "$mcrun/$ncfile"
    set newnc = "tmp_${basemcrun}_$ncfile"
    # convert lon into the UNLIMITED record dimension:
    ncpdq -a lon,time -O $oldnc $newnc >> $logfile
    # put Monte-Carlo run number into lon:
    $basedir/tools/ncclamp/ncclamp $newnc lon 0 $mcrun eq
  end
  echo " done"

  # concatenate files along lon:
  ncrcat -O tmp_*_$ncfile tmp_$ncfile

  # revert to time as record dimension:
  ncpdq -a time,lon -O tmp_$ncfile $ncfile >> $logfile

end

# ferret jnl files for scatter plots:
mv $basedir/_scatterplot1.jnl tmp_scatterplot1.jnl
mv $basedir/_scatterplot2.jnl .
mv $basedir/_histogram_k.jnl .
sort tmp_scatterplot1.jnl > _scatterplot1.jnl
ln -s $basedir/jnl/montecarlo.jnl     .
ln -s $basedir/jnl/scatterplot_mc.jnl .

rm tmp_*.nc tmp_*.jnl
cd -

##############################################################################

echo "\nThe model output is now in:"
echo "$outputdir/montecarlo/`basename ${outputdir}`"
cd $outputdir
ls runs
ls -la *.*
cd -

exit

##############################################################################
