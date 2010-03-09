#! /bin/tcsh -f
# Time-stamp: <2010-02-22 17:36:45 sander>

##############################################################################

if (! -d output/montecarlo) mkdir output/montecarlo
set outputdir = "$PWD/output/montecarlo/`date +'%Y-%m-%d-%H:%M:%S'`"
mkdir $outputdir

set logfile = "$outputdir/montecarlo.log"

set maxline=5

set line=0
while ($line<$maxline)
  @ line++ 
  set mcfct_seed = `head -n$line montecarlo/mcfct_seed.txt | tail -n1`
  set mcrun = `printf "%4.4d" $line`
  mkdir $outputdir/$mcrun
  # test if mecca.nml is suitable for Monte-Carlo runs:
  set test = `grep mcfct_seed mecca.nml`
  if ( "$test" == "" ) then
    echo "ERROR: mecca.nml does not contain mcfct_seed"
    exit
  endif
  # copy to a temporary file. In-place editing with "sed -i" is not
  # possible because it converts the link mecca.nml into a real file
  cp mecca.nml tmp_mecca.nml
  sed "s|.*mcfct_seed.*| mcfct_seed = $mcfct_seed|" tmp_mecca.nml > mecca.nml
  rm tmp_mecca.nml
  echo "Run number $mcrun with seed = $mcfct_seed" | tee -a $logfile
  ./caaba.exe > caaba.log
  cp -p caaba.log mecca.nml *.nc *.dat $outputdir/$mcrun
end

##############################################################################

echo "Creating zip file of caaba model code. Please wait..."
gmake zip >> $logfile
mv `basename $PWD`.zip $outputdir

##############################################################################

echo "Merging netcdf files"

# merge netcdf files, based on example from:
# http://nco.sourceforge.net/nco.html#Averaging-vs_002e-Concatenating

cd $outputdir

foreach fullfilename (0001/*.nc)

  set ncfile = `basename $fullfilename`
  echo "Working on $ncfile"

  echo -n "Monte-Carlo run"
  foreach mcrun (0*)
    echo -n " $mcrun"
    set oldnc = "$mcrun/$ncfile"
    set newnc = "tmp_${mcrun}_$ncfile"
    # convert lon into the UNLIMITED record dimension:
    ncpdq -a lon,time -O $oldnc $newnc >> $logfile
    # put Monte-Carlo run number into lon:
    ncclamp $newnc lon 0 $mcrun eq
  end
  echo " done"

  # concatenate files along lon:
  ncrcat -O tmp_*_$ncfile tmp_$ncfile

  # revert to time as record dimension:
  ncpdq -a time,lon -O tmp_$ncfile $ncfile >> $logfile

end

rm tmp_*.nc
cd -

##############################################################################

echo "The model output is now in ${outputdir}:"
ls -la $outputdir

exit

##############################################################################
