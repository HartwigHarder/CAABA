#! /bin/tcsh -f

# creates temporary file inputfile.nc for each entry in $1.nc and calls CAABA
# original code: Hartwig Harder, 2009
# modified:      Rolf Sander,    2009-....

# Before starting this script, make sure that "ncks" from the netCDF
# Operators is available, e.g. with "nco_init".

##############################################################################

if ( $#argv == 0 ) then 
  echo " Usage: ./LoopCaaba <filename.nc> [LineNumber]"
  echo "   reads <filename.nc> and calls caaba for each entry"
  exit 1
endif

# define hline string
set hline = "---------------------------------------"
set hline = "$hline$hline"

echo $hline
echo "inputfile = $1"

set fname      = "$1"
set tmpFname   = "inputfile.nc"
set sum_dat    = "sum.dat"
set header_dat = "header.dat"

set OneShot = 0
# current line number in netcdf file 
if ( $#argv == 2 ) then # did we supply line number
  set OneShot = $2
endif

set line = 0

if ( $OneShot > 0 ) then
  set line = $OneShot
endif

if ( -e $sum_dat )    rm $sum_dat
if ( -e $header_dat ) rm $header_dat

set ret = (`ncks -O -d T_AX,$line $fname $tmpFname`) # read first line

##############################################################################

while (${#ret} == 0)
  set err = 0
  # --------------------------------------------------------------------------
  # pressure:
  set press = (`ncks -s "%10.10g" -H -C -d T_AX,$line -v press $fname`)
  set erg = (`echo $press | awk '{if ($1<0) print "ERROR"}'`)
  if ( ${#erg} > 0) then
    echo "Problem with pressure P = " $ret " in line " $line
    set err = 1
  endif
  # --------------------------------------------------------------------------
  # temperature:
  set temp = (`ncks -s "%10.10g" -H -C -d T_AX,$line -v temp $fname |awk '{print $1+273.15}'`)
  set erg = (`echo $temp | awk '{if ($1<0) print "ERROR"}'`)
  if ( ${#erg} > 0) then
    echo "Problem with temperature T = " $ret " in line " $line
    set err = 1
  endif
  # --------------------------------------------------------------------------
  # GPS time:
  set TimeGPS = (`ncks -s "%10.10g" -H -C -d T_AX,$line -v TGPS $fname`)
  # set TimeGPS = (`ncks -s "%10.10g" -H -C -d T_AX,$line -v Tgps $fname`)
  # --------------------------------------------------------------------------

  if ( $err == 0 ) then

    cd ..
    # create nml file:
    set nmlfile = "multirun/input/caaba_multirun.nml"
    echo "! -*- f90 -*- (created by loopcaaba.tcsh, do not edit\!)" > $nmlfile
    echo "&CAABA"                           >> $nmlfile
    echo "USE_MECCA = T"                    >> $nmlfile
    echo "USE_READJ = T"                    >> $nmlfile
    echo "temp = $temp"                     >> $nmlfile
    echo "press = $press"                   >> $nmlfile
    echo "photrat_channel = 'readj'"        >> $nmlfile
    echo "init_spec = 'multirun/$tmpFname'" >> $nmlfile
    echo "init_j = 'multirun/$tmpFname'"    >> $nmlfile
    echo "l_steady_state_stop = T"          >> $nmlfile
    echo "/"                                >> $nmlfile
    ln -fs $nmlfile caaba.nml
    rm caaba_*.nc
    ./caaba.exe > caaba.log
    cd -	

    set MaxTime = (`ncks -M ../caaba_mecca.nc | awk '/name = time, size =/ {print $8}'`)
    @ MaxTime--
    printf "%4d) CAABA for %s finished (MaxTime=%d)\n" $line $fname:t $MaxTime
    if ( $MaxTime > 5 ) then
      ncks -H -d time,$MaxTime ../caaba_mecca.nc | sed 's/=/ = /g' | awk 'BEGIN {printf "%g ",'$TimeGPS' }{if (NF>4) printf "%g ",$15} END {print}' >> $sum_dat
      ncks -H -d time,$MaxTime ../caaba_mecca.nc | sed 's/=/ = /g' | awk 'BEGIN {printf "TimeGPS[99999] "}{if (NF>4) printf "%s ",$13} END {print}' >> $header_dat
      # ncks -A -d time,$MaxTime ../caaba_mecca.nc caaba_mecca_all.nc
      # ncks -A -d time,$MaxTime ../caaba_messy.nc caaba_messy_all.nc
    else
      echo "Problem: MaxTime <65"
    endif

  else

    printf "%4d) skipping...\n" $line

  endif

  if ( $OneShot > 0 ) exit 0 # only one line
  @ line++ 
  set ret = (`ncks -O -d T_AX,$line $fname $tmpFname`)
end

##############################################################################

if (! -d ../output/multirun) mkdir ../output/multirun
set dirname = "../output/multirun/$fname:t:r"

if ( -d $dirname ) then
  echo "removing old data in output directory $dirname"
  rm $dirname/*
else
  echo "creating new output directory $dirname"
  mkdir $dirname
endif
echo "the output files are:"
cp -p ../caaba_*.nc ../caaba.log ../$nmlfile $dirname
cp -p $sum_dat $header_dat $dirname
ls -l $dirname
echo $hline

# cleanup:
rm $tmpFname

##############################################################################

exit
