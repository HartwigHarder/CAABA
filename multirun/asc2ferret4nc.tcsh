#! /bin/tcsh -f
# Authors: Rolf Sander & Hartwig Harder, 2009

##############################################################################

if ( "$1" == "" ) then
  echo "asc2ferret4nc.tcsh = convert ascii to netcdf"
  echo "usage:"
  echo "   asc2ferret4nc.tcsh <myfile.txt>"
  echo "or:"
  echo "   asc2ferret4nc.tcsh *.txt"
  exit
endif

foreach inputfile ($argv)
  # convert from DOS (CRLF) to UNIX format:
  #fromdos "$inputfile"
  gawk -f asc2ferret4nc.awk "$inputfile"
  ferret -batch -script "$inputfile:r"
  # ferret renames "temp" to "TEMP" etc. Revert this now:
  ncrename -v PRESS,press $inputfile:r.nc
  ncrename -v TEMP,temp   $inputfile:r.nc
  # cleanup temporary files:
  rm $inputfile:r.jnl $inputfile:r_*.dat
  rm time_ax_dum.nc metafile.plt 
end

exit
