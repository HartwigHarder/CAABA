#! /bin/tcsh -f
# Authors: Rolf Sander & Hartwig Harder, 2009

##############################################################################

set inputdir = "input"

echo "\nChoose an input file from the $inputdir directory:"
echo "[q/number, default=loop over all files]"
set inn = "0"
set allfiles = $inputdir/*.nc
foreach i ($allfiles) # list all possibilities
  @ inn=$inn + 1
  echo "$inn) $allfiles[$inn]:t"
end
set inputstring = "$<"
if ( "$inputstring" == "q" ) exit 1
if(($inputstring <= $#allfiles) && ($inputstring >= 1)) then
  set inputfile = $allfiles[$inputstring]
  ./loopcaaba.tcsh "$inputfile"
else
  foreach inputfile ($inputdir/*.nc)
    ./loopcaaba.tcsh "$inputfile"
  end
endif

exit

##############################################################################
