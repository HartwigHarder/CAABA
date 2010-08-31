#! /bin/tcsh -f
# Time-stamp: <2010-07-27 15:31:19 sander>
# _zipcaaba.tcsh: create a zip file of caaba code

# if the current directory is only a link, the following cd command
# will jump into the real directory:
cd `/bin/pwd`

if ( "$1" == "" ) then
  echo "This script should be called via the Makefile, e.g.:"
  echo "  gmake zip    --> archive important files"
  echo "  gmake zipall --> archive all files"
  exit
endif

set dirname = `basename $PWD`
set zipfile = $PWD/$dirname.zip
if ( -e $zipfile) then
  echo "Renaming old $dirname.zip file to $dirname.zip~"
  mv -f $zipfile $zipfile~
endif

cd ..

# zip options:
# -o make zipfile as old as latest entry
# -r recurse into directories
# -x '...' exclude files
if ( "$1" == "zipall" ) then
  zip -or $zipfile $dirname
else 
  zip -or $zipfile $dirname \
    -x '*~' -x '*.mod' -x '*.exe' -x '*.o' -x '*.a' -x '*.nc' \
    -x '*.log' -x '*.old' -x '*/ferret.jnl' -x '*.zip' -x '*.tar' \
    -x '*.ps' -x '*.dat' \
    -x '*.aux' -x '*.bbl' -x '*.toc' -x '*.blg' \
    -x '*/Makefile.m' -x '*_e5.inc' -x '*e4chem*' \
    -x $dirname/'output/?*' \
    -x '*/tmp/*' 
endif

# add input files for CAABA (even though they are *.nc files):
zip -o $zipfile $dirname/traject/*.nc
zip -o $zipfile $dirname/multirun/input/*.nc
zip -o $zipfile $dirname/input/*.nc

# Symbolic links are now included as the whole files in the zip archive.
# However, some links are internal links (i.e. between directories that
# are both included in the zip file). They must be stored as links. This
# is done by overwriting them in the zip file:

# zip options:
# -y store symbolic links as the link instead of the referenced file
zip -oy $zipfile \
$dirname/caaba.nml \
$dirname/messy_mecca_kpp.f90 \
$dirname/messy_mecca_kpp_function.f90 \
$dirname/messy_mecca_kpp_global.f90 \
$dirname/messy_mecca_kpp_initialize.f90 \
$dirname/messy_mecca_kpp_integrator.f90 \
$dirname/messy_mecca_kpp_jacobian.f90 \
$dirname/messy_mecca_kpp_jacobiansp.f90 \
$dirname/messy_mecca_kpp_linearalgebra.f90 \
$dirname/messy_mecca_kpp_monitor.f90 \
$dirname/messy_mecca_kpp_parameters.f90 \
$dirname/messy_mecca_kpp_precision.f90 \
$dirname/messy_mecca_kpp_rates.f90 \
$dirname/messy_mecca_kpp_util.f90 \
$dirname/messy_mecca.f90 \
$dirname/messy_mecca_aero.f90 \
$dirname/messy_mecca_khet.f90

if ( "$2" == "verbose" ) then
  echo "\nThe zipfile has been created:"
  ls -la $zipfile
endif

exit
