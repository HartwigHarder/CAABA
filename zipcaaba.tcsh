#! /bin/tcsh -f
# Time-stamp: <2009-01-21 11:13:04 sander>
# zipcaaba: create a zip file of caaba code

if ( "$1" == "" ) then
  echo "This script shoud be used via the Makefile, e.g.:"
  echo "  gmake zip     --> archive important files"
  echo "  gmake zipall  --> archive all files"
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
# -y store symbolic links as the link instead of the referenced file
# -x '...' exclude files
if ( "$1" == "zipall" ) then
  zip -ory $zipfile $dirname
  else 
  zip -ory $zipfile $dirname \
    -x '*~' -x '*.mod' -x '*.exe' -x '*.o' -x '*.a' -x '*.nc' \
    -x '*.old' -x '*.zip' -x '*.tar' -x '*/tmp/*' \
    -x '*.ps' -x '*.dat' -x '*/Makefile.m' -x $dirname/'output/?*' \
    -x $dirname/'temporaryfile*'
endif

echo "\nThe zipfile has been created:"
ls -la $zipfile

exit
