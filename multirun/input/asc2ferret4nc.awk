# generate a ferret skript to convert ascii data to netcdf data
# the netcdf file will be overwritten if it exists already!
# usage : gawk -f asc2ferret4ncdf ASCIIFILE.DAT > FERRETSKRIPTNAME
#         ferret -batch -script FERRETSKRIPTNAME
#
# Hartwig Harder Jul 2008

BEGIN {
  MAXCOL=30;
  MAXLINE=20;
  # read header line (we assume it's in the first line of the file, needs
  # to be adjusted if neccessary):
  Header=getline;

  # first extract filename:
  split(FILENAME,fnameBase,".");
  # if we can't split filename at . then report error
  if (length(fnameBase)<1) {
    print "ERROR finding base filename in" fnameBase;
      exit;
  }

  fnameDataCounter=1;
  fnameData=(fnameBase[1] "_" fnameDataCounter ".dat");
  fnameScript=(fnameBase[1] ".jnl");
  # print ">" fnameData "<";

  print "!Auto generated ferret skript to read ascii data and save them as netcdf" > fnameScript;

  if (NF>MAXCOL) {
    UseCol=MAXCOL;
  }  else {
    UseCol=NF;
  }

  # prepare Ferret (Thanks Pit)
  print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" >> fnameScript;
  print "!!! read ascii files and create netcdf output on time axis " >> fnameScript;
  print "!!! PETER HOOR 08.08.2008" >> fnameScript;
  print "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" >> fnameScript;
  print "!!! ERASE OLD SETTINGS" >> fnameScript;
  print "cancel data /all" >> fnameScript;
  print "cancel var /all"     >> fnameScript;
  print "cancel reg /all" >> fnameScript;
  print "!!! DEFINE DUMMY AXIS (EVENTUALLY not necessary?)"  >> fnameScript;
  print "define axis/T=0:1e6:1/unit=days t_dum"  >> fnameScript;
  print "define grid/t=t_dum t_dum_grid"  >> fnameScript;
  printf "file/skip=1/grid=t_dum_grid/VARIABLES=\"Tgps1\" %s\n",fnameData  >> fnameScript;
  print "show grid Tgps1"  >> fnameScript;
  print "!!! DEFINE TIME AXIS and GRID"  >> fnameScript;
  print "define axis/T/from/unit=days/name=t_ax/T0=\"1-JAN-2007 00:00:00\" Tgps1"  >> fnameScript;
  print "define grid/t=t_ax t_grid"  >> fnameScript;
  print "!!! SAVE TIME AXIS and GRID for LATER USE"  >> fnameScript;
  print "save/clobber/file=time_ax_dum.nc t[gt=t_ax]"  >> fnameScript;
  print "cancel data /all"  >> fnameScript;
  print "cancel var /all"  >> fnameScript;
  print "!!! RELOAD just created TIME GRID "  >> fnameScript;
  print "use time_ax_dum"  >> fnameScript;
  print "define grid/t=t_ax t_grid"  >> fnameScript;
  print "!!! RELOAD ASCII FILE, but on predefined time grid "  >> fnameScript;

  # generate the ascii load command in the form file/skip=1/VARIABLES=name1,name2,... FILENAME
  fnameDataCounter=1;
  printf "file/skip=1/format=delimited/delimiter=\"\\b\"/grid=t_grid/VARIABLES=\""  >> fnameScript;
  for (i=1; i<NF; i++) {
    printf "%s", $i  >> fnameScript;
    if ( ((i)% UseCol)==0) { # we need to generate one load statement for each set of Variables
      printf "\" %s\n\n", (fnameBase[1] "_"fnameDataCounter ".dat")  >> fnameScript;
      fnameDataCounter++; # next data block
      printf "file/skip=1/type=numeric/format=delimited/delimiter=\"\\b\"/grid=t_grid/VARIABLES=\""  >> fnameScript;
    } else {
      printf "," >> fnameScript;
    }
  }
  printf "%s\" %s\n",$i, (fnameBase[1] "_" fnameDataCounter ".dat")   >> fnameScript;

  # generate save to netcdf command in the form save/FILE=FILENAME.nc name1,name2,...
  printf "\n!!! now save data\n" >> fnameScript;
  printf "save/file=%s.nc/CLOBBER ", fnameBase[1]  >> fnameScript;
  for (i=1; i<NF; i++) {
    printf "%s", $i "[d=" int((i-1)/UseCol)+2 "]"  >> fnameScript;
    if ( ((i)% UseCol)==0) { # we need to generate one load statement for each set of Variables
      printf "\n\n" >> fnameScript;
      printf "save/append/file=%s.nc/CLOBBER ", fnameBase[1]  >> fnameScript;
    } else {
      printf "," >> fnameScript;
    }
  }
  printf "%s\n", $i "[d=" int((i-1)/UseCol)+2 "]"  >> fnameScript;
  print "!!! check, if TIME axis ok"  >> fnameScript;
  print "exit"  >> fnameScript;
  printf "! found %d fields\n", NF  >> fnameScript;

  # setup header for individual data files:
  fnameDataCounter=1;
  fnameData=(fnameBase[1] "_" fnameDataCounter ".dat");
  printf ""  > fnameData;
  for (i=1; i<=NF; i++) {
    printf "%15s ", $i >>fnameData;
    if ( ((i)% UseCol)==0) { # we need to generate a new file
      printf "\n" >> fnameData;
      fnameDataCounter++; # next data block
      fnameData=(fnameBase[1] "_" fnameDataCounter ".dat");
    }
  }
  printf "\n" >> fnameData;

}

{
  fnameDataCounter=1;
  fnameData=(fnameBase[1] "_" fnameDataCounter ".dat");
  printf ""  > fnameData;
  for (i=1; i<=NF; i++) {
    if ($i=="NaN")
      printf "%15.12g ",-1.e34 >> fnameData;
    else
      printf "%15.12g ", $i >> fnameData;
    if ( ((i)% UseCol)==0) { # we need to address a new file
      printf "\n" >> fnameData; # finish line in old one
      fnameDataCounter++; # next data block
      fnameData=(fnameBase[1] "_" fnameDataCounter ".dat");
    }
  }
  printf "\n" >> fnameData;
}

END {
  print "!! Normal end";
}



