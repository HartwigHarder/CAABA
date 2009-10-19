#! /bin/tcsh -f
# dieses Skript erzeugt für jeden Eintrag aus dem im Argument 1 
# angegebene NetCDF File ein temporäres netcdf File mit nur einer Zeile 
# und ruft das CAABA Box model auf

#set echo verbose

if ( $#argv == 0 ) then 
   echo " Usage : LoopCaaba filename [LineNumber]"
   echo "    reads filename and calls caaba for each entry"
   exit 1
endif

# some path information

set p_modelbase=~/caaba_2.5d
set p_nml=$p_modelbase/nml/default

rm $p_modelbase/output/sum.asc
rm $p_modelbase/output/header.asc


# initialize nco for netcdf utilities
nco_init

set fname=$argv[1]
# output filename
set tmpFname=$p_modelbase/inputfile.nc

set OneShot=0
# Aktuelle Zeilennummer im netcdf file 
if ( $#argv == 2 ) then # did we supply line number
   set OneShot=$argv[2]
endif

set Zeile=0

if ( $OneShot > 0 ) then
   set Zeile=$OneShot
endif
# clear log file
rm $p_modelbase/caaba.log

# read first line
set ret=(`ncks -O -d T_AX,$Zeile $fname $tmpFname`)
while (${#ret} == 0)
    set err=0
    set ret=(`ncks -s "%10.10g" -H -C -d T_AX,$Zeile -v P_STAT $fname`)
    set Press=$ret
    set erg=(`echo $Press | awk '{if ($1<0) print "ERROR"}'`)
  #
   if ( ${#erg} > 0) then
	echo "Problem with pressure P=" $ret " in line " $Zeile
 	set err=1
   endif

    set ret=(`ncks -s "%10.10g" -H -C -d T_AX,$Zeile -v AIRTEMP_FZJ__C $fname |awk '{print $1+273.15}'`)
    set Temp=$ret
    set erg=(`echo $Temp | awk '{if ($1<0) print "ERROR"}'`)
   #
   if ( ${#erg} > 0) then
	echo "Problem with temperature T=" $ret " in line " $Zeile
 	set err=1
   endif

    set ret=(`ncks -s "%10.10g" -H -C -d T_AX,$Zeile -v TGPS $fname`)
 #   set ret=(`ncks -s "%10.10g" -H -C -d T_AX,$Zeile -v Tgps $fname`)
    set TimeGPS=$ret

   if ( $err == 0 ) then
      echo " Status " $status " STARTE NUN CAABA mit Zeile" $Zeile " aus " $fname " nach " $tmpFname       
      sed '/temp/atemp='$Temp $p_nml/original.nml > $p_nml/temp1.nml 
      sed '/press/apress='$Press $p_nml/temp1.nml > $p_nml/caaba.nml
      set p_current=`pwd`
      cd $p_modelbase
      ./caaba.exe >> caaba.log
      mv -f *.nc output/
      cd output
      set MaxTime=(`ncks -M caaba_mecca.nc | awk '/name = time, size =/ {print $8}'`)
      @ MaxTime--
      if ( $MaxTime > 5 ) then
          ncks -H -d time,$MaxTime caaba_mecca.nc | sed 's/=/ = /g' | awk 'BEGIN {printf "%g ",'$TimeGPS' }{if (NF>4) printf "%g ",$15} END {print}' >> sum.asc
          ncks -H -d time,$MaxTime caaba_mecca.nc | sed 's/=/ = /g' | awk 'BEGIN {printf "TimeGPS[99999] "}{if (NF>4) printf "%s ",$13} END {print}' >> header.asc
 
#     ncks -A -d time,$MaxTime caaba_mecca.nc caaba_mecca_all.nc
 #     ncks -A -d time,$MaxTime caaba_messy.nc caaba_messy_all.nc

      endif

      cd $p_current

   endif
   if ( $OneShot > 0 ) then # only one line
      exit 0                # stop here and now
   endif

   @ Zeile++ 
   set ret=(`ncks -O -d T_AX,$Zeile $fname $tmpFname`)
end

exit 0
   

