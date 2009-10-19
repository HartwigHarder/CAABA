#!/bin/bash

# Author: Mike Long, University of Virginia, Charlottesville, USA (2009)

#
 if [ -f $1"/caaba_mecca.nc" ]
     then
     status=0
     cp $1/caaba_mecca.nc ./caaba_tmp.nc
     cp $1/caaba_mecca.nc $1/caaba_mecca.nc.orig
 else
     status=1
 fi
 if [ $status = 1 ]
     then
     echo
     echo "A directory containing caaba_mecca.nc must be provided"
     echo "at the command line. (e.g. ./caaba_nc_parser /path/to/output)"
     echo
     exit
 fi

# Species list
#spc_list='Cl Br NO O3'
 spc_list='HNO3 HCHO HONO'
# spc_list='Cl'

# echo $spc_list
#- Equation extraction
 for spcs in $spc_list
   do
   cat ../mecca/mecca.eqn | awk '/EQUATIONS/,/INLINE/' | awk '/'$spcs'/' | awk '!/^{/' | awk '{ print $1}' > tmp1
   if [ $spcs = "O3" ] # Prevent reading species with O3 in name (e.g. HNO3)
   then
   cat ../mecca/mecca.eqn | awk '/EQUATIONS/,/INLINE/' | awk '/\ '$spcs'/' | awk '!/^{/' | awk '{ print $1}' > tmp1
   fi
   cat tmp1 | sed 's/</RR/g' | sed 's/>//g' > tmp2
#   cat tmp2 | awk '!/RR/' > tmp1

   lines=`cat tmp2 | wc -l`
#   echo $lines
   if [ $lines = 0 ]
       then
       echo "No reactions found for species " $spcs
       rm tmp1 tmp2
   else
   
       rxns=`cat tmp2`

       rm tmp1 tmp2

       for rxn in $rxns
	 do
	 ncks -A -v $rxn ./caaba_tmp.nc RR$spcs.nc # Extract all reaction rates for $spcs
       done
   
       ncks -A -v '^RRG' RR$spcs.nc RRG$spcs.nc   # Extract gas-phase reaction rates
       wait $!
       ncks -A -v '^RRA' RR$spcs.nc RRAQ$spcs.nc  # Extract aqueous reaction rates
       wait $!
       ncks -A -v '^RREQ' RR$spcs.nc RRAQ$spcs.nc # Append Equilirium reaction rates
       wait $!
       ncks -A -v '^RRH' RR$spcs.nc RRH$spcs.nc   # Extract Henry's Law reaction rates
   fi

 done

 dirname=`basename $1`

 mkdir $1/../$dirname-parsed

 ncks -A -v '^RR' ./caaba_tmp.nc caaba_Rates.nc
 ncks -A -x -v '^RR' ./caaba_tmp.nc caaba_mecca.nc

 rm caaba_tmp.nc

 mv *.nc $1/../$dirname-parsed
 cp $1/caaba_messy.nc $1/../$dirname-parsed
 cp $1/caaba_mecca_aero.nc $1/../$dirname-parsed
