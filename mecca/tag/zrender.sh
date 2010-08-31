#! /bin/tcsh -f
#
# S. Gromov, MPI-C, 2008
# script to produce a PS (with necessary adjustments for following PDF) from GraphViz DOT
#

set dot = "~/soft/graphviz/bin/dot"
echo "dot location is: "$dot

if ("$2" == "") then
  set otype = ps2
else
  set otype = $2
endif

if ("$otype" == "ps2") then
  set oext="ps"
else 
  set oext="$otype"
endif

# rendering graphviz sketches to ps
# assuming $1 is dotfile, if empty - searching for all dot files in the dir

if ("$1" == "") then
  set dotfiles = (*.dot)
else
  set dotfiles = ($1)
endif

foreach dotfile ($dotfiles) 
 echo "rendering $dotfile..."
 
 set fext = $dotfile:e
 set fname = `basename $dotfile $fext`

 $dot -v -T$otype -s300 -Gsplines=true -Goverlap=false -Eweight=5.0 -Gmindist=10.0 -Elen=10.0 -o${fname}${oext} $dotfile
end

foreach dotfile ($dotfiles)

 set fext = $dotfile:e
 set fname = `basename $dotfile $fext`
 set psfile = ${fname}${oext}
 
 echo "processing $psfile..."

 ./zsub $psfile AgencyR AgencyFB-Reg
 ./zsub $psfile "/bold { 2 setlinewidth }" "/bold { 5 setlinewidth }"
 # following lines are to make concetrate edges bold
 ./zsub $psfile  "/solid" "/sol+id"
 ./zsub $psfile "solid" "bold"
 ./zsub $psfile "/sol+id" "/solid"

# ps2ps $psfile
 ps2pdf $psfile
end
