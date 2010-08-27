#! /bin/tcsh -f
# Authors: Rolf Sander & Hartwig Harder, 2009

##############################################################################

set modelbase = ".."
set inputdir = "input"
set scratchdir = "/tmp"
set ModelStartTime=(`date '+%y%m%d_%H%M'`)
set outbase=$modelbase"/output/multirun/"

if (! -d $outbase) then
   mkdir $outbase
endif

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
  set outputdir = $outbase$ModelStartTime"_""$inputfile:t:r"
  ./loopcaaba.tcsh "$inputfile" "$outputdir"
    gawk -f $modelbase/mecca/rxnrates.awk -v jnlfile1=$scratchdir/dummy1.jnl -vjnlfile2=$scratchdir/dummy2.jnl -v fnBudget=$outputdir/budget.dat -v fnBudgetUnkown=$outputdir/budgetUnKnwn.dat -vfnHeader=$outputdir/header.dat -v selSpecies="OH" $modelbase/mecca/gas.eqn
    gawk -f $modelbase/mecca/rxnrates.awk -v jnlfile1=$scratchdir/dummy1.jnl -vjnlfile2=$scratchdir/dummy2.jnl -v fnBudget=$outputdir/budgetHO2.dat -v fnBudgetUnkown=$outputdir/budgetUnKnwn.dat -vfnHeader=$outputdir/header.dat -v selSpecies="HO2" $modelbase/mecca/gas.eqn
else
  foreach inputfile ($inputdir/*.nc)
    set outputdir = $outbase$ModelStartTime"_""$inputfile:t:r"
    ./loopcaaba.tcsh "$inputfile" "$outputdir"
    # gawk -f $modelbase/mecca/rxnrates.awk -v jnlfile1=$scratchdir/dummy1.jnl -vjnlfile2=$scratchdir/dummy2.jnl -v fnBudget=$outputdir/budget.dat -v fnBudgetUnkown=$outputdir/budgetUnKnwn.dat -vfnHeader=$outputdir/header.dat -v selSpecies="OH" $modelbase/mecca/gas.eqn
    # gawk -f $modelbase/mecca/rxnrates.awk -v jnlfile1=$scratchdir/dummy1.jnl -vjnlfile2=$scratchdir/dummy2.jnl -v fnBudget=$outputdir/budgetHO2.dat -v fnBudgetUnkown=$outputdir/budgetUnKnwn.dat -vfnHeader=$outputdir/header.dat -v selSpecies="HO2" $modelbase/mecca/gas.eqn
  end
endif

exit

##############################################################################
