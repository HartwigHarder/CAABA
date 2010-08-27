#!/usr/bin/tcsh -f

### Hella Riede, MPI for Chemistry, Mainz, January 2010
### delete 
### - diagnostic tracers for reaction rates (DT* (old) and RR*)
### - specified in diagtrac file
### from mecca.eqn to make two mecca.eqn files comparable
### run in local caaba root directory
### NOTE: at the moment diagnostic tracers influence the kpp internal time
###   stepping thus slightly changing the numerics of the simulation


# output file (convenient: *.log automatically removed by 'make distclean')
set outfile = nodiag.log 

### default variables
###set eqnfile = mecca.eqn
###set diagdir = diagtrac
set eqnfile = mecca.eqn
set diagdir = diagtrac

#-----------------------------------------------------------------------------
echo "START ----- nodiag.tcsh ----------"
echo "mecca equation file: $eqnfile"

### get diagtracfile from mecca.eqn
set dtfile = $diagdir/`awk '$1 ~ /diagtracfile/ {print $3}' $eqnfile | sed 's|,||g' | sed 's|&||g' | sed "s|'||g"`.tex
if (-e $dtfile) then
  echo "diagnostic tracer file: $dtfile"
  set l_dt = 1
else
  echo "no diagnostic tracer file was used to generate mecca.eqn"
  set l_dt = 0
endif

### inform whether diagnostic tracers for reaction rates present
set l_rr = `awk 'BEGIN{l_rr=0}$1~/^</{if(($0~"DT")||($0~"RR")){l_rr=l_rr+1}}END{print l_rr}' $eqnfile`
if ($l_rr > 0) then
  echo "$l_rr diagnostic tracers for reaction rates were used"
else
  echo "no diagnostic tracers for reaction rates were used"
endif

if ($l_dt == 0 && $l_rr == 0) then
  echo "no diagnostic tracers -> nothing to do"
  echo "END ----- nodiag.tcsh ----------"
  exit 0
endif

### loop over mecca.eqn lines
### if current line not a reaction: write to output
### if reaction: replace any reaction rate diagnostic tracers
###   (DT* (old) and RR*)
###   AND delete diagnostic tracers specific for that reaction
rm -f $outfile
set ct   = 1
set nout = `wc -l $eqnfile | awk '{print $1}'`
while ($ct <= $nout)
  set cline = "`head -$ct $eqnfile | tail -1`"
  set l_reac = `echo $cline:q | awk '{if($1~/^</){print "1"}else{print "0"}}'`
  if ($l_reac == 0) then
    echo $cline:q >> $outfile 
  else
    ### remove reaction rate diagnostic tracers
    set rnum = `echo $cline:q | awk '{print $1}'`
    echo $cline:q |\
      awk -v rnum=$rnum '{gsub("<","",rnum);gsub(">","",rnum);gsub(" DT"rnum" \\+ ","");gsub(" RR"rnum" \\+ ","");print}'\
      > $outfile.tmp.log
    set cline = "`cat $outfile.tmp.log`"
    ### remove diag tracers from diagtrac file
    # find diagtracs for current reaction
    if (-e $dtfile) then
      awk -v rnum=$rnum '$1 ~ rnum {print}' $dtfile > $outfile.dt.log
      # How many lines for this reaction?
      set nlin = `wc -l $outfile.dt.log | awk '{print $1}'`
      if ($nlin > 0) echo "$nlin diagnostic tracer(s) for reaction $rnum"
      # loop over diagnostic tracers
      set ctd = 1
      while ($ctd <= $nlin)
        # delete <SPACE><[stoich. coeff] diagtrac><SPACE>+<SPACE>
        set dtrac  = `head -$ctd $outfile.dt.log | sed 's|&| & |g' | tail -1 | awk '{j=3;while(j<=NF){if($j!="&"){print $j;j++}else{j=NF+1}}}'`
        echo "  $dtrac"
        echo $cline:q | awk -v dtrac="$dtrac" '{string=" "dtrac" \\+ ";gsub(string,"");print}' > $outfile.tmp.log
        set cline = "`cat $outfile.tmp.log`"
        @ ctd++
      end
    endif # diagtrac file
    echo $cline:q >> $outfile
  endif # l_reac = 1
  @ ct++
end

### delete file copies
rm -f $outfile.dt.log
rm -f $outfile.tmp.log

echo "END ----- nodiag.tcsh ----------"
exit 0
