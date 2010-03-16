# ----------------------------------------------------------------------------
#
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2006
#
# Time-stamp: <2006-09-11 10:42:19 sander>
#
# substitute.awk substitutes each line in the input file that contains "regexp"
# with the contents of the file "newtext". Output is written to "outfile".
#
# example usage:
# gawk -f substitute.awk -v regexp="REGEXP" -v newtext=NEWTEXT -v outfile=gas2.eqn gas.eqn

# ----------------------------------------------------------------------------

BEGIN {
#  op_pj_20091023+
#system("echo -n > " outfile) # create empty file
#system("rm -f " outfile"; touch "outfile) # create empty file
#system("echo > " outfile) # create empty file
system("echo -n | sed \"s|-n||g\" > " outfile) # create empty file
#  op_pj_20091023-
status = -1
}

# ----------------------------------------------------------------------------

{
if (match($0, regexp) == 0) {
  print >> outfile
} else {
  status = 0 # OK, the reaction number has been found
  system("cat " newtext " >> " outfile)
}
}

# ----------------------------------------------------------------------------

END {
if (status<0) {
  print "ERROR: reaction number has not been found!"
}
}

# ----------------------------------------------------------------------------
