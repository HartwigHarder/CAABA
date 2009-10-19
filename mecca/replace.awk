# ----------------------------------------------------------------------------
#
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2006
#
# Time-stamp: <2006-09-13 18:49:12 sander>
#
# replace.awk replaces individual reactions in KPP equation files
#
# example usage:
# gawk -f replace.awk -v infile="gas.eqn" -v outfile="gas2.eqn" example.rpl
#
# ----------------------------------------------------------------------------

BEGIN {
rplfile = "tmp_1.rpl"
eqnfile = "tmp_1.eqn"
system("cp " infile " " eqnfile)
}

# ----------------------------------------------------------------------------

{

if (match($0, "^#REPLACE[ 	]*<([A-Za-z0-9_]*)>", arr) != 0) {
  # #REPLACE command was found and the reaction number, i.e. the 
  # "([A-Za-z0-9]+)" part of the above regexp, is stored in arr[1].
  system("echo -n > " rplfile) # create empty file
  getline
  # loop until #ENDREPLACE is found:
  while (match($0, "^#ENDREPLACE") == 0) {
    # add main reaction number into angle brackets:
    print gensub("<([A-Za-z0-9_]+)>", "<" arr[1] "\\1>", "g") >> rplfile
    getline
  }
  if ( arr[1] == "" ) {
    print "Adding new reaction(s) ..."
    system("cat " rplfile " >> " eqnfile)
  } else {
    printf "Replacing reaction %s ...\n", arr[1]
    # insert the new equations into the eqn file:
    command = "gawk -f substitute.awk -v regexp=\"^[ 	]*<" arr[1] ">\" -v newtext=" rplfile " -v outfile=" outfile " " eqnfile
    system(command)
    system("cp -f " outfile " " eqnfile)
  }
} else {
  # #REPLACE command was not found.
  # Empty lines and comments starting with "//" are okay,
  # otherwise print an error message:
  if ( (match($0, "^[ 	]*$") == 0) && (match($0, "^//") == 0) ) {
    printf "ERROR: %s\n", $0
  }
}

}

# ----------------------------------------------------------------------------

END {
system("cp -f " eqnfile " " outfile)
system("rm " rplfile " " eqnfile)
}

# ----------------------------------------------------------------------------
