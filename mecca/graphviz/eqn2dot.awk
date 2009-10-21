###############################################################################
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2008-2009
#
# Time-stamp: <2009-08-17 15:00:21 sander>
#
# eqn2dot.awk transforms a KPP eqn file into a dot file for graphviz.
#
# for usage, see xmeccagraph
#
##############################################################################

BEGIN {
  # --------------------------------------------------------------------------
  #print "running eqn2dot.awk..."
  # remove suffix:
  basename = gensub("\\.[^.]+$", "", "g", ARGV[1])
  # define name of output files:
  dependfile = "depend.dot"
  nodefile   = "node.dot"
  logfile = basename ".log"
  # --------------------------------------------------------------------------
  # define spc1list:
  command = ("grep -E '^ *[A-Za-z0-9_]+' " spc1file "| sed 's|=.*||' | tr '\n' ' '")
  #print command
  (command | getline) # store spc1list in $0 by getline
  close(command) # close grep command to avoid too many open processes
  spc1list = " " $0 " "
  # printf "spc1list = %s\n", spc1list
  # --------------------------------------------------------------------------
  # define spc2list:
  command = ("grep -E '^ *[A-Za-z0-9_]+' " spc2file "| sed 's|=.*||' | tr '\n' ' '")
  #print command
  (command | getline) # store spc2list in $0 by getline
  close(command) # close grep command to avoid too many open processes
  spc2list = " " $0 " "
  # printf "spc2list = %s\n", spc2list
  # --------------------------------------------------------------------------
  # write header line:
  printf ""        > dependfile
  printf ""        > nodefile
  print "log file" > logfile
  # initialize some strings:
  errorstring = ""
  unknown = "????"
  # --------------------------------------------------------------------------
}

##############################################################################

# showspc1 returns 1 (=true) if the species is included in spc1list
# and 0 (=false) if not:
function showspc1(species)
  {return (match(spc1list, " " species " ") != 0)}

##############################################################################

# showspc2 returns 1 (=true) if the species is included in spc2list
# and 0 (=false) if not:
function showspc2(species)
  {return (match(spc2list, " " species " ") != 0)}

##############################################################################

{
  # --------------------------------------------------------------------------
  # remove aerosol bin indicator:
  gsub("_a##", "")
  gsub("##", "")
  # store equation ID like <G3102b> in eqnid
  if (match($0, "<([A-Za-z_0-9]+)>", arr) != 0) {
    eqnid = arr[1]
    # printf "eqnid     = %s\n", eqnid >> logfile
    baseeqnid = gensub("([A-Z]+[0-9]+).*", "\\1", "g", eqnid)
    # printf "baseeqnid = %s\n", baseeqnid >> logfile
    printf "\nline: %s\n", $0 >> logfile
  } else {
    eqnid = unknown
    # printf "\nline: %s\n", $0 >> logfile
    # printf "no eqnid found\n" >> logfile
  }
  # --------------------------------------------------------------------------
  # does current line contain a chemical equation, i.e. something like
  # " = ... : ... ; " ?
  if (match($0, "=.*:.*;") != 0) {
    # check if equation ID exists
    if (eqnid==unknown) {
      errorstring = sprintf("%s\nERROR: This reaction has no eqnid:\n  %s",
        errorstring, $0)
    }

    # delete all comments {...} from $0:
    gsub("{[^}]*}", "")
    # delete all equation IDs <...> from $0:
    gsub("<([A-Za-z_0-9]+)>", "")
    # reduce multiple spaces to one:
    gsub("  +", " ")
    # remove stoichiometric factors:
    gsub(" [0-9.]+ ", " ")
    # remove leading spaces:
    gsub("^ +", "")
    printf "equation    = %s\n", $0 >> logfile
    # split into reactants and products using the kpp-syntax separators "=:"
    split($0, arr, " *[=:] *")
    reactants = arr[1]
    products  = arr[2]
    printf "reactants   = %s\n", reactants >> logfile
    printf "products    = %s\n", products  >> logfile
    # split into indivdual reactants and products
    split(reactants, reactant, " *[+] *")
    split(products,  product,  " *[+] *")

    for (i in reactant) {
      otherreactants = ""
      for (k in reactant) {
        if (i != k) {
          if (otherreactants == "") {
            otherreactants = reactant[k]
          } else {
            otherreactants = otherreactants " + " reactant[k]
          }
        }
      }

      # definition of label string:
      if(labeltype=="eqnid")    {labelstring = eqnid }
      if(labeltype=="reactant") {labelstring = otherreactants}
      if(labeltype=="both")     {labelstring = otherreactants " (" eqnid ")"}

      reactantshowspc1 = showspc1(reactant[i])
      reactantshowspc2 = showspc2(reactant[i])
      #printf "%s %s\n", reactant[i], showspc1(reactant[i]) >> logfile
      for (j in product) {
        productshowspc1 = showspc1(product[j])
        productshowspc2 = showspc2(product[j])
        #if(reactantshowspc1 && productshowspc1) {
        if((reactantshowspc1 || productshowspc1) && 
          reactantshowspc2 && productshowspc2) {
          printf "%s -> %s [label = \"%s\"];\n", 
            reactant[i], product[j], labelstring >> dependfile
          if(reactantshowspc1) {printf "%s\n", reactant[i] >> nodefile}
          if(productshowspc1)  {printf "%s\n", product[j]  >> nodefile}
        }
      }
    }

    # write a line to the output file
    printf "DOT: %s %s -> %s;\n",
        eqnid, reactants, products >> logfile
  }
  # --------------------------------------------------------------------------
}

##############################################################################

END {
  #printf "Input file:  %s\n", ARGV[1]
}

##############################################################################
