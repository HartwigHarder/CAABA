# ----------------------------------------------------------------------------
#
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2009
#
# Time-stamp: <2009-08-17 15:31:35 sander>
#
# spc_extract.awk extracts subsets of species from *.spc file
#
# usage: invoke via xgraphvizall
#
# ----------------------------------------------------------------------------

BEGIN {
  #printf "working on %s...\n", ARGV[1]
  logfile  = "spc_extract.log"
  all_spc  = "all.spc"
  C_spc    = "C.spc"
  C1_spc   = "C1.spc"
  C2_spc   = "C2.spc"
  C3_spc   = "C3.spc"
  C4_spc   = "C4.spc"
  C5_spc   = "C5.spc"
  Cl_spc   = "Cl.spc"
  Br_spc   = "Br.spc"
  I_spc    = "I.spc"
  dontedit = "DO NOT EDIT! This file was created automatically by spc_extract"
  printf "%s\n",    dontedit > logfile
  printf "// %s\n", dontedit > all_spc
  printf "// %s\n", dontedit > C_spc
  printf "// %s\n", dontedit > C1_spc
  printf "// %s\n", dontedit > C2_spc
  printf "// %s\n", dontedit > C3_spc
  printf "// %s\n", dontedit > C4_spc
  printf "// %s\n", dontedit > C5_spc
  printf "// %s\n", dontedit > Cl_spc
  printf "// %s\n", dontedit > Br_spc
  printf "// %s\n", dontedit > I_spc
}

# ----------------------------------------------------------------------------

{
  printf "\nline: |%s|\n", $0 >> logfile
  # delete all comments {...} from $0:
  gsub("^//.*}", "")
  # does current line contain a chemical composition?
  if (match($0, "^ *[A-z0-9_]+ *=[A-z0-9+ ]+;") != 0) {
    # extract the composition "=...;" from current line, and
    # add leading and trailing "+" signs:
    atoms = "+" gensub("^ *[A-z0-9_]+ *=([A-z0-9+ ]+);.*", "\\1", "g", $0) "+"
    printf "atoms = |%s|\n", atoms >> logfile
    # remove all spaces:
    gsub(" +", "", atoms)
    printf "atoms = |%s|\n", atoms >> logfile
    # if suitable, write line to spc files:
    if (match($0, "^RR") == 0) {
      printf "%s\n", $0 >> all_spc
    }
    if (match(atoms, "\\+[0-9]*C\\+") != 0) {
      printf "all C = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> C_spc
    }
    if (match(atoms, "\\+[1]*C\\+") != 0) {
      printf "C1    = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> C1_spc
    }
    if (match(atoms, "\\+2C\\+") != 0) {
      printf "C2    = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> C2_spc
    }
    if (match(atoms, "\\+3C\\+") != 0) {
      printf "C3    = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> C3_spc
    }
    if (match(atoms, "\\+4C\\+") != 0) {
      printf "C4    = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> C4_spc
    }
    if (match(atoms, "\\+5C\\+") != 0) {
      printf "C5    = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> C5_spc
    }
    if (match(atoms, "\\+[0-9]*Cl\\+") != 0) {
      printf "Cl    = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> Cl_spc
    }
    if (match(atoms, "\\+[0-9]*Br\\+") != 0) {
      printf "Br    = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> Br_spc
    }
    if (match(atoms, "\\+[0-9]*I\\+") != 0) {
      printf "I     = TRUE\n", atoms >> logfile
      printf "%s\n", $0 >> I_spc
    }
  }
}

# ----------------------------------------------------------------------------
