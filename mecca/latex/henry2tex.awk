# ----------------------------------------------------------------------------
#
# Authors:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany
#
# Time-stamp: <2008-02-13 13:16:54 sander>
#
# henry2tex.awk transforms henry coefficients in messy_mecca_mbl.f90 into tex.
#
# usage:
# setenv LC_COLLATE C ; gawk -f henry2tex.awk ../smcl/messy_mecca_aero.f90
#
# ----------------------------------------------------------------------------

BEGIN {
  print "running henry2tex.awk..."
  # define name of output file
  hfile   = "mecca_henry.tex"
  logfile = "henry2tex.log"
  # write header line
  dontedit = "% this file was created automatically by henry2tex, do not edit!"
  print dontedit > hfile
  printf "henry2tex.log\n\n" > logfile
  # initialize some strings
  errorstring = ""
  unknown = "???"
}

# ----------------------------------------------------------------------------

{
  # does current line define a Henry's law coefficient?
  if (match($0, "^[^!]*zhenry.*= *henry_T") != 0) {
    # printf "KH definition = %s\n", $0 >> logfile

    # store species' name in species
    if (match($0, "ind_([A-Za-z0-9]+)", arr0) != 0) {
      basespecies = arr0[1]
      species = "\\kpp{" arr0[1] "}"
    } else {
      species = unknown
      errorstring = sprintf("%s\nERROR: No species found:\n  %s",
        errorstring, $0)
    }
    printf "species = %s\n", basespecies >> logfile

    # store values in KH and minDHR
    if (match($0, "henry_T\\((.*),(.*), *ztemp *\\)", arr) != 0) {
      KH     = arr[1]
      minDHR = arr[2]
    } else {
      KH     = unknown
      minDHR = unknown
      errorstring = sprintf("%s\nERROR: KH and minDHR not found:\n  %s",
        errorstring, $0)
    }
    # remove precision (_dp) from strings:
    gsub("_[dD][pP]", "", KH)
    gsub("_[dD][pP]", "", minDHR)
    # remove spaces from strings:
    gsub(" ", "", KH)
    gsub(" ", "", minDHR)
    # convert exponential to LaTeX style:
    # (replacement text "\\1" works only with gensub but not with gsub):
    KH     = gensub("[eE]([-+]?[0-9]+)", "\\\\E{\\1}", "g", KH)
    minDHR = gensub("[eE]([-+]?[0-9]+)", "\\\\E{\\1}", "g", minDHR)
    printf "KH      = %s\n", KH     >> logfile
    printf "minDHR  = %s\n", minDHR >> logfile

    # store BibTeX references like {&1400} in ref
    if (match($0, "{(&+)([^}]*)}", arr) != 0) {
      ref = arr[2]
      if (ref=="") {
        ref = "see note\\showhenrynote{" basespecies "}"
      } else {
        # put each BibTeX reference into \citet{}
        # here, "&" represents the matched regexp
        gsub("[^, ]+", "\\citet{&}", ref)
        # if there are two ampersands like {&&1400} read also the footnote
        if (arr[1]=="&&") {
          ref = ref "$^*$\\showhenrynote{" basespecies "}"
        }
      }
    } else {
      ref = unknown
      errorstring = sprintf("%s\nERROR: No reference found:\n  %s",
        errorstring, $0)
    }
    printf "ref     = %s\n", ref >> logfile

    # add a line to LaTeX file only if current species is used,
    # i.e. if ind_XYZ>0
    # look for gas phase species not aqueous:
    command = ("grep -E \"INTEGER.*ind_" basespecies \
      " = [^0]\" ../" paramfile)
    # print command >> logfile
    if ((command | getline) > 0) {
      # write a line into the LaTeX table
      # printf "%-15s & %-20s & %10s & %-15s \\\\\n",
      printf "%s & %s & %s & %s \\\\\n",
        species, KH, minDHR, ref >> hfile
      printf "index   > 0\n\n", basespecies >> logfile
    } else {
      printf "index   = 0\n\n", basespecies >> logfile
    }
    # close grep command to avoid too many open processes
    close(command)

  }
}

# ----------------------------------------------------------------------------

END {
  printf "Input file:     %s\n", ARGV[1]
  printf "Parameter file: %s\n", paramfile
  printf "Output file:    %s\n", hfile
  print errorstring
}

# ----------------------------------------------------------------------------
