# ----------------------------------------------------------------------------
#
# Authors:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany
#
# Time-stamp: <2008-02-13 13:16:47 sander>
#
# alpha2tex.awk transforms alpha in messy_mecca_mbl.f90 into tex.
#
# usage:
# setenv LC_COLLATE C ; gawk -f alpha2tex.awk ../smcl/messy_mecca_aero.f90
#
# ----------------------------------------------------------------------------

BEGIN {
  print "running alpha2tex.awk..."
  # define name of output file
  hfile   = "mecca_alpha.tex"
  logfile = "alpha2tex.log"
  # write header line
  dontedit = "% this file was created automatically by alpha2tex, do not edit!"
  print dontedit > hfile
  printf "alpha2tex.log\n\n" > logfile
  # initialize some strings
  errorstring = ""
  unknown = "???"
}

# ----------------------------------------------------------------------------

{
  # does current line define an accommodation coefficient?
  if (match($0, "^[^!]*alpha.*= *alpha_T") != 0) {
    # printf "alpha definition = %s\n", $0 >> logfile

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

    # store values in alpha and minDHR
    if (match($0, "alpha_T\\((.*),(.*), *ztemp *\\)", arr) != 0) {
      alpha  = arr[1]
      minDHR = arr[2]
    } else {
      alpha  = unknown
      minDHR = unknown
      errorstring = sprintf("%s\nERROR: alpha and minDHR not found:\n  %s",
        errorstring, $0)
    }
    # remove precision (_dp) from strings:
    gsub("_[dD][pP]", "", alpha)
    gsub("_[dD][pP]", "", minDHR)
    # remove spaces from strings:
    gsub(" ", "", alpha)
    gsub(" ", "", minDHR)
    # convert exponential to LaTeX style:
    # (replacement text "\\1" works only with gensub but not with gsub):
    alpha  = gensub("[eE]([-+]?[0-9]+)", "\\\\E{\\1}", "g", alpha)
    minDHR = gensub("[eE]([-+]?[0-9]+)", "\\\\E{\\1}", "g", minDHR)
    printf "alpha   = %s\n", alpha  >> logfile
    printf "minDHR  = %s\n", minDHR >> logfile

    # store BibTeX references like {&1400} in ref
    if (match($0, "{(&+)([^}]*)}", arr) != 0) {
      ref = arr[2]
      if (ref=="") {
        ref = "see note\\showalphanote{" basespecies "}"
      } else {
        # put each BibTeX reference into \citet{}
        # here, "&" represents the matched regexp
        gsub("[^, ]+", "\\citet{&}", ref)
        # if there are two ampersands like {&&1400} read also the footnote
        if (arr[1]=="&&") {
          ref = ref "$^*$\\showalphanote{" basespecies "}"
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
        species, alpha, minDHR, ref >> hfile
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
