# ----------------------------------------------------------------------------
#
# Authors:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2003-2005
#   Astrid Kerkweg, Max-Planck-Institute, Mainz, Germany, 2005
#
# Time-stamp: <2009-04-27 10:19:19 sander>
#
# eqn2tex.awk transforms eqn into tex.
#
# usage:
# gawk -f eqn2tex.awk ../mecca.eqn
#
# ----------------------------------------------------------------------------

BEGIN {
  print "running eqn2tex.awk..."
  # remove "../", then replace "." by "_" in input file
  basename = gensub("\\.\\./", "", "g", ARGV[1])
  gsub("\\.", "_", basename)
  # define name of output files
  hfile   = basename "_h.tex"
  afile   = basename "_a.tex"
  gfile   = basename "_g.tex"
  jfile   = basename "_j.tex"
  phfile  = basename "_ph.tex"
  hetfile = basename "_het.tex"
  eqfile  = basename "_eq.tex"
  logfile = "eqn2tex.log"
  # write header line
  dontedit = "% this file was created automatically by eqn2tex, do not edit!"
  print dontedit > hfile
  print dontedit > afile
  print dontedit > gfile
  print dontedit > jfile
  print dontedit > phfile
  print dontedit > hetfile
  print dontedit > eqfile
  print "log file" > logfile
  # initialize some strings
  errorstring = ""
  unknown = "????"
}

# ----------------------------------------------------------------------------

{
  printf "\nline: %s\n", $0 >> logfile
  # store equation ID like G3102b in eqnid:
  if (match($0, "<([A-Za-z_0-9]+)>", arr) != 0) {
    eqnid = arr[1]
    printf "eqnid     = %s\n", eqnid >> logfile
    baseeqnid = gensub("([A-Z]+[0-9]+).*", "\\1", "g", eqnid)
    printf "baseeqnid = %s\n", baseeqnid >> logfile
  } else {
    eqnid = unknown
    printf "no eqnid found\n" >> logfile
  }
  # does current line contain a chemical equation, i.e. something like
  # " = ... : ... ; " ?
  if (match($0, "=.*:.*;") != 0) {
    printf "equation  = %s\n", $0 >> logfile
    # check if equation ID exists
    if (eqnid==unknown) {
      errorstring = sprintf("%s\nERROR: This reaction has no eqnid:\n  %s",
        errorstring, $0)
    }
    # store marker like {%StTrGNJ} in marker
    if (match($0, "{%([^}]*)}", arr) != 0) {
      marker = arr[1]
      printf "marker    = %s\n", marker >> logfile
    } else {
      marker = ""
      printf "no marker found\n" >> logfile
    }
    # store alternative LaTeX text for the rate
    # constant's A-factor like {@} in klatex
    if (match($0, "{@([^}]*)}", arr) != 0) {
      klatex = arr[1]
      # replace <> by {}
      gsub("<", "{", klatex)
      gsub(">", "}", klatex)
      printf "klatex    = %s\n", klatex >> logfile
    } else {
      klatex = ""
      printf "no klatex found\n" >> logfile
    }
    # store alternative LaTeX text for the rate constant's exponent
    # like {$} in kexplatex
    if (match($0, "{\\$([^}]*)}", arr) != 0) {
      kexplatex = arr[1]
      # replace <> by {}
      gsub("<", "{", kexplatex)
      gsub(">", "}", kexplatex)
      printf "kexplatex = %s\n", kexplatex >> logfile
    } else {
      kexplatex = ""
      printf "no kexplatex found\n" >> logfile
    }
    # store BibTeX references like {&1400} in ref
    if (match($0, "{(&+)([^}]*)}", arr) != 0) {
      ref = arr[2]
      if (ref=="") {
        ref = "see note\\shownote{" baseeqnid "}"
      } else {
        # put each BibTeX reference into \citet{}
        # here, "&" represents the matched regexp
        gsub("[^, ]+", "\\citet{&}", ref)
        # if there are two ampersands like {&&1400} read also the footnote
        if (arr[1]=="&&") {
          ref = ref "$^*$\\shownote{" baseeqnid "}"
        }
      }
      printf "ref       = %s\n", ref >> logfile
    } else {
      if (match(eqnid, "^EQ[0-9]*b") != 0) {
        # equilibrium backward reaction EQ##b doesn't need a reference
        printf "no ref needed for reaction %s\n", eqnid >> logfile
      } else {
        ref = "\\rule{8mm}{3mm}"
        errorstring = sprintf("%s\nERROR: reaction %s has no reference",
          errorstring, eqnid)
        printf "no ref found\n" >> logfile
      }
    }
    printf "equation  = %s\n", $0 >> logfile
    # delete all comments {...} from $0
    gsub("{[^}]*}", "")
    # delete all equation IDs <...> from $0
    gsub("<([A-Za-z_0-9]+)>", "")
    printf "equation  = %s\n", $0 >> logfile
    # reduce multiple spaces to one
    gsub("  +", " ")
    printf "equation  = %s\n", $0 >> logfile
    # split into equation and rateconst using the kpp-syntax separators ":;"
    split($0, arr, " *[:;] *")
    equation  = arr[1]
    rateconst = "\\code{" arr[2] "}"
    printf "equation  = %s\n", equation >> logfile
    printf "rateconst = %s\n", rateconst >> logfile
    # put LaTeX command \kpp{} around each specie in equation
    # here, "&" represents the matched regexp
    gsub("[A-Za-z][A-Za-z0-9_]*", "\\kpp{&}", equation)
    printf "equation  = %s\n", equation >> logfile
    # create reaction arrow
    gsub("=", "$\\rightarrow$", equation)
    printf "equation  = %s\n", equation >> logfile
    # use alternative LaTeX text for rate constant base?
    if (klatex!="") {
      rateconst = klatex
    }
    # write a line into the specific LaTeX table
    if (match(eqnid, "^G[0-9]") != 0) {
      # G = gas-phase reaction
      printf "\\code{%s} & %s & %s & %s & %s\\\\\n",
        eqnid, marker, equation, rateconst, ref >> gfile
      if (kexplatex!="") {
        errorstring = sprintf("%s\nERROR: reaction %s cannot use {$%s}",
          errorstring, eqnid, kexplatex)
      }
    } else if (match(eqnid, "^J[0-9]") != 0) {
      # J = photolysis reaction
      printf "\\code{%s} & %s & %s & %s & %s\\\\\n",
        eqnid, marker, equation, rateconst, ref >> jfile
      if (kexplatex!="") {
        errorstring = sprintf("%s\nERROR: reaction %s cannot use {$%s}",
          errorstring, eqnid, kexplatex)
      }
    } else if (match(eqnid, "^PH[0-9]") != 0) {
      # PH = aqueous-phase photolysis reaction
      printf "\\code{%s} & %s & %s & %s & %s\\\\\n",
        eqnid, marker, equation, rateconst, ref >> phfile
      if (kexplatex!="") {
        errorstring = sprintf("%s\nERROR: reaction %s cannot use {$%s}",
          errorstring, eqnid, kexplatex)
      }
    } else if (match(eqnid, "^HET[0-9]") != 0) {
      # HET = HET reaction
      printf "\\code{%s} & %s & %s & %s & %s\\\\\n",
        eqnid, marker, equation, rateconst, ref >> hetfile
      if (kexplatex!="") {
        errorstring = sprintf("%s\nERROR: reaction %s cannot use {$%s}",
          errorstring, eqnid, kexplatex)
      }
    } else if (match(eqnid, "^A[0-9]") != 0) {
      # A = aqueous-phase reaction
      printf "\\code{%s} & %s & %s & %s & %s & %s\\\\\n",
        eqnid, marker, equation, rateconst, kexplatex, ref >> afile
    } else if (match(eqnid, "^EQ[0-9]*b") != 0) {
      # backward reaction, do nothing
    } else if (match(eqnid, "^EQ[0-9]*f") != 0) {
      # EQ = equilibria (acid-base and others)
      # delete f in equation id
      gsub("f_", "_", eqnid)
      # replace rightarrow by rightleftharpoons
      gsub("rightarrow", "rightleftharpoons", equation)
      printf "\\code{%s} & %s & %s & %s & %s & %s\\\\\n",
        eqnid, marker, equation, rateconst, kexplatex, ref >> eqfile
    } else if (match(eqnid, "^H[0-9]") != 0) {
      # H = aqueous-phase heterogenous and Henry reaction
      printf "\\code{%s} & %s & %s & %s & %s\\\\\n",
        eqnid, marker, equation, rateconst,  ref >> hfile
      if (kexplatex!="") {
        errorstring = sprintf("%s\nERROR: reaction %s cannot use {$%s}",
          errorstring, eqnid, kexplatex)
      }
    } else {
      errorstring = sprintf("%s\nERROR: unknown type of equation ID: %s",
        errorstring, eqnid)
      printf "\\code{%s} & %s & %s & %s & %s & %s\\\\\n", eqnid, marker, equation, rateconst, kexplatex, ref >> logfile
    }
  } else {
    # not an equation line
    # check for LaTeX text {@...}
    if (match($0, "{@([^}]*)}", arr) != 0) {
      klatex = arr[1]
      printf "klatex    = %s\n", klatex >> logfile
      # write a line into the specific LaTeX table
      if (match(eqnid, "^G[0-9]") != 0) {
        # G = gas-phase reaction
        printf "%s\n", klatex >> gfile
      } else if (match(eqnid, "^J[0-9]") != 0) {
        # J = photolysis reaction
        printf "%s\n", klatex >> jfile
      } else if (match(eqnid, "^PH[0-9]") != 0) {
        # PH = aqueous-phase photolysis reaction
        printf "%s\n", klatex >> phfile
      } else if (match(eqnid, "^HET[0-9]") != 0) {
        # HET = HET reaction
        printf "%s\n", klatex >> hetfile
      } else if (match(eqnid, "^A[0-9]") != 0) {
        # A = aqueous-phase reaction
        printf "%s\n", klatex >> afile
      } else if (match(eqnid, "^EQ[0-9]") != 0) {
        # EQ = equilibria (acid-base and others)
        printf "%s\n", klatex >> eqfile
      } else if (match(eqnid, "^H[0-9]") != 0) {
        # H = aqueous-phase heterogenous and Henry reaction
        printf "%s\n", klatex >> hfile
      } else {
        errorstring = sprintf("%s\nERROR: unknown type of equation ID: %s",
          errorstring, eqnid)
      }
    }
  }
}

# ----------------------------------------------------------------------------

END {
  printf "Input file:   %s\n", ARGV[1]
  printf "Output files: %s_*.tex\n", basename
  print errorstring
  print "(error messages about dummy reactions like D05 can be ignored)"
}

# ----------------------------------------------------------------------------
