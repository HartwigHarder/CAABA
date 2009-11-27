# ----------------------------------------------------------------------------
#
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2008
#
# Time-stamp: <2009-11-20 14:47:08 sander>
#
# mcfct.awk adds Monte-Carlo factor to rate coefficients in *.eqn file
#
# usage:
# gawk -f mcfct.awk mecca.eqn
#
# ----------------------------------------------------------------------------

BEGIN {
  i = 1
}

# ----------------------------------------------------------------------------

# note that in awk, "log" = natural logarithm = ln

{
  # does current line contain a chemical equation, i.e. something like
  # " = ... : ... ; " ?
  if (match($0, "=.*:.*;") != 0) {
    # assign uncertainty to the rate constant:
    if (match($0, "{§§([^}]*)}", arr) != 0) {
        # uncertainty is given as Delta_log_k (IUPAC):
        lnf = arr[1] * log(10)
    } else {
      if (match($0, "{§([^}]*)}", arr) != 0) {
        # uncertainty is given as factor f (JPL):
        lnf = log(arr[1])
      } else {
        # default uncertainty assumed to be exp(0.2) which is about +/- 22%
        lnf = 0.2
      }
    }
    # lnf now contains ln(f) where f is the uncertainty factor
    # define replacement string:
    replstring = ": EXP(" lnf "*mcfct(" i ")) *"
    gsub(":", replstring)
    i = i + 1
  }
  printf "%s\n", $0
}

# ----------------------------------------------------------------------------
