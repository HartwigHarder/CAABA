# ----------------------------------------------------------------------------
#
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2008
#
# Time-stamp: <2009-01-12 17:38:33 sander>
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

{
  # does current line contain a chemical equation, i.e. something like
  # " = ... : ... ; " ?
  if (match($0, "=.*:.*;") != 0) {
    # uncertainty of the rate constant:
    if (match($0, "{§([^}]*)}", arr) != 0) {
      dlogk = arr[1]
    } else {
      dlogk = 0.1 # default value: +/- 10%
    }
    # define replacement string:
      replstring = ": 10.**(" dlogk "*mcfct(" i ")) *"
    gsub(":", replstring)
    i = i + 1
  }
  printf "%s\n", $0
}

# ----------------------------------------------------------------------------
