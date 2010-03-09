# ----------------------------------------------------------------------------
#
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2008
#
# Time-stamp: <2010-02-22 15:42:38 sander>
#
# mcfct.awk adds Monte-Carlo factor to rate coefficients in *.eqn file
#
# usage:
# gawk -f mcfct.awk mecca.eqn
# gawk -f mcfct.awk minigas.eqn > mecca.eqn ; diff minigas.eqn mecca.eqn
#
# ----------------------------------------------------------------------------

BEGIN {
  i = 1
}

# ----------------------------------------------------------------------------

# note that in awk, "log" = natural logarithm = ln

# (replacement text "\\1" works only with gensub but not with gsub):

{

  while (match($0, "{§§[^}]*}") != 0) {
    # logarithmic uncertainty (e.g. IUPAC evaluation):
    $0 = gensub("{§§([^}]*)}", "*EXP(\\1*LOG(10.)*mcfct(" i++ "))", "", $0)
  }

  while (match($0, "{§}") != 0) {
    # default uncertainty assumed to be 1.25:
    $0 = gensub("{§}", "*EXP(LOG(1.25)*mcfct(" i++ "))", "", $0)
  }

  while (match($0, "{§[^}]*}") != 0) {
    # uncertainty factor (e.g. JPL evaluation):
    $0 = gensub("{§([^}]*)}", "*EXP(LOG(\\1)*mcfct(" i++ "))", "", $0)
  }

  printf "%s\n", $0

}

# ----------------------------------------------------------------------------

# END{
# }

# ----------------------------------------------------------------------------
