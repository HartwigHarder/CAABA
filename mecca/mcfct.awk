# ----------------------------------------------------------------------------
#
# Author:
#   Rolf Sander, Max-Planck-Institute, Mainz, Germany, 2008-...
#
# Time-stamp: <2010-07-20 14:25:33 sander>
#
# mcfct.awk adds Monte-Carlo factors to rate coefficients in *.eqn file
#
# - usage:
#   gawk -f mcfct.awk mecca.eqn
#
# ----------------------------------------------------------------------------

BEGIN {
  i = 1
}

# ----------------------------------------------------------------------------

# (replacement text "\\1" works only with gensub but not with gsub):

# note that:
#             exp( ln(f) * mcfct ) = f^mcfct

{

  while (match($0, "{§§[^}]*}") != 0) {
    # logarithmic uncertainty (e.g. IUPAC evaluation):
    $0 = gensub("{§§([^}]*)}", "*EXP(\\1*LOG(10.)*mcexp(" i++ "))", "", $0)
  }

  while (match($0, "{§}") != 0) {
    # default uncertainty assumed to be 1.25:
    $0 = gensub("{§}", "*EXP(LOG(1.25)*mcexp(" i++ "))", "", $0)
  }

  while (match($0, "{§[^}]*}") != 0) {
    # uncertainty factor (e.g. JPL evaluation):
    $0 = gensub("{§([^}]*)}", "*EXP(LOG(\\1)*mcexp(" i++ "))", "", $0)
  }

  printf "%s\n", $0

}

# ----------------------------------------------------------------------------

END{
  # make the number of random numbers available to Fortran code via MAX_MCEXP:
  print  "#INLINE F90_GLOBAL"
  print  "  ! from mcfct.awk:"
  printf "  INTEGER, PARAMETER :: MAX_MCEXP = %d\n", i-1
  print  "!KPPPP_DIRECTIVE vector variable definition start"
  print  "  REAL :: mcexp(MAX_MCEXP) ! Monte-Carlo factor"
  print  "!KPPPP_DIRECTIVE vector variable definition end"
  print  "#ENDINLINE {above lines go to messy_mecca_kpp_global}"
}

# ----------------------------------------------------------------------------
