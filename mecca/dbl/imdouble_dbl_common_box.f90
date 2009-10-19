! ==============================================================================
! {%CMODEL}_dbl_common
! generated: {%TIMEDATE}
!
! This module is automaticly generated by imdouble utility
!
! inter-configuration module: common utils, consts
! level: boxmodel    (so far)
!
! {$DBL_INFO} ! this is a template file for imdouble utility
!
! [Gromov, MPIC, 2007-2008]
! ==============================================================================

! - general doubling parameters (as conditional defines) -----------------------

#include "{%CMODEL}_dbl_parameters.inc"

! ------------------------------------------------------------------------------

MODULE {%CMODEL}_dbl_common_box

  USE messy_mecca_kpp ! dp, ...

  IMPLICIT NONE

  REAL(dp), PARAMETER :: UNDEF = -1E+34_dp           ! undefined value mask

  PUBLIC

CONTAINS

! ==============================================================================

! calculation of the major isotope atoms number from 2 isotopologues
! concentration and number of constituent isotope-tagged atoms
  ELEMENTAL REAL(dp) FUNCTION maj2iso(major, minor, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: major, minor
    INTEGER,  INTENT(IN) :: atoms

    maj2iso = ( major + minor ) * REAL(atoms,dp) - minor

  END FUNCTION maj2iso

! ------------------------------------------------------------------------------

! calculation of the major isotope atoms number from 3 isotopologues
! concentration and number of constituent isotope-tagged atoms
  ELEMENTAL REAL(dp) FUNCTION maj3iso(major, minor_cur, minor_oth, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: major, minor_cur, minor_oth
    INTEGER,  INTENT(IN) :: atoms

    maj3iso = ( major + minor_cur + minor_oth ) * REAL(atoms,dp) - &
                      ( minor_cur + minor_oth )

  END FUNCTION maj3iso

! ------------------------------------------------------------------------------

! calculation of the isotopologue ratio from major and minor isotopologue
! molecules concentration and number of constituent isotope-tagged atoms
  ELEMENTAL REAL(dp) FUNCTION isoR2m(major, minor, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: major, minor
    INTEGER,  INTENT(IN) :: atoms

    isoR2m = minor / maj2iso(major,minor,atoms)

  END FUNCTION isoR2m

! ------------------------------------------------------------------------------

! calculation of the isotopologue ratio from major and two minor isotopologue
! molecules concentration and number of constituent isotope-tagged atoms
  ELEMENTAL REAL(dp) FUNCTION isoR3m(major, minor_cur, minor_oth, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: major, minor_cur, minor_oth
    INTEGER,  INTENT(IN) :: atoms

    isoR3m = minor_cur / maj3iso(major,minor_cur,minor_oth,atoms)

  END FUNCTION isoR3m

! ------------------------------------------------------------------------------

! calculation of the isotopologue ratio from the delta and reference ratio
  ELEMENTAL REAL(dp) FUNCTION isoRd(delta, Rst)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: delta, Rst

    isoRd = Rst * (delta + 1.0_dp)

  END FUNCTION isoRd

! ------------------------------------------------------------------------------

! calculation of the delta value from major and minor isotopologue molecules
! concentration, number of constituent isotope-tagged atoms, reference ratio
! NOT in permil
  ELEMENTAL REAL(dp) FUNCTION delta2(major, minor, Rst, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: major, minor, Rst
    INTEGER,  INTENT(IN) :: atoms

    IF ( maj2iso(major,minor,atoms) .NE. 0.0_dp ) THEN
      delta2 = ( isoR2m(major,minor,atoms) / Rst - 1.0_dp )
    ELSE
      delta2 = UNDEF
    ENDIF

  END FUNCTION delta2

! ------------------------------------------------------------------------------

! calculation of the delta value from major and two minor isotopologue molecules 
! concentration, number of constituent isotope-tagged atoms, reference ratio
! NOT in permil
  ELEMENTAL REAL(dp) FUNCTION delta3(major, minor_cur, minor_oth, Rst_cur, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: major, minor_cur, minor_oth, Rst_cur
    INTEGER,  INTENT(IN) :: atoms

    IF ( maj3iso(major,minor_cur,minor_oth,atoms) .NE. 0.0_dp ) THEN
      delta3 = ( isoR3m(major,minor_cur,minor_oth,atoms) / Rst_cur - 1.0_dp )
    ELSE
      delta3 = UNDEF
    ENDIF

  END FUNCTION delta3

! ------------------------------------------------------------------------------

! calculation of the minor isotopologue fraction 
! using delta (o/oo) & R ref. values and molecule atoms no.
  ELEMENTAL REAL(dp) FUNCTION isofrac2f(delta, Rst, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: delta       ! NOT in permil
    REAL(dp), INTENT(IN) :: Rst
    INTEGER,  INTENT(IN) :: atoms
    REAL(dp)             :: gamma

    gamma = isoRd(delta,Rst)
    isofrac2f = ( gamma * REAL(atoms,dp) ) / ( gamma + 1.0_dp )

  END FUNCTION isofrac2f

! -----------------------------------------------------------------------------

! calculation of the major isotopologue fraction 
! using delta (o/oo) & R ref. values and molecule atoms no.
  ELEMENTAL REAL(dp) FUNCTION isofrac2r(delta, Rst, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: delta       ! NOT in permil
    REAL(dp), INTENT(IN) :: Rst
    INTEGER,  INTENT(IN) :: atoms
    REAL(dp)             :: gamma

    gamma = isoRd(delta,Rst)
    isofrac2r = ( gamma * REAL(1 - atoms,dp) + 1.0_dp ) / ( gamma + 1.0_dp )

  END FUNCTION isofrac2r

! -----------------------------------------------------------------------------

! calculation of the budget fraction of the first of two minor 
! isotopologues in case of three isotopologues tagging
  ELEMENTAL REAL(dp) FUNCTION isofrac3f(delta1, R1, delta2, R2, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: delta1, delta2        ! NOT in permil
    REAL(dp), INTENT(IN) :: R1, R2
    INTEGER,  INTENT(IN) :: atoms

    isofrac3f = isoRd(delta1,R1) * REAL(atoms,dp) / &
                  ( (isoRd(delta1,R1) + isoRd(delta2,R2)) + 1.0_dp )

  END FUNCTION isofrac3f

! -----------------------------------------------------------------------------

! calculation of the major isotopologue fraction 
! in case of three isotopologues tagging
  ELEMENTAL REAL(dp) FUNCTION isofrac3r(delta1, R1, delta2, R2, atoms)

    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: delta1, delta2        ! NOT in permil
    REAL(dp), INTENT(IN) :: R1, R2
    INTEGER,  INTENT(IN) :: atoms
    REAL(dp)             :: gamma

    gamma = REAL(atoms,dp)
    isofrac3r =  gamma / ( (isoRd(delta1,R1) + isoRd(delta2,R2)) + 1.0_dp ) - &
                 gamma + 1.0_dp

  END FUNCTION isofrac3r

! -----------------------------------------------------------------------------

! kierate gives a factor you need to multiply standart reaction rate
! to account KIE with a certain eps value
! eps = (Ki/Kj - 1), 
! Ki & Kj - overall rate constants for the isotopes with mass i, j
! input eps should be expressed in per mil (o/oo), so then it is divided by 1000
  ELEMENTAL REAL(dp) FUNCTION kierate(eps)
    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: eps         ! NOT in permil
    kierate = 1.0_dp / (eps + 1.0_dp)
  END FUNCTION kierate

! ==============================================================================

END MODULE {%CMODEL}_dbl_common_box

! *****************************************************************************

