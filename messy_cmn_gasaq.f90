!*****************************************************************************
!                Time-stamp: <2010-04-12 15:16:45 sander>
!*****************************************************************************

! This file contains physicochemical data of species that exist in the
! GAS phase as well as in the AQueous phase. Currently, it contains only
! the soluble species from the MECCA gas.spc file. However, further
! species could be added here as long as the name does not conflict with
! any MECCA names. Several subroutines are provided that can supply
! these data to any MESSy submodel that needs them, e.g. MECCA or SCAV.

! Usage:
! - step 1: During the initialization phase, "CALL cmn_gasaq_initialize"
!           from the base model.
! - step 2: Later, but also during the initialization phase, each
!           submodel that needs the data should create its own data
!           array with appropriate indices (e.g. ind_*) and fill it
!           using the function get_gasaq() which is provided here.
! - step 3: During the time loop, the submodels should access the
!           submodel-specific data array. get_gasaq() should not be used
!           during the time loop because it needs to find the values by
!           comparing the names.

MODULE messy_cmn_gasaq

  USE messy_main_constants_mem, ONLY: DP, STRLEN_KPPSPECIES, &
    MH, MC, MN, MF, MNa, MO, MS, MCl, MBr, MI, MHg
  USE messy_main_tools,         ONLY: str
  USE messy_main_blather,       ONLY: warning
  IMPLICIT NONE

  PRIVATE
  REAL(DP), PARAMETER :: DUMMY   = -999.999_dp
  INTEGER,  PARAMETER :: MAXSIZE = 100
  INTEGER :: n_gasaq

  TYPE GASAQ_TYPE
    CHARACTER(STRLEN_KPPSPECIES) :: name ! species name
    REAL(DP) :: Henry_T0   ! Henry constant at T0 = 298 K [M/atm]
    REAL(DP) :: Henry_Tdep ! its temperature dependence [K]
    REAL(DP) :: alpha_T0   ! accommodation coefficient alpha at T0 = 298 K [1]
    REAL(DP) :: alpha_Tdep ! its temperature dependence [K]
    !qqq REAL(DP) :: dryreac ! dryreac
    !qqq REAL(DP) :: pss     ! pseudo-soil-solubility
    REAL(DP) :: M          ! molar mass [kg]
  END TYPE GASAQ_TYPE
  TYPE(GASAQ_TYPE), DIMENSION(MAXSIZE) :: gasaq

  ! public subroutines and functions:
  PUBLIC :: cmn_gasaq_initialize, get_gasaq

CONTAINS

  ! --------------------------------------------------------------------------

  SUBROUTINE cmn_gasaq_initialize(status)

    IMPLICIT NONE
    INTEGER, INTENT(OUT) :: status

    status = 0 ! status = okay

    ! set default values:
    gasaq(:) = GASAQ_TYPE('               ', DUMMY, DUMMY, DUMMY, DUMMY, DUMMY)

    CALL def_all_species(status) ! define all species
    IF (status/=0) RETURN
    CALL add_all_henry(status)   ! add Henry's law coefficients
    CALL add_all_alpha(status)   ! add accommodation coefficients
    !qqq CALL add_all_dryreac(status) ! add dryreac and pseudo-soil-solubility
    CALL final_check(status)

  END SUBROUTINE cmn_gasaq_initialize

  ! --------------------------------------------------------------------------

  SUBROUTINE def_all_species(status)

    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=*), PARAMETER :: substr = 'def_all_species'

    n_gasaq = 0
    ! O:
    CALL add_species('O2',       MO*2.)
    CALL add_species('O3',       MO*3.)
    ! H:
    CALL add_species('OH',       MO+MH)
    CALL add_species('HO2',      MH+MO*2.)
    CALL add_species('H2O2',     MH*2.+MO*2.)
    ! N:
    CALL add_species('NH3',      MN+MH*3.)
    CALL add_species('NO',       MN+MO)
    CALL add_species('NO2',      MN+MO*2.)
    CALL add_species('NO3',      MN+MO*3.)
    CALL add_species('N2O5',     MN*2.+MO*5.)
    CALL add_species('HONO',     MH+MO+MN+MO)
    CALL add_species('HNO3',     MH+MN+MO*3.)
    CALL add_species('HNO4',     MH+MN+MO*4.)
    ! C:
    CALL add_species('CH3O2',    MC+MH*3.+MO*2.)
    CALL add_species('CH3OOH',   MC+MH*4.+MO*2.)
    CALL add_species('CO2',      MC+MO*2.)
    CALL add_species('HCHO',     MC+MH*2.+MO)
    CALL add_species('HCOOH',    MC+MH*2.+MO*2.)
    ! Cl:
    CALL add_species('Cl2',      MCl*2.)
    CALL add_species('HCl',      MH+MCl)
    CALL add_species('HOCl',     MH+MO+MCl)
    CALL add_species('ClNO3',    MCl+MN+MO*3.)
    ! Br:
    CALL add_species('Br2',      MBr*2.)
    CALL add_species('HBr',      MH+MBr)
    CALL add_species('HOBr',     MH+MO+MBr)
    CALL add_species('BrNO3',    MBr+MN+MO*3.)
    CALL add_species('BrCl',     MBr+MCl)
    ! I:
    CALL add_species('I2',       MI*2.)
    CALL add_species('IO',       MI+MO)
    CALL add_species('OIO',      MI+MO*2.)
    CALL add_species('I2O2',     MI*2.+MO*2.)
    CALL add_species('HI',       MH+MI)
    CALL add_species('HOI',      MH+MO+MI)
    CALL add_species('HIO3',     MH+MI+MO*3.)
    CALL add_species('INO2',     MI+MN+MO*2.)
    CALL add_species('INO3',     MI+MN+MO*3.)
    CALL add_species('ICl',      MI+MCl)
    CALL add_species('IBr',      MI+MBr)
    ! S:
    CALL add_species('SO2',      MS+MO*2.)
    CALL add_species('H2SO4',    MH*2.+MS+MO*4.)
    CALL add_species('CH3SO3H',  MC+MH*4.+MS+MO*3.)
    CALL add_species('DMS',      MC*2.+MH*6.+MS)
    CALL add_species('DMSO',     MC*2.+MH*6.+MS+MO)
    ! Hg:
    CALL add_species('Hg',       MHg)
    CALL add_species('HgO',      MHg+MO)
    CALL add_species('HgCl2',    MHg+MCl*2.)
    CALL add_species('HgBr2',    MHg+MBr*2.)
    CALL add_species('ClHgBr',   MHg+MCl+MBr)
    CALL add_species('BrHgOBr',  MHg+MO+MBr*2.)
    CALL add_species('ClHgOBr',  MHg+MO+MCl+MBr)

    IF (n_gasaq>MAXSIZE) THEN
      CALL warning("MAXSIZE too small, set to >= "//str(n_gasaq), substr)
      status = -1
    ENDIF

  CONTAINS

    SUBROUTINE add_species(name, M)

      CHARACTER(LEN=*), INTENT(IN) :: name
      REAL(DP),         INTENT(IN) :: M     ! molar mass [g/mol]
      INTEGER :: i

      n_gasaq = n_gasaq + 1
      IF (n_gasaq>MAXSIZE) RETURN

      ! loop over previously defined species (loop is skipped completely
      ! if this is the first call of add_species):
      DO i = 1, n_gasaq
        IF (TRIM(gasaq(i)%name)==name) THEN
          CALL warning("Species "//name//" has been defined already.", substr)
          status = -1
        ENDIF
      ENDDO

      gasaq(n_gasaq)%name = name
      gasaq(n_gasaq)%M    = M / 1E3_dp ! converted to [kg/mol]
    END SUBROUTINE add_species

  END SUBROUTINE def_all_species

  ! --------------------------------------------------------------------------

  SUBROUTINE add_all_henry(status)

    USE messy_main_constants_mem, ONLY: HUGE_DP
    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=*), PARAMETER :: substr = 'add_all_henry'

    ! The following definitions are read by henry2tex.awk which
    ! transforms the data into a LaTeX table. Therefore, the syntax must be:
    ! "CALL add_henry('XYZ', KH, minDHR) ! {&REF}"
    ! O:
    CALL add_henry('O2',       1.3E-3_dp,          1500._dp) ! {&190}
    CALL add_henry('O3',       1.2E-2_dp,          2560._dp) ! {&87}
    ! H:
    CALL add_henry('OH',       3.0E1_dp,           4300._dp) ! {&515}
    CALL add_henry('HO2',      3.9E3_dp,           5900._dp) ! {&515}
    CALL add_henry('H2O2',     1.E5_dp,            6338._dp) ! {&311}
    ! N:
    CALL add_henry('NH3',      58._dp,             4085._dp) ! {&87}
    CALL add_henry('NO',       1.9E-3_dp,          1480._dp) ! {&449}
    CALL add_henry('NO2',      7.0E-3_dp,          2500._dp) ! {&&59}
    CALL add_henry('NO3',      2._dp,              2000._dp) ! {&219}
    CALL add_henry('N2O5',     HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('HONO',     4.9E1_dp,           4780._dp) ! {&449}
    CALL add_henry('HNO3',     2.45E6_dp/1.5E1_dp, 8694._dp) ! {&&530}
    CALL add_henry('HNO4',     1.2E4_dp,           6900._dp) ! {&797}
    ! C:
    CALL add_henry('CH3O2',    6._dp,              5600._dp) ! {&&46}
    CALL add_henry('CH3OOH',   3.0E2_dp,           5322._dp) ! {&311}
    CALL add_henry('CO2',      3.1E-2_dp,          2423._dp) ! {&87}
    CALL add_henry('HCHO',     7.0E3_dp,           6425._dp) ! {&87}
    CALL add_henry('HCOOH',    3.7E3_dp,           5700._dp) ! {&87}
    ! Cl:
    CALL add_henry('Cl2',      9.2E-2_dp,          2081._dp) ! {&1038}
    CALL add_henry('HCl',      2./1.7_dp,          9001._dp) ! {&530}
    CALL add_henry('HOCl',     6.7E2_dp,           5862._dp) ! {&315}
    CALL add_henry('ClNO3',    HUGE_DP,               0._dp) ! {&&}
    ! Br:
    CALL add_henry('Br2',      7.7E-1_dp,          3837._dp) ! {&1038}
    CALL add_henry('HBr',      1.3_dp,            10239._dp) ! {&&530}
    CALL add_henry('HOBr',     9.3E1_dp,           5862._dp) ! {&&446}
    CALL add_henry('BrNO3',    HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('BrCl',     9.4E-1_dp,          5600._dp) ! {&1038}
    ! I:
    CALL add_henry('I2',       3._dp,              4431._dp) ! {&582}
    CALL add_henry('IO',       4.5E2_dp,           5862._dp) ! {&&}
    CALL add_henry('OIO',      HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('I2O2',     HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('HI',       HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('HOI',      4.5E2_dp,           5862._dp) ! {&&162}
    CALL add_henry('HIO3',     HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('INO2',     HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('INO3',     HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('ICl',      1.1E2_dp,           5600._dp) ! {&&}
    CALL add_henry('IBr',      2.4E1_dp,           5600._dp) ! {&&}
    ! S:
    CALL add_henry('SO2',      1.2_dp,             3120._dp) ! {&87}
    CALL add_henry('H2SO4',    1.E11_dp,              0._dp) ! {&&}
    CALL add_henry('CH3SO3H',  HUGE_DP,               0._dp) ! {&&}
    CALL add_henry('DMS',      5.4E-1_dp,          3500._dp) ! {&1525}
    CALL add_henry('DMSO',     5.E4_dp,            6425._dp) ! {&&389}
    ! Hg:
    CALL add_henry('Hg',       0.13_dp,               0._dp) ! {&2171}
    CALL add_henry('HgO',      3.2E6_dp,              0._dp) ! {&2285}
    CALL add_henry('HgCl2',    2.4E7_dp,              0._dp) ! {&2285}
    CALL add_henry('HgBr2',    2.4E7_dp,              0._dp) ! {&&}
    CALL add_henry('ClHgBr',   2.4E7_dp,              0._dp) ! {&&}
    CALL add_henry('BrHgOBr',  2.4E7_dp,              0._dp) ! {&&}
    CALL add_henry('ClHgOBr',  2.4E7_dp,              0._dp) ! {&&}

  CONTAINS

    SUBROUTINE add_henry(name, Henry_T0, Henry_Tdep)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: name
      REAL(DP),         INTENT(IN)    :: Henry_T0
      REAL(DP),         INTENT(IN)    :: Henry_Tdep
      INTEGER :: i
      LOGICAL :: l_found
      l_found = .FALSE.
      DO i = 1, n_gasaq
        IF (TRIM(gasaq(i)%name)==name) THEN
          l_found = .TRUE.
          IF ((ABS(gasaq(i)%Henry_T0-DUMMY)>TINY(0._dp)).OR. &
            (ABS(gasaq(i)%Henry_Tdep-DUMMY)>TINY(0._dp))) THEN
            CALL warning("Henry for "//name//" has been added already.", substr)
            status = -1
          ELSE
            gasaq(i)%Henry_T0   = Henry_T0
            gasaq(i)%Henry_Tdep = Henry_Tdep
          ENDIF
        ENDIF
      ENDDO
      IF (.NOT.l_found) THEN
        CALL warning(name//" does not exist, cannot add Henry.", substr)
        status = -1
      ENDIF
    END SUBROUTINE add_henry

  END SUBROUTINE add_all_henry

  ! --------------------------------------------------------------------------

  SUBROUTINE add_all_alpha(status)

    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=*), PARAMETER :: substr = 'add_all_alpha'

    ! default values:
    REAL(DP), PARAMETER :: alpha_T0   = 0.1_dp
    REAL(DP), PARAMETER :: alpha_Tdep = 0._dp

    ! The following definitions are read by alpha2tex.awk which
    ! transforms the data into a LaTeX table. Therefore, the syntax must be:
    ! "CALL add_alpha('XYZ', alpha0, minDHR) ! {&REF}"
    ! O:
    CALL add_alpha('O2',       0.01_dp,         2000._dp) ! {&&}
    CALL add_alpha('O3',       0.002_dp,      alpha_Tdep) ! {&&826}
    ! H:
    CALL add_alpha('OH',       0.01_dp,       alpha_Tdep) ! {&&1047}
    CALL add_alpha('HO2',      0.5_dp,        alpha_Tdep) ! {&1864}
    CALL add_alpha('H2O2',     0.077_dp,        3127._dp) ! {&32}
    ! N:
    CALL add_alpha('NH3',      0.06_dp,       alpha_Tdep) ! {&&826}
    CALL add_alpha('NO',       5.0E-5_dp,     alpha_Tdep) ! {&&448}
    CALL add_alpha('NO2',      0.0015_dp,     alpha_Tdep) ! {&&176}
    CALL add_alpha('NO3',      0.04_dp,       alpha_Tdep) ! {&&1048}
    CALL add_alpha('N2O5',     alpha_T0,      alpha_Tdep) ! {&&826}
    CALL add_alpha('HONO',     0.04_dp,       alpha_Tdep) ! {&&826}
    CALL add_alpha('HNO3',     0.5_dp,        alpha_Tdep) ! {&&930}
    CALL add_alpha('HNO4',     alpha_T0,      alpha_Tdep) ! {&&826}
    ! C:
    CALL add_alpha('CH3O2',    0.01_dp,         2000._dp) ! {&&}
    CALL add_alpha('CH3OOH',   0.0046_dp,       3273._dp) ! {&844}
    CALL add_alpha('CO2',      0.01_dp,         2000._dp) ! {&&}
    CALL add_alpha('HCHO',     0.04_dp,       alpha_Tdep) ! {&&826}
    CALL add_alpha('HCOOH',    0.014_dp,        3978._dp) ! {&826}
    ! Cl:
    CALL add_alpha('Cl2',      0.038_dp,        6546._dp) ! {&380}
    CALL add_alpha('HCl',      0.074_dp,        3072._dp) ! {&&1161}
    CALL add_alpha('HOCl',     0.5_dp,        alpha_Tdep) ! {&&}
    CALL add_alpha('ClNO3',    0.108_dp,      alpha_Tdep) ! {&&1647}
    ! Br:
    CALL add_alpha('Br2',      0.038_dp,        6546._dp) ! {&380}
    CALL add_alpha('HBr',      0.032_dp,        3940._dp) ! {&&1161}
    CALL add_alpha('HOBr',     0.5_dp,        alpha_Tdep) ! {&&930}
    CALL add_alpha('BrNO3',    0.063_dp,      alpha_Tdep) ! {&&1647}
    CALL add_alpha('BrCl',     0.38_dp,         6546._dp) ! {&&}
    ! I:
    CALL add_alpha('I2',       0.01_dp,         2000._dp) ! {&&}
    CALL add_alpha('IO',       0.5_dp,          2000._dp) ! {&&}
    CALL add_alpha('OIO',      0.01_dp,       alpha_Tdep) ! {&&}
    CALL add_alpha('I2O2',     alpha_T0,        2000._dp) ! {&&}
    CALL add_alpha('HI',       0.036_dp,        4130._dp) ! {&&1161}
    CALL add_alpha('HOI',      0.5_dp,        alpha_Tdep) ! {&&}
    CALL add_alpha('HIO3',     0.01_dp,       alpha_Tdep) ! {&&}
    CALL add_alpha('INO2',     alpha_T0,        2000._dp) ! {&&}
    CALL add_alpha('INO3',     alpha_T0,        2000._dp) ! {&&}
    CALL add_alpha('ICl',      0.018_dp,        2000._dp) ! {&2159}
    CALL add_alpha('IBr',      0.018_dp,        2000._dp) ! {&&}
    ! S:
    CALL add_alpha('SO2',      0.11_dp,       alpha_Tdep) ! {&826}
    CALL add_alpha('H2SO4',    0.65_dp,       alpha_Tdep) ! {&&1205}
    CALL add_alpha('CH3SO3H',  0.076_dp,        1762._dp) ! {&389}
    CALL add_alpha('DMS',      alpha_T0,      alpha_Tdep) ! {&&}
    CALL add_alpha('DMSO',     0.048_dp,        2578._dp) ! {&389}
    ! Hg:
    CALL add_alpha('Hg',       alpha_T0,      alpha_Tdep) ! {&&}
    CALL add_alpha('HgO',      alpha_T0,      alpha_Tdep) ! {&&}
    CALL add_alpha('HgCl2',    alpha_T0,      alpha_Tdep) ! {&&}
    CALL add_alpha('HgBr2',    alpha_T0,      alpha_Tdep) ! {&&}
    CALL add_alpha('ClHgBr',   alpha_T0,      alpha_Tdep) ! {&&}
    CALL add_alpha('BrHgOBr',  alpha_T0,      alpha_Tdep) ! {&&}
    CALL add_alpha('ClHgOBr',  alpha_T0,      alpha_Tdep) ! {&&}

  CONTAINS

    SUBROUTINE add_alpha(name, alpha_T0, alpha_Tdep)
      IMPLICIT NONE
      CHARACTER(LEN=*), INTENT(IN)    :: name
      REAL(DP),             INTENT(IN)    :: alpha_T0
      REAL(DP),             INTENT(IN)    :: alpha_Tdep
      INTEGER :: i
      LOGICAL :: l_found
      l_found = .FALSE.
      DO i = 1, n_gasaq
        IF (TRIM(gasaq(i)%name)==name) THEN
          l_found = .TRUE.
          IF ((ABS(gasaq(i)%alpha_T0-DUMMY)>TINY(0._dp)).OR. &
            (ABS(gasaq(i)%alpha_Tdep-DUMMY)>TINY(0._dp))) THEN
            CALL warning("alpha for "//name//" has been added already.", substr)
            status = -1
          ELSE
            gasaq(i)%alpha_T0   = alpha_T0
            gasaq(i)%alpha_Tdep = alpha_Tdep
          ENDIF
        ENDIF
      ENDDO
      IF (.NOT.l_found) THEN
        CALL warning(name//" does not exist, cannot add alpha.", substr)
        status = -1
      ENDIF
    END SUBROUTINE add_alpha

  END SUBROUTINE add_all_alpha

  ! --------------------------------------------------------------------------

  SUBROUTINE final_check(status)

    IMPLICIT NONE
    INTEGER, INTENT(INOUT) :: status
    CHARACTER(LEN=*), PARAMETER :: substr = 'final_check'
    INTEGER :: i

    DO i = 1, n_gasaq
      IF ((ABS(gasaq(i)%Henry_T0-DUMMY)<TINY(0._dp)).OR. &
        (ABS(gasaq(i)%Henry_Tdep-DUMMY)<TINY(0._dp))) THEN
        CALL warning("Henry for "//TRIM(gasaq(i)%name)// &
          " was never added.", substr)
        status = -1
      ENDIF
      IF ((ABS(gasaq(i)%alpha_T0-DUMMY)<TINY(0._dp)).OR. &
        (ABS(gasaq(i)%alpha_Tdep-DUMMY)<TINY(0._dp))) THEN
        CALL warning("alpha for "//TRIM(gasaq(i)%name)// &
          " was never added.", substr)
        status = -1
      ENDIF
    ENDDO

  END SUBROUTINE final_check

  ! --------------------------------------------------------------------------

  INTEGER FUNCTION get_gasaq(name, &
    Henry_T0, Henry_Tdep, alpha_T0, alpha_Tdep, M)

    IMPLICIT NONE

    CHARACTER(LEN=*),   INTENT(IN)  :: name
    REAL(DP), OPTIONAL, INTENT(OUT) :: Henry_T0
    REAL(DP), OPTIONAL, INTENT(OUT) :: Henry_Tdep
    REAL(DP), OPTIONAL, INTENT(OUT) :: alpha_T0
    REAL(DP), OPTIONAL, INTENT(OUT) :: alpha_Tdep
    REAL(DP), OPTIONAL, INTENT(OUT) :: M

    INTEGER :: i

    get_gasaq = 1 ! set status to error until species "name" is found
    DO i = 1, MAXSIZE
      IF (TRIM(gasaq(i)%name)==name) THEN
        IF(PRESENT(Henry_T0))   Henry_T0   = gasaq(i)%Henry_T0
        IF(PRESENT(Henry_Tdep)) Henry_Tdep = gasaq(i)%Henry_Tdep
        IF(PRESENT(alpha_T0))   alpha_T0   = gasaq(i)%alpha_T0
        IF(PRESENT(alpha_Tdep)) alpha_Tdep = gasaq(i)%alpha_Tdep
        IF(PRESENT(M))          M          = gasaq(i)%M
        get_gasaq = 0 ! status = okay
      ENDIF
    ENDDO

  END FUNCTION get_gasaq

!*****************************************************************************
END MODULE messy_cmn_gasaq
!*****************************************************************************
