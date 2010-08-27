!****************************************************************************
!                Time-stamp: <2010-06-14 13:07:11 sander>
!****************************************************************************

! Definitions of machine precision constants as Fortran PARAMETERs for MESSy
! Definitions of physical constants as Fortran PARAMETERs for MESSy

! Authors:
! Rolf Sander,     MPICH, 2004: original code
! Patrick Joeckel, MPICH, 2004: preprocessor-directives removed; the 
!                               BASEMODEL now may use the constants of the
!                               Modular Earth Submodel System ...

MODULE messy_main_constants_mem

  IMPLICIT NONE
  INTRINSIC :: SELECTED_INT_KIND, SELECTED_REAL_KIND, TINY
  ! PUBLIC is already default

  CHARACTER(LEN=*), PARAMETER :: modstr = 'MESSy'
  CHARACTER(LEN=*), PARAMETER :: modver = '2.3zq'

  ! MACHINE PRECISION CONSTANTS
  INTEGER, PARAMETER :: sp = SELECTED_REAL_KIND(6,37)
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12,307)
  INTEGER, PARAMETER :: i4 = SELECTED_INT_KIND(9)
  INTEGER, PARAMETER :: i8 = SELECTED_INT_KIND(14)
  INTEGER, PARAMETER :: wp = dp
  REAL(DP), PARAMETER :: TINY_DP = TINY(0._dp) ! mz_rs_20060114
  REAL(DP), PARAMETER :: HUGE_DP = HUGE(0._dp) ! mz_rs_20100409

  ! FLAGS
  REAL(DP), PARAMETER :: FLAGGED_BAD = -1.0E+34_dp  ! FERRET

  ! mz_rs_20070904+
  ! STRINGS FOR UNIFORM OUTPUT:
  CHARACTER(LEN=*), PARAMETER :: HLINE1 = &
    '*************************************'// &
    '*************************************'
  CHARACTER(LEN=*), PARAMETER :: HLINE2 = &
    '-------------------------------------'// &
    '-------------------------------------'
  CHARACTER(LEN=*), PARAMETER :: HLINE3 = &
    '.....................................'// &
    '.....................................'
  ! mz_rs_20070904-

  ! STRING LENGTHs
  INTEGER, PARAMETER :: STRLEN_SHORT      =   8
  ! mz_rs_20100331+
  ! I'm not sure if 15 is really the upper limit for the length of
  ! KPP species. However, we currently don't have any species
  ! with more than 15 characters, and that works fine...
  INTEGER, PARAMETER :: STRLEN_KPPSPECIES =  15
  ! mz_rs_20100331-
  INTEGER, PARAMETER :: STRLEN_MEDIUM     =  24
  INTEGER, PARAMETER :: STRLEN_LONG       =  64
  INTEGER, PARAMETER :: STRLEN_VLONG      =  80
  INTEGER, PARAMETER :: STRLEN_ULONG      = 256

  ! PHYSICAL CONSTANTS
  REAL(dp), PARAMETER :: pi      = 3.14159265358979323846_dp
  REAL(dp), PARAMETER :: R_gas   = 8.314409_dp  ! R [J/K/mol]
  ! Stephan-Boltzmann constant
  REAL(dp), PARAMETER :: stbo    = 5.67E-8_dp   ! [W/m2/K4]
  REAL(dp), PARAMETER :: N_A     = 6.022045E23_dp ! Avogadro constant [1/mol]
  REAL(dp), PARAMETER :: g       = 9.80665_dp   ! gravity acceleration [m/s2]
  REAL(dp), PARAMETER :: T0      = 298.15_dp    ! standard temperature [K]
  REAL(dp), PARAMETER :: T0_INV  = 1._DP / T0   ! 1/T0 [1/K]
  REAL(dp), PARAMETER :: atm2Pa  = 101325._dp   ! conversion from [atm] to [Pa]
  REAL(dp), PARAMETER :: cal2J   = 4.1868_dp    ! conversion from [cal] to [J]
  REAL(dp), PARAMETER :: k_B     = 1.380662E-23_dp ! Boltzmann constant [J/K]
  REAL(dp), PARAMETER :: c_vKar  = 0.4_dp       !  Karman constant [?]

  ! MXXX = molar mass of element XXX [g/mol]
  REAL(dp), PARAMETER :: MH  =   1.01_dp
  REAL(dp), PARAMETER :: MC  =  12.01_dp
  REAL(dp), PARAMETER :: MN  =  14.01_dp
  REAL(dp), PARAMETER :: MF  =  19.00_dp
  REAL(dp), PARAMETER :: MNa =  22.99_dp
  REAL(dp), PARAMETER :: MO  =  16.00_dp
  REAL(dp), PARAMETER :: MS  =  32.07_dp
  REAL(dp), PARAMETER :: MCl =  35.45_dp
  REAL(dp), PARAMETER :: MBr =  79.90_dp
  REAL(dp), PARAMETER :: MI  = 126.90_dp
  REAL(dp), PARAMETER :: MHg = 200.59_dp
  ! M_XXX = molar mass of compounds [g/mol]
  REAL(dp), PARAMETER :: M_O3  = MO*3._dp      ! molar mass of ozone [g/mol]
  REAL(dp), PARAMETER :: M_H2O = MH*2._dp + MO ! molar mass of H2O [g/mol]

  ! DRY AIR AND WATER VAPOUR THERMODYNAMIC CONSTANTS
  REAL(dp), PARAMETER :: tmelt   = 273.15_dp    ! melting temp. of ice/snow [K]
  REAL(dp), PARAMETER :: rho_H2O = 999.97_dp    ! density of H2O [kg/m3]
  REAL(dp), PARAMETER :: M_air   = 28.970_dp    ! molar mass of dry air [g/mol]
  REAL(dp), PARAMETER :: cp_air  = 1005.46_dp   ! specific heat of dry air at
                                                ! constant pressure [J/K/kg]
  ! mz_ap_20090519+
  REAL(dp), PARAMETER :: alv   = 2.5008e6_dp    ! latent heat for vaporisation 
  !                                             ! [J/kg]
  REAL(dp), PARAMETER :: als   = 2.8345e6_dp    ! latent heat for sublimation
  !                                             ! [J/kg]
  REAL(dp), PARAMETER :: alf   = als-alv        ! latent heat for fusion [J/kg]
  
  ! mz_ap_20090519-

  ! gas constant for dry air [J/K/kg]
  REAL(dp), PARAMETER :: rd      = 1000._dp * R_gas/M_air ! 287.05_dp
  ! gas constant for water vapour
  REAL(dp), PARAMETER :: rv      = 1000._dp * R_gas/M_H2O ! 461.51_dp
  ! specific heat of water vapour at constant pressure [J/K/kg]
  REAL(dp), PARAMETER :: cpv     = 1869.46_dp
  ! dimensionless auxiliary constants
  REAL(dp), PARAMETER :: vtmpc1  = rv/rd-1.0_dp
  REAL(dp), PARAMETER :: vtmpc2  = cpv/cp_air-1.0_dp
  REAL(dp), PARAMETER :: MM_eps  = M_H2O/M_air ! mz_hr_20070323

  ! cloud and radiation
  REAL(dp), SAVE     :: ceffmin = 10.0_dp    ! min eff.radius for ice cloud
  REAL(dp),PARAMETER :: ceffmax = 150.0_dp   ! max eff.radius for ice cloud
  REAL(dp), SAVE     :: ccwmin  = 1.0e-7_dp  ! cloud water limit for cover>0

  ! PLANETARY PARAMETERS
  REAL(dp), PARAMETER :: radius_earth = 6371000.0_dp ! radius of the Earth [m]
  REAL(dp), PARAMETER :: OneDay       = 86400.0_dp   ! one day [s]
  ! fu_kk_20061002+
  REAL(dp), PARAMETER :: solc  = 1365.0_dp           ! solar constant [W/m2]
  !REAL(dp), PARAMETER :: solc  = 1365.41_dp          ! solar constant [W/m2]
  ! fu_kk_20061002-
  ! *ratio: atmospheric height/radius of the earth.
  REAL(dp), PARAMETER :: crae = 0.1277e-02_dp

  ! mz_ab_20090525+
  REAL(dp), PARAMETER:: AM = 1.673e-27     ! Atomic mass unit
  REAL(dp), PARAMETER:: ELCH =  1.602E-19  ! Electron charge

  REAL(dp), PARAMETER:: TWOPI = pi*2._dp      ! Pi*2.
  REAL(dp), PARAMETER:: PI_2  = pi*0.5_dp     ! Pi/2.
  REAL(dp), PARAMETER:: DTR   = pi/180._dp    ! Degrees to radians
  REAL(dp), PARAMETER:: RTD   = 180._dp/pi    ! Radians to degrees
  ! mz_ab_20090525-

END MODULE messy_main_constants_mem

!*****************************************************************************
