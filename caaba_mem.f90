! Time-stamp: <2009-02-27 17:48:57 sander>

! This file declares global variables for caaba

! Authors:
! Hella Riede, MPICH, Mainz, 2007: original code
! Rolf Sander, MPICH, Mainz, 2007:

! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Foundation; either version 2
! of the License, or (at your option) any later version.
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! You should have received a copy of the GNU General Public License
! along with this program; if not, get it from:
! http://www.gnu.org/copyleft/gpl.html

!*****************************************************************************

MODULE caaba_mem

  USE messy_main_constants_mem, ONLY: STRLEN_VLONG, STRLEN_MEDIUM, &
                                      STRLEN_ULONG, DP

  IMPLICIT NONE

  ! PUBLIC is already default
  SAVE

  ! in SUBROUTINE caaba_init, caaba_version will be set to that of MECCA:
  CHARACTER(LEN=STRLEN_MEDIUM) :: caaba_version = ''

  ! caaba namelist:
  LOGICAL  :: USE_JVAL            = .FALSE. ! (messy_main_switch)
  LOGICAL  :: USE_MECCA           = .FALSE. ! (messy_main_switch)
  LOGICAL  :: USE_READJ           = .FALSE. ! (messy_main_switch)
  LOGICAL  :: USE_SAPPHO          = .FALSE. ! (messy_main_switch)
  LOGICAL  :: USE_SEMIDEP         = .FALSE. ! (messy_main_switch)
  LOGICAL  :: USE_TRAJECT         = .FALSE. ! (messy_main_switch)
  LOGICAL  :: l_ff                = .FALSE. ! frostflower model run?
  LOGICAL  :: l_oomph             = .FALSE. ! OOMPH model run?
  LOGICAL  :: l_skipkpp           = .FALSE. ! skip KPP calculations?
  LOGICAL  :: l_steady_state_stop = .FALSE. ! stop caaba at steady state?
  LOGICAL  :: l_injectNOx         = .FALSE. ! additional NOx emissions?
  LOGICAL  :: l_spechum           = .FALSE. ! spec or rel humidity for ext input?
  REAL(DP) :: degree_lat          = 45._DP  ! default lat in degrees
  REAL(DP) :: degree_lon          =  0._DP  ! default lon in degrees (0:360)
  REAL(DP) :: startday            = 80._DP
  REAL(DP) :: Ca_precip           =  0._DP  ! relative CaCO3 precipitation (0...1)
  REAL(DP) :: t_NOxon             = -1._DP  ! start of injection (day of run)
  REAL(DP) :: t_NOxoff            = -1._DP  ! end of injection (day of run)
  REAL(DP) :: runlast             = -1._DP  ! in days
  CHARACTER(LEN=STRLEN_VLONG)  :: ext_runtime     = '' ! external runtime
  CHARACTER(LEN=STRLEN_VLONG)  :: time_step       = '' ! time step length string
  CHARACTER(LEN=STRLEN_ULONG)  :: init_spec       = ''
  CHARACTER(LEN=STRLEN_ULONG)  :: init_j          = ''
  INTEGER                      :: init_j_index
  CHARACTER(LEN=STRLEN_MEDIUM) :: photrat_channel = ''
  CHARACTER(LEN=STRLEN_ULONG)  :: input_physc     = ''
  CHARACTER(LEN=STRLEN_ULONG)  :: input_jval      = ''
  CHARACTER(LEN=STRLEN_VLONG)  :: vlat            = 'LAT'   ! external latitude variable
  CHARACTER(LEN=STRLEN_VLONG)  :: vlon            = 'LON'   ! external longitude variable
  CHARACTER(LEN=STRLEN_VLONG)  :: vpress          = 'PRESS' ! external pressure variable
  CHARACTER(LEN=STRLEN_VLONG)  :: vtemp           = 'TEMP'  ! external temperature variable
  CHARACTER(LEN=STRLEN_VLONG)  :: vrelhum         = ''      ! external relative humidity variable
  CHARACTER(LEN=STRLEN_VLONG)  :: vspechum        = ''      ! external specific humidity variable

  ! TIME
  LOGICAL  :: l_ext_runtime = .FALSE. ! external runtime given
  INTEGER  :: t0year, t0month, t0day, t0hour, t0min, t0sec ! start time vars
  REAL(DP) :: model_time, model_start, model_end ! in s
  REAL(DP) :: percent_done = 0._DP
  REAL(DP) :: time0_jul = 0._DP ! Julian date of time origin
  REAL(DP) :: firstjan_jul = 0._DP ! Julian date of 01-JAN-<t0year>
  REAL(DP) :: time_step_len
  REAL(DP) :: tuf = 1._DP ! time unit factor
  REAL(DP) :: runtime     = -1._DP  ! in days
  CHARACTER(LEN=STRLEN_VLONG) :: &
    time_string = 'seconds since 2000-01-01 00:00:00' ! time origin

  ! PHOTOLYSIS
  LOGICAL  :: l_input_jval  = .FALSE. ! external j-values?
  INTEGER  :: lyear, lmonth, lday, lhour, lmin, lsec ! Gregorian localtime
  REAL(DP) :: localtime = 0._DP
  REAL(DP) :: cossza = 1._DP ! (initial dummy value)
  REAL(DP) :: degree_sza
  REAL(DP) :: x_j_no2                 ! external J_NO2
  INTEGER  :: jval_clev  ! current pressure level in jval

  ! PHYS / CHEM
  REAL(DP), DIMENSION(:), ALLOCATABLE :: C
  REAL(DP) :: cair                ! concentration of air [mcl/cc]
  REAL(DP) :: temp   = 293._DP    ! temperature [K]
  REAL(DP) :: press  = 101325._DP ! pressure [Pa]
  REAL(DP) :: relhum = 0.81_DP    ! relative humidity [0 - 1]
  REAL(DP) :: zmbl   = 1000._DP   ! height of the mbl [m]

  REAL(DP) :: spechum             ! specific humidity [?]

END MODULE caaba_mem

!*****************************************************************************
