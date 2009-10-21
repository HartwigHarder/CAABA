! Time-stamp: <2009-01-08 09:57:31 sander>

! This module controls the MESSy submodel CALLs

! Authors:
! Rolf Sander, MPICH, Mainz, 2003-2007
! Hella Riede, MPICH, Mainz, 2007

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

MODULE messy_main_control_cb

  ! This is the base model interface layer (BMIL), i.e. the interface between
  ! the main box model program and the MESSy submodels

  USE caaba_mem, ONLY: USE_JVAL, USE_MECCA, USE_READJ, USE_SAPPHO, &
#ifdef E4CHEM
                       USE_E4CHEM,                                 &
#endif
                       USE_SEMIDEP, USE_TRAJECT

  IMPLICIT NONE

CONTAINS

  !***************************************************************************

  SUBROUTINE messy_init

    USE caaba_mem,                ONLY: model_time, model_end, runtime, &
                                        time_string, time0_jul, &
                                        t0year, t0month, t0day, t0hour, t0min, &
                                        t0sec, firstjan_jul
    USE messy_main_constants_mem, ONLY: TINY_DP, HLINE1, DP
    USE messy_main_timer,         ONLY: gregor2julian, eval_time_str

    USE messy_jval_box,           ONLY:    jval_init
    USE messy_mecca_box,          ONLY:   mecca_init
    USE messy_readj_box,          ONLY:   readj_init
    USE messy_sappho_box,         ONLY:  sappho_init
    USE messy_traject_box,        ONLY: traject_init
#ifdef E4CHEM
    USE messy_e4chem_box,         ONLY:  e4chem_init
#endif

    IMPLICIT NONE

    INTRINSIC :: ABS, TRIM

    INTEGER :: status

    ! special requirements:
    ! - traject_init must be called first to have physical boundary conditions
    !   and thus cair right when initializing specs in routine x0
    IF (USE_TRAJECT) CALL traject_init
    IF (USE_MECCA)   CALL   mecca_init
    IF (USE_READJ)   CALL   readj_init
    IF (USE_SAPPHO)  CALL  sappho_init
    IF (USE_JVAL)    CALL    jval_init
#ifdef E4CHEM
    IF (USE_E4CHEM)  CALL  e4chem_init
#endif

    WRITE(*,*) HLINE1
    WRITE(*,*) 'Input/Output time unit and origin: ', TRIM(time_string)
    WRITE(*,'(1X, "Start day  = ", F13.8, "  (=", F18.8, " s)")') &
      model_time/86400._dp, model_time
    WRITE(*,'(1X, "End day    = ", F13.8, "  (=", F18.8, " s)")') &
      model_end/86400._dp, model_end
    WRITE(*,'(1X, "runtime    = ", F13.8, "  (=", F18.8, " s)")') &
      runtime, runtime * 86400._dp
    IF (ABS(runtime*86400._dp-model_end+model_time) > TINY_DP) THEN
      PRINT *, 'Error in messy_init: runtime inconsistency'
      PRINT *, 'model_end-model_time = ', model_end-model_time
      PRINT *, 'runtime              = ', runtime*86400._dp
      STOP
    ENDIF
    WRITE(*,*) HLINE1

    ! determine start time vars (time0_jul is dummy here)
    CALL eval_time_str(status, time_string, time0_jul, &
                       t0year, t0month, t0day, t0hour, t0min, t0sec) 
    IF (status/=0) STOP

    ! determine Julian date of time origin
    time0_jul = gregor2julian(t0year, t0month, t0day, t0hour, t0min, t0sec)

    ! determine Julian date of 01-JAN of start year (t0year)
    firstjan_jul = gregor2julian(t0year, 1, 1, 0, 0, 0)

  END SUBROUTINE messy_init

  !***************************************************************************

  SUBROUTINE messy_physc

    USE messy_jval_box,    ONLY:    jval_physc
    USE messy_mecca_box,   ONLY:   mecca_physc
    USE messy_sappho_box,  ONLY:  sappho_physc
    USE messy_semidep_box, ONLY: semidep_physc
    USE messy_traject_box, ONLY: traject_physc
#ifdef E4CHEM
    USE messy_e4chem_box,  ONLY:  e4chem_physc
#endif

    ! Do not change the order of *_physc subroutines!
    IF (USE_TRAJECT) CALL traject_physc
    IF (USE_SEMIDEP) CALL semidep_physc
    IF (USE_JVAL)    CALL    jval_physc
    IF (USE_SAPPHO)  CALL  sappho_physc
    IF (USE_MECCA)   CALL   mecca_physc
#ifdef E4CHEM
    IF (USE_E4CHEM)  CALL  e4chem_physc
#endif

  END SUBROUTINE messy_physc

  !***************************************************************************

  SUBROUTINE messy_result

    USE messy_jval_box,    ONLY:    jval_result
    USE messy_mecca_box,   ONLY:   mecca_result
    USE messy_sappho_box,  ONLY:  sappho_result
    USE messy_traject_box, ONLY: traject_result
#ifdef E4CHEM
    USE messy_e4chem_box,  ONLY:  e4chem_result
#endif

    IF (USE_MECCA)   CALL   mecca_result
    IF (USE_JVAL)    CALL    jval_result
    IF (USE_SAPPHO)  CALL  sappho_result
    IF (USE_TRAJECT) CALL traject_result
#ifdef E4CHEM
    IF (USE_E4CHEM)  CALL  e4chem_result
#endif

  END SUBROUTINE messy_result

  !***************************************************************************

  SUBROUTINE messy_finish

    USE messy_jval_box,    ONLY:    jval_finish
    USE messy_mecca_box,   ONLY:   mecca_finish
    USE messy_sappho_box,  ONLY:  sappho_finish
    USE messy_traject_box, ONLY: traject_finish
#ifdef E4CHEM
    USE messy_e4chem_box,  ONLY:  e4chem_finish
#endif

    IF (USE_JVAL)    CALL    jval_finish
    IF (USE_MECCA)   CALL   mecca_finish
    IF (USE_SAPPHO)  CALL  sappho_finish
    IF (USE_TRAJECT) CALL traject_finish
#ifdef E4CHEM
    IF (USE_E4CHEM)  CALL  e4chem_finish
#endif

  END SUBROUTINE messy_finish

  !***************************************************************************

END MODULE messy_main_control_cb

!*****************************************************************************
