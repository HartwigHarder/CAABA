!*****************************************************************************
!                Time-stamp: <2009-07-08 13:41:10 sander>
!*****************************************************************************

! submodel MECCA
! Calculates chemistry
! written by:
!   Astrid Kerkweg, MPICH, Mainz, June 2003/Jan 2004
!   Rolf Sander,    MPICH, Mainz, 2003-2005

!*****************************************************************************

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

MODULE messy_mecca

  USE messy_main_constants_mem, ONLY: DP, HLINE2, STRLEN_SHORT

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: mecca_read_nml_ctrl           ! read CTRL namelist and initialize
  PUBLIC :: initialize_kpp_variables
  PUBLIC :: steady_state_reached
  PUBLIC :: define_mcfct

  CHARACTER(LEN=*), PUBLIC, PARAMETER :: modstr = 'mecca' ! name of module
  CHARACTER(LEN=*), PUBLIC, PARAMETER :: modver = '2.5j'  ! module version
  LOGICAL, PUBLIC, SAVE :: l_aero     ! switch for aero chemistry

  ! GLOBAL CTRL-NAMELIST
  CHARACTER(LEN=STRLEN_SHORT), PUBLIC, SAVE :: mecca_aero = 'AUTO' ! for l_aero
  LOGICAL, PUBLIC, SAVE :: l_force_khet = .FALSE. ! switch for khet
  LOGICAL, PUBLIC, SAVE :: l_kpp_debug  = .FALSE. ! switch for kpp debugging
  LOGICAL, PUBLIC, SAVE :: l_tag        = .FALSE. ! switch for tagging
  LOGICAL, PUBLIC, SAVE :: l_dbl        = .FALSE. ! switch for doubling
  INTEGER, PUBLIC, SAVE :: mcfct_seed   = 0       ! Monte-Carlo factor seed

CONTAINS

  ! --------------------------------------------------------------------------

  SUBROUTINE mecca_read_nml_ctrl(status, iou)

    ! READ MECCA NAMELIST, CHECK IT, AND INITIALIZE GLOBAL VARIABLES
    !
    ! Author: Astrid Kerkweg, MPICH, June 2003
    !         Rolf Sander, 2003, 2008

    USE messy_main_tools, ONLY: read_nml_open, read_nml_check, read_nml_close
    USE messy_mecca_kpp,  ONLY: REQ_AEROSOL, REQ_MCFCT

    IMPLICIT NONE

    INTRINSIC :: TRIM

    ! I/O
    INTEGER, INTENT(OUT) :: status ! error status
    INTEGER, INTENT(IN)  :: iou    ! logical I/O unit

    ! LOCAL
    LOGICAL :: lex   ! file exists?
    INTEGER :: fstat ! file status
    CHARACTER(LEN=*), PARAMETER :: substr = 'mecca_read_nml_ctrl'

    NAMELIST /CTRL/ mecca_aero, l_force_khet, l_kpp_debug, l_tag, l_dbl, &
      mcfct_seed

    ! INITIALIZE
    status = 1 ! error

    ! INPUT NAMELIST
    CALL read_nml_open(lex, substr, iou, 'CTRL', modstr)
    IF (.NOT.lex) RETURN    ! <modstr>.nml does not exist

    READ(iou, NML=CTRL, IOSTAT=fstat)
    CALL read_nml_check(fstat, substr, iou, 'CTRL', modstr)
    IF (fstat /= 0) RETURN  ! error while reading namelist

    SELECT CASE (TRIM(mecca_aero))
    CASE ('ON')
      WRITE(*,*) 'setting l_aero = T because mecca_aero = ON'
      l_aero = .TRUE.
    CASE ('OFF')
      WRITE(*,*) 'setting l_aero = F because mecca_aero = OFF'
      l_aero = .FALSE.
    CASE ('AUTO')
      WRITE(*,*) 'setting l_aero = REQ_AEROSOL'
      l_aero = REQ_AEROSOL
    CASE DEFAULT
      WRITE(*,*) 'mecca_aero = ', TRIM(mecca_aero)
      WRITE(*,*) 'mecca_aero must be [ON/OFF/AUTO]'
      RETURN ! return with status=1
    END SELECT

    IF (REQ_AEROSOL.NEQV.l_aero) &
      WRITE(*,*) 'WARNING: REQ_AEROSOL and l_aero are different!'
    WRITE(*,*) 'mecca_aero   = ', TRIM(mecca_aero)
    WRITE(*,*) 'REQ_AEROSOL  = ', REQ_AEROSOL
    WRITE(*,*) 'l_aero       = ', l_aero
    WRITE(*,*) 'l_force_khet = ', l_force_khet
    WRITE(*,*) 'l_kpp_debug  = ', l_kpp_debug
    WRITE(*,*) 'l_tag        = ', l_tag
    WRITE(*,*) 'l_dbl        = ', l_dbl
    IF (REQ_MCFCT) &
      WRITE(*,*) 'mcfct_seed   = ', mcfct_seed

    CALL read_nml_close(substr, iou, modstr)
    status = 0 ! no error

  END SUBROUTINE mecca_read_nml_ctrl

  ! --------------------------------------------------------------------------

  SUBROUTINE initialize_kpp_variables

    USE messy_mecca_kpp,            ONLY: initialize, rtol, atol &
                                        , ind_OH, ind_NO3, ind_Cl &
                                        , ind_Br, ind_O1D

    IMPLICIT NONE

    ! initialize kpp variables
    CALL initialize

!    rtol(:) = 1E-3_DP ! relative tolerance
!    atol(:) = 1._DP   ! absolute tolerance
    rtol(:) = 1E-2_dp ! relative tolerance
    atol(:) = 1E1_dp   ! absolute tolerance

     IF (ind_OH  /= 0) atol(ind_OH)  = 1._dp
     IF (ind_NO3 /= 0) atol(ind_NO3) = 1._dp
     IF (ind_Cl  /= 0) atol(ind_Cl)  = 1._dp
     IF (ind_Br  /= 0) atol(ind_Br)  = 1._dp
     IF (ind_O1D /= 0) atol(ind_O1D) = 1._dp

  END SUBROUTINE initialize_kpp_variables

  ! --------------------------------------------------------------------------

  LOGICAL FUNCTION steady_state_reached(c)

    USE messy_mecca_kpp, ONLY: ind_OH
    IMPLICIT NONE
    REAL(DP), DIMENSION(:), INTENT(IN) :: c
    REAL(DP), SAVE :: old_oh = 0.
    REAL(DP) ::  change 
     
    steady_state_reached = .FALSE.

    change=abs((old_oh-c(ind_oh))/c(ind_oh))
    IF (change<1e-3) steady_state_reached = .TRUE.

    old_oh = c(ind_oh)

  END FUNCTION steady_state_reached

  ! --------------------------------------------------------------------------

  SUBROUTINE define_mcfct(status, mcfct)

    USE messy_main_rnd,   ONLY: RND_MTW_GAUSS, rnd_init, rnd_number, rnd_finish
    USE messy_mecca_kpp,  ONLY: EQN_TAGS
    IMPLICIT NONE
    INTEGER,                INTENT(OUT) :: status
    REAL(DP), DIMENSION(:), INTENT(OUT) :: mcfct
    INTEGER :: i, id_rnd

    ! assign a set of normally distributed random numbers to mcfct:
    CALL rnd_init(status, id_rnd, RND_MTW_GAUSS, mcfct_seed)
    IF (status/=0) RETURN
    CALL rnd_number(id_rnd, mcfct(:))
    CALL rnd_finish(id_rnd)

    ! below, the Monte-Carlo method can be restricted to certain rate
    ! coefficients (choose one):
    DO i=1, SIZE(mcfct)
      ! do not apply mcfct to photolysis reactions:
      IF (EQN_TAGS(i)(1:1)=='J') mcfct(i) = 0.
      ! apply mcfct only to photolysis reactions:
      ! IF (EQN_TAGS(i)(1:1)/='J') mcfct(i) = 0.
    ENDDO

  END SUBROUTINE define_mcfct

!*****************************************************************************

END MODULE messy_mecca

!*****************************************************************************
