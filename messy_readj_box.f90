!*****************************************************************************
!                Time-stamp: <2008-11-10 12:09:27 sander>
!*****************************************************************************

! READJ = READ J-values (photolysis rate coefficients)

! Author:
! Rolf Sander,    MPICH, Mainz, 2008

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

MODULE messy_readj_box

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: readj_init

CONTAINS

  !***************************************************************************

  SUBROUTINE readj_init

    ! read J-values from netcdf file
    ! (code is based on init_spec from Hella Riede)

    USE messy_main_constants_mem, ONLY: STRLEN_VLONG, DP, HLINE2
    USE messy_main_tools,         ONLY: ucase ! conversion to uppercase
    USE caaba_mem,                ONLY: init_j, init_j_index
    USE caaba_io,                 ONLY: nf, open_input_file, nf90_inquire, &
                                        nf90_inquire_variable, &
                                        nf90_get_var, close_file
    USE messy_readj               ! jx
    USE messy_cmn_photol_mem      ! IP_MAX, ip_*, jname

    INTRINSIC :: TRIM

    INTEGER                     :: jp, n_var, n_dim, varid_x, ncid_j
    CHARACTER(LEN=STRLEN_VLONG) :: name_x, name_j, basename
    LOGICAL                     :: l_initialized

    PRINT *, HLINE2
    basename = TRIM(init_j(INDEX(init_j, '/', back=.TRUE.)+1:))
    PRINT *, 'subroutine readj_init'
    jx(:) = -999. ! negative dummy value
    CALL open_input_file(ncid_j, init_j) ! get ID for input file
    CALL nf(nf90_inquire(ncid_j, n_dim, n_var))
    PRINT *, 'The file '//TRIM(init_j)//' contains', n_var, 'variables'
    DO varid_x = 1,n_var ! loop over all variables in netcdf file
      CALL nf(nf90_inquire_variable(ncid_j, varid_x, name_x))
      CALL ucase(name_x) ! convert to uppercase for comparison
      IF (INDEX(name_x,'J_') == 1) THEN ! only variables starting with 'J_'
        l_initialized = .FALSE.
        DO jp = 1, IP_MAX
          name_j = 'J_'//jname(jp)
          CALL ucase(name_j) ! convert to uppercase for comparison
          IF (TRIM(name_j) == TRIM(name_x)) THEN
            CALL nf(nf90_get_var(ncid_j, varid_x, jx(jp), &
              start= (/1,1,1,init_j_index/)))
            l_initialized = .TRUE.
            EXIT
          ENDIF
        ENDDO ! jp loop
        IF (.NOT.l_initialized) &
          PRINT *, TRIM(name_x)//' found in '//TRIM(basename)//' but not used'
      ENDIF
    ENDDO ! netcdf file loop

    PRINT *, 'Results of initialization of J_*:'
    DO jp = 1, IP_MAX
      WRITE(*,'(1X,A)',ADVANCE='NO') 'J_'//jname(jp)//' ='
      IF (jx(jp)<0.) THEN
        PRINT *, ' 0 (not initialized)'
        jx(jp) = 0.
      ELSE
        WRITE(*,'(1PE12.4)') jx(jp)
      ENDIF
    ENDDO

    CALL close_file(ncid_j)

  END SUBROUTINE readj_init

  !***************************************************************************

END MODULE messy_readj_box

!*****************************************************************************
