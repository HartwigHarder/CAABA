! -*- f90 -*- Time-stamp: <2008-05-29 12:14:13 sander>

! This file produces ascii output. It can be used if no netcdf
! library is available.

! Author:
! Rolf Sander,    MPICH, 2003-2008

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

  USE messy_main_constants_mem, ONLY: dp

  IMPLICIT NONE

  INTEGER, PARAMETER, PUBLIC :: nf90_noerr = 0

CONTAINS

  !*****************************************************************************

  SUBROUTINE nf(status)
    INTEGER :: status
    IF (status /= nf90_noerr) THEN
      WRITE (*,*) 'ERROR: netcdf functions not implemented for ASCII files.'
      STOP
    ENDIF
  END SUBROUTINE nf

!*****************************************************************************

  ! dummy function
  INTEGER FUNCTION nf90_get_att(ncid, varid, name, values)
    INTEGER :: ncid, varid
    CHARACTER(LEN=*) :: name, values
    nf90_get_att = -1
  END FUNCTION nf90_get_att

  !***************************************************************************

  ! dummy function
  INTEGER FUNCTION nf90_get_var(ncid, varid, values, start)
    INTEGER :: ncid, varid
    REAL(dp) :: values
    INTEGER, DIMENSION(:), OPTIONAL :: start
    nf90_get_var = -1
  END FUNCTION nf90_get_var

  !***************************************************************************

  ! dummy function
  INTEGER FUNCTION nf90_inq_varid(ncid, name, varid)
    INTEGER :: ncid, varid
    CHARACTER(LEN=*) :: name
    nf90_inq_varid = -1
  END FUNCTION nf90_inq_varid

  !***************************************************************************

  ! dummy function
  INTEGER FUNCTION nf90_inq_dimid(ncid, name, dimid)
    INTEGER :: ncid, dimid
    CHARACTER(LEN=*) :: name
    nf90_inq_dimid = -1
  END FUNCTION nf90_inq_dimid

  !***************************************************************************

  ! dummy function
  INTEGER FUNCTION nf90_inquire_dimension(ncid, dimid, name, len)
    INTEGER :: ncid, dimid
    CHARACTER(LEN=*), OPTIONAL :: name
    INTEGER, OPTIONAL :: len
    nf90_inquire_dimension = -1
  END FUNCTION nf90_inquire_dimension

  !*****************************************************************************

  ! dummy function
  INTEGER FUNCTION nf90_inquire(ncid, nDimensions, nVariables)
    INTEGER :: ncid
    INTEGER :: nDimensions, nVariables
    nf90_inquire = -1
  END FUNCTION nf90_inquire

  !*****************************************************************************

  ! dummy function
  INTEGER FUNCTION nf90_inquire_variable(ncid, varid, name)
    INTEGER :: ncid, varid
    CHARACTER(LEN=*) :: name
    nf90_inquire_variable = -1
  END FUNCTION nf90_inquire_variable

  !***************************************************************************

  FUNCTION find_next_free_unit() RESULT(unit)

    INTEGER :: unit
    LOGICAL :: found, opened
    INTEGER :: i
    
    found = .FALSE.
    DO i=100,200
      INQUIRE(unit=i,opened=opened)
      IF (.NOT.opened) THEN
        unit = i
        found = .TRUE.
        EXIT
      END IF
    END DO

  END FUNCTION find_next_free_unit

  !***************************************************************************

  SUBROUTINE open_input_file(ncid, filename)

    USE messy_main_constants_mem, ONLY: STRLEN_VLONG
    IMPLICIT NONE

    INTEGER :: ncid
    CHARACTER(LEN=STRLEN_VLONG) :: filename

    PRINT *,'ERROR: open_input_file not implemented for ASCII files.'
    STOP

  END SUBROUTINE open_input_file

  !***************************************************************************

  SUBROUTINE open_output_file (f90unit, filename, species, units, long_name)

    IMPLICIT NONE

    CHARACTER(*), INTENT(IN) :: filename, species(:), units(:)
    CHARACTER(*), OPTIONAL, INTENT(IN) :: long_name(:)
    INTEGER, INTENT(OUT) :: f90unit

    INTEGER :: i

    f90unit = find_next_free_unit()

    OPEN(f90unit,file=filename//'.dat')

    DO i = 1, SIZE(species)
      IF (PRESENT(long_name)) THEN
        WRITE (f90unit,'(I3,A,A20,2A)') &
          i, ') name = ', TRIM(long_name(i)), ', unit = ', TRIM(units(i))
      ELSE
        WRITE (f90unit,'(I3,A,A20,2A)') &
          i, ') name = ', TRIM(species(i)), ', unit = ', TRIM(units(i))
      ENDIF
    END DO

  END SUBROUTINE open_output_file

  !***************************************************************************

  SUBROUTINE write_output_file (f90unit, time, x)

    IMPLICIT NONE

    INTEGER,  INTENT(IN) :: f90unit
    REAL(dp), INTENT(in) :: time
    REAL(dp), INTENT(in) :: x(:)

    INTEGER :: i

    ! write timestep
    WRITE (f90unit,*)
    WRITE (f90unit,*) 'time = ',time

    species_loop: DO i = 1, SIZE(x)
      WRITE (f90unit,'(I3,A,1PG17.8E3)') i, ') ', x(i)
    END DO species_loop

  END SUBROUTINE write_output_file

  !***************************************************************************

  SUBROUTINE close_file (f90unit)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: f90unit

    CLOSE (f90unit)

  END SUBROUTINE close_file

  !***************************************************************************

