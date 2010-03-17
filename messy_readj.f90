!*****************************************************************************
!                Time-stamp: <2008-11-10 13:00:02 sander>
!*****************************************************************************

! READJ = READ J-values (photolysis rate coefficients)

! Authors:
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

MODULE messy_readj

  USE messy_main_constants_mem, ONLY: DP, PI
  USE messy_cmn_photol_mem,     ONLY: IP_MAX

  IMPLICIT NONE
  PRIVATE

  ! pointer to photolysis rate coeff.
  REAL(DP), PUBLIC, DIMENSION(IP_MAX), SAVE :: jx

  ! **************************************************************************

END MODULE messy_readj
