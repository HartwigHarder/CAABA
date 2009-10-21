!*****************************************************************************
!                Time-stamp: <2009-08-03 19:22:09 sander>
!*****************************************************************************

! SAPPHO = Simplified And Parameterized PHOtolysis rates

! Authors:
! Rolf Sander,    MPICH, Mainz, 2003-2007
! Hella Riede,    MPICH, Mainz, 2007

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

MODULE messy_sappho_box

  USE caaba_io,                 ONLY: open_output_file, write_output_file,         &
                                      close_file
  USE messy_main_constants_mem, ONLY: PI, DP
  USE caaba_mem,                ONLY: photo_scenario, model_time
  USE messy_main_tools,         ONLY: PTR_3D_ARRAY
  USE messy_sappho              ! jx, jvalues
  USE messy_cmn_photol_mem      ! IP_MAX, ip_*, jname

  IMPLICIT NONE

  INTEGER :: ncid_sappho
  REAL(DP) :: photon

  PRIVATE
  PUBLIC :: sappho_init   ! initialize J-values
  PUBLIC :: sappho_physc  ! calculate J values
  PUBLIC :: sappho_result
  PUBLIC :: sappho_finish

CONTAINS

  !*****************************************************************************

  SUBROUTINE sappho_init

    CALL open_output_file(ncid_sappho, 'caaba_sappho', (/ &
      'photon  ',             &
      'J_O1D   ', 'J_O3P   ', 'J_H2O2  ', 'J_NO2   ', 'J_NOO2  ', &
      'J_NO2O  ', 'J_N2O5  ', 'J_HNO3  ', 'J_CH3OOH', 'J_CHOH  ', &
      'J_COH2  ', 'J_HOCl  ', 'J_Cl2O2 ', 'J_ClNO2 ', 'J_ClNO3 ', &
      'J_Cl2   ', 'J_HOBr  ', 'J_BrNO2 ', 'J_BrNO3 ', 'J_Br2   ', &
      'J_BrCl  ', 'J_IO    ', 'J_HOI   ', 'J_INO3  ', 'J_CH3I  ', &
      'J_I2    ', 'J_BrO   ', 'J_ICl   ', 'J_IBr   ', 'J_INO2  ', &
      'J_C3H7I ', 'J_CH2ClI', 'J_CH2I2 ', 'J_OClO  ', 'J_HNO4  ', &
      'J_HONO  ', 'J_CH3Br ', 'J_O2    '                          &
      /), (/ &
      '   ',                  &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s'                               &
      /), (/ &
      'photon    ',               &
      'J(O^1D)   ', 'J(O^3P)   ', 'J(H_2O_2) ', 'J(NO_2)   ', 'J(NOO_2)  ', &
      'J(NO_2O)  ', 'J(N_2O_5) ', 'J(HNO_3)  ', 'J(CH_3OOH)', 'J(CHOH)   ', &
      'J(COH_2)  ', 'J(HOCl)   ', 'J(Cl_2O_2)', 'J(ClNO_2) ', 'J(ClNO_3) ', &
      'J(Cl_2)   ', 'J(HOBr)   ', 'J(BrNO_2) ', 'J(BrNO_3) ', 'J(Br_2)   ', &
      'J(BrCl)   ', 'J(IO)     ', 'J(HOI)    ', 'J(INO_3)  ', 'J(CH_3I)  ', &
      'J(I_2)    ', 'J(BrO)    ', 'J(ICl)    ', 'J(IBr)    ', 'J(INO_2)  ', &
      'J(C_3H_7I)', 'J(CH_2ClI)', 'J(CH_2I_2)', 'J(OClO)   ', 'J(HNO_4)  ', &
      'J(HONO)   ', 'J(CH_3Br) ', 'J_O2      '                              &
      /) )

    jx(:) = 0.

  END SUBROUTINE sappho_init

  !*****************************************************************************

  SUBROUTINE sappho_physc

    USE caaba_mem, ONLY: cossza, degree_lat

    IMPLICIT NONE

    CALL jvalues(cossza, photo_scenario, degree_lat*PI/180., photon)

  END SUBROUTINE sappho_physc

  !*****************************************************************************

  SUBROUTINE sappho_finish

    CALL close_file(ncid_sappho)

  END SUBROUTINE sappho_finish

  !*****************************************************************************

  SUBROUTINE sappho_result

    ! write results to output file:
    CALL write_output_file(ncid_sappho, model_time, (/ &
      photon,          &
      jx(ip_O1D),  jx(ip_O3P),    jx(ip_H2O2),  jx(ip_NO2),   jx(ip_NOO2),   &
      jx(ip_NO2O), jx(ip_N2O5),   jx(ip_HNO3),  jx(ip_CH3OOH),jx(ip_CHOH),   &
      jx(ip_COH2), jx(ip_HOCl),   jx(ip_Cl2O2), jx(ip_ClNO2), jx(ip_ClNO3),  &
      jx(ip_Cl2),  jx(ip_HOBr),   jx(ip_BrNO2), jx(ip_BrNO3), jx(ip_Br2),    &
      jx(ip_BrCl), jx(ip_IO),     jx(ip_HOI),   jx(ip_INO3),  jx(ip_CH3I),   &
      jx(ip_I2),   jx(ip_BrO),    jx(ip_ICl),   jx(ip_IBr),   jx(ip_INO2),   &
      jx(ip_C3H7I),jx(ip_CH2ClI), jx(ip_CH2I2), jx(ip_OClO),  jx(ip_HNO4),   &
      jx(ip_HONO), jx(ip_CH3Br),  jx(ip_O2) /))

  END SUBROUTINE sappho_result

  !*****************************************************************************

END MODULE messy_sappho_box

!*******************************************************************************
