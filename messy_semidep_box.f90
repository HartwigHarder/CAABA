! Time-stamp: <2009-03-10 14:03:47 sander>

! SEMIDEP = Simplified EMIssion and DEPosition

! Authors:
! Rolf Sander,    MPICH, Mainz, 2003-2007
! Hella Riede,    MPICH, Mainz, 2007

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

MODULE messy_semidep_box

  USE messy_mecca_kpp            ! dp, ind_*
  USE messy_main_constants_mem,  ONLY: OneDay, N_A
  USE caaba_mem,                 ONLY: model_time, zmbl, l_ff, l_oomph, &
                                       l_injectNOx, t_NOxon, t_NOxoff,  &
                                       time_step_len, startday, c

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: semidep_physc

CONTAINS

  !***************************************************************************

  SUBROUTINE semidep_physc

    CALL emission ! emission
    CALL drydep   ! dry deposition

  END SUBROUTINE semidep_physc

  !***************************************************************************
  
  SUBROUTINE emission

    ! Emission and deposition are currently calculated with Euler forward.
    ! This should be changed if numerical problems occur.

    USE messy_main_constants_mem, ONLY: MH, MC ! for OOMPH only
    IMPLICIT NONE
    REAL(dp) :: fct, fct2, fct3

    !-------------------------------------------------------------------------

    ! emission rates [molec(g)/(cm2*s)] conv. to [molec(g)/cm3]
    fct = time_step_len / (100. * zmbl)
    ! conversion 1 nmol/m2/day = 7E5 mcl/cm2/s:
    fct2 = fct * 1E-9 * N_A * 1E-4 / OneDay
    ! conversion from ng/(m2*s²)to g/(cm2*s):
    fct3 = 1.E-4 * 1.E-9 ! cm2/m2 * g/ng
    IF (l_ff) THEN
      CALL emission_ff      ! frost flower scenario
    ELSEIF (l_oomph) THEN
      CALL emission_oomph   ! OOMPH scenario
    ELSE
      CALL emission_default ! default = mbl
    ENDIF

    ! additional NO(x) EMISSIONS via injectNOx
    IF (l_injectNOx .AND. &
      (model_time >= (t_NOxon+startday)*OneDay)   .AND. &
      (model_time <= (t_NOxoff+startday)*OneDay)) THEN
      PRINT *, 'NO emissions'
      IF (ind_NO      /= 0) c(ind_NO)     = c(ind_NO)  + 1.0E11 * fct
    ENDIF

    !-------------------------------------------------------------------------

  CONTAINS

    !-------------------------------------------------------------------------

    SUBROUTINE emission_default
      IF (ind_O3     /= 0) c(ind_O3)     = c(ind_O3)     + 5.E10 * fct ! ref0203, p. 6699
      IF (ind_NO     /= 0) c(ind_NO)     = c(ind_NO)     + 1.5E9 * fct
      IF (ind_NH3    /= 0) c(ind_NH3)    = c(ind_NH3)    + 1.0E9 * fct
      IF (ind_DMS    /= 0) c(ind_DMS)    = c(ind_DMS)    + 2.0E9 * fct
      IF (ind_CHBr3  /= 0) c(ind_CHBr3)  = c(ind_CHBr3)  + 221.4 * fct2 ! Lucy
      IF (ind_CH3I   /= 0) c(ind_CH3I)   = c(ind_CH3I)   + 6.0E6 * fct ! ref0897
      IF (ind_C3H7I  /= 0) c(ind_C3H7I)  = c(ind_C3H7I)  + 1.0E7 * fct ! ref0897
      ! emissions of short-lived iodocarbons are scaled by a factor of 10 to
      ! indicate that they are not evenly distributed throughout the whole mbl
      IF (ind_CH2ClI /= 0) c(ind_CH2ClI) = c(ind_CH2ClI) + 185. *10.*fct2 !Lucy
      IF (ind_CH2I2  /= 0) c(ind_CH2I2)  = c(ind_CH2I2)  + 54.7 *10.*fct2 !Lucy
      !IF (ind_CH2ClI /= 0) c(ind_CH2ClI) = c(ind_CH2ClI) + 2.0E7 * fct ! ref0897
      !IF (ind_CH2I2  /= 0) c(ind_CH2I2)  = c(ind_CH2I2)  + 3.0E7 * fct ! ref0897
    END SUBROUTINE emission_default

    !-------------------------------------------------------------------------

    SUBROUTINE emission_ff
      IF (ind_NO     /= 0) c(ind_NO)     = c(ind_NO)     + 1.0E8 * fct
    END SUBROUTINE emission_ff

    !-------------------------------------------------------------------------

    SUBROUTINE emission_oomph
      ! OOMPH MARINE BACKGROUND
      IF (ind_CH3COCH3 /= 0) c(ind_CH3COCH3) = c(ind_CH3COCH3) + 1.4E7 * fct ! 4Igen
      !IF (ind_CH3COCH3 /= 0) c(ind_CH3COCH3) = c(ind_CH3COCH3) + 1.1E7 * fct ! 4GEN
      IF (ind_CH3CHO   /= 0) c(ind_CH3CHO)   = c(ind_CH3CHO)   + 1.55E9 * fct ! 4Igen
      !IF (ind_CH3CHO   /= 0) c(ind_CH3CHO)   = c(ind_CH3CHO)   + 1.3E9 * fct ! 4GEN
      IF (ind_DMS    /= 0) c(ind_DMS)    = c(ind_DMS)    + 2.9E9 * fct ! 4Igen
      !IF (ind_DMS    /= 0) c(ind_DMS)    = c(ind_DMS)    + 2.55E9 * fct ! 4GEN
      !IF (ind_DMS    /= 0) c(ind_DMS)    = c(ind_DMS)    + 3.5E9 * fct ! 3rd gen
      !IF (ind_DMS    /= 0) c(ind_DMS)    = c(ind_DMS)    + 2.0E9 * fct
      IF (ind_ISOP   /= 0) c(ind_ISOP)   = c(ind_ISOP)   + 5._dp * N_A/(5._dp*MC+8._dp*MH) * fct3 * fct ! 4Igen
      !IF (ind_ISOP   /= 0) c(ind_ISOP)   = c(ind_ISOP)   + 4.25_dp * N_A/(5_dp*MC+8_dp*MH) * fct3 * fct ! 4gen
      IF (ind_NH3    /= 0) c(ind_NH3)    = c(ind_NH3)    + 1.0E9 * fct
      IF (ind_NO     /= 0) c(ind_NO)     = c(ind_NO)     + 0.1E9 * fct
      !IF (ind_PO3    /= 0) c(ind_PO3)    = c(ind_PO3)    + 5.E10 * fct
      IF (ind_O3     /= 0) c(ind_O3)     = c(ind_O3)     + 5.E10 * fct
      IF (ind_CHBr3  /= 0) c(ind_CHBr3)  = c(ind_CHBr3)  + 221.4 * fct2 ! Lucy
      IF (ind_CH3I   /= 0) c(ind_CH3I)   = c(ind_CH3I)   + 6.0E6 * fct ! ref0897
      IF (ind_C3H7I  /= 0) c(ind_C3H7I)  = c(ind_C3H7I)  + 1.0E7 * fct ! ref0897
      ! emissions of short-lived iodocarbons are scaled by a factor of 10 to
      ! indicate that they are not evenly distributed throughout the whole mbl
      IF (ind_CH2ClI /= 0) c(ind_CH2ClI) = c(ind_CH2ClI) + 185. *10.*fct2 !Lucy
      IF (ind_CH2I2  /= 0) c(ind_CH2I2)  = c(ind_CH2I2)  + 54.7 *10.*fct2 !Lucy
      !IF (ind_CH2ClI /= 0) c(ind_CH2ClI) = c(ind_CH2ClI) + 2.0E7 * fct ! ref0897
      !IF (ind_CH2I2  /= 0) c(ind_CH2I2)  = c(ind_CH2I2)  + 3.0E7 * fct ! ref0897
    END SUBROUTINE emission_oomph

    !-------------------------------------------------------------------------

  END SUBROUTINE emission

  !***************************************************************************

  SUBROUTINE drydep

    ! deposition velocities [cm/s]

    IMPLICIT NONE
    REAL(dp) :: fct

    fct = time_step_len / (zmbl * 100.)

    IF (l_ff) THEN
      ! no deposition for frostflower model setup
    ELSE
      ! default values for mbl:
      IF (ind_O3      /= 0) c(ind_O3)      = (1.-fct*0.04) * c(ind_O3)
      IF (ind_H2O2    /= 0) c(ind_H2O2)    = (1.-fct*0.5)  * c(ind_H2O2)
      IF (ind_NH3     /= 0) c(ind_NH3)     = (1.-fct*0.1)  * c(ind_NH3)
      IF (ind_NO2     /= 0) c(ind_NO2)     = (1.-fct*0.1)  * c(ind_NO2)
      IF (ind_N2O5    /= 0) c(ind_N2O5)    = (1.-fct*1.0)  * c(ind_N2O5)
      IF (ind_HNO3    /= 0) c(ind_HNO3)    = (1.-fct*2.0)  * c(ind_HNO3)
      !IF (ind_CH3OH   /= 0) c(ind_CH3OH)   = (1.-fct*0.08) * c(ind_CH3OH) ! 4GEN, Jacob2005
      IF (ind_CH3OOH  /= 0) c(ind_CH3OOH)  = (1.-fct*0.1)  * c(ind_CH3OOH)
      IF (ind_HCHO    /= 0) c(ind_HCHO)    = (1.-fct*0.5)  * c(ind_HCHO)
      IF (ind_HCOOH   /= 0) c(ind_HCOOH)   = (1.-fct*1.0)  * c(ind_HCOOH)
      !IF (ind_CH3COCH3 /= 0) c(ind_CH3COCH3) = (1.-fct*0.1) * c(ind_CH3COCH3) ! 4GEN, Jacob2005
      IF (ind_HCl     /= 0) c(ind_HCl)     = (1.-fct*2.0)  * c(ind_HCl)
      IF (ind_HOCl    /= 0) c(ind_HOCl)    = (1.-fct*1.0)  * c(ind_HOCl)
      IF (ind_ClNO3   /= 0) c(ind_ClNO3)   = (1.-fct*1.0)  * c(ind_ClNO3)
      IF (ind_HBr     /= 0) c(ind_HBr)     = (1.-fct*2.0)  * c(ind_HBr)
      IF (ind_HOBr    /= 0) c(ind_HOBr)    = (1.-fct*1.0)  * c(ind_HOBr)
      IF (ind_BrNO3   /= 0) c(ind_BrNO3)   = (1.-fct*1.0)  * c(ind_BrNO3)
      IF (ind_I2O2    /= 0) c(ind_I2O2)    = (1.-fct*1.0)  * c(ind_I2O2)
      IF (ind_HI      /= 0) c(ind_HI)      = (1.-fct*1.0)  * c(ind_HI)
      IF (ind_HOI     /= 0) c(ind_HOI)     = (1.-fct*1.0)  * c(ind_HOI)
      IF (ind_INO2    /= 0) c(ind_INO2)    = (1.-fct*1.0)  * c(ind_INO2)
      IF (ind_INO3    /= 0) c(ind_INO3)    = (1.-fct*1.0)  * c(ind_INO3)
      IF (ind_SO2     /= 0) c(ind_SO2)     = (1.-fct*0.5)  * c(ind_SO2)
      IF (ind_H2SO4   /= 0) c(ind_H2SO4)   = (1.-fct*2.0)  * c(ind_H2SO4)
      IF (ind_CH3SO3H /= 0) c(ind_CH3SO3H) = (1.-fct*1.0)  * c(ind_CH3SO3H)
      IF (ind_DMSO    /= 0) c(ind_DMSO)    = (1.-fct*1.0)  * c(ind_DMSO)
    ENDIF

  END SUBROUTINE drydep

  !***************************************************************************

END MODULE messy_semidep_box

!*****************************************************************************
