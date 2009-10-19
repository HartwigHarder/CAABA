!*****************************************************************************
!                Time-stamp: <2009-06-16 14:40:07 sander>
!*****************************************************************************

! mecca-aero is a part of submodel MECCA
! Calculates (halogen) aerosol chemistry
! written by Astrid Kerkweg, MPICH, Mainz, 2003
! Rolf Sander, 2003: strict separation between core and e5 files

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

MODULE messy_mecca_aero

  USE messy_main_constants_mem,  ONLY: R_gas, T0
  USE messy_mecca_kpp,           ONLY: dp, APN, NSPEC                   &
                                     , ind_Cl2, ind_HBr, ind_HOBr       &
                                     , ind_BrNO2, ind_BrNO3, ind_Br2    &
                                     , ind_BrCl, ind_HI, ind_HOI        &
                                     , ind_I2O2, ind_ICl, ind_IBr       &
                                     , ind_INO2, ind_INO3, ind_I2       &
                                     , ind_CH3I, ind_CH2I2, ind_C3H7I   &
                                     , ind_CH2ClI, ind_DMSO, ind_CH3SO2 &
                                     , ind_CH3SO3, ind_CH3SO3H          &
                                     , ind_CO, ind_CO2, ind_OH          &
                                     , ind_HO2, ind_CH3O2, ind_Cl       &
                                     , ind_ClO, ind_OClO, ind_Br        &
                                     , ind_BrO, ind_I, ind_IO           &
                                     , ind_O2, ind_OIO, ind_HIO3        &
                                     , ind_NO, ind_NO2, ind_HNO3        &
                                     , ind_NH3, ind_SO2, ind_H2SO4      &
                                     , ind_O3, ind_CH4, ind_C2H6        &
                                     , ind_HCOOH, ind_HCHO, ind_H2O2    &
                                     , ind_CH3OOH, ind_HONO, ind_PAN    &
                                     , ind_N2O5, ind_HNO4, ind_NO3      &
                                     , ind_DMS, ind_HCl, ind_HOCl       &
                                     , ind_ClNO2, ind_ClNO3             &
                                     , ind_Hg, ind_HgO, ind_HgCl2       &
                                     , ind_HgBr2, ind_ClHgBr            &
                                     , ind_BrHgOBr, ind_ClHgOBr         &
                                     , ind_Clm_a, ind_Brm_a, ind_H2O_a

  IMPLICIT NONE
  PRIVATE

  CHARACTER(len=*), PARAMETER, PUBLIC :: submodstr='mecca_aero'


  INTEGER, PUBLIC, DIMENSION(APN) :: idt_Hp      = 0
  INTEGER, PUBLIC, DIMENSION(APN) :: idt_Clm     = 0
  INTEGER, PUBLIC, DIMENSION(APN) :: idt_Brm     = 0
  INTEGER, PUBLIC, DIMENSION(APN) :: idt_Im      = 0
  INTEGER, PUBLIC, DIMENSION(APN) :: idt_IO3m    = 0
  INTEGER, PUBLIC, DIMENSION(APN) :: idt_HCO3m   = 0
  INTEGER, PUBLIC, DIMENSION(APN) :: idt_Nap     = 0
  INTEGER, PUBLIC                 :: idt_Brsalt  = 0
  INTEGER, PUBLIC                 :: idt_Brorg   = 0
  INTEGER, PUBLIC                 :: idt_BrSScap = 0

  PUBLIC :: mecca_aero_trans_coeff
  PUBLIC :: mecca_aero_henry
  PUBLIC :: mecca_aero_calc_k_ex
  PUBLIC :: mecca_aero_diag
  PUBLIC :: density
  PUBLIC :: layerthickness

CONTAINS

  !***************************************************************************

  SUBROUTINE mecca_aero_trans_coeff(radius, ztemp, zpress, ykmt)

    ! 1.) Calculation of the transfer coefficient for the mean mode actual
    ! radius
    ! transfer coefficient after Schwarz, 1986 (see Sander & Crutzen '96)
    ! k_mt=1/(r**2/(3*D_gas)+4*r/(3*v_mean*alpha))
    ! if : D_gas=lambda*v_mean/3. then we can rewrite k_mt as:
    ! k_mt=vmean/(r**2/lambda+4*r/(3*alpha))

    REAL(dp), INTENT(IN)  :: radius(APN)
    REAL(dp), INTENT(IN)  :: ztemp
    REAL(dp), INTENT(IN)  :: zpress
    REAL(dp), INTENT(OUT) :: ykmt(APN,0:NSPEC)

    INTEGER  :: jn, zkc
    REAL(dp) :: zx1, zrc_t,  zlambda
    REAL(dp) :: intfac(APN)
    REAL(dp) :: vmean(0:NSPEC)
    REAL(dp) :: alpha(0:NSPEC)

    ! multiplying factor to make the error of ignoring the need of
    ! integration over the mode minimal with little numerical effort
    ! deduced for box-model study

    intfac(:) = 1.0_dp ! right for bins
    !intfac(1) = 0.84_dp ! optimized value for M7 accumulation mode
    !intfac(2) = 0.55_dp ! optimized value for M7 coarse mode

    ! zlambda = mean free path
    ! 101325 Pa * 6.6E-8 m / 293.15 K = 2.28E-5 Pa*m/K is from
    ! equation (10-106) in Pruppacher and Klett = ref0064
    zlambda = 2.28E-5 * ztemp / zpress

    ! calculate mean velocities
    CALL mecca_aero_vmean(vmean, ztemp)
    ! calculate accommodation coefficients alpha
    CALL mecca_aero_alpha(alpha, ztemp)
    zrc_t = 1.E-9_dp             ! radius threshold for kmt calculation
    zx1   = 0.

    ykmt(:,:) = 0.
    DO jn = 1,NSPEC
      DO zkc = 1,APN ! aerosol mode (phase)
        IF (radius(zkc).GE.zrc_t.AND.alpha(jn).GT.0.) THEN
          zx1 = 1./(radius(zkc)*(radius(zkc)/zlambda+4./(3.*alpha(jn))))
        ELSE
          zx1 = 0.
        ENDIF
        ykmt(zkc,jn) = vmean(jn)*zx1*intfac(zkc)
      ENDDO
    ENDDO

    ! 2.) Calculation of mean (integrated) transfer coefficient for each mode
    ! omitted because the numerical effort for the numerical integration is
    ! too high

  CONTAINS

    ! ------------------------------------------------------------------------

    SUBROUTINE mecca_aero_vmean(vmean, ztemp)

      REAL(dp) :: vmean(0:NSPEC)
      REAL(dp) :: ztemp

      vmean(:) = -999.999_dp ! dummy value

      ! O
      vmean(ind_O2)      = vmean_func(3.2E-2_dp,  ztemp)
      vmean(ind_O3)      = vmean_func(4.8E-2_dp,  ztemp)
      ! H
      vmean(ind_OH)      = vmean_func(1.7E-2_dp,  ztemp)
      vmean(ind_HO2)     = vmean_func(3.3E-2_dp,  ztemp)
      vmean(ind_H2O2)    = vmean_func(3.4E-2_dp,  ztemp)
      ! N
      vmean(ind_NH3)     = vmean_func(1.7E-2_dp,  ztemp)
      vmean(ind_NO)      = vmean_func(3.E-2_dp,   ztemp)
      vmean(ind_NO2)     = vmean_func(4.6E-2_dp,  ztemp)
      vmean(ind_NO3)     = vmean_func(6.2E-2_dp,  ztemp)
      vmean(ind_N2O5)    = vmean_func(1.08E-1_dp, ztemp)
      vmean(ind_HONO)    = vmean_func(4.7E-2_dp,  ztemp)
      vmean(ind_HNO3)    = vmean_func(6.3E-2_dp,  ztemp)
      vmean(ind_HNO4)    = vmean_func(6.3E-2_dp,  ztemp)
      ! C
      vmean(ind_CH4)     = vmean_func(1.6E-2_dp,  ztemp)
      vmean(ind_CH3O2)   = vmean_func(4.7E-2_dp,  ztemp)
      vmean(ind_CH3OOH)  = vmean_func(4.8E-2_dp,  ztemp)
      vmean(ind_HCHO)    = vmean_func(3.E-2_dp,   ztemp)
      vmean(ind_CO)      = vmean_func(2.8E-2_dp,  ztemp)
      vmean(ind_CO2)     = vmean_func(4.4E-2_dp,  ztemp)
      vmean(ind_HCOOH)   = vmean_func(4.6E-2_dp,  ztemp)
      vmean(ind_C2H6)    = vmean_func(3.E-2_dp,   ztemp)
      vmean(ind_PAN)     = vmean_func(1.21E-1_dp, ztemp)
      ! Cl
      vmean(ind_Cl)      = vmean_func(3.5E-2_dp,  ztemp)
      vmean(ind_Cl2)     = vmean_func(7.1E-2_dp,  ztemp)
      vmean(ind_ClO)     = vmean_func(5.1E-2_dp,  ztemp)
      vmean(ind_HCl)     = vmean_func(3.6E-2_dp,  ztemp)
      vmean(ind_HOCl)    = vmean_func(5.2E-2_dp,  ztemp)
      vmean(ind_OClO)    = vmean_func(6.7E-2_dp,  ztemp)
      vmean(ind_ClNO2)   = vmean_func(8.1E-2_dp,  ztemp)
      vmean(ind_ClNO3)   = vmean_func(9.7E-2_dp,  ztemp)
      ! Br
      vmean(ind_Br)      = vmean_func(8.E-2_dp,   ztemp)
      vmean(ind_Br2)     = vmean_func(1.6E-1_dp,  ztemp)
      vmean(ind_BrO)     = vmean_func(9.6E-2_dp,  ztemp)
      vmean(ind_HBr)     = vmean_func(8.1E-2_dp,  ztemp)
      vmean(ind_HOBr)    = vmean_func(9.7E-2_dp,  ztemp)
      vmean(ind_BrNO2)   = vmean_func(1.26E-1_dp, ztemp)
      vmean(ind_BrNO3)   = vmean_func(1.42E-1_dp, ztemp)
      vmean(ind_BrCl)    = vmean_func(1.15E-1_dp, ztemp)
      ! I
      vmean(ind_I)       = vmean_func(1.27E-1_dp, ztemp)
      vmean(ind_I2)      = vmean_func(2.54E-1_dp, ztemp)
      vmean(ind_IO)      = vmean_func(1.43E-1_dp, ztemp)
      vmean(ind_OIO)     = vmean_func(1.59E-1_dp, ztemp)
      vmean(ind_I2O2)    = vmean_func(2.86E-1_dp, ztemp)
      vmean(ind_HI)      = vmean_func(1.28E-1_dp, ztemp)
      vmean(ind_HOI)     = vmean_func(1.44E-1_dp, ztemp)
      vmean(ind_HIO3)    = vmean_func(1.76E-1_dp, ztemp)
      vmean(ind_INO2)    = vmean_func(1.73E-1_dp, ztemp)
      vmean(ind_INO3)    = vmean_func(1.89E-1_dp, ztemp)
      vmean(ind_CH3I)    = vmean_func(1.42E-1_dp, ztemp)
      vmean(ind_CH2I2)   = vmean_func(2.68E-1_dp, ztemp)
      vmean(ind_C3H7I)   = vmean_func(1.7E-1_dp,  ztemp)
      vmean(ind_ICl)     = vmean_func(1.62E-1_dp, ztemp)
      vmean(ind_CH2ClI)  = vmean_func(1.76E-1_dp, ztemp)
      vmean(ind_IBr)     = vmean_func(2.07E-1_dp, ztemp)
      ! S
      vmean(ind_SO2)     = vmean_func(6.4E-2_dp,  ztemp)
      vmean(ind_H2SO4)   = vmean_func(9.8E-2_dp,  ztemp)
      vmean(ind_CH3SO3H) = vmean_func(9.6E-2_dp,  ztemp)
      vmean(ind_DMS)     = vmean_func(6.2E-2_dp,  ztemp)
      vmean(ind_DMSO)    = vmean_func(7.8E-2_dp,  ztemp)
      vmean(ind_CH3SO2)  = vmean_func(7.9E-2_dp,  ztemp)
      vmean(ind_CH3SO3)  = vmean_func(9.5E-2_dp,  ztemp)
      ! Hg
      vmean(ind_Hg)      = vmean_func(200.59E-3_dp, ztemp)
      vmean(ind_HgO)     = vmean_func(216.59E-3_dp, ztemp)
      vmean(ind_HgCl2)   = vmean_func(271.49E-3_dp, ztemp)
      vmean(ind_HgBr2)   = vmean_func(360.39E-3_dp, ztemp)
      vmean(ind_ClHgBr)  = vmean_func(315.94E-3_dp, ztemp)
      vmean(ind_BrHgOBr) = vmean_func(376.39E-3_dp, ztemp)
      vmean(ind_ClHgOBr) = vmean_func(331.94E-3_dp, ztemp)
      ! dummies
      ! mz_ak_20030514      vmean(ind_DGtAig)  = vmean_func(30.1E-3)
      ! mz_ak_20030514      vmean(ind_DGtAsg)  = vmean_func(30.1E-3)

    END SUBROUTINE mecca_aero_vmean

    ! ------------------------------------------------------------------------

    REAL(dp) FUNCTION vmean_func(molweight, ptemp)

      ! mean molecular speed from Maxwell-Boltzmann distribution:
      ! vmean=sqrt(8*R_gas*T/(M*pi))      (M in kg/mol)
      ! vmean in m/s
      ! sqrt(8*R_gas/pi)=4.60138

      REAL(dp) :: molweight
      REAL(dp) :: ptemp

      INTRINSIC :: SQRT

      vmean_func = SQRT(ptemp/molweight)*4.60138

    END FUNCTION vmean_func

    ! ------------------------------------------------------------------------

    SUBROUTINE mecca_aero_alpha(alpha, ztemp)

      ! calculation of accommodation coefficients needed for kmt calculations

      REAL(dp) :: alpha(0:NSPEC)
      REAL(dp) :: ztemp

      alpha(:) = 0.1_dp

      ! The following definitions of alpha are read by alpha2tex.awk which
      ! transforms the data into a LaTeX table. Therefore, the syntax must be:
      ! "alpha(ind_XYZ) = alpha_T(alpha0, minDHR) ! {&REF}"

      alpha(ind_O2)      = alpha_T(0.01_dp,   2000._dp, ztemp) ! {&&}
      alpha(ind_O3)      = alpha_T(0.002_dp,     0._dp, ztemp) ! {&&826}
      alpha(ind_OH)      = alpha_T(0.01_dp,      0._dp, ztemp) ! {&&1047}
      alpha(ind_HO2)     = alpha_T(0.5_dp,       0._dp, ztemp) ! {&1864}
      alpha(ind_H2O2)    = alpha_T(0.077_dp,  3127._dp, ztemp) ! {&32}
      alpha(ind_NH3)     = alpha_T(0.06_dp,      0._dp, ztemp) ! {&&826}
      alpha(ind_NO)      = alpha_T(5.0E-5_dp,    0._dp, ztemp) ! {&&448}
      alpha(ind_NO2)     = alpha_T(0.0015_dp,    0._dp, ztemp) ! {&&176}
      alpha(ind_NO3)     = alpha_T(0.04_dp,      0._dp, ztemp) ! {&&1048}
      alpha(ind_N2O5)    = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&826}
      alpha(ind_HONO)    = alpha_T(0.04_dp,      0._dp, ztemp) ! {&&826}
      alpha(ind_HNO3)    = alpha_T(0.5_dp,       0._dp, ztemp) ! {&&930}
      alpha(ind_HNO4)    = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&826}
      alpha(ind_CH3O2)   = alpha_T(0.01_dp,   2000._dp, ztemp) ! {&&}
      alpha(ind_CH3OOH)  = alpha_T(0.0046_dp, 3273._dp, ztemp) ! {&844}
      alpha(ind_HCHO)    = alpha_T(0.04_dp,      0._dp, ztemp) ! {&&826}
      alpha(ind_HCOOH)   = alpha_T(0.014_dp,  3978._dp, ztemp) ! {&826}
      alpha(ind_CO2)     = alpha_T(0.01_dp,   2000._dp, ztemp) ! {&&}
      alpha(ind_Cl2)     = alpha_T(0.038_dp,  6546._dp, ztemp) ! {&380}
      alpha(ind_HCl)     = alpha_T(0.074_dp,  3072._dp, ztemp) ! {&&1161}
      alpha(ind_HOCl)    = alpha_T(0.5_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_ClNO3)   = alpha_T(0.108_dp,     0._dp, ztemp) ! {&&1647}
      alpha(ind_Br2)     = alpha_T(0.038_dp,  6546._dp, ztemp) ! {&380}
      alpha(ind_HBr)     = alpha_T(0.032_dp,  3940._dp, ztemp) ! {&&1161}
      alpha(ind_HOBr)    = alpha_T(0.5_dp,       0._dp, ztemp) ! {&&930}
      alpha(ind_BrNO3)   = alpha_T(0.063_dp,     0._dp, ztemp) ! {&&1647}
      alpha(ind_BrCl)    = alpha_T(0.38_dp,   6546._dp, ztemp) ! {&&}
      alpha(ind_I2)      = alpha_T(0.01_dp,   2000._dp, ztemp) ! {&&}
      alpha(ind_IO)      = alpha_T(0.5_dp,    2000._dp, ztemp) ! {&&}
      alpha(ind_OIO)     = alpha_T(0.01_dp,      0._dp, ztemp) ! {&&}
      alpha(ind_I2O2)    = alpha_T(0.1_dp,    2000._dp, ztemp) ! {&&}
      alpha(ind_HI)      = alpha_T(0.036_dp,  4130._dp, ztemp) ! {&&1161}
      alpha(ind_HOI)     = alpha_T(0.5_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_HIO3)    = alpha_T(0.01_dp,      0._dp, ztemp) ! {&&}
      alpha(ind_INO2)    = alpha_T(0.1_dp,    2000._dp, ztemp) ! {&&}
      alpha(ind_INO3)    = alpha_T(0.1_dp,    2000._dp, ztemp) ! {&&}
      alpha(ind_ICl)     = alpha_T(0.018_dp,  2000._dp, ztemp) ! {&2159}
      alpha(ind_IBr)     = alpha_T(0.018_dp,  2000._dp, ztemp) ! {&&}
      alpha(ind_SO2)     = alpha_T(0.11_dp,      0._dp, ztemp) ! {&826}
      alpha(ind_H2SO4)   = alpha_T(0.65_dp,      0._dp, ztemp) ! {&&1205}
      alpha(ind_CH3SO3H) = alpha_T(0.076_dp,  1762._dp, ztemp) ! {&389}
      alpha(ind_DMSO)    = alpha_T(0.048_dp,  2578._dp, ztemp) ! {&389}
      alpha(ind_Hg)      = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_HgO)     = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_HgCl2)   = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_HgBr2)   = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_ClHgBr)  = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_BrHgOBr) = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&}
      alpha(ind_ClHgOBr) = alpha_T(0.1_dp,       0._dp, ztemp) ! {&&}

    END SUBROUTINE mecca_aero_alpha

    ! ------------------------------------------------------------------------

    REAL(dp) FUNCTION alpha_T(alpha0, minDHR, ptemp)

      REAL(dp) :: alpha0, minDHR, ptemp

      INTRINSIC :: EXP

      alpha_T = 1._dp / (1._dp+(1._dp/alpha0-1._dp) * &
        EXP(minDHR*((1._dp/T0)-(1._dp/ptemp))))

    END FUNCTION alpha_T

    ! ------------------------------------------------------------------------

  END SUBROUTINE mecca_aero_trans_coeff

  ! --------------------------------------------------------------------------

  SUBROUTINE mecca_aero_henry(ztemp, yhenry)

    ! Temp dependent Henry constants
    ! inverse dimensionless Henry constant: k_(H,inv)^cc:=1/(k_H^cp*RT)
    ! in equilibrium: XXXaq = k_H^cp * LWC * XXXg

    ! 0.082=8.3145*10^3/101325=R/p_0*10^3 :
    ! R includes conversion from M/atm --> mol/(m^3*Pa)
    ! so k_H^cp is taken as M/atm (the most common literature unit):

    REAL(dp), INTENT(IN)  :: ztemp
    REAL(dp), INTENT(out) :: yhenry(0:NSPEC)

    INTEGER  :: jn
    REAL(dp) :: zhenry(0:NSPEC), zfac

    zhenry(:)   = -999.999_dp !qqq dummy value

    zhenry(ind_HI)     = 0.0_dp       ! (dissociation)

    ! The following definitions of zhenry are read by henry2tex.awk which
    ! transforms the data into a LaTeX table. Therefore, the syntax must be:
    ! "zhenry(ind_XYZ) = henry_T(KH, minDHR, ztemp) ! {&REF}"

    zhenry(ind_O2)     = henry_T(1.3E-3_dp,          1500._dp, ztemp) ! {&190}
    zhenry(ind_O3)     = henry_T(1.2E-2_dp,          2560._dp, ztemp) ! {&87}
    zhenry(ind_OH)     = henry_T(3.0E1_dp,           4300._dp, ztemp) ! {&515}
    zhenry(ind_HO2)    = henry_T(3.9E3_dp,           5900._dp, ztemp) ! {&515}
    zhenry(ind_H2O2)   = henry_T(1.E5_dp,            6338._dp, ztemp) ! {&311}
    zhenry(ind_NH3)    = henry_T(58._dp,             4085._dp, ztemp) ! {&87}
    zhenry(ind_NO)     = henry_T(1.9E-3_dp,          1480._dp, ztemp) ! {&449}
    zhenry(ind_NO2)    = henry_T(7.0E-3_dp,          2500._dp, ztemp) ! {&&59}
    zhenry(ind_NO3)    = henry_T(2._dp,              2000._dp, ztemp) ! {&219}
    ! N2O5             = infinity                              ! {&&}
    zhenry(ind_HONO)   = henry_T(4.9E1_dp,           4780._dp, ztemp) ! {&449}
    zhenry(ind_HNO3)   = henry_T(2.45E6_dp/1.5E1_dp, 8694._dp, ztemp) ! {&&530}
    zhenry(ind_HNO4)   = henry_T(1.2E4_dp,           6900._dp, ztemp) ! {&797}
    zhenry(ind_CH3O2)  = henry_T(6._dp,              5600._dp, ztemp) ! {&&46}
    zhenry(ind_CH3OOH) = henry_T(3.0E2_dp,           5322._dp, ztemp) ! {&311}
    zhenry(ind_HCHO)   = henry_T(7.0E3_dp,           6425._dp, ztemp) ! {&87}
    zhenry(ind_HCOOH)  = henry_T(3.7E3_dp,           5700._dp, ztemp) ! {&87}
    zhenry(ind_CO2)    = henry_T(3.1E-2_dp,          2423._dp, ztemp) ! {&87}
    zhenry(ind_Cl2)    = henry_T(9.2E-2_dp,          2081._dp, ztemp) ! {&1038}
    zhenry(ind_HCl)    = henry_T(2._dp/1.7_dp,       9001._dp, ztemp) ! {&530}
    zhenry(ind_HOCl)   = henry_T(6.7E2_dp,           5862._dp, ztemp) ! {&315}
    ! ClNO3            = infinity                              ! {&&}
    zhenry(ind_Br2)    = henry_T(7.7E-1_dp,          3837._dp, ztemp) ! {&1038}
    zhenry(ind_HBr)    = henry_T(1.3_dp,            10239._dp, ztemp) ! {&&530}
    zhenry(ind_HOBr)   = henry_T(9.3E1_dp,           5862._dp, ztemp) ! {&&446}
    ! BrNO3            = infinity                              ! {&&}
    zhenry(ind_BrCl)   = henry_T(9.4E-1_dp,          5600._dp, ztemp) ! {&1038}
    zhenry(ind_I2)     = henry_T(3._dp,              4431._dp, ztemp) ! {&582}
    zhenry(ind_IO)     = henry_T(4.5E2_dp,           5862._dp, ztemp) ! {&&}
    ! OIO              = infinity                              ! {&&}
    ! I2O2             = infinity                              ! {&&}
    ! HI               = infinity                              ! {&&}
    zhenry(ind_HOI)    = henry_T(4.5E2_dp,           5862._dp, ztemp) ! {&&162}
    ! HIO3             = infinity                              ! {&&}
    ! INO2             = infinity                              ! {&&}
    ! INO3             = infinity                              ! {&&}
    zhenry(ind_ICl)    = henry_T(1.1E2_dp,           5600._dp, ztemp) ! {&&}
    zhenry(ind_IBr)    = henry_T(2.4E1_dp,           5600._dp, ztemp) ! {&&}
    zhenry(ind_SO2)    = henry_T(1.2_dp,             3120._dp, ztemp) ! {&87}
    zhenry(ind_H2SO4)  = henry_T(1.E11_dp,              0._dp, ztemp) ! {&&}
    ! CH3SO3H          = infinity                              ! {&&}
    zhenry(ind_DMSO)   = henry_T(5.E4_dp,            6425._dp, ztemp) ! {&&389}
    zhenry(ind_Hg)     = henry_T(0.13_dp,               0._dp, ztemp) ! {&2171}
    zhenry(ind_HgO)    = henry_T(3.2E6_dp,              0._dp, ztemp) ! {&2285}
    zhenry(ind_HgCl2)  = henry_T(2.4E7_dp,              0._dp, ztemp) ! {&2285}
    zhenry(ind_HgBr2)  = henry_T(2.4E7_dp,              0._dp, ztemp) ! {&&}
    zhenry(ind_ClHgBr) = henry_T(2.4E7_dp,              0._dp, ztemp) ! {&&}
    zhenry(ind_BrHgOBr)= henry_T(2.4E7_dp,              0._dp, ztemp) ! {&&}
    zhenry(ind_ClHgOBr)= henry_T(2.4E7_dp,              0._dp, ztemp) ! {&&}
    ! unit: mol/(l*atm) --> mol(aq)/m3(aq) / mol(g)/m3(g) (i.e. dimensionless)
    ! FCT=1.e3*8.3145*T/p_0=0.082*T
    ! PLUS conversion to inverse henry constant:  h_(H,inv)^cc = 1/k_H^cc
    ! i.e. mol(aq)/m3(aq) / mol(g)/m3(air) ->  mol(g)/m3(air) / mol(aq)/m3(aq)

    zfac = R_gas / 101325. * 1.E3 * ztemp

    yhenry(:)  = 0.
    DO jn=1,NSPEC
      IF (zhenry(jn).GT.0._dp) THEN
        yhenry(jn) = 1./(zhenry(jn)*zfac)
      ELSE
        ! "else": zhenry=0 <=> k_H^cc=infinity
        yhenry(jn) = 0._dp
      ENDIF
    ENDDO

  END SUBROUTINE mecca_aero_henry

  ! --------------------------------------------------------------------------

  REAL(dp) FUNCTION henry_T(KH0, minDHR, ptemp)

    REAL(dp) :: KH0, minDHR, ptemp

    INTRINSIC :: EXP

    ! 1/298.=3.3557e-3
    henry_T = KH0*EXP(minDHR*((1._dp/ptemp)-3.3557E-3_dp))

  END FUNCTION henry_T

  ! ------------------------------------------------------------------------

  SUBROUTINE mecca_aero_calc_k_ex(loghet, xaer, lwc, C, ykmt, yhenry &
                                  , k_exf,k_exb,k_exf_N2O5           &
                                  , k_exf_ClNO3, k_exf_BrNO3)

    ! calculation of gas-aqueous-phase exchange coefficients

    INTRINSIC :: REAL

    LOGICAL,  INTENT(IN)  :: loghet(:)
    REAL(dp), INTENT(IN)  :: xaer(:)
    REAL(dp), INTENT(IN)  :: lwc(:)
    REAL(dp), INTENT(IN)  :: C(:)
    REAL(dp), INTENT(IN)  :: ykmt(APN,0:NSPEC)
    REAL(dp), INTENT(IN)  :: yhenry(0:NSPEC)
    REAL(dp), INTENT(OUT) :: k_exf(APN,NSPEC)
    REAL(dp), INTENT(OUT) :: k_exb(APN,NSPEC)
    REAL(dp), INTENT(OUT) :: k_exf_N2O5(APN)
    REAL(dp), INTENT(OUT) :: k_exf_ClNO3(APN)
    REAL(dp), INTENT(OUT) :: k_exf_BrNO3(APN)

    INTEGER               :: jn, zkc
    REAL(dp)              :: zhetT

    k_exf(:,:) = 0.
    k_exb(:,:) = 0.
    DO jn = 1,NSPEC   ! loop over species
      DO zkc = 1, APN ! loop over modes/bins
        ! reaction rate coefficients
        k_exf(zkc,jn) = xaer(zkc)*ykmt(zkc,jn)*lwc(zkc)   ! forward
        k_exb(zkc,jn) = xaer(zkc)*ykmt(zkc,jn)*yhenry(jn) ! backward
      ENDDO
    ENDDO

    DO zkc=1,APN
      IF (ind_H2O_a(zkc)/=0 .AND. loghet(zkc)) THEN
        zhetT = C(ind_H2O_a(zkc))
        IF (ind_Clm_a(zkc)/=0) zhetT = zhetT + 5.E2 * C(ind_Clm_a(zkc))
        IF (ind_Brm_a(zkc)/=0) zhetT = zhetT + 3.E5 * C(ind_Brm_a(zkc))
        k_exf_N2O5(zkc)  = xaer(zkc) * lwc(zkc) * ykmt(zkc,IND_N2O5)  / zhetT
        k_exf_ClNO3(zkc) = xaer(zkc) * lwc(zkc) * ykmt(zkc,IND_ClNO3) / zhetT
        k_exf_BrNO3(zkc) = xaer(zkc) * lwc(zkc) * ykmt(zkc,IND_BrNO3) / zhetT
      ELSE
        k_exf_N2O5(zkc)  = 0._DP
        k_exf_ClNO3(zkc) = 0._DP
        k_exf_BrNO3(zkc) = 0._DP
      ENDIF
    ENDDO

  END SUBROUTINE mecca_aero_calc_k_ex

  ! --------------------------------------------------------------------------

  SUBROUTINE mecca_aero_diag(zmr_Brsalt, zmr_Brorg, zmr_BrSScap &
       , zmrac_Brsalt, zmrac_Brorg, zmrac_BrSScap)

    IMPLICIT NONE

    REAL(dp), INTENT(IN)    :: zmr_BrSScap,  zmrac_BrSScap
    REAL(dp), INTENT(IN)    :: zmr_Brsalt,   zmr_Brorg
    REAL(dp), INTENT(INOUT) :: zmrac_Brsalt, zmrac_Brorg

    REAL(dp) :: loss
    REAL(dp) :: Brsum, zmrcapdiff

    ! Define ratio between Brorg and Brsalt
    ! loss1 = Brsalt /(Brorg + Brsalt)
    Brsum =  zmr_Brsalt + zmr_Brorg
    IF (Brsum < TINY(Brsum) .OR. Brsum < 10.e-25) THEN
       loss = 1._dp
    ELSE
       loss =  zmr_Brsalt / Brsum
    ENDIF
    ! calculate sum of captured bromine 
    ! as the tracer BrSScap is accumlated the difference is the
    ! sum of captured Bromine in one timestep.
    zmrcapdiff   = zmrac_BrSScap - zmr_BrSScap
    ! depending on the ratio of the origines of Br atomes they are
    ! captured and loose there prior identitiy
    zmrac_Brsalt = zmrac_Brsalt - loss * zmrcapdiff
    zmrac_Brorg  = zmrac_Brorg  - (1._dp-loss) * zmrcapdiff

  END SUBROUTINE mecca_aero_diag

  ! --------------------------------------------------------------------------

  ELEMENTAL REAL(dp) FUNCTION density(press, temp, sphum)

    ! calculate air density in kg/m3

    USE messy_main_constants_mem, ONLY: R_gas, M_air, M_H2O

    REAL(dp), INTENT(in) :: press
    REAL(dp), INTENT(in) :: temp
    REAL(dp), INTENT(in) :: sphum

    density = press /( R_gas * &
      temp * (1._dp +(M_air / M_H2O -1._dp) *sphum))

  END FUNCTION density

  !-----------------------------------------------------------------------------

  ELEMENTAL REAL(dp) FUNCTION layerthickness(geopot_u, geopot_l)

    USE messy_main_constants_mem,  ONLY: g

    REAL(dp), INTENT(in) :: geopot_u
    REAL(dp), INTENT(in) :: geopot_l
    !LOCAL

    layerthickness  = (geopot_u-geopot_l)/ g
  END FUNCTION layerthickness

  !*****************************************************************************

END MODULE messy_mecca_aero

!*****************************************************************************
