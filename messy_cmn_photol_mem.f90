!****************************************************************************
!                Time-stamp: <2008-11-10 12:01:30 sander>
!****************************************************************************

! Definitions that all photolysis submodels have in common

! Author:
! Rolf Sander,     MPICH, 2008: original code

MODULE messy_cmn_photol_mem

  IMPLICIT NONE

  ! ip_* = index of photolysis
  INTEGER, PUBLIC, PARAMETER :: &
    ip_O2       =  1, ip_O3P      =  2, ip_O1D      =  3, ip_H2O2     =  4, &
    ip_NO2      =  5, ip_NO2O     =  6, ip_NOO2     =  7, ip_N2O5     =  8, &
    ip_HNO3     =  9, ip_HNO4     = 10, ip_PAN      = 11, ip_HONO     = 12, &
    ip_CH3OOH   = 13, ip_COH2     = 14, ip_CHOH     = 15, ip_PAA      = 16, &
    ip_CH3CHO   = 17, ip_CH3COCH3 = 18, ip_CH3COCHO = 19, ip_HOCl     = 20, &
    ip_OClO     = 21, ip_Cl2O2    = 22, ip_ClNO3    = 23, ip_ClNO2    = 24, &
    ip_Cl2      = 25, ip_BrO      = 26, ip_HOBr     = 27, ip_BrCl     = 28, &
    ip_BrNO3    = 29, ip_BrNO2    = 30, ip_Br2      = 31, ip_CCl4     = 32, &
    ip_CH3Cl    = 33, ip_CH3CCl3  = 34, ip_CFCl3    = 35, ip_CF2Cl2   = 36, &
    ip_CH3Br    = 37, ip_CF2ClBr  = 38, ip_CF3Br    = 39, ip_CH3I     = 40, &
    ip_C3H7I    = 41, ip_CH2ClI   = 42, ip_CH2I2    = 43, ip_IO       = 44, &
    ip_HOI      = 45, ip_I2       = 46, ip_ICl      = 47, ip_IBr      = 48, &
    ip_INO2     = 49, ip_INO3     = 50, ip_SO2      = 51, ip_SO3      = 52, &
    ip_OCS      = 53, ip_CS2      = 54, ip_H2O      = 55, ip_N2O      = 56, &
    ip_NO       = 57, ip_CO2      = 58, ip_HCl      = 59, ip_CHCl2Br  = 60, &
    ip_CHClBr2  = 61, ip_CH2ClBr  = 62, ip_CH2Br2   = 63, ip_CHBr3    = 64, &
    ip_SF6      = 65, ip_NO3NOO   = 66, ip_ClONO2   = 67, ip_MACR     = 68, &
    ip_MVK      = 69, ip_GLYOX    = 70, ip_HOCH2CHO = 71, ip_CH4      = 72

  ! IP_MAX must be set to the highest ip_* value from the definitions above:
  INTEGER, PUBLIC, PARAMETER :: IP_MAX = 72

  CHARACTER(LEN=9), PUBLIC, PARAMETER, DIMENSION(IP_MAX) :: jname = (/ &
    'O2       ', 'O3P      ', 'O1D      ', 'H2O2     ', &
    'NO2      ', 'NO2O     ', 'NOO2     ', 'N2O5     ', &
    'HNO3     ', 'HNO4     ', 'PAN      ', 'HONO     ', &
    'CH3OOH   ', 'COH2     ', 'CHOH     ', 'PAA      ', &
    'CH3CHO   ', 'CH3COCH3 ', 'CH3COCHO ', 'HOCl     ', &
    'OClO     ', 'Cl2O2    ', 'ClNO3    ', 'ClNO2    ', &
    'Cl2      ', 'BrO      ', 'HOBr     ', 'BrCl     ', &
    'BrNO3    ', 'BrNO2    ', 'Br2      ', 'CCl4     ', &
    'CH3Cl    ', 'CH3CCl3  ', 'CFCl3    ', 'CF2Cl2   ', &
    'CH3Br    ', 'CF2ClBr  ', 'CF3Br    ', 'CH3I     ', &
    'C3H7I    ', 'CH2ClI   ', 'CH2I2    ', 'IO       ', &
    'HOI      ', 'I2       ', 'ICl      ', 'IBr      ', &
    'INO2     ', 'INO3     ', 'SO2      ', 'SO3      ', &
    'OCS      ', 'CS2      ', 'H2O      ', 'N2O      ', &
    'NO       ', 'CO2      ', 'HCl      ', 'CHCl2Br  ', &
    'CHClBr2  ', 'CH2ClBr  ', 'CH2Br2   ', 'CHBr3    ', &
    'SF6      ', 'NO3NOO   ', 'ClONO2   ', 'MACR     ', &
    'MVK      ', 'GLYOX    ', 'HOCH2CHO ', 'CH4      '  &
    /)

END MODULE messy_cmn_photol_mem

!*****************************************************************************
