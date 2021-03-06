!*****************************************************************************
! Time-stamp: <2010-08-17 16:32:20 sander>
!*****************************************************************************

! MECCA: Module Efficiently Calculating the Chemistry of the Atmosphere

! Authors:
! Rolf Sander,    MPICH, Mainz, 2003-...: original code
! Astrid Kerkweg, MPICH, Mainz, 2003-2007: halogen/aerosol chemistry
! Hella Riede,    MPICH, Mainz, 2007:

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

MODULE messy_mecca_box

  USE caaba_mem,                  ONLY: startday, model_time, model_end,      &
                                        time_step_len, init_spec,             &
                                        c, cair, temp, press, relhum,         &
                                        l_ff, Ca_precip, l_steady_state_stop, &
                                        photrat_channel
  USE messy_cmn_photol_mem        ! IP_MAX, ip_*, jname
  USE messy_mecca_kpp             ! ind_*, kpp_integrate, rconst,
                                  ! APN, initialize_indexarrays,
                                  ! SPC_NAMES, EQN_TAGS, NSPEC, NREACT
  USE messy_mecca_dbl_box,        ONLY: mecca_dbl_init, mecca_dbl_postprocess,&
                                        mecca_dbl_result, mecca_dbl_finish
  USE messy_mecca_tag_box,        ONLY: mecca_tag_init, mecca_tag_process,    &
                                        mecca_tag_result, mecca_tag_finish

  USE messy_main_constants_mem,   ONLY: N_A, rho_H2O, M_H2O, R_gas,          &
                                        OneDay, STRLEN_VLONG,                &
                                        pi, HLINE2, TINY_DP
  USE messy_mecca,                ONLY: l_aero, l_tag, l_dbl, modstr

  USE caaba_io,                   ONLY: nf, open_input_file, nf90_inquire, &
                                        nf90_inquire_variable, &
                                        nf90_get_var, open_output_file, &
                                        write_output_file, close_file

  IMPLICIT NONE

  INTEGER, PARAMETER :: NBL = 1 ! N_block_length
  ! declarations for all variables that are transported to the SMCL via fill
  ! subroutines (or via kpp_integrate for "C")
  REAL(DP) :: jx(IP_MAX) = 0.
  REAL(DP), DIMENSION(:), ALLOCATABLE :: xaer
  REAL(DP) :: cvfac(APN)
  REAL(DP), DIMENSION(:), ALLOCATABLE :: lwc
  REAL(DP) :: k_exf(APN,NSPEC)
  REAL(DP) :: k_exb(APN,NSPEC)
  REAL(DP) :: k_exf_N2O5(APN)
  REAL(DP) :: k_exf_ClNO3(APN)
  REAL(DP) :: k_exf_BrNO3(APN)
  REAL(DP) :: dummy_khet_Tr(IHT_MAX) ! dummy, not needed in box model
  REAL(DP) :: dummy_khet_St(IHS_MAX) ! dummy, not needed in box model
  ! (for C, cair, and temp, see caaba_mem.f90)
  ! (see also important notes about temp, press, and cair in gas.eqn!)

  REAL(DP), DIMENSION(NSPEC) :: output
  CHARACTER(LEN=20) :: c_unit(NSPEC) ! c unit
  INTEGER :: ncid_aero, ncid_tracer, ncid_spec

  ! sea water composition, relative to total salt:
  REAL(DP) :: HCO3m_rel, NO3m_rel, Clm_rel, Brm_rel, Im_rel, IO3m_rel, &
    SO4mm_rel

  REAL(DP), DIMENSION(:), ALLOCATABLE :: csalt, exchng, radius
  REAL(DP), DIMENSION(:), ALLOCATABLE :: c0_NH4p, c0_Nap
  REAL(DP), DIMENSION(:), ALLOCATABLE :: &
    c0_HCO3m, c0_NO3m, c0_Clm, c0_Brm, c0_Im, c0_IO3m, c0_SO4mm, c0_HSO4m

  REAL(DP) :: mcexp(MAX_MCEXP) ! Monte-Carlo factor

  PRIVATE
  PUBLIC :: mecca_init   ! initialize aero chemistry
  PUBLIC :: mecca_physc  ! calculate chemistry
  PUBLIC :: mecca_result ! print results
  PUBLIC :: mecca_finish ! close files

CONTAINS

  !***************************************************************************

  SUBROUTINE mecca_init

    USE messy_main_tools,         ONLY: str
    USE messy_mecca_kpp,          ONLY: REQ_MCFCT
    USE messy_mecca,              ONLY: mecca_read_nml_ctrl, &
                                        initialize_kpp_variables, &
                                        mcexp_seed, define_mcexp
    USE messy_mecca_aero,         ONLY: mecca_aero_init_gasaq
    USE messy_main_constants_mem, ONLY: MH, MC, MO, MS, MCl, MBr
    USE messy_main_tools,         ONLY: psatf, psat

    IMPLICIT NONE

    INTRINSIC :: TRIM

    CHARACTER(LEN=20)   :: lwc_unit(APN)
    CHARACTER(LEN=7)    :: lwc_name(APN)
    INTEGER, PARAMETER  :: iou = 999   ! I/O unit
    INTEGER             :: status ! error status
    INTEGER             :: i, jb, jr

    REAL(DP), PARAMETER :: rho_sw = 1025._dp    ! density of sea water [kg/m3]
    REAL(DP) :: csw_HCO3m, csw_NO3m, csw_Clm, csw_Brm, csw_Im, csw_IO3m, &
                csw_SO4mm, csw_total

    ALLOCATE(xaer(APN))
    ALLOCATE(radius(APN))
    ALLOCATE(lwc(APN))
    ALLOCATE(csalt(APN))
    ALLOCATE(c0_NH4p(APN))
    ALLOCATE(c0_Nap(APN))
    ALLOCATE(c0_HCO3m(APN))
    ALLOCATE(c0_NO3m(APN))
    ALLOCATE(c0_Clm(APN))
    ALLOCATE(c0_Brm(APN))
    ALLOCATE(c0_Im(APN))
    ALLOCATE(c0_IO3m(APN))
    ALLOCATE(c0_SO4mm(APN))
    ALLOCATE(c0_HSO4m(APN))
    ALLOCATE(exchng(APN))

    ! read mecca ctrl namelist:
    CALL mecca_read_nml_ctrl(status, iou)
    IF (status /= 0) STOP

    ! read kpp ctrl namelist:
    CALL initialize_kpp_ctrl(status, iou, modstr)
    IF (status /= 0) STOP

    ! csw_* = concentration in sea water [mol/L]
    ! mass fraction w_i [kg/kg] (Tab. 4 of ref2307 = Millero et al. 2008)
    ! HCO3- + 2 CO3-- = 0.10481 + 2*0.01434 = 0.13349 g/kg
    csw_HCO3m = 0.00013349 * rho_sw / (MH+MC+MO*3.) * (1.-Ca_precip)
    csw_NO3m  = 1E-6 ! see Fig. 1D in ref2428 = Duce et al. 2008
    ! activate next line for Southern Ocean:
    !csw_NO3m  = 25E-6 ! ref2428 Southern Ocean
    csw_Clm   = 0.01935271 * rho_sw / MCl
    csw_Brm   = 0.00006728 * rho_sw / MBr
    csw_Im    = 7.4E-8  ! Tsunogai, Tab. 1a, ref0845
    csw_IO3m  = 2.64E-7 ! Tsunogai, Tab. 1a, ref0845
    csw_SO4mm = 0.00271235 * rho_sw / (MS+MO*4.)
    csw_total = csw_HCO3m + csw_NO3m + csw_Clm + csw_Brm + csw_Im &
      + csw_IO3m + csw_SO4mm

    ! sea water composition, relative to total salt
    HCO3m_rel = csw_HCO3m / csw_total
    NO3m_rel  = csw_NO3m  / csw_total
    Clm_rel   = csw_Clm   / csw_total
    Brm_rel   = csw_Brm   / csw_total
    Im_rel    = csw_Im    / csw_total
    IO3m_rel  = csw_IO3m  / csw_total
    SO4mm_rel = csw_SO4mm / csw_total

    IF (l_aero) THEN
      PRINT *, 'Sea water composition, relative to total salt:'
      PRINT *, 'HCO3m_rel = ', HCO3m_rel
      PRINT *, 'NO3m_rel  = ', NO3m_rel
      PRINT *, 'Clm_rel   = ', Clm_rel
      PRINT *, 'Brm_rel   = ', Brm_rel
      PRINT *, 'Im_rel    = ', Im_rel
      PRINT *, 'IO3m_rel  = ', IO3m_rel
      PRINT *, 'SO4mm_rel = ', SO4mm_rel
      PRINT *, 'SUM       = ', &
        HCO3m_rel+NO3m_rel+Clm_rel+Brm_rel+Im_rel+IO3m_rel+SO4mm_rel
    ENDIF

    C(:) = 0. ! default value unless explicitly initialized
    CALL initialize_kpp_variables

    ! Monte-Carlo:
    IF (REQ_MCFCT) THEN
      CALL define_mcexp(status, mcexp)
      IF (status /= 0) STOP
      DO i=1, MAX_MCEXP
        WRITE(*,'(A,I4,A,F12.8)') ' mcexp(', i, ') = ', mcexp(i)
        ! mz_rs_20100727+
        ! In this example, only mcexp(40) is used, all other mcexp(:)
        ! are set to zero. Look at mecca.eqn to find which reaction is
        ! affect by mcexp(40). This can be used to make Monte-Carlo
        ! simulations where only one (or a few) rate coefficients are
        ! modified.
        ! IF (i/=40) mcexp(i) = 0.                 ! modify 1 rate coefficient
        ! IF ((i/=40).OR.(i/=50)) mcexp(i) = 0.    ! modify 2 rate coefficients
        ! mz_rs_20100727-
      ENDDO
      PRINT *, 'mcexp(avg)  = ', SUM(mcexp)/MAX_MCEXP
    ENDIF

    ! choose sat. press definition: psat (series) or psatf(function)
    ! choose relhum definition: WMO or traditional
    ! must be consistent with mecca_physc!
    !mz_hr_20080226+
    ! using relhum = (mixing ratio H2O)/(sat. mixing ratio H2O): WMO def.
    !c(ind_H2O) = cair * relhum * psatf(temp) / &
    !  (press + (relhum-1.) * psatf(temp))
    ! using relhum = p(H2O)/ps(H2O):
    !c(ind_H2O) = cair * relhum * psat(temp) / press
    !mz_hr_20080226-
    CALL x0 ! initial gas phase mixing ratios
    IF (l_aero) THEN
      CALL define_aerosol ! define radius, lwc, etc.
      ! define gas-aq physicochemical constants:
      PRINT *, HLINE2
      PRINT *, "         Henry's law coefficients "// &
        "and accommodation coefficients"
      PRINT *, HLINE2
      PRINT *, 'species           Henry_T0 Henry_Tdep'// &
        '   alpha_T0 alpha_Tdep         M'
      PRINT *, '                   [M/atm]        [K]'// &
        '        [1]        [K]  [kg/mol]'
      PRINT *, HLINE2
      CALL mecca_aero_init_gasaq(l_print=.TRUE.)
    ENDIF
    IF (l_tag)  CALL mecca_tag_init
    IF (l_dbl)  CALL mecca_dbl_init

    ! ------------------------------------------------------------------------

    ! open output files and write headers:

    ! define units for reaction rates RR* and species:
    DO i = 1,NSPEC
      IF (INDEX(SPC_NAMES(i),'RR') == 1) THEN
        c_unit(i) = 'mol/mol/s'
      ELSE
        c_unit(i) = 'mol/mol' 
      ENDIF
    ENDDO
    CALL open_output_file(ncid_tracer, 'caaba_mecca', SPC_NAMES, c_unit)

    IF (l_aero) THEN
      DO jb = 1, APN
        lwc_name(jb) = 'lwc_a'//str(jb,'(I2.2)')
      ENDDO
      lwc_unit(:) = 'm3/m3'
      CALL open_output_file(ncid_aero, 'caaba_mecca_aero', lwc_name, lwc_unit)
    ENDIF

    ! ------------------------------------------------------------------------

    PRINT *, HLINE2
    PRINT *, '*** Info about MECCA and KPP:'
    PRINT *, timestamp
    PRINT *, gas_spc_file,     ' (', gas_spc_file_sum,     ')'
    PRINT *, aqueous_spc_file, ' (', aqueous_spc_file_sum, ')'
    PRINT *, mecca_spc_file,   ' (', mecca_spc_file_sum,   ')'
    PRINT *, gas_eqn_file,     ' (', gas_eqn_file_sum,     ')'
    PRINT *, aqueous_eqn_file, ' (', aqueous_eqn_file_sum, ')'
    PRINT *, mecca_eqn_file,   ' (', mecca_eqn_file_sum,   ')'
    PRINT *, 'rplfile      = ', rplfile
    PRINT *, 'wanted       = ', wanted
    PRINT *, 'diagtracfile = ', diagtracfile
    PRINT *, 'tagdbl       = ', tagdbl
    PRINT *, 'kppoption    = ', kppoption
    PRINT *, 'KPP_HOME     = ', KPP_HOME
    PRINT *, 'KPP_version  = ', KPP_version
    PRINT *, 'integr       = ', integr

    ! ------------------------------------------------------------------------

  END SUBROUTINE mecca_init

  !***************************************************************************

  SUBROUTINE define_aerosol

    USE caaba_mem, ONLY: degree_lat, zmix, zmbl
    IMPLICIT NONE

    !LOCAL
    INTEGER :: jb
    REAL    :: scalefactor

    CALL initialize_indexarrays

    ! ------------------------------------------------------------------------
    ! For each aerosol phase jb = 1...APN, the following properties must be
    ! defined inside the appropriate CASE section:
    ! xaer(jb)     = 0. or 1. = aerosol on/off
    ! radius(jb)   = radius [m]
    ! lwc(jb)      = liquid water content [m3/m3]
    ! csalt(jb)    = c(salt) = total salt concentration  [mcl/cm3(air)]
    ! c0_NH4p(jb)  = initial c(NH4+)  in fresh aerosol   [mcl/cm3(air)]
    ! c0_HCO3m(jb) = initial c(HCO3-) in fresh aerosol   [mcl/cm3(air)]
    ! c0_NO3m(jb)  = initial c(NO3-)  in fresh aerosol   [mcl/cm3(air)]
    ! c0_Clm(jb)   = initial c(Cl-)   in fresh aerosol   [mcl/cm3(air)]
    ! c0_Brm(jb)   = initial c(Br-)   in fresh aerosol   [mcl/cm3(air)]
    ! c0_Im(jb)    = initial c(I-)    in fresh aerosol   [mcl/cm3(air)]
    ! c0_IO3m(jb)  = initial c(IO3-)  in fresh aerosol   [mcl/cm3(air)]
    ! c0_SO4mm(jb) = initial c(SO4--) in fresh aerosol   [mcl/cm3(air)]
    ! c0_HSO4m(jb) = initial c(HSO4-) in fresh aerosol   [mcl/cm3(air)]
    ! exchng(jb)   = 1/tau = exchange with fresh aerosol [1/s]
    ! ------------------------------------------------------------------------
    SELECT CASE (APN)
    CASE (2) ! all values for APN=2 are from MOCCA
      ! index 1 = sulfate aerosol
      xaer(1)     = 1.
      radius(1)   = 0.0882E-6
      lwc(1)      = 1.08E-12
      csalt(1)    = 3.74074074074 * lwc(1) * N_A / 1.E3 ! mol/L -> mcl/cm3(air)
      c0_NH4p(1)  = csalt(1)
      c0_HCO3m(1) = 0.
      c0_NO3m(1)  = 0.
      c0_Clm(1)   = 0.
      c0_Brm(1)   = 0.
      c0_Im(1)    = 0.
      c0_IO3m(1)  = 0.
      c0_SO4mm(1) = 0.
      c0_HSO4m(1) = csalt(1)
      exchng(1)   = 1. / (7.*OneDay) ! exchange with fresh aerosol [1/s]
      ! index 2 = sea-salt aerosol
      IF (l_ff) THEN
        xaer(2)   = 0. ! frost flower aerosol will be switched on later
        radius(2) = 1.0E-6   ! assumed, see email 23 Dec 04
        IF (degree_lat>0.) THEN
          ! Arctic:
          lwc(2)    = 5.0E-10  ! assumed for fresh frost flower "plume"
        ELSE
          ! Antarctic:
          lwc(2)    = 3.0E-10  ! assumed for fresh frost flower "plume"
        ENDIF
        csalt(2)  = 5. * lwc(2) * N_A / 1.E3 ! mol/L -> mcl/cm3(air)
      ELSE
        xaer(2)   = 1.
        radius(2) = 1.67E-6  ! radius [m]
        lwc(2)    = 3.04E-11 ! liquid water content [m3/m3]
        csalt(2)  = 5.3 * lwc(2) * N_A / 1.E3 ! mol/L -> mcl/cm3(air)
      ENDIF
      c0_NH4p(2)  = 0.
      c0_HCO3m(2) = HCO3m_rel * csalt(2)
      c0_NO3m(2)  = 0.
      c0_Clm(2)   = Clm_rel   * csalt(2)
      c0_Brm(2)   = Brm_rel   * csalt(2)
      c0_Im(2)    = Im_rel    * csalt(2)
      c0_IO3m(2)  = IO3m_rel  * csalt(2)
      c0_SO4mm(2) = SO4mm_rel * csalt(2)
      c0_HSO4m(2) = 0.
      exchng(2)   = 1. / (2.*OneDay) ! exchange with fresh aerosol [1/s]
      ! ----------------------------------------------------------------------
    CASE (3) ! 1 and 2 = same as for CASE(2); 3 = ocean surface
      ! index 1 = sulfate aerosol
      xaer(1)     = 1.
      radius(1)   = 0.0882E-6
      lwc(1)      = 1.08E-12
      csalt(1)    = 3.74074074074 * lwc(1) * N_A / 1.E3 ! mol/L -> mcl/cm3(air)
      c0_NH4p(1)  = csalt(1)
      c0_HCO3m(1) = 0.
      c0_NO3m(1)  = 0.
      c0_Clm(1)   = 0.
      c0_Brm(1)   = 0.
      c0_Im(1)    = 0.
      c0_IO3m(1)  = 0.
      c0_SO4mm(1) = 0.
      c0_HSO4m(1) = csalt(1)
      exchng(1)   = 1. / (7.*OneDay) ! exchange with fresh aerosol [1/s]
      ! index 2 = sea-salt aerosol
      xaer(2)   = 1.
      radius(2) = 1.67E-6  ! radius [m]
      lwc(2)    = 3.04E-11 ! liquid water content [m3/m3]
      csalt(2)  = 5.3 * lwc(2) * N_A / 1.E3 ! mol/L -> mcl/cm3(air)
      c0_NH4p(2)  = 0.
      c0_HCO3m(2) = HCO3m_rel * csalt(2)
      c0_NO3m(2)  = 0.
      c0_Clm(2)   = Clm_rel   * csalt(2)
      c0_Brm(2)   = Brm_rel   * csalt(2)
      c0_Im(2)    = Im_rel    * csalt(2)
      c0_IO3m(2)  = IO3m_rel  * csalt(2)
      c0_SO4mm(2) = SO4mm_rel * csalt(2)
      c0_HSO4m(2) = 0.
      exchng(2)   = 1. / (2.*OneDay) ! exchange with fresh aerosol [1/s]
      ! index 3 = ocean surface
      xaer(3)   = 1.
      radius(3) = -999. ! radius [m] negative dummy value for ocean surface
      lwc(3)    = zmix / zmbl ! liquid water content [m3/m3]
      ! the concentration of sea water is about 0.53 mol/L
      csalt(3)  = 0.53 * lwc(3) * N_A / 1.E3 ! mol/L -> mcl/cm3(air)
      c0_NH4p(3)  = 0.
      c0_HCO3m(3) = HCO3m_rel * csalt(3)
      c0_NO3m(3)  = NO3m_rel  * csalt(3)
      c0_Clm(3)   = Clm_rel   * csalt(3)
      c0_Brm(3)   = Brm_rel   * csalt(3)
      c0_Im(3)    = Im_rel    * csalt(3)
      c0_IO3m(3)  = IO3m_rel  * csalt(3)
      c0_SO4mm(3) = SO4mm_rel * csalt(3)
      c0_HSO4m(3) = 0.
      exchng(3)   = 1. / (2.*OneDay) ! exchange with deeper layers [1/s]

      ! ----------------------------------------------------------------------
    CASE (5)
      ! IMPORTANT: for testing purposes, the whole aerosol distribution
      ! has been enlarged by a scalefactor!!!
      scalefactor = 5.
      !
      ! index 1-2 = sulfate aerosol
      xaer(1:2)     = 1.
      ! derived from Ntot=2.80E+08 m-3, R=5.50E-08 m, and lg(sigma)=0.111
      ! radius [m]
      radius(1)  = 5.300E-08
      radius(2)  = 7.745E-08
      ! liquid water content [m3/m3]
      lwc(1)     = 9.412E-14 * scalefactor
      lwc(2)     = 1.397E-13 * scalefactor
      ! cations and anions
      csalt(1:2)    = 3.7 * lwc(1:2)*N_A/1.E3 ! [mol/L] conv to [mcl/cm3(air)]
      c0_NH4p(1:2)  = csalt(1:2)
      c0_HCO3m(1:2) = 0.
      c0_NO3m(1:2)  = 0.
      c0_Clm(1:2)   = 0.
      c0_Brm(1:2)   = 0.
      c0_Im(1:2)    = 0.
      c0_IO3m(1:2)  = 0.
      c0_SO4mm(1:2) = 0.
      c0_HSO4m(1:2) = csalt(1:2)
      exchng(1:2)   = 1. / (7.*OneDay) ! exchange with fresh aerosol [1/s]
      ! index 3-5 = sea-salt aerosol
      xaer(3:5)     = 1.
      ! derived from Ntot=6.60E+05 m-3, R=8.70E-07 m, and lg(sigma)=0.191
      ! radius [m]
      radius(3)  = 6.467E-07
      radius(4)  = 1.414E-06
      radius(5)  = 3.092E-06
      ! liquid water content [m3/m3]
      lwc(3)     = 3.938E-13 * scalefactor
      lwc(4)     = 3.066E-12 * scalefactor
      lwc(5)     = 1.865E-12 * scalefactor
      ! cations and anions
      csalt(3:5)    = 5.3 * lwc(3:5)*N_A/1.E3 ! [mol/L] conv to [mcl/cm3(air)]
      c0_NH4p(3:5)  = 0.
      c0_HCO3m(3:5) = HCO3m_rel * csalt(3:5)
      c0_NO3m(3:5)  = 0.
      c0_Clm(3:5)   = Clm_rel   * csalt(3:5)
      c0_Brm(3:5)   = Brm_rel   * csalt(3:5)
      c0_Im(3:5)    = Im_rel    * csalt(3:5)
      c0_IO3m(3:5)  = IO3m_rel  * csalt(3:5)
      c0_SO4mm(3:5) = SO4mm_rel * csalt(3:5)
      c0_HSO4m(3:5) = 0.
      exchng(3:5)   = 1. / (2.*OneDay) ! exchange with fresh aerosol [1/s]
      ! ----------------------------------------------------------------------
    CASE (8)
      ! IMPORTANT: for testing purposes, the whole aerosol distribution
      ! has been enlarged by a scalefactor!!!
      scalefactor = 1. ! qqq 5.
      !
      !---------------------------------
      ! define sea salt aerosol bin 1-7
      !---------------------------------
      xaer(1:7)     = 1.
      ! radius [m]
      radius(1)  = 0.230E-06
      radius(2)  = 0.445E-06
      radius(3)  = 0.900E-06
      radius(4)  = 1.650E-06
      radius(5)  = 3.550E-06
      radius(6)  = 7.500E-06
      radius(7)  = 14.50E-06
      ! liquid water content [m3/m3]
      lwc(1)     = 0.3E-12 * scalefactor
      lwc(2)     = 0.9E-12 * scalefactor
      lwc(3)     = 3.4E-12 * scalefactor
      lwc(4)     = 5.4E-12 * scalefactor
      lwc(5)     = 13.9E-12 * scalefactor
      lwc(6)     = 16.5E-12 * scalefactor
      lwc(7)     = 7.9E-12 * scalefactor
      ! cations and anions
      csalt(1:7)    = 5.3 * lwc(1:7)*N_A/1.E3 ! [mol/L] conv to [mcl/cm3(air)]
      c0_NH4p(1:7)  = 0.
      c0_HCO3m(1:7) = HCO3m_rel * csalt(1:7)
      c0_NO3m(1:7)  = 0.
      c0_Clm(1:7)   = Clm_rel   * csalt(1:7)
      c0_Brm(1:7)   = Brm_rel   * csalt(1:7)
      c0_Im(1:7)    = Im_rel    * csalt(1:7)
      c0_IO3m(1:7)  = IO3m_rel  * csalt(1:7)
      c0_SO4mm(1:7) = SO4mm_rel * csalt(1:7)
      c0_HSO4m(1:7) = 0.
      exchng(1:7)   = 1. / (2.*OneDay) ! exchange with fresh aerosol [1/s]
      ! TODO: calculate explicitly the dry deposition velocity and a typical
      ! mixing height for each aerosol size and obtain the aerosol lifetime
      ! from those
      !---------------------------------
      ! define sulfate aerosol bin 8
      !---------------------------------
      xaer(8)     = 1.
      ! radius [m]
      radius(8)   = 0.0882E-6
      ! liquid water content [m3/m3]
      lwc(8)      = 1.8E-12
      ! cations and anions
      csalt(8)    = 3.7 * lwc(8) * N_A / 1.E3 ! [mol/L], conv to [mcl/cm3(air)]
      c0_NH4p(8)  = csalt(8)
      c0_HCO3m(8) = 0.
      c0_NO3m(8)  = 0.
      c0_Clm(8)   = 0.
      c0_Brm(8)   = 0.
      c0_Im(8)    = 0.
      c0_IO3m(8)  = 0.
      c0_SO4mm(8) = 0.
      c0_HSO4m(8) = csalt(8)
      exchng(8)   = 1. / (7.*OneDay) ! exchange with fresh aerosol [1/s]
      ! ----------------------------------------------------------------------
    CASE (10)
      ! index 1-5 = sulfate aerosol
      xaer(1:5)     = 1.
      ! derived from Ntot=2.80E+08 m-3, R=5.50E-08 m, and lg(sigma)=0.111
      ! radius [m]
      radius(1)  = 3.626E-08
      radius(2)  = 5.300E-08
      radius(3)  = 7.745E-08
      radius(4)  = 1.132E-07
      radius(5)  = 1.654E-07
      ! liquid water content [m3/m3]
      lwc(1)     = 9.949E-15
      lwc(2)     = 9.412E-14
      lwc(3)     = 1.397E-13
      lwc(4)     = 3.093E-14
      lwc(5)     = 9.147E-16
      ! cations and anions
      csalt(1:5)    = 3.7 * lwc(1:5)*N_A/1.E3 ! [mol/L] conv to [mcl/cm3(air)]
      c0_NH4p(1:5)  = csalt(1:5)
      c0_HCO3m(1:5) = 0.
      c0_NO3m(1:5)  = 0.
      c0_Clm(1:5)   = 0.
      c0_Brm(1:5)   = 0.
      c0_Im(1:5)    = 0.
      c0_IO3m(1:5)  = 0.
      c0_SO4mm(1:5) = 0.
      c0_HSO4m(1:5) = csalt(1:5)
      exchng(1:5)   = 1. / (7.*OneDay) ! exchange with fresh aerosol [1/s]
      ! index 6-10 = sea-salt aerosol
      xaer(6:10)     = 1.
      ! derived from Ntot=6.60E+05 m-3, R=8.70E-07 m, and lg(sigma)=0.191
      ! radius [m]
      radius(6)  = 2.957E-07
      radius(7)  = 6.467E-07
      radius(8)  = 1.414E-06
      radius(9)  = 3.092E-06
      radius(10) = 6.762E-06
      ! liquid water content [m3/m3]
      lwc(6)     = 4.151E-15
      lwc(7)     = 3.938E-13
      lwc(8)     = 3.066E-12
      lwc(9)     = 1.865E-12
      lwc(10)    = 6.702E-14
      ! cations and anions
      csalt(6:10)    = 5.3 * lwc(6:10)*N_A/1.E3 ! [mol/L] conv to [mcl/cm3(air)]
      c0_NH4p(6:10)  = 0.
      c0_HCO3m(6:10) = HCO3m_rel * csalt(6:10)
      c0_NO3m(6:10)  = 0.
      c0_Clm(6:10)   = Clm_rel   * csalt(6:10)
      c0_Brm(6:10)   = Brm_rel   * csalt(6:10)
      c0_Im(6:10)    = Im_rel    * csalt(6:10)
      c0_IO3m(6:10)  = IO3m_rel  * csalt(6:10)
      c0_SO4mm(6:10) = SO4mm_rel * csalt(6:10)
      c0_HSO4m(6:10) = 0.
      exchng(6:10)   = 1. / (2.*OneDay) ! exchange with fresh aerosol [1/s]
      ! ----------------------------------------------------------------------
    ! CASE (12)
      ! ...
      ! ----------------------------------------------------------------------
    CASE DEFAULT
      PRINT *, 'No aerosol definition available for ', APN, ' aerosol sizes.'
      STOP
    END SELECT
    ! ------------------------------------------------------------------------

    DO jb=1, APN
      IF (ind_H2O_a(jb)   /= 0) c(ind_H2O_a(jb))   = &
        rho_H2O * lwc(jb) * 1000./M_H2O * N_A/1.E6
      IF (ind_NH4p_a(jb)  /= 0) c(ind_NH4p_a(jb))  = c0_NH4p(jb)
      IF (ind_HCO3m_a(jb) /= 0) c(ind_HCO3m_a(jb)) = c0_HCO3m(jb)
      IF (ind_NO3m_a(jb)  /= 0) c(ind_NO3m_a(jb))  = c0_NO3m(jb)
      IF (ind_Clm_a(jb)   /= 0) c(ind_Clm_a(jb))   = c0_Clm(jb)
      IF (ind_Brm_a(jb)   /= 0) c(ind_Brm_a(jb))   = c0_Brm(jb)
      IF (ind_Im_a(jb)    /= 0) c(ind_Im_a(jb))    = c0_Im(jb)
      IF (ind_IO3m_a(jb)  /= 0) c(ind_IO3m_a(jb))  = c0_IO3m(jb)
      IF (ind_SO4mm_a(jb) /= 0) c(ind_SO4mm_a(jb)) = c0_SO4mm(jb)
      IF (ind_HSO4m_a(jb) /= 0) c(ind_HSO4m_a(jb)) = c0_HSO4m(jb)
      ! Na+ is a generic cation to keep the ion balance
      IF (ind_Nap_a(jb) /= 0) THEN
        c0_Nap(jb) = 0.
        IF (ind_NH4p_a(jb)  /= 0) c0_Nap(jb) = c0_Nap(jb) - c(ind_NH4p_a(jb))
        IF (ind_HCO3m_a(jb) /= 0) c0_Nap(jb) = c0_Nap(jb) + c(ind_HCO3m_a(jb))
        IF (ind_NO3m_a(jb)  /= 0) c0_Nap(jb) = c0_Nap(jb) + c(ind_NO3m_a(jb))
        IF (ind_Clm_a(jb)   /= 0) c0_Nap(jb) = c0_Nap(jb) + c(ind_Clm_a(jb))
        IF (ind_Brm_a(jb)   /= 0) c0_Nap(jb) = c0_Nap(jb) + c(ind_Brm_a(jb))
        IF (ind_Im_a(jb)    /= 0) c0_Nap(jb) = c0_Nap(jb) + c(ind_Im_a(jb))
        IF (ind_IO3m_a(jb)  /= 0) c0_Nap(jb) = c0_Nap(jb) + c(ind_IO3m_a(jb))
        IF (ind_SO4mm_a(jb) /= 0) c0_Nap(jb) = c0_Nap(jb) + 2.*c(ind_SO4mm_a(jb))
        IF (ind_HSO4m_a(jb) /= 0) c0_Nap(jb) = c0_Nap(jb) + c(ind_HSO4m_a(jb))
        c(ind_Nap_a(jb)) = c0_Nap(jb)
      ENDIF
    ENDDO

    ! cvfac: conversion factor dm^3(aq)/mol => cm^3(air)/molecule
    cvfac(:) = 1.E3 / ( N_A * lwc(:) )

    ! ------------------------------------------------------------------------

    ! print aerosol properties:
    PRINT *, HLINE2
    PRINT *, '              aerosol properties'
    PRINT *, HLINE2
    PRINT *, 'APN         r       LWC         N         A   1 mol/L =   1 mol/L ='
    PRINT *, '          [m]   [m3/m3]    [1/m3]   [m2/m3]   [mcl/cm3]   [mol/mol]'
    PRINT *, HLINE2
    DO jb=1, APN
      WRITE(*,'(A2,I2.2,4(1PE10.2),2(1PE12.4))') ' A', &
        jb, radius(jb), lwc(jb), &
        lwc(jb)*3./(4.*pi*radius(jb)**3), &  ! number
        3.*lwc(jb)/radius(jb), &             ! surface
        1. / cvfac(jb), &                    ! [mol/L] --> [mcl/cm3]
        lwc(jb) * 1E3 * R_gas * temp / press ! [mol/L] --> [mol/mol]
    ENDDO
    PRINT *, HLINE2
    PRINT *, '              aerosol anion concentrations [mol/L]'
    PRINT *, HLINE2
    PRINT *, 'APN    HCO3-      Cl-      Br-       I-'// &
             '     IO3-    SO4--    HSO4-'
    PRINT *, HLINE2
    DO jb=1, APN
      WRITE(*,'(A2,I2.2,7(1PE9.2))') ' A', jb, &
        c0_HCO3m(jb) *1E3/(lwc(jb)*N_A), &
        c0_NO3m(jb)  *1E3/(lwc(jb)*N_A), &
        c0_Clm(jb)   *1E3/(lwc(jb)*N_A), &
        c0_Brm(jb)   *1E3/(lwc(jb)*N_A), &
        c0_Im(jb)    *1E3/(lwc(jb)*N_A), &
        c0_IO3m(jb)  *1E3/(lwc(jb)*N_A), &
        c0_SO4mm(jb) *1E3/(lwc(jb)*N_A), &
        c0_HSO4m(jb) *1E3/(lwc(jb)*N_A)
    ENDDO
    PRINT *, HLINE2
    PRINT *, '              aerosol cation concentrations [mol/L]'
    PRINT *, HLINE2
    PRINT *, 'APN     NH4+      Na+'
    PRINT *, HLINE2
    DO jb=1, APN
      WRITE(*,'(A2,I2.2,2(1PE9.2))') ' A', jb, &
        c0_NH4p(jb)  *1E3/(lwc(jb)*N_A), &
        c0_Nap(jb)   *1E3/(lwc(jb)*N_A)
    ENDDO

  END SUBROUTINE define_aerosol

  !***************************************************************************

  SUBROUTINE x0

    USE caaba_mem,         ONLY: init_scenario, degree_lat
    USE messy_main_tools,  ONLY: ucase   ! conversion to uppercase

    INTRINSIC :: TRIM

    INTEGER                     :: i, n_var, n_dim, varid_x, ct_spc
    REAL(DP)                    :: mr_x ! mixing ration of species x
    CHARACTER(LEN=STRLEN_VLONG) :: name_x, name_spc

    ! ------------------------------------------------------------------------

    ! initialize some mixing ratios
    ! values in mol/mol, cair converts to particles/cm3
    SELECT CASE (TRIM(init_scenario))
    CASE ('')
      CALL x0_simple
    CASE ('FF_ANTARCTIC')
      CALL x0_ff_antarctic
    CASE ('FF_ARCTIC')
      CALL x0_ff_arctic
    CASE ('FREE_TROP')
      CALL x0_free_trop
    CASE ('LAB')
      CALL x0_lab
    CASE ('MBL')
      CALL x0_mbl
    CASE ('MIM2')
      CALL x0_mim2
    CASE ('OOMPH')
      CALL x0_oomph
    CASE ('STRATO')
      ! choose one:
      !CALL x0_strato10
      CALL x0_strato20
      !CALL x0_meso
    CASE DEFAULT
      PRINT *, 'ERROR, init_scenario '//TRIM(init_scenario)//' is not defined'
      STOP
    END SELECT

    ! ------------------------------------------------------------------------

    !mz_hr_20061120+
    ! external chemical species' initialization
    IF (TRIM(init_spec)/="") THEN
      PRINT *, HLINE2
      PRINT *, 'External chemical initialization:'
      PRINT *, HLINE2
      CALL open_input_file(ncid_spec, init_spec) ! get ID for input file
      CALL nf(nf90_inquire(ncid_spec, n_dim, n_var)) ! no. dims, vars (=specs)
      !print *, 'mm_box: ncid_spec = ', ncid_spec
      !print *, 'mm_box: no. vars = ', n_var

      varid_x = 1
      DO WHILE (varid_x <= n_var) ! loop over ext init species
        ! get names of variables:
        CALL nf(nf90_inquire_variable(ncid_spec, varid_x, name_x))
        ! convert to uppercase for comparison:
        CALL ucase(name_x)
        !print *, 'mm_box: ext spec no. = ', varid_x,'*'
        !print *, 'mm_box: name = ', name_x,'*'
        ct_spc = 1
        DO WHILE (ct_spc <= NSPEC) ! loop over chemical species (kpp)
          name_spc = SPC_NAMES(ct_spc)
          ! convert to uppercase for comparison:
          CALL ucase(name_spc)
          !print *, 'mm_box: ct_spc = ',ct_spc
          !print *, 'mm_box: name_spc = ', name_spc,'*'
          !print *, 'mm_box: name_x   = ', name_x,'*'
          IF (TRIM(name_spc) == TRIM(name_x)) THEN
            CALL nf(nf90_get_var(ncid_spec, varid_x, mr_x))
            c(ct_spc) = mr_x * cair
            WRITE(*,'(A,A16,1PE10.2,A,1PE10.2,A)') '     initialized: ', &
              name_x, mr_x, ' mol/mol   = ', c(ct_spc), ' mcl/cm3'
            !print *, 'mm_box: mr(', ct_spc, ') = ', mr_x
            !print *, 'mm_box: c(', ct_spc, ')  = ', c(ct_spc)
            !ct_spc = ct_spc + 1
            EXIT
          ELSE
            ct_spc = ct_spc + 1
            IF (ct_spc .GT. NSPEC) THEN
              WRITE(*,'(A,A16)') ' NOT initialized: ', name_x
            ENDIF
          ENDIF
        ENDDO ! kpploop
        varid_x = varid_x + 1
      ENDDO ! extloop
      CALL close_file(ncid_spec)
    ENDIF
    !mz_hr_20061120-

    ! ------------------------------------------------------------------------

    PRINT *, HLINE2
    PRINT *, 'Initial gas-phase mixing ratios and concentrations:'
    PRINT *, HLINE2
    DO i = 1,NSPEC
      IF (c(i)>0) THEN
        WRITE(*,'(2A,1PE10.2,A,1PE10.2,A)') ' ', SPC_NAMES(i), &
          c(i)/cair, ' mol/mol   = ', c(i), ' mcl/cm3'
      ENDIF
    ENDDO

    ! ------------------------------------------------------------------------

  CONTAINS

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_simple
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     =  1.8E-06 * cair
      IF (ind_CO       /= 0) c(ind_CO)      =  70.E-09 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)     = 350.E-06 * cair
    END SUBROUTINE x0_simple

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_ff_antarctic
      IF (ind_O3       /= 0) c(ind_O3)      =  30.E-09 * cair
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      IF (ind_NO       /= 0) c(ind_NO)      =  2.0E-12 * cair
      IF (ind_NO2      /= 0) c(ind_NO2)     =  2.0E-12 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     =  1.8E-06 * cair
      IF (ind_HCHO     /= 0) c(ind_HCHO)    =  50.E-12 * cair
      IF (ind_CH3CHO   /= 0) c(ind_CH3CHO)  =  10.E-12 * cair
      IF (ind_CO       /= 0) c(ind_CO)      = 170.E-09 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)     = 350.E-06 * cair
      IF (ind_DMS      /= 0) c(ind_DMS)     =  10.E-12 * cair
      IF (ind_SO2      /= 0) c(ind_SO2)     =  30.E-12 * cair
      IF (ind_CH3I     /= 0) c(ind_CH3I)    =  2.0E-12 * cair
      IF (ind_CH3Br    /= 0) c(ind_CH3Br)   =  5.0E-12 * cair
      IF (ind_C2H4     /= 0) c(ind_C2H4)    =  10.E-12 * cair
      IF (ind_C2H2     /= 0) c(ind_C2H2)    =  10.E-12 * cair
      IF (ind_C2H6     /= 0) c(ind_C2H6)    = 300.E-12 * cair
      IF (ind_Hg       /= 0) c(ind_Hg)      = 1.68E-13 * cair
    END SUBROUTINE x0_ff_antarctic

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_ff_arctic
      IF (ind_O3       /= 0) c(ind_O3)      =  40.E-09 * cair
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      IF (ind_NO       /= 0) c(ind_NO)      =  10.E-12 * cair
      IF (ind_NO2      /= 0) c(ind_NO2)     =  10.E-12 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     =  1.8E-06 * cair
      IF (ind_HCHO     /= 0) c(ind_HCHO)    = 200.E-12 * cair
      IF (ind_CH3CHO   /= 0) c(ind_CH3CHO)  = 100.E-12 * cair
      IF (ind_CO       /= 0) c(ind_CO)      = 170.E-09 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)     = 350.E-06 * cair
      IF (ind_DMS      /= 0) c(ind_DMS)     =  10.E-12 * cair
      IF (ind_SO2      /= 0) c(ind_SO2)     = 100.E-12 * cair
      IF (ind_CH3I     /= 0) c(ind_CH3I)    =  2.0E-12 * cair
      IF (ind_CH3Br    /= 0) c(ind_CH3Br)   =  5.0E-12 * cair
      IF (ind_C2H4     /= 0) c(ind_C2H4)    =   26E-12 * cair ! ref1737
      ! see also: C2H4 = 100E-12 ! ref0351, Tab.1, 3 Apr
      IF (ind_C2H2     /= 0) c(ind_C2H2)    =  329E-12 * cair ! ref1737
      ! see also: C2H2 = 840E-12 ! ref0351, Tab.1, 3 Apr
      IF (ind_C2H6     /= 0) c(ind_C2H6)    =  2.0E-09 * cair
      IF (ind_Hg       /= 0) c(ind_Hg)      = 1.68E-13 * cair
    END SUBROUTINE x0_ff_arctic

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_free_trop
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)     = 350.E-06 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     = 1.8E-06 * cair
      IF (ind_H2       /= 0) c(ind_H2)      = 0.6E-06 * cair
    END SUBROUTINE x0_free_trop

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_lab
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     =  1.8E-06 * cair
      !qqq todo: more values from Sergej?
    END SUBROUTINE x0_lab

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_mbl
      IF (ind_H2       /= 0) c(ind_H2)      =   1.E-06 * cair
      IF (ind_O3       /= 0) c(ind_O3)      =  25.E-09 * cair
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      !IF (ind_NO      /= 0) c(ind_NO)      =  10.E-12 * cair
      IF (ind_NO2      /= 0) c(ind_NO2)     =  20.E-12 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     =  1.8E-06 * cair
      IF (ind_HCHO     /= 0) c(ind_HCHO)    = 300.E-12 * cair
      !IF (ind_CH3CHO  /= 0) c(ind_CH3CHO)  =  1.0E-10 * cair
      IF (ind_CO       /= 0) c(ind_CO)      =  70.E-09 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)     = 350.E-06 * cair
      IF (ind_DMS      /= 0) c(ind_DMS)     =  60.E-12 * cair
      IF (ind_SO2      /= 0) c(ind_SO2)     =  90.E-12 * cair
      IF (ind_CH3I     /= 0) c(ind_CH3I)    =  2.0E-12 * cair
      IF (ind_H2O2     /= 0) c(ind_H2O2)    = 600.E-12 * cair
      IF (ind_NH3      /= 0) c(ind_NH3)     = 200.E-12 * cair
      IF (ind_HNO3     /= 0) c(ind_HNO3)    =  5.0E-12 * cair
      IF (ind_HCl      /= 0) c(ind_HCl)     =  40.E-12 * cair
      IF (ind_C3H7I    /= 0) c(ind_C3H7I)   =  1.0E-12 * cair
      IF (ind_CH3OH    /= 0) c(ind_CH3OH)   = 300.E-12 * cair
      !IF (ind_C5H8    /= 0) c(ind_C5H8)    =  1.0E-09 * cair
      IF (ind_Hg       /= 0) c(ind_Hg)      = 1.68E-13 * cair

      ! examples for initializing aqueous-phase species:
      ! (qqq the index must not be greater than APN)
      ! IF (ind_NH4p_a(2)  /=0) c(ind_NH4p_a(2))  = 300.E-12 * cair
      ! 1 nmol/L DMS:
      ! IF (ind_DMS_a(3) /=0) c(ind_DMS_a(3)) = 1E-9 * lwc(3) * N_A / 1.E3 * cair

    END SUBROUTINE x0_mbl

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_mim2
      IF (ind_O3       /= 0) c(ind_O3)      =  30.E-09 * cair
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_NO2      /= 0) c(ind_NO2)     = 100.E-12 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     =  1.8E-06 * cair
      IF (ind_CO       /= 0) c(ind_CO)      =  70.E-09 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)     = 350.E-06 * cair
      IF (ind_C5H8     /= 0) c(ind_C5H8)    =  1.0E-09 * cair
    END SUBROUTINE x0_mim2

    ! ------------------------------------------------------------------------

    SUBROUTINE x0_oomph
      ! fixed species:
      IF (ind_CO2      /= 0) c(ind_CO2)     = 382.E-06 * cair
      IF (ind_N2       /= 0) c(ind_N2)      = 780.E-03 * cair
      IF (ind_O2       /= 0) c(ind_O2)      = 210.E-03 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)     = 1.75E-06 * cair ! JW

      ! def:   default as in usual mbl setup
      ! Heard: North Atlantic Campaign NAMBLEX
      ! JW:    J. Williams' guess
      ! RS:    Rolf Sander's guess
      ! Sa:    Sander's lit compilation
      ! Singh: Hanwant Singh's missions 15, 16, 18 (remote clean air)
      !        in tropical Pacific = ref0314
      ! Wa:    Warneck: Nat Atm + info (page) = ref0067
      ! mod<#> different modifications to reach steady state earlier

      ! REMOTE MARINE BACKGROUND
      PRINT *, 'OOMPH init: marine background'
      IF (ind_ACETOL   /= 0) c(ind_ACETOL)  = 205.E-12 * cair ! 3rdgen
      !IF (ind_ACETOL   /= 0) c(ind_ACETOL)  = 250.E-12 * cair ! new
      IF (ind_HYPERACET    /= 0) c(ind_HYPERACET)   =  0.74E-12 * cair ! 4Igen
      !IF (ind_HYPERACET    /= 0) c(ind_HYPERACET)   =  0.7E-12 * cair ! 4gen
      !IF (ind_CH3CHO   /= 0) c(ind_CH3CHO)  = 100.E-12 * cair ! def(off)
      IF (ind_C3H7I    /= 0) c(ind_C3H7I)   =  0.6E-12 * cair ! 4Igen
      !IF (ind_C3H7I    /= 0) c(ind_C3H7I)   =  0.7E-12 * cair ! mod5
      IF (ind_CH3CHO   /= 0) c(ind_CH3CHO)  = 100.E-12 * cair ! 4gen
      IF (ind_CH3COCH3 /= 0) c(ind_CH3COCH3)= 100.E-12 * cair ! 4gen
      IF (ind_MGLYOX /= 0) c(ind_MGLYOX)= 230.E-12 * cair ! 4Igen
      !IF (ind_MGLYOX /= 0) c(ind_MGLYOX)= 280.E-12 * cair ! 3rdgen
      !IF (ind_MGLYOX /= 0) c(ind_MGLYOX)= 340.E-12 * cair ! new
      IF (ind_CH3I     /= 0) c(ind_CH3I)    =  1.24E-12 * cair ! 4Igen
      !IF (ind_CH3I     /= 0) c(ind_CH3I)    =  1.3E-12 * cair ! mod8
      IF (ind_CH3OH    /= 0) c(ind_CH3OH)   = 500.E-12 * cair ! 4gen
      !IF (ind_CH3OH    /= 0) c(ind_CH3OH)   = 1.47E-09 * cair ! 3rdgen
      !IF (ind_CH3OH    /= 0) c(ind_CH3OH)   = 1.44E-09 * cair ! mod10
      !IF (ind_CH3OH    /= 0) c(ind_CH3OH)   = 800.E-12 * cair ! mod9+Sinha
      !IF (ind_CH3OH    /= 0) c(ind_CH3OH)   = 300.E-12 * cair ! def+JW
      !IF (ind_CH3OOH   /= 0) c(ind_CH3OOH)  = 0.E-12 * cair ! 4Igentest
      IF (ind_CH3OOH   /= 0) c(ind_CH3OOH)  = 590.E-12 * cair ! 3rdgen
      !IF (ind_CH3OOH   /= 0) c(ind_CH3OOH)  = 530.E-12 * cair ! mod8
      !IF (ind_CH3CO2H  /= 0) c(ind_CH3CO2H) = 0.E-09 * cair ! 4Igentest
      IF (ind_CH3CO2H  /= 0) c(ind_CH3CO2H) = 1.34E-09 * cair ! 4Igen
      !IF (ind_CH3CO2H  /= 0) c(ind_CH3CO2H) = 1.54E-09 * cair ! 4gen
      !IF (ind_CH3CO2H  /= 0) c(ind_CH3CO2H) = 1.48E-09 * cair ! 3rdgen
      !IF (ind_CH3CO2H  /= 0) c(ind_CH3CO2H) = 1.50E-09 * cair ! new
      IF (ind_CO       /= 0) c(ind_CO)      =  35.E-09 * cair ! 4Igen
      !IF (ind_CO       /= 0) c(ind_CO)      = 115.E-09 * cair ! 3rdgen, 4gen
      !IF (ind_CO       /= 0) c(ind_CO)      = 113.E-09 * cair ! mod6
      IF (ind_DMS      /= 0) c(ind_DMS)     = 200.E-12 * cair ! 4gen
      !IF (ind_DMS      /= 0) c(ind_DMS)     = 130.E-12 * cair ! 3rdgen
      !IF (ind_DMS      /= 0) c(ind_DMS)     =  30.E-12 * cair ! mod8
      IF (ind_DMSO     /= 0) c(ind_DMSO)    =  18.E-12 * cair ! 4Igen
      IF (ind_H2       /= 0) c(ind_H2)      =   1.E-06 * cair ! def
      IF (ind_H2O2     /= 0) c(ind_H2O2)    = 220.E-12 * cair ! 4Igen
      !IF (ind_H2O2     /= 0) c(ind_H2O2)    = 260.E-12 * cair ! mod6
      !IF (ind_H2O2     /= 0) c(ind_H2O2)    = 552.E-12 * cair ! Singh
      IF (ind_HCHO     /= 0) c(ind_HCHO)    = 220.E-12 * cair ! 4Igen
      !IF (ind_HCHO     /= 0) c(ind_HCHO)    = 200.E-12 * cair ! 3rdgen
      !IF (ind_HCHO     /= 0) c(ind_HCHO)    = 180.E-12 * cair ! mod3
      !IF (ind_HCHO     /= 0) c(ind_HCHO)    = 200.E-12 * cair ! Sa
      IF (ind_HCOOH    /= 0) c(ind_HCOOH)   =   7.E-12 * cair ! 4Igen
      !IF (ind_HCOOH    /= 0) c(ind_HCOOH)   =   8.E-12 * cair ! 3rdgen
      !IF (ind_HCOOH    /= 0) c(ind_HCOOH)   =  11.E-12 * cair ! mod5
      IF (ind_HCl      /= 0) c(ind_HCl)     =  30.E-12 * cair ! 4gen
      !IF (ind_HCl      /= 0) c(ind_HCl)     =  40.E-12 * cair ! def,3gen
      !IF (ind_HCl      /= 0) c(ind_HCl)     = 100.E-12 * cair ! Sa
      IF (ind_HNO3     /= 0) c(ind_HNO3)    = 0.15E-12 * cair ! mod3
      IF (ind_ISON     /= 0) c(ind_ISON)    =  2.4E-12 * cair ! 4Igen
      !IF (ind_ISON     /= 0) c(ind_ISON)    =  3.4E-12 * cair ! 4gen
      !IF (ind_ISON     /= 0) c(ind_ISON)    =  2.2E-12 * cair ! 3rdgen
      !IF (ind_ISON     /= 0) c(ind_ISON)    =  2.8E-12 * cair ! new
      IF (ind_ISOOH    /= 0) c(ind_ISOOH)   =  30.E-12 * cair ! 4Igen
      !IF (ind_ISOOH    /= 0) c(ind_ISOOH)   =  40.E-12 * cair ! new
      !IF (ind_ISOOH    /= 0) c(ind_ISOOH)   =  40.E-12 * cair ! new
      IF (ind_C5H8     /= 0) c(ind_C5H8)    =  50.E-12 * cair ! new
      IF (ind_MVK      /= 0) c(ind_MVK)     = 130.E-12 * cair ! 4Igen
      !IF (ind_MVK      /= 0) c(ind_MVK)     = 160.E-12 * cair ! 3rdgen
      !IF (ind_MVK      /= 0) c(ind_MVK)     = 200.E-12 * cair ! new
      !IF (ind_MVKOOH   /= 0) c(ind_MVKOOH)  = 0.E-12 * cair ! 4Igentest
      IF (ind_MVKOOH   /= 0) c(ind_MVKOOH)  = 180.E-12 * cair ! 4Igen
      !IF (ind_MVKOOH   /= 0) c(ind_MVKOOH)  = 260.E-12 * cair ! 3rdgen
      !IF (ind_MVKOOH   /= 0) c(ind_MVKOOH)  = 295.E-12 * cair ! new
      IF (ind_NACA     /= 0) c(ind_NACA)    =  1.6E-12 * cair ! 4Igen
      !IF (ind_NACA     /= 0) c(ind_NACA)    =  2.1E-12 * cair ! 4gen
      !IF (ind_NACA     /= 0) c(ind_NACA)    = 1.35E-12 * cair ! 3rdgen
      !IF (ind_NACA     /= 0) c(ind_NACA)    = 1.65E-12 * cair ! new
      IF (ind_NH3      /= 0) c(ind_NH3)     = 170.E-12 * cair ! 4Igen
      !IF (ind_NH3      /= 0) c(ind_NH3)     = 200.E-12 * cair ! 4gen
      !IF (ind_NH3      /= 0) c(ind_NH3)     = 150.E-12 * cair ! 3rdgen
      !IF (ind_NH3      /= 0) c(ind_NH3)     = 250.E-12 * cair ! mod8
      !IF (ind_NO       /= 0) c(ind_NO)      =  10.E-12 * cair ! def(off)
      !IF (ind_NO       /= 0) c(ind_NO)      =  0.3E-12 * cair ! JW (<<Singh)
      !IF (ind_NO2      /= 0) c(ind_NO2)     = 1.0E-12 * cair ! JW (<<Singh)
      IF (ind_NO2      /= 0) c(ind_NO2)     =  2.E-12 * cair ! 4gen
      !IF (ind_NO2      /= 0) c(ind_NO2)     =  1.6E-12 * cair ! mod3
      IF (ind_O3       /= 0) c(ind_O3)      = 10.4E-09 * cair ! 4Igen
      !IF (ind_O3       /= 0) c(ind_O3)      = 10.0E-09 * cair ! 3rdgen
      !IF (ind_O3       /= 0) c(ind_O3)      = 10.6E-09 * cair ! mod6
      IF (ind_CH3CO3H      /= 0) c(ind_CH3CO3H)     = 300.E-12 * cair ! 4Igen
      !IF (ind_CH3CO3H      /= 0) c(ind_CH3CO3H)     = 460.E-12 * cair ! 4gen
      !IF (ind_CH3CO3H      /= 0) c(ind_CH3CO3H)     = 390.E-12 * cair ! 3rdgen
      !IF (ind_CH3CO3H      /= 0) c(ind_CH3CO3H)     = 420.E-12 * cair ! new
      IF (ind_PAN      /= 0) c(ind_PAN)     =   2.E-12 * cair ! 4gen
      !IF (ind_PAN      /= 0) c(ind_PAN)     =   1.E-12 * cair ! new
      IF (ind_SO2      /= 0) c(ind_SO2)     = 135.E-12 * cair ! 4Igen
      !IF (ind_SO2      /= 0) c(ind_SO2)     = 130.E-12 * cair ! 3rdgen
      !IF (ind_SO2      /= 0) c(ind_SO2)     =  35.E-12 * cair ! mod8

      ! NOT TAKEN INTO ACCOUNT EVEN THOUGH PRESENT:
      !IF (ind_C2H2     /= 0) c(ind_C2H2)     = 28.7E-12 * cair ! Singh
      !IF (ind_C2H4     /= 0) c(ind_C2H4)     =  21.E-12 * cair ! Singh
      !IF (ind_C2H6     /= 0) c(ind_C2H6)     = 28.7E-12 * cair ! Singh
      !IF (ind_C3H6     /= 0) c(ind_C3H6)     = 11.4E-12 * cair ! Singh
      !IF (ind_C3H8     /= 0) c(ind_C3H8)     =  16.E-12 * cair ! Singh
      !IF (ind_CH3COCH3 /= 0) c(ind_CH3COCH3) = 300.E-12 * cair ! JW
      !IF (ind_PAN      /= 0) c(ind_PAN)      =   2.E-12 * cair ! Singh
    END SUBROUTINE x0_oomph

    ! ------------------------------------------------------------------------

    ! mz_ab_20091111+
    SUBROUTINE x0_strato20

      ! stratosphere, 20 hPa
      ! from scout02 (ProSECCO simulation)

      IF (ind_H        /= 0) c(ind_H)      =   1.E-12 * cair
      IF (ind_OH       /= 0) c(ind_OH)     =   1.E-16 * cair
      IF (ind_HO2      /= 0) c(ind_HO2)    =   1.E-15 * cair
      IF (ind_N        /= 0) c(ind_N)      =   1.E-12 * cair
      IF (ind_NO3      /= 0) c(ind_NO3)    =   1.E-12 * cair
      IF (ind_N2O5     /= 0) c(ind_N2O5)   =   1.E-10 * cair
      IF (ind_HNO4     /= 0) c(ind_HNO4)   =   1.E-10 * cair
      IF (ind_CL       /= 0) c(ind_CL)     =   1.E-21 * cair
      IF (ind_CLO      /= 0) c(ind_CLO)    =   1.E-15 * cair
      IF (ind_HOCl     /= 0) c(ind_HOCl)   =  40.E-12 * cair
      IF (ind_CL2O2    /= 0) c(ind_CL2O2)  =   1.E-13 * cair
      IF (ind_CL2      /= 0) c(ind_CL2)    =   1.E-13 * cair
      IF (ind_CH3O2    /= 0) c(ind_CH3O2)  =   1.E-12 * cair
      IF (ind_N2O      /= 0) c(ind_N2O)    =  1.3E-07 * cair
      IF (ind_CO       /= 0) c(ind_CO)     =  1.4E-08 * cair
      IF (ind_CH3OOH   /= 0) c(ind_CH3OOH) =  12.E-12 * cair
      IF (ind_ClNO3    /= 0) c(ind_ClNO3)  =   6.E-10 * cair
      IF (ind_CFCl3    /= 0) c(ind_CFCl3)  =  1.4E-11 * cair
      IF (ind_CF2Cl2   /= 0) c(ind_CF2Cl2) =   1.E-12 * cair
      IF (ind_CH3CL    /= 0) c(ind_CH3CL)  =   1.E-12 * cair
      IF (ind_CCL4     /= 0) c(ind_CCL4)   =   1.E-12 * cair
      IF (ind_CH3CCL3  /= 0) c(ind_CH3CCL3)=   1.E-12 * cair
      IF (ind_HNO3     /= 0) c(ind_HNO3)   =   5.E-09 * cair
      IF (ind_H2O      /= 0) c(ind_H2O)    =   1.E-12 * cair
      IF (ind_O3P      /= 0) c(ind_O3P)    =   9.E-34 * cair
      IF (ind_O1D      /= 0) c(ind_O1D)    =   1.E-16 * cair
      IF (ind_H2       /= 0) c(ind_H2)     =   5.E-07 * cair
      IF (ind_O3       /= 0) c(ind_O3)     =   4.E-06 * cair
      IF (ind_NO       /= 0) c(ind_NO)     =   1.E-24 * cair
      IF (ind_NO2      /= 0) c(ind_NO2)    =   1.E-09 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)    =  1.8E-06 * cair
      IF (ind_HCHO     /= 0) c(ind_HCHO)   =   7.E-11 * cair
      IF (ind_CO       /= 0) c(ind_CO)     =  70.E-09 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)    = 350.E-06 * cair
      IF (ind_H2O2     /= 0) c(ind_H2O2)   = 450.E-12 * cair
      IF (ind_HCl      /= 0) c(ind_HCl)    = 400.E-12 * cair
      ! additional for mecca
      IF (ind_SO2      /= 0) c(ind_SO2)    =   0. * cair
      IF (ind_O2       /= 0) c(ind_O2)     = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)     = 780.E-03 * cair

    END SUBROUTINE x0_strato20
    ! mz_ab_20091111-

    ! ------------------------------------------------------------------------

    ! mz_ab_20091111+
    SUBROUTINE x0_strato10

      ! stratosphere, 10 hPa
      IF (ind_H        /= 0) c(ind_H)      =   1.E-16 * cair
      IF (ind_OH       /= 0) c(ind_OH)     =   1.E-16 * cair
      IF (ind_HO2      /= 0) c(ind_HO2)    =   1.E-15 * cair
      IF (ind_N        /= 0) c(ind_N)      =   1.E-12 * cair
      IF (ind_NO3      /= 0) c(ind_NO3)    =   1.E-12 * cair
      IF (ind_N2O5     /= 0) c(ind_N2O5)   =   1.E-10 * cair
      IF (ind_HNO4     /= 0) c(ind_HNO4)   =   1.5E-10 * cair
      IF (ind_CL       /= 0) c(ind_CL)     =   1.E-21 * cair
      IF (ind_CLO      /= 0) c(ind_CLO)    =   1.E-15 * cair
      IF (ind_HOCl     /= 0) c(ind_HOCl)   =  40.E-12 * cair
      IF (ind_CL2O2    /= 0) c(ind_CL2O2)  =   1.E-13 * cair
      IF (ind_CL2      /= 0) c(ind_CL2)    =   1.E-13 * cair
      IF (ind_CH3O2    /= 0) c(ind_CH3O2)  =   1.E-12 * cair
      IF (ind_N2O      /= 0) c(ind_N2O)    =  1.3E-07 * cair
      IF (ind_CO       /= 0) c(ind_CO)     =  1.4E-08 * cair
      IF (ind_CH3OOH   /= 0) c(ind_CH3OOH) =  12.E-12 * cair
      IF (ind_ClNO3    /= 0) c(ind_ClNO3)  =   6.E-10 * cair
      IF (ind_CFCl3    /= 0) c(ind_CFCl3)  =  1.4E-11 * cair
      IF (ind_CF2Cl2   /= 0) c(ind_CF2Cl2) =   1.E-12 * cair
      IF (ind_CH3CL    /= 0) c(ind_CH3CL)  =   1.E-12 * cair
      IF (ind_CCL4     /= 0) c(ind_CCL4)   =   1.E-12 * cair
      IF (ind_CH3CCL3  /= 0) c(ind_CH3CCL3)=   1.E-12 * cair
      IF (ind_HNO3     /= 0) c(ind_HNO3)   =   5.E-09 * cair
      IF (ind_H2O      /= 0) c(ind_H2O)    = 4.251E-06 * cair
      IF (ind_O3P      /= 0) c(ind_O3P)    =   9.E-34 * cair
      IF (ind_O1D      /= 0) c(ind_O1D)    =   1.E-16 * cair
      IF (ind_H2       /= 0) c(ind_H2)     =   5.E-07 * cair
      IF (ind_O3       /= 0) c(ind_O3)     =   8.E-06 * cair
      IF (ind_NO       /= 0) c(ind_NO)     =   1.E-24 * cair
      IF (ind_NO2      /= 0) c(ind_NO2)    =   1.E-09 * cair
      IF (ind_CH4      /= 0) c(ind_CH4)    =  1.8E-06 * cair
      IF (ind_HCHO     /= 0) c(ind_HCHO)   =   7.E-11 * cair
      IF (ind_CO       /= 0) c(ind_CO)     =  70.E-09 * cair
      IF (ind_CO2      /= 0) c(ind_CO2)    = 350.E-06 * cair
      IF (ind_H2O2     /= 0) c(ind_H2O2)   = 180.E-12 * cair
      IF (ind_HCl      /= 0) c(ind_HCl)    = 400.E-12 * cair
      ! additional for mecca
      IF (ind_SO2      /= 0) c(ind_SO2)    =   0. * cair
      IF (ind_O2       /= 0) c(ind_O2)     = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)     = 780.E-03 * cair

    END SUBROUTINE x0_strato10
    ! mz_ab_20091111-

    ! ------------------------------------------------------------------------

    ! mz_ab_20091111+
    SUBROUTINE x0_meso

      ! mesosphere, 0.01 hPa
      IF (ind_H        /= 0) c(ind_H)      = 1.876E-07* cair
      IF (ind_OH       /= 0) c(ind_OH)     = 1.240E-08* cair
      IF (ind_HO2      /= 0) c(ind_HO2)    = 5.012E-09* cair
      IF (ind_N        /= 0) c(ind_N)      = 2.114E-10* cair
      IF (ind_NO3      /= 0) c(ind_NO3)    = 3.083E-21* cair
      IF (ind_N2O5     /= 0) c(ind_N2O5)   = 9.072E-27* cair
      IF (ind_HNO4     /= 0) c(ind_HNO4)   = 4.754E-18* cair
      IF (ind_CL       /= 0) c(ind_CL)     = 8.001E-11* cair
      IF (ind_CLO      /= 0) c(ind_CLO)    = 1.564E-13* cair
      IF (ind_HOCl     /= 0) c(ind_HOCl)   = 7.015E-15* cair
      IF (ind_CL2O2    /= 0) c(ind_CL2O2)  = 1.558E-25* cair
      IF (ind_CL2      /= 0) c(ind_CL2)    = 1.E-13* cair ! ????
      IF (ind_CH3O2    /= 0) c(ind_CH3O2)  = 1.E-12* cair! ????
      IF (ind_N2O      /= 0) c(ind_N2O)    = 7.077E-11* cair
      IF (ind_H2O2     /= 0) c(ind_H2O2)   = 1.685E-12* cair
      IF (ind_CO       /= 0) c(ind_CO)     = 9.862E-07* cair
      IF (ind_CH3OOH   /= 0) c(ind_CH3OOH) = 3.558E-13* cair
      IF (ind_ClNO3    /= 0) c(ind_ClNO3)  = 1.460E-22* cair
      IF (ind_CFCl3    /= 0) c(ind_CFCl3)  = 2.854E-38* cair
      IF (ind_CF2Cl2   /= 0) c(ind_CF2Cl2) = 2.776E-17* cair
      IF (ind_CH3CL    /= 0) c(ind_CH3CL)  = 2.212E-14* cair
      IF (ind_CCL4     /= 0) c(ind_CCL4)   = 5.605E-45* cair
      IF (ind_CH3CCL3  /= 0) c(ind_CH3CCL3)= 1.401E-44* cair
      IF (ind_HNO3     /= 0) c(ind_HNO3)   = 2.084E-13* cair
      IF (ind_H2O      /= 0) c(ind_H2O)    = 4.567E-06* cair
      IF (ind_O3P      /= 0) c(ind_O3P)    = 5.350E-06* cair
      IF (ind_O1D      /= 0) c(ind_O1D)    = 3.143E-14* cair
      IF (ind_H2       /= 0) c(ind_H2)     = 1.310E-06* cair
      IF (ind_O3       /= 0) c(ind_O3)     = 7.823E-08* cair
      IF (ind_NO       /= 0) c(ind_NO)     = 2.454E-09* cair
      IF (ind_NO2      /= 0) c(ind_NO2)    = 1.685E-12* cair
      IF (ind_CH4      /= 0) c(ind_CH4)    = 1.113E-07* cair
      IF (ind_HCHO     /= 0) c(ind_HCHO)   = 1.417E-12* cair
      IF (ind_CO       /= 0) c(ind_CO)     = 9.862E-07* cair
      IF (ind_CO2      /= 0) c(ind_CO2)    = 3.641E-04* cair
      IF (ind_H2O2     /= 0) c(ind_H2O2)   = 1.169E-10* cair
      IF (ind_HCl      /= 0) c(ind_HCl)    = 3.342E-09* cair
      ! additional for mecca
      IF (ind_SO2      /= 0) c(ind_SO2)    =   0. * cair
      IF (ind_O2       /= 0) c(ind_O2)     = 210.E-03 * cair
      IF (ind_N2       /= 0) c(ind_N2)     = 780.E-03 * cair

    END SUBROUTINE x0_meso
    ! mz_ab_20091111-

    ! ------------------------------------------------------------------------

  END SUBROUTINE x0

  !***************************************************************************

  SUBROUTINE mecca_physc

    USE messy_mecca_kpp,         ONLY: REQ_MCFCT
    USE messy_mecca,             ONLY: steady_state_reached
    USE messy_mecca_aero,        ONLY: mecca_aero_calc_k_ex, &
                                       mecca_aero_calc_k_ex_ocean
    USE messy_main_tools,        ONLY: psatf, psat
    USE messy_readj,             ONLY: jx_readj => jx
    USE messy_sappho,            ONLY: jx_sappho => jx
    USE messy_jval,              ONLY: jval_gp
    USE caaba_mem,               ONLY: l_skipkpp, jval_clev, zmix


    LOGICAL :: l_het(APN) =.TRUE.
    REAL(DP), DIMENSION(NBL,NSPEC) :: cbl
    INTEGER :: ip, status

    IF (l_ff) THEN
      ! at day 4 switch frost flowers on:
      IF (model_time >= (startday+4.)*OneDay) THEN
        xaer(2) = 1.
      ENDIF
    ENDIF

    ! see mecca_init for explanation
    !mz_hr_20080226+
    ! must be consistent with mecca_init!
    ! WMO relhum + function for psat
    !c(ind_H2O) = cair * relhum * psatf(temp) / &
    !  (press + (relhum-1.) * psatf(temp))
    ! traditional relhum + series for psat
    !c(ind_H2O) = cair * relhum * psat(temp) / press
    !mz_hr_20080226-

    CALL fill_temp(status, SPREAD(temp,1,NBL))
    CALL fill_cair(status, SPREAD(cair,1,NBL))
    CALL fill_press(status, SPREAD(press,1,NBL)) ! mz_pj_20080716
    ! qqq: is it necessary to call fill_mcexp in the time loop, or
    ! can that be done once during the initialization?
    IF (REQ_MCFCT) THEN
      CALL fill_mcexp(status, SPREAD(mcexp,1,NBL))
    ENDIF

    ! dummy values:
    dummy_khet_St(:) = 0.
    dummy_khet_Tr(:) = 0.
    CALL fill_khet_Tr(status, SPREAD(dummy_khet_Tr,1,NBL))
    CALL fill_khet_St(status, SPREAD(dummy_khet_St,1,NBL))

    SELECT CASE (TRIM(photrat_channel))
    CASE('jval')
      DO ip=1, IP_MAX
        jx(ip) = jval_gp(ip)%ptr(1,jval_clev,1)
      ENDDO
    CASE('readj')
      DO ip=1, IP_MAX
        jx(ip) = jx_readj(ip)
      ENDDO
    CASE('sappho')
      DO ip=1, IP_MAX
        jx(ip) = jx_sappho(ip)
      ENDDO
    ENDSELECT

    ! transfer of jx in mecca to jx in kpp
    CALL fill_jx(status, SPREAD(jx,1,NBL))

    IF (l_aero) THEN
      CALL aerosol_exchng ! exchange with fresh aerosol
      ! first, calculate exchange coefficients for aerosols:
      CALL mecca_aero_calc_k_ex( &
        radius, temp, press, l_het, xaer, lwc, c, &         ! in
        k_exf, k_exb, k_exf_N2O5, k_exf_ClNO3, k_exf_BrNO3) ! out
      ! next, calculate exchange coefficients for ocean surface:
      CALL mecca_aero_calc_k_ex_ocean(xaer, radius, temp, zmix, k_exf, k_exb)
      CALL fill_lwc(status, SPREAD(lwc,1,NBL))
      CALL fill_cvfac(status, SPREAD(cvfac,1,NBL))
      CALL fill_xaer(status, SPREAD(xaer,1,NBL))
      CALL fill_k_exf(status, SPREAD(k_exf,1,NBL))
      CALL fill_k_exb(status, SPREAD(k_exb,1,NBL))
      CALL fill_k_exf_N2O5(status, SPREAD(k_exf_N2O5,1,NBL))
      CALL fill_k_exf_ClNO3(status, SPREAD(k_exf_ClNO3,1,NBL))
      CALL fill_k_exf_BrNO3(status, SPREAD(k_exf_BrNO3,1,NBL))
    ENDIF

    CALL check_range('before kpp:',c(:))
    IF (.NOT.l_skipkpp) THEN
      c(:) = MAX(c(:),0._DP) ! set negative values to zero
      cbl = SPREAD(c,1,NBL) ! add one dummy dimension
      CALL kpp_integrate(time_step_len,cbl)  ! main kpp call
      c = cbl(1,:)          ! remove the dummy dimension
      CALL check_range('after kpp: ',c(:))
    ENDIF

    IF (l_dbl) CALL mecca_dbl_postprocess
    ! 1 for the # of steps to take:
    IF (l_tag) CALL mecca_tag_process(time_step_len, C, press, cair, temp)

    IF (l_steady_state_stop) THEN
      IF (steady_state_reached(c(:))) THEN
        PRINT *, 'steady-state reached at day = ', model_time/OneDay
        ! change model_end so that model run will stop next time step:
        model_end = model_time + time_step_len
      ENDIF
    ENDIF

  CONTAINS

    !-------------------------------------------------------------------------

    SUBROUTINE aerosol_exchng

      ! Aerosol exchange is currently calculated with Euler forward.
      ! This should be changed if numerical problems occur.

      USE messy_main_tools, ONLY: str

      IMPLICIT NONE

      REAL(DP) :: factor
      INTEGER :: i, jb

      DO jb = 1,APN

        IF (l_ff) THEN
          ! (no additional cations and anions from fresh particles
          ! for frostflower model setup)
        ELSE
          ! additional cations and anions from fresh particles:
          factor = time_step_len * exchng(jb)
          ! adjustment for operator splitting:
          factor = factor / (1.-factor)
          IF (ind_NH4p_a(jb)  /=0) &
            c(ind_NH4p_a(jb))  = c(ind_NH4p_a(jb))  + factor * c0_NH4p(jb)
          IF (ind_Nap_a(jb)   /=0) &
            c(IND_Nap_a(jb))   = c(IND_Nap_a(jb))   + factor * c0_Nap(jb)
          IF (ind_HCO3m_a(jb) /=0) &
            c(ind_HCO3m_a(jb)) = c(ind_HCO3m_a(jb)) + factor * c0_HCO3m(jb)
          IF (ind_NO3m_a(jb)  /=0) &
            c(ind_NO3m_a(jb))  = c(ind_NO3m_a(jb))  + factor * c0_NO3m(jb)
          IF (ind_Clm_a(jb)   /=0) &
            c(ind_Clm_a(jb))   = c(ind_Clm_a(jb))   + factor * c0_Clm(jb)
          IF (ind_Brm_a(jb)   /=0) &
            c(ind_Brm_a(jb))   = c(ind_Brm_a(jb))   + factor * c0_Brm(jb)
          IF (ind_Im_a(jb)    /=0) &
            c(ind_Im_a(jb))    = c(ind_Im_a(jb))    + factor * c0_Im(jb)
          IF (ind_IO3m_a(jb)  /=0) &
            c(ind_IO3m_a(jb))  = c(ind_IO3m_a(jb))  + factor * c0_IO3m(jb)
          IF (ind_SO4mm_a(jb) /=0) &
            c(ind_SO4mm_a(jb)) = c(ind_SO4mm_a(jb)) + factor * c0_SO4mm(jb)
          IF (ind_HSO4m_a(jb) /=0) &
            c(ind_HSO4m_a(jb)) = c(ind_HSO4m_a(jb)) + factor * c0_HSO4m(jb)
        ENDIF

        ! loss of cations and anions via particle sedimentation
        ! only if xaer=1, otherwise factor=1
        factor = 1. - xaer(jb) * time_step_len * exchng(jb)
        DO i = 1,NSPEC
          ! all species whose names contain '_a##' are lost via
          ! particle sedimentation (except for RR*)
          IF (INDEX(SPC_NAMES(i),'_a'//TRIM(str(jb,'(I2.2)'))) /= 0) THEN
            IF (INDEX(SPC_NAMES(i),'RR') /= 1) THEN
              !PRINT *,'sedi of ', TRIM(SPC_NAMES(i)), ' from bin ', jb
              c(i) = c(i) * factor
            ELSE
              !PRINT *,'no sedi of ', TRIM(SPC_NAMES(i)), ' from bin ', jb
            ENDIF
          ENDIF
        ENDDO

        IF (l_ff) THEN
          ! lwc decreases due to particle loss:
          lwc(jb) = lwc(jb) * factor
          ! update cvfac with new LWC:
          cvfac(jb) = 1.E3 / ( N_A * lwc(jb) )
        ENDIF

        ! The aqueous-phase concentration of water may have changed in
        ! the sedimentation code above. To get the correct value, it is
        ! determined from the current LWC:
        IF (ind_H2O_a(jb) /= 0) c(ind_H2O_a(jb)) = &
          rho_H2O * lwc(jb) * 1000./M_H2O * N_A/1.E6

      ENDDO

    END SUBROUTINE aerosol_exchng

    !-------------------------------------------------------------------------

    SUBROUTINE check_range(infostring,conc)

      ! print a warning if a concentration is not in the correct range

      CHARACTER(LEN=*), INTENT(IN) :: infostring
      REAL(DP),         INTENT(IN) :: conc(:) ! tracer concentration
      INTEGER :: jt

      INTRINSIC :: SIZE

      tracer_loop: DO jt=1,SIZE(conc)
        IF (SPC_NAMES(jt)(1:2) == "RR") CYCLE ! no checks for reaction rates
        IF (INDEX(SPC_NAMES(jt),"_a03")/=0) CYCLE ! no checks for ocean conc
        wrong_conc: IF ((conc(jt)<0._DP).OR.(conc(jt)>cair)) THEN
          WRITE(*,'(2A,F10.0,A,1PG12.3E3,2A)') infostring, &
            ' time =', model_time, &
            ', c =', conc(jt), ' mcl/cm3 for ', TRIM(SPC_NAMES(jt))
        ENDIF wrong_conc
      ENDDO tracer_loop

    END SUBROUTINE check_range

    !-------------------------------------------------------------------------

  END SUBROUTINE mecca_physc

  !***************************************************************************

  SUBROUTINE mecca_result

    IMPLICIT NONE
    REAL(DP), SAVE, DIMENSION(NSPEC) :: c_old = 0.
    INTEGER :: i

    IF (l_aero) CALL write_output_file(ncid_aero, model_time, lwc)

    output = c/cair
    DO i = 1,NSPEC
      IF (INDEX(SPC_NAMES(i),'RR') == 1) THEN
        ! for the reaction rates RR*, divide the difference to previous
        ! value by the time step length:
        output(i) = (c(i)-c_old(i)) / (cair*time_step_len)
      ENDIF
    ENDDO
    c_old = c
    CALL write_output_file(ncid_tracer, model_time, output)

    IF (l_tag) CALL mecca_tag_result(model_time)
    IF (l_dbl) CALL mecca_dbl_result(model_time)

  END SUBROUTINE mecca_result

  !***************************************************************************

  SUBROUTINE mecca_finish

    USE messy_main_tools, ONLY: find_next_free_unit
    IMPLICIT NONE
    INTEGER :: ncid_c_end, ncid_k_end, unit, i
    CHARACTER(LEN=20), PARAMETER :: k_unit(NREACT) = '(cm^3)^(1-N)/s'

    ! write final values of c (c_end) to nc file:
    CALL open_output_file(ncid_c_end, 'caaba_mecca_c_end', &
      SPC_NAMES, c_unit)
    CALL write_output_file(ncid_c_end, model_time, output)
    CALL close_file(ncid_c_end)

    ! write final values of rconst (k_end) to nc file:
    CALL open_output_file(ncid_k_end, 'caaba_mecca_k_end', &
      EQN_TAGS, k_unit)
    CALL write_output_file(ncid_k_end, model_time, rconst)
    CALL close_file(ncid_k_end)
    
    IF (REQ_MCFCT) THEN
      ! create ferret *.jnl files:
      unit = find_next_free_unit(100,200)
      ! plot histogram of all rate coefficients:
      OPEN(unit, FILE='_histogram_k.jnl', status='UNKNOWN')
      DO i = 1,NREACT
        WRITE (unit,*) 'GO frequency_histogram_rs ', TRIM(EQN_TAGS(i))
      ENDDO
      WRITE (unit,*) 'GO newpage'
      CLOSE(unit)
      ! make plots for all species:
      OPEN(unit, FILE='_scatterplot1.jnl', status='UNKNOWN')
      DO i = 1,NSPEC
        WRITE (unit,*) 'GO _scatterplot2 ', TRIM(SPC_NAMES(i))
      ENDDO
      CLOSE(unit)
      ! plot against all rate coefficients:
      OPEN(unit, FILE='_scatterplot2.jnl', status='UNKNOWN')
      WRITE (unit,*) 'GO frequency_histogram_rs ($1)'
      DO i = 1,NREACT
        WRITE (unit,*) 'GO scatterplot_mc ($1) ', TRIM(EQN_TAGS(i))
      ENDDO
      WRITE (unit,*) 'GO newpage'
      CLOSE(unit)
    ENDIF

    IF (l_aero) CALL close_file(ncid_aero)

    CALL close_file(ncid_tracer)

    IF (l_tag) CALL mecca_tag_finish
    IF (l_dbl) CALL mecca_dbl_finish

    DEALLOCATE(c0_HCO3m, c0_NO3m, c0_Clm, c0_Brm, c0_Im, c0_IO3m, &
      c0_SO4mm, c0_HSO4m)
    DEALLOCATE(xaer, radius, lwc, csalt, c0_NH4p, c0_Nap, exchng)

  END SUBROUTINE mecca_finish

  !***************************************************************************

END MODULE messy_mecca_box

!*****************************************************************************
