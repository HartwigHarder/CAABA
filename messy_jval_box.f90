!*****************************************************************************
!                Time-stamp: <2009-01-08 10:03:41 sander>
!*****************************************************************************

! JVAL = calculation of J-VALues (photolysis rate coefficients)

! Author:
! Rolf Sander,     MPICH, Sep 2007

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

MODULE messy_jval_box

  USE messy_main_constants_mem, ONLY: DP
  USE caaba_io,  ONLY: open_output_file, write_output_file, &
                       close_file
  USE messy_cmn_photol_mem      ! IP_MAX, ip_*, jname
  USE messy_jval ! ONLY: jval_gp, jname, ip_MAX, jval_read_nml_ctrl,
  !       lookup, lookup_io,
  !       aerosol_data, jvalues, lp

  IMPLICIT NONE

  INTEGER :: ncid_jval

  INTEGER, PARAMETER :: nsza =  1
  INTEGER, PARAMETER :: nlev = 19 ! number of levels
  INTEGER, PARAMETER :: jrow =  1 ! must be jrow=1 for box model

  PRIVATE

  PUBLIC :: jval_init   ! initialize J-values
  PUBLIC :: jval_physc  ! calculate J values
  PUBLIC :: jval_result
  PUBLIC :: jval_finish

CONTAINS

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_init

    USE messy_main_timer, ONLY: read_tseries_data ! mz_pj_20071029

    IMPLICIT NONE

    INTEGER :: status ! status flag
    INTEGER :: ip

    ! read jval ctrl namelist:
    CALL jval_read_nml_ctrl(status, 999)
    IF (status /= 0) STOP

    ! mz_pj_20071029+
    IF (TRIM(time_control%method) /= 'constant') THEN
       ! USE SMCL-TOOLS HERE, NO BROADCAST REQUIRED
       CALL read_tseries_data(status, modstr, TRIM(c_solc_data), 99, tseries)
    END IF
    ! mz_pj_20071029-

    ! intialize aerosol data
    CALL aerosol_data ! aerosol optical data (Shettle, Fenn)

!!$    ! intialize flux data
!!$    CALL flux_data

    lp(:)=.TRUE.

    CALL open_output_file(ncid_jval, 'caaba_jval', (/ &
      'J_O1D   ', 'J_O3P   ', 'J_H2O2  ', 'J_NO2   ', 'J_NOO2  ', &
      'J_NO2O  ', 'J_N2O5  ', 'J_HNO3  ', 'J_CH3OOH', 'J_CHOH  ', &
      'J_COH2  ', 'J_HOCl  ', 'J_Cl2O2 ', 'J_ClNO2 ', 'J_ClNO3 ', &
      'J_Cl2   ', 'J_HOBr  ', 'J_BrNO2 ', 'J_BrNO3 ', 'J_Br2   ', &
      'J_BrCl  ', 'J_IO    ', 'J_HOI   ', 'J_INO3  ', 'J_CH3I  ', &
      'J_I2    ', 'J_BrO   ', 'J_ICl   ', 'J_IBr   ', 'J_INO2  ', &
      'J_C3H7I ', 'J_CH2ClI', 'J_CH2I2 ', 'J_OClO  ', 'J_HNO4  ', &
      'J_HONO  ', 'J_CH3Br ', 'J_O2    '                          &
      /), (/ &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s',      '1/s',      '1/s',      &
      '1/s',      '1/s',      '1/s'                               &
      /), (/ &
      'J(O^1D)   ', 'J(O^3P)   ', 'J(H_2O_2) ', 'J(NO_2)   ', 'J(NOO_2)  ', &
      'J(NO_2O)  ', 'J(N_2O_5) ', 'J(HNO_3)  ', 'J(CH_3OOH)', 'J(CHOH)   ', &
      'J(COH_2)  ', 'J(HOCl)   ', 'J(Cl_2O_2)', 'J(ClNO_2) ', 'J(ClNO_3) ', &
      'J(Cl_2)   ', 'J(HOBr)   ', 'J(BrNO_2) ', 'J(BrNO_3) ', 'J(Br_2)   ', &
      'J(BrCl)   ', 'J(IO)     ', 'J(HOI)    ', 'J(INO_3)  ', 'J(CH_3I)  ', &
      'J(I_2)    ', 'J(BrO)    ', 'J(ICl)    ', 'J(IBr)    ', 'J(INO_2)  ', &
      'J(C_3H_7I)', 'J(CH_2ClI)', 'J(CH_2I_2)', 'J(OClO)   ', 'J(HNO_4)  ', &
      'J(HONO)   ', 'J(CH_3Br) ', 'J(O2)     '                              &
      /) )

    ALLOCATE(jval_gp(ip_MAX))
    DO ip = 1, ip_MAX
      ALLOCATE(jval_gp(ip)%ptr(nsza,nlev,1))
      jval_gp(ip)%ptr(:,:,:) = 0.
    END DO

    ALLOCATE(fhuv_3d(nsza,nlev,1))
    ALLOCATE(fhuvdna_3d(nsza,nlev,1))

  END SUBROUTINE jval_init

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_physc

    USE caaba_mem,        ONLY: cossza, press, x_j_no2, l_input_jval, &
                                lyear, lmonth, lday, lhour, jval_clev
    USE messy_main_tools, ONLY: nn_index

    IMPLICIT NONE

    REAL(DP), DIMENSION(nsza,nlev+1) :: v3
    REAL(DP), DIMENSION(nsza,nlev)   :: jpress
    REAL(DP), DIMENSION(nsza,nlev+1) :: relo3
    REAL(DP), DIMENSION(nsza,nlev)   :: jrhum
    REAL(DP), DIMENSION(nsza,nlev)   :: jtemp
    REAL(DP), DIMENSION(nsza)        :: albedo
    REAL(DP), DIMENSION(nsza,nlev)   :: aclc
    REAL(DP), DIMENSION(nsza)        :: slf
    REAL(DP), DIMENSION(nsza,nlev)   :: clp
    LOGICAL                      :: lmidatm
    LOGICAL                      :: l_heating
    INTEGER                      :: pbllev   ! number of levels in pbl
    INTEGER                      :: status   ! mz_pj_20071029
    INTEGER :: ii  ! counter
    REAL(DP), DIMENSION(nlev) :: z_jpress

    ! global average values are extracted with ferret from messy and
    ! jval_diag streams using e.g.: "list jrhum[i=@ave,j=@ave,l=1]"
 
    ! vertical ozone column [mcl/cm2]
    v3(1,:)    = (/ &
      3.366E+17, 1.437E+18, 4.085E+18, 5.428E+18, 6.157E+18, 6.583E+18, &
      6.860E+18, 7.070E+18, 7.227E+18, 7.343E+18, 7.436E+18, 7.523E+18, &
      7.605E+18, 7.678E+18, 7.740E+18, 7.788E+18, 7.822E+18, 7.844E+18, &
      7.857E+18, 7.862E+18 /)
    ! relative ozone, i.e. ozone mixing ratio [mol/mol]
    relo3(1,:) = (/ &
      7.182E-06, 8.319E-06, 4.172E-06, 2.041E-06, 9.525E-07, 4.334E-07, &
      2.571E-07, 1.514E-07, 9.760E-08, 5.775E-08, 5.064E-08, 4.394E-08, &
      3.980E-08, 3.636E-08, 3.209E-08, 2.807E-08, 2.479E-08, 2.242E-08, &
      2.105E-08, 2.065E-08 /)
    ! pressure [Pa]
    jpress(1,:) = (/ &
      1000., 3000., 5040., 7339., 10248., 14053., 18935., 24966., 32107., &
      40212., 49027., 58204., 67317., 75897., 83472., 89631., 94099.,     &
      96838., 98169. /)
    ! relative humidity [%]
    jrhum(1,:)  = (/ &
      0.23, 1.57, 3.52, 11.73, 24.55, 25.31, 27.45, 36.46, 44.52, 46.27,  &
      46.48, 49.18, 51.73, 57.95, 72.82, 80.71, 81.66, 77.65, 76.18 /)
    ! temperature [K]
    jtemp(1,:)  = (/ &
      230.6, 218.2, 211.7, 207.0, 205.6, 210.9, 218.1, 225.8, 235.7, 246.6, &
      256.4, 264.2, 270.6, 275.4, 278.2, 280.9, 283.2, 284.9, 285.7 /)
    albedo(:)  = 0.07
    aclc(:,:)  = 0.            ! assume clear sky
    slf(:)     = 0.            ! 0 = sea
    ! clp = cloud liquid water path per layer [g/m^2]
    clp(:,:)   = 0.            ! assume clear sky
    lmidatm    = .FALSE.
    l_heating  = .FALSE.
    pbllev     = 5

    ! mz_pj_20071029+ mz_hr_20080609+
    ! use solar cycle data for 01.01.2000 00 UTC
    ! replace later by real time -> YEAR, MONTH, DAY, HOUR
    ! orbital parameter is set to 1.0 AU here (no orbital variation)
    CALL jval_solar_time_control(status, lyear, lmonth, lday, lhour, REAL(1.0_dp))
    ! mz_pj_20071029- mz_hr_20080609-

!!$    CALL flux_data ! mz_pj_20071029: update flux data

    ! calculate jvalues
    ! messy_jval.f90 wants REAL, not REAL(DP)
    CALL jvalues(jrow,                               &
      REAL(v3),                                      &
      REAL((/ cossza /)), REAL(jpress), REAL(relo3), &
      REAL(jrhum), REAL(jtemp), REAL(albedo),        &
      REAL(aclc), REAL(slf), REAL(clp),              &
      lmidatm, l_heating, pbllev)

    !mz_hr_20080220+
    ! calculate pressure level in jpress according to current pressure
    ii = 1
    DO WHILE (ii <= nlev)
      z_jpress(ii) = jpress(1,ii)
      ii = ii + 1
    END DO 
    call nn_index(z_jpress, press, jval_clev)
    !mz_hr_20080220-

    !mz_hr_20080228+
    ! scale j-values at current level (jval_clev) with external J_NO2
    ! only j-value at current level will be used by kpp and external
    ! J_NO2 is only available for the current (trajectory) level
    ! ip_NO2 = 5 (see messy_jval.f90)
    ! J_spec' = J_spec(jval)/J_NO2(jval) * J_NO2(ext)
    IF (l_input_jval) THEN
      ! correction only above certain value for J_NO2(jval)
      IF (jval_gp(5)%ptr(1,jval_clev,1) >= 1.E-10_dp) THEN
      WRITE(*,'(A,E9.3,A,E9.3)') &
        'jval_physc: J_NO2 = ', jval_gp(5)%ptr(1,jval_clev,1), &
        ', corrected to ',x_j_no2
        ! scaling for all j-values
        DO ii = 1, ip_MAX
          jval_gp(ii)%ptr(1,jval_clev,1) = &
            jval_gp(ii)%ptr(1,jval_clev,1)/jval_gp(5)%ptr(1,jval_clev,1) * x_j_no2
          !print *, 'jval_ph: J_(', ii, ')corr = ', jval_gp(ii)%ptr(1,jval_clev,1)
        END DO
      ENDIF
    ENDIF
    !mz_hr_20080228-

  END SUBROUTINE jval_physc

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_finish

    USE messy_main_timer, ONLY: clean_tseries_data ! mz_pj_20071029

    IMPLICIT NONE

    INTEGER :: ip

    DO ip = 1, ip_MAX
      DEALLOCATE(jval_gp(ip)%ptr)
      NULLIFY(jval_gp(ip)%ptr)
    ENDDO
    DEALLOCATE(jval_gp)    ; NULLIFY(jval_gp)
    DEALLOCATE(fhuv_3d)    ; NULLIFY(fhuv_3d)
    DEALLOCATE(fhuvdna_3d) ; NULLIFY(fhuvdna_3d)

    CALL clean_tseries_data(tseries) ! mz_pj_20071029

    CALL close_file(ncid_jval)

  END SUBROUTINE jval_finish

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_result

    USE caaba_mem, ONLY: model_time, percent_done, jval_clev
    USE messy_main_constants_mem, ONLY: FLAGGED_BAD !mz_hr_20080220

    IMPLICIT NONE


!!$    INTEGER :: ip
!!$    DO ip=1, ip_MAX
!!$       CALL write_output_file(ncid_jval, &
!!$         'J_'//TRIM(jname(ip)), &
!!$         jval_gp(ip)%ptr(:,:,1) &
!!$         )
!!$    ENDDO

    ! write results to output file:
    ! %ptr(nsza,nlev,1) = %ptr(1,nlev,1)
    IF (percent_done > 0.) THEN
      CALL write_output_file(ncid_jval, model_time, (/ &
        jval_gp(ip_O1D)%ptr(1,jval_clev,1),    jval_gp(ip_O3P)%ptr(1,jval_clev,1),    &
        jval_gp(ip_H2O2)%ptr(1,jval_clev,1),   jval_gp(ip_NO2)%ptr(1,jval_clev,1),    &
        jval_gp(ip_NOO2)%ptr(1,jval_clev,1),   jval_gp(ip_NO2O)%ptr(1,jval_clev,1),   &
        jval_gp(ip_N2O5)%ptr(1,jval_clev,1),   jval_gp(ip_HNO3)%ptr(1,jval_clev,1),   &
        jval_gp(ip_CH3OOH)%ptr(1,jval_clev,1), jval_gp(ip_CHOH)%ptr(1,jval_clev,1),   &
        jval_gp(ip_COH2)%ptr(1,jval_clev,1),   jval_gp(ip_HOCl)%ptr(1,jval_clev,1),   &
        jval_gp(ip_Cl2O2)%ptr(1,jval_clev,1),  jval_gp(ip_ClNO2)%ptr(1,jval_clev,1),  &
        jval_gp(ip_ClNO3)%ptr(1,jval_clev,1),  jval_gp(ip_Cl2)%ptr(1,jval_clev,1),    &
        jval_gp(ip_HOBr)%ptr(1,jval_clev,1),   jval_gp(ip_BrNO2)%ptr(1,jval_clev,1),  &
        jval_gp(ip_BrNO3)%ptr(1,jval_clev,1),  jval_gp(ip_Br2)%ptr(1,jval_clev,1),    &
        jval_gp(ip_BrCl)%ptr(1,jval_clev,1),   jval_gp(ip_IO)%ptr(1,jval_clev,1),     &
        jval_gp(ip_HOI)%ptr(1,jval_clev,1),    jval_gp(ip_INO3)%ptr(1,jval_clev,1),   &
        jval_gp(ip_CH3I)%ptr(1,jval_clev,1),   jval_gp(ip_I2)%ptr(1,jval_clev,1),     &
        jval_gp(ip_BrO)%ptr(1,jval_clev,1),    jval_gp(ip_ICl)%ptr(1,jval_clev,1),    &
        jval_gp(ip_IBr)%ptr(1,jval_clev,1),    jval_gp(ip_INO2)%ptr(1,jval_clev,1),   &
        jval_gp(ip_C3H7I)%ptr(1,jval_clev,1),  jval_gp(ip_CH2ClI)%ptr(1,jval_clev,1), &
        jval_gp(ip_CH2I2)%ptr(1,jval_clev,1),  jval_gp(ip_OClO)%ptr(1,jval_clev,1),   &
        jval_gp(ip_HNO4)%ptr(1,jval_clev,1),   jval_gp(ip_HONO)%ptr(1,jval_clev,1),   &
        jval_gp(ip_CH3Br)%ptr(1,jval_clev,1),  jval_gp(ip_O2)%ptr(1,jval_clev,1)      /))
    ELSE ! jpress not defined in first call of jval_result (before jval_physc)
      CALL write_output_file(ncid_jval, model_time, (/ &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD,  &
        FLAGGED_BAD, FLAGGED_BAD   /))
    ENDIF

  END SUBROUTINE jval_result

  ! --------------------------------------------------------------------------

END MODULE messy_jval_box

!*****************************************************************************
