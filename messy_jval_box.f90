!*****************************************************************************
!                Time-stamp: <2010-03-19 08:57:59 joec_pa>
!*****************************************************************************

! JVAL = calculation of J-VALues (photolysis rate coefficients)

! Authors:
! Rolf Sander,          MPICH, 2007-...
! Andreas Baumgaertner, MPICH, 2009: photo_strato with 90 levels added

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
  USE messy_jval ! ONLY: jval_gp, jval_read_nml_ctrl,
  !       lookup, lookup_io, aerosol_data, jvalues, lp, modstr

  IMPLICIT NONE
  PRIVATE

  INTEGER :: ncid_jval

  INTEGER, PARAMETER :: nsza =  1
  INTEGER :: nlev ! number of levels
  INTEGER, PARAMETER :: jrow =  1 ! must be jrow=1 for box model

  REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: v3
  REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: jpress  ! given pressure levels
  REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: relo3
  REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: jrhum
  REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: jtemp
  REAL(DP), ALLOCATABLE, DIMENSION(:)   :: albedo
  REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: aclc
  REAL(DP), ALLOCATABLE, DIMENSION(:)   :: slf
  REAL(DP), ALLOCATABLE, DIMENSION(:,:) :: clp
  LOGICAL :: lmidatm
  LOGICAL :: l_heating
  INTEGER :: pbllev           ! number of levels in pbl

  PUBLIC :: jval_init   ! initialize J-values
  PUBLIC :: jval_physc  ! calculate J values
  PUBLIC :: jval_result
  PUBLIC :: jval_finish

CONTAINS

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_init

    USE messy_main_import_tseries, ONLY: read_tseries_data ! mz_pj_20071029
    USE caaba_mem,                 ONLY: photo_scenario

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

    CALL open_output_file(ncid_jval, 'caaba_jval', &
      (/ ('J_'//jname(ip), ip=1,IP_MAX) /), &
      (/ ('1/s',           ip=1,IP_MAX) /), &
      (/ ('J_'//jname(ip), ip=1,IP_MAX) /)) ! longname same as name

    SELECT CASE (TRIM(photo_scenario))
    !qqq todo: CASE ('FF_ANTARCTIC','FF_ARCTIC')
      !qqq todo: CALL photo_ff
    !qqq todo: CASE ('FREE_TROP')
      !qqq todo: CALL photo_free_trop
    CASE ('','MBL','MIM2','OOMPH')
      CALL photo_mbl
    CASE ('STRATO')
      CALL photo_strato
    CASE DEFAULT
      PRINT *, 'ERROR, photo_scenario '//TRIM(photo_scenario)// &
        ' is not defined in '//TRIM(modstr)//'.'
      STOP
    END SELECT

    !-------------------------------------------------------------------------

  CONTAINS

    !-------------------------------------------------------------------------

    SUBROUTINE allocate_arrays

      ALLOCATE(jval_gp(IP_MAX))
      DO ip = 1, IP_MAX
        ALLOCATE(jval_gp(ip)%ptr(nsza,nlev,1))
        jval_gp(ip)%ptr(:,:,:) = 0.
      END DO

      ALLOCATE(fhuv_3d(nsza,nlev,1))
      ALLOCATE(fhuvdna_3d(nsza,nlev,1))

      ALLOCATE(v3(nsza,nlev+1))
      ALLOCATE(jpress(nsza,nlev))
      ALLOCATE(relo3(nsza,nlev+1))
      ALLOCATE(jrhum(nsza,nlev))
      ALLOCATE(jtemp(nsza,nlev))
      ALLOCATE(albedo(nsza))
      ALLOCATE(aclc(nsza,nlev))
      ALLOCATE(slf(nsza))
      ALLOCATE(clp(nsza,nlev))

    END SUBROUTINE allocate_arrays

    !-------------------------------------------------------------------------

    SUBROUTINE photo_mbl

      nlev = 19
      CALL allocate_arrays

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
      aclc(:,:)  = 0.      ! assume clear sky
      slf(:)     = 0.      ! 0 = sea
      ! clp = cloud liquid water path per layer [g/m^2]
      clp(:,:)   = 0.      ! assume clear sky
      lmidatm    = .FALSE.
      l_heating  = .FALSE.
      pbllev     = 5       ! number of levels in pbl
    END SUBROUTINE photo_mbl

    !-------------------------------------------------------------------------

    SUBROUTINE photo_strato

      nlev = 90
      CALL allocate_arrays

      ! vertical ozone column [mcl/cm2]
      v3(1,:)    = (/ &
        9.585E+12, 2.593E+13, 2.351E+14, 7.025E+14, 1.322E+15, 2.108E+15, &
        3.092E+15, 4.312E+15, 5.843E+15, 7.789E+15, 1.029E+16, 1.346E+16, &
        1.747E+16, 2.246E+16, 2.861E+16, 3.616E+16, 4.540E+16, 5.665E+16, &
        7.030E+16, 8.678E+16, 1.066E+17, 1.302E+17, 1.581E+17, 1.907E+17, &
        2.288E+17, 2.728E+17, 3.233E+17, 3.810E+17, 4.463E+17, 5.199E+17, &
        6.024E+17, 6.941E+17, 7.955E+17, 9.070E+17, 1.029E+18, 1.163E+18, &
        1.308E+18, 1.466E+18, 1.637E+18, 1.822E+18, 2.020E+18, 2.229E+18, &
        2.448E+18, 2.674E+18, 2.906E+18, 3.143E+18, 3.385E+18, 3.628E+18, &
        3.874E+18, 4.121E+18, 4.366E+18, 4.609E+18, 4.848E+18, 5.081E+18, &
        5.306E+18, 5.524E+18, 5.733E+18, 5.930E+18, 6.112E+18, 6.276E+18, &
        6.422E+18, 6.551E+18, 6.664E+18, 6.765E+18, 6.856E+18, 6.940E+18, &
        7.018E+18, 7.092E+18, 7.161E+18, 7.225E+18, 7.284E+18, 7.334E+18, &
        7.376E+18, 7.412E+18, 7.443E+18, 7.472E+18, 7.501E+18, 7.530E+18, &
        7.560E+18, 7.592E+18, 7.627E+18, 7.664E+18, 7.702E+18, 7.742E+18, &
        7.782E+18, 7.823E+18, 7.862E+18, 7.894E+18, 7.916E+18, 7.929E+18, &
        7.935E+18 /)
      ! relative ozone, i.e. ozone mixing ratio [mol/mol]
      relo3(1,:) = (/ &
        4.015E-07, 7.810E-07, 9.022E-07, 9.528E-07, 1.010E-06, 1.083E-06, &
        1.174E-06, 1.290E-06, 1.433E-06, 1.604E-06, 1.804E-06, 2.031E-06, &
        2.281E-06, 2.548E-06, 2.840E-06, 3.152E-06, 3.485E-06, 3.832E-06, &
        4.203E-06, 4.586E-06, 4.970E-06, 5.367E-06, 5.762E-06, 6.135E-06, &
        6.483E-06, 6.810E-06, 7.102E-06, 7.363E-06, 7.588E-06, 7.785E-06, &
        7.948E-06, 8.090E-06, 8.201E-06, 8.280E-06, 8.331E-06, 8.354E-06, &
        8.360E-06, 8.366E-06, 8.316E-06, 8.163E-06, 7.909E-06, 7.582E-06, &
        7.204E-06, 6.810E-06, 6.407E-06, 5.993E-06, 5.585E-06, 5.185E-06, &
        4.781E-06, 4.375E-06, 3.960E-06, 3.548E-06, 3.156E-06, 2.794E-06, &
        2.454E-06, 2.132E-06, 1.816E-06, 1.501E-06, 1.225E-06, 9.909E-07, &
        7.922E-07, 6.395E-07, 5.220E-07, 4.307E-07, 3.607E-07, 3.065E-07, &
        2.615E-07, 2.219E-07, 1.852E-07, 1.493E-07, 1.148E-07, 8.642E-08, &
        6.697E-08, 5.531E-08, 4.807E-08, 4.365E-08, 4.096E-08, 3.948E-08, &
        3.833E-08, 3.733E-08, 3.574E-08, 3.348E-08, 3.141E-08, 2.937E-08, &
        2.732E-08, 2.549E-08, 2.413E-08, 2.293E-08, 2.188E-08, 2.083E-08, &
        2.083E-08 /)
      ! pressure [Pa]
      jpress(1,:) = (/ &
            0.99,      3.18,      5.81,      8.96,     12.74,     17.17, &
           22.27,     28.14,     34.88,     42.64,     51.43,     61.28, &
           72.21,     84.23,     97.45,    111.99,    127.99,    145.59, &
          164.95,    186.24,    209.56,    234.97,    262.66,    292.85, &
          325.76,    361.63,    400.73,    443.34,    489.79,    540.42, &
          595.44,    655.07,    719.68,    789.69,    865.56,    947.77, &
         1036.86,   1133.41,   1238.02,   1351.39,   1474.24,   1607.37, &
         1751.63,   1907.95,   2077.31,   2260.76,   2459.51,   2674.83, &
         2908.17,   3161.09,   3436.18,   3736.45,   4063.97,   4421.67, &
         4813.36,   5242.81,   5713.73,   6230.96,   6799.36,   7422.58, &
         8104.57,   8852.52,   9675.99,  10584.35,  11587.46,  12697.23, &
        13926.26,  15286.00,  16787.98,  18443.60,  20268.79,  22280.20, &
        24495.96,  26934.59,  29615.81,  32566.47,  35815.20,  39391.63, &
        43328.65,  47656.74,  52414.16,  57639.89,  63371.77,  69660.11, &
        76471.58,  83472.54,  89631.42,  94099.31,  96838.38,  98169.53 /)
      ! relative humidity [%]
      jrhum(1,:)  = (/ &
        0.02221367, 0.00052341, 0.00007564, 0.00002883, 0.00001122, &
        0.00000587, 0.00000430, 0.00000392, 0.00000389, 0.00000393, &
        0.00000398, 0.00000403, 0.00000410, 0.00000422, 0.00000444, &
        0.00000483, 0.00000553, 0.00000659, 0.00000822, 0.00001056, &
        0.00001384, 0.00001838, 0.00002442, 0.00003288, 0.00004403, &
        0.00005887, 0.00007834, 0.00010309, 0.00013586, 0.00017868, &
        0.00023361, 0.00030174, 0.00038190, 0.00047473, 0.00057979, &
        0.00069729, 0.00082382, 0.00096785, 0.00113070, 0.00132470, &
        0.00154490, 0.00180692, 0.00210759, 0.00245610, 0.00284590, &
        0.00329761, 0.00384404, 0.00447615, 0.00528961, 0.00622343, &
        0.00743052, 0.00898439, 0.01109167, 0.01431917, 0.01959432, &
        0.02817679, 0.04253354, 0.06687789, 0.10524502, 0.16381378, &
        0.23504007, 0.29133943, 0.32201385, 0.32918236, 0.33116952, &
        0.33988434, 0.36355028, 0.39882454, 0.45356584, 0.51465833, &
        0.56314117, 0.59511203, 0.60645521, 0.60647291, 0.58347756, &
        0.55428118, 0.52944815, 0.50482678, 0.48309720, 0.46583462, &
        0.46191990, 0.47942528, 0.51354384, 0.55175561, 0.60617954, &
        0.66909349, 0.73569524, 0.79188657, 0.77944380, 0.77184021 /)
      ! temperature [K]
      jtemp(1,:)  = (/ &
        177.71, 191.80, 206.49, 216.20, 224.97, 232.31, 238.68, 243.87, &
        248.33, 252.11, 255.02, 257.09, 258.57, 259.59, 259.92, 259.86, &
        259.39, 258.71, 257.61, 256.24, 254.80, 253.05, 251.09, 249.20, &
        247.32, 245.35, 243.36, 241.31, 239.28, 237.32, 235.42, 233.75, &
        232.22, 230.84, 229.64, 228.64, 227.77, 226.81, 225.81, 224.71, &
        223.67, 222.64, 221.59, 220.60, 219.64, 218.72, 217.82, 216.96, &
        216.12, 215.29, 214.44, 213.55, 212.65, 211.76, 210.81, 209.72, &
        208.48, 207.06, 205.48, 204.00, 203.00, 202.97, 203.37, 204.56, &
        205.96, 207.48, 208.88, 210.18, 211.43, 212.98, 215.10, 217.89, &
        221.35, 225.32, 229.81, 234.52, 239.52, 244.53, 249.54, 254.41, &
        259.14, 263.66, 268.05, 272.38, 276.33, 279.82, 282.58, 284.53, &
        286.02, 286.67  /)
      ! mz_ab_20090909-

      albedo(:)  = 0.07
      aclc(:,:)  = 0.            ! assume clear sky
      slf(:)     = 0.            ! 0 = sea
      ! clp = cloud liquid water path per layer [g/m^2]
      clp(:,:)   = 0.            ! assume clear sky
      lmidatm    = .TRUE.
      l_heating  = .FALSE.
      pbllev     = 5
    END SUBROUTINE photo_strato

    !-------------------------------------------------------------------------

  END SUBROUTINE jval_init

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_physc

    USE caaba_mem,        ONLY: cossza, press, x_j_no2, l_input_jval, &
                                lyear, lmonth, lday, lhour, jval_clev, &
                                photo_scenario
    USE messy_main_tools, ONLY: nn_index

    IMPLICIT NONE

    INTEGER :: status
    INTEGER :: ii ! counter

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

    ! mz_hr_20080228+
    ! calculate pressure level in jpress according to current pressure
    CALL nn_index(jpress(1,:), press, jval_clev)
    IF (press > 1.1 * jpress(1,nlev)) THEN
      WRITE (*,*) 'Warning jval_physc: pressure more than 10% above '// &
        'highest standard atmosphere pressure'
    ENDIF
    IF (press < 0.9 * jpress(1,1)) THEN
      WRITE (*,*) 'Warning jval_physc: pressure more than 10% below '// &
        'lowest standard atmosphere pressure'
    ENDIF

    ! scale j-values at current level (jval_clev) with external J_NO2
    ! only j-value at current level will be used by kpp and external
    ! J_NO2 is only available for the current (trajectory) level
    ! ip_NO2 = 5 (see messy_jval.f90)
    ! J_spec' = J_spec(jval)/J_NO2(jval) * J_NO2(ext)
    IF (l_input_jval) THEN
      ! correction only above certain value for J_NO2(jval)
      IF (jval_gp(5)%ptr(1,jval_clev,1) >= 1.E-10_dp) THEN
        ! correction only above certain value for external jval
        ! otherwise: possible problem with integrator if set to 0
        IF (x_j_no2 >= 1.E-10_dp) THEN
        WRITE(*,'(A,E9.3,A,E9.3)') &
          'jval_physc: J_NO2 = ', jval_gp(5)%ptr(1,jval_clev,1), &
          ', corrected to ',x_j_no2
          ! scaling for all j-values
          DO ii = 1, IP_MAX
            jval_gp(ii)%ptr(1,jval_clev,1) = &
              jval_gp(ii)%ptr(1,jval_clev,1)/jval_gp(5)%ptr(1,jval_clev,1) * x_j_no2
            !print *, 'jval_ph: J_(', ii, ')corr = ', jval_gp(ii)%ptr(1,jval_clev,1)
          END DO
        ENDIF
      ENDIF
    ENDIF
    ! mz_hr_20080228-

    !-------------------------------------------------------------------------

  END SUBROUTINE jval_physc

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_finish

    USE messy_main_import_tseries, ONLY: clean_tseries_data ! mz_pj_20071029

    IMPLICIT NONE

    INTEGER :: ip

    DO ip = 1, IP_MAX
      DEALLOCATE(jval_gp(ip)%ptr)
      NULLIFY(jval_gp(ip)%ptr)
    ENDDO
    DEALLOCATE(jval_gp)    ; NULLIFY(jval_gp)
    DEALLOCATE(fhuv_3d)    ; NULLIFY(fhuv_3d)
    DEALLOCATE(fhuvdna_3d) ; NULLIFY(fhuvdna_3d)

    DEALLOCATE(v3)
    DEALLOCATE(jpress)
    DEALLOCATE(relo3)
    DEALLOCATE(jrhum)
    DEALLOCATE(jtemp)
    DEALLOCATE(albedo)
    DEALLOCATE(aclc)
    DEALLOCATE(slf)
    DEALLOCATE(clp)

    CALL clean_tseries_data(tseries)

    CALL close_file(ncid_jval)

  END SUBROUTINE jval_finish

  ! --------------------------------------------------------------------------

  SUBROUTINE jval_result

    USE caaba_mem, ONLY: model_time, percent_done, jval_clev
    USE messy_main_constants_mem, ONLY: FLAGGED_BAD

    IMPLICIT NONE

    REAL(DP), DIMENSION(IP_MAX) :: jval_array
    INTEGER :: ip

    IF (percent_done > 0.) THEN
      ! The following loop is necessary because the component (after the
      ! "%") to the right of a part reference with nonzero rank cannot
      ! have the POINTER attribute. In other words, we cannot use
      ! "jval_gp(:)%ptr(1,jval_clev,1)"
      DO ip = 1, IP_MAX
        jval_array(ip) = jval_gp(ip)%ptr(1,jval_clev,1)
      ENDDO
      CALL write_output_file(ncid_jval, model_time, jval_array)
    ELSE
      ! jval_clev not defined in first call of jval_result (before jval_physc)
      CALL write_output_file(ncid_jval, model_time, &
        (/ (FLAGGED_BAD, ip=1,IP_MAX) /))
    ENDIF

  END SUBROUTINE jval_result

  ! --------------------------------------------------------------------------

END MODULE messy_jval_box

!*****************************************************************************
