! Time-stamp: <2009-01-08 09:57:06 sander>

! This file contains routines needed for box-trajectory procedures.
! It can only be used if the netcdf library is available.

! Author: Hella Riede, MPCH Mainz, 2006-2007

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

MODULE messy_traject_box

  USE caaba_io,         ONLY: nf90_get_att, nf90_get_var, nf90_inq_varid, &
                              nf90_inq_dimid, nf90_inquire_dimension, &
                              nf90_noerr, nf, open_output_file, &
                              write_output_file, close_file
  USE messy_main_timer,         ONLY: gregor2julian, julian2gregor
  USE messy_main_constants_mem, ONLY: N_A, R_gas, i4, dp, STRLEN_MEDIUM, &
                                      STRLEN_VLONG, OneDay
  USE caaba_mem,             ONLY: tuf, time_string, relhum,          &
                                   degree_lat, degree_lon,            &
                                   model_time, model_start, model_end,&
                                   runtime, runlast, l_spechum, &
                                   spechum, &
                                   zmbl, time_step_len, startday, &
                                   c, cair, temp, press, x_j_no2, l_input_jval
  USE messy_mecca_kpp,          ONLY: ind_H2O

  IMPLICIT NONE

  ! LOCAL netCDF vars
  INTEGER :: dimid_time, len_time  ! dimension ID time, no. time values
  INTEGER :: varid_time, varid_press, varid_relhum, varid_temp, &
             varid_lat, varid_lon, varid_jno2

  INTEGER :: ncid_physc, ncid_traj, ncid_jval

  ! LOCAL variables
  INTEGER  :: waypct  ! way point counter
  INTEGER  :: intpct  = 0  ! kpp (integration) point counter
  REAL(dp) :: time_step_len_orig ! to keep original time step after time step is varied
  !mz_hr_20071029+ from caaba_mem
  !REAL(dp) :: model_start
  !mz_hr_20071029-
  LOGICAL  :: l_next_trajp = .FALSE.

  ! LOCAL interpolation slots
  REAL(dp) :: time1, time2     ! 2 time slots for interpolation
  REAL(dp) :: relhum1, relhum2 ! 2 rel. humidity ...
  REAL(dp) :: temp1, temp2     ! 2 temperature   ...
  REAL(dp) :: press1, press2   ! 2 pressure      ...
  REAL(dp) :: lat1, lat2       ! 2 latitude      ...
  REAL(dp) :: lon1, lon2       ! 2 longitude      ...
  REAL(dp) :: p_old, T_old
  !mz_hr_20080227+
  REAL(dp) :: jno21, jno22     ! 2 J_NO2
  !mz_hr_20080227-

  ! LOCAL eval_time vars
  !mz_hr_20080229+ moved to messy_main_tools and messy_main_control_cb
  !INTEGER :: start_yr, start_mon, start_day, start_hr, &
  !           start_min, start_sec
  !REAL(dp)                     :: start_juldate
  !mz_hr_20080229-
  !mz_hr_20080229+ localtime should also be output in orig time unit
  ! => convert and use time_string for output
  !CHARACTER(LEN=STRLEN_MEDIUM) :: odate, otime
  !mz_hr_20080229-

CONTAINS

  !***************************************************************************

  SUBROUTINE get_model_start_end()

    USE caaba_mem,          ONLY: l_ext_runtime, vlat, vlon, vpress, vtemp, &
                                  vrelhum, vspechum
    USE messy_main_constants_mem, ONLY: TINY_DP
    USE messy_main_tools, ONLY: spec2relhum
    USE messy_main_timer, ONLY: eval_time_str

    IMPLICIT NONE
    SAVE

    INTRINSIC :: ABS

    INTEGER :: status

    ! LOCAL
    !mz_hr_20080519 INTEGER               :: status
    !INTEGER              :: time_string_len

    ! number of time values
    CALL nf(nf90_inq_dimid(ncid_physc, "TIME", dimid_time)) ! dimid of time?
    CALL nf(nf90_inquire_dimension(ncid_physc, dimid_time, len = len_time))

    ! time units
    CALL nf(nf90_inq_varid(ncid_physc, "TIME", varid_time)) ! varid of time?
    CALL nf(nf90_get_att(ncid_physc, varid_time, "units", time_string))

    ! eval time units string
    !CALL eval_time_units(time_string)
    !mz_hr_20080228
    CALL eval_time_str(status, time_string, tuf) ! used from caaba_mem
    IF (status/=0) STOP
    ! CALL eval_time_units() ! used from caaba_mem
    !mz_hr_20080228-

    ! trajectory end time
    CALL nf(nf90_get_var(ncid_physc, varid_time, model_end, &
      start = (/ len_time /)))
    ! convert to seconds
    model_end   = model_end * tuf

    ! trajectory start time
    CALL nf(nf90_get_var(ncid_physc, varid_time, model_start, &
      start = (/ 1 /)))
    !convert to seconds
    model_start = model_start * tuf

    ! model start time:
    IF (runlast > 0._dp) THEN
      IF ( (model_end - runlast * OneDay) < model_start) THEN
        WRITE(*,*) 'Error in get_model_start_end:&
          & Parameter runlast exceeds beginning of trajectory!'
        STOP
      ENDIF
      ! start runlast days before model_end:
      model_start = model_end - runlast * OneDay
      WRITE(*,*) 'runlast overrides intrinsic trajectory runtime'
    ENDIF

    ! model end time:
    IF (l_ext_runtime) THEN
      IF ( (model_start + runtime * OneDay) > model_end) THEN
        WRITE(*,*) 'Error in get_model_start_end:&
          & Parameter runtime exceeds end of trajectory!'
        STOP
      ENDIF
      model_end = model_start + runtime * OneDay
      WRITE(*,*) 'external runtime overrides trajectory runtime'
      WRITE(*,*) 'runtime    = ', runtime,       ' days'
    ELSE ! runtime in days determined by trajectory data
      !mz_hr_20071029+
      runtime = (model_end - model_start)/OneDay
      !runtime = (model_end - model_start)/86400._dp
      !mz_hr_20071029-
    ENDIF

    ! if the user specifies both, runtime and runlast in the namelist,
    ! an inner section of the trajectory is chosen

    ! find out between which waypts starting point is
    DO waypct = 2, len_time, 1

      CALL nf(nf90_get_var(ncid_physc, varid_time, time2, &
          start = (/ waypct /)))
      time2 = time2 * tuf

      IF (time2 > model_start) THEN
        ! initialize 1st and 2nd value slot of each trajectory var
        CALL nf(nf90_get_var(ncid_physc, varid_time, time1, &
          start = (/ waypct-1 /)))
        time1 = time1 * tuf

        CALL nf(nf90_inq_varid(ncid_physc, vtemp, varid_temp))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "TEMP", varid_temp))
        CALL nf(nf90_get_var(ncid_physc, varid_temp, temp1, &
          start = (/ waypct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_temp, temp2, &
          start = (/ waypct /)))

        CALL nf(nf90_inq_varid(ncid_physc, vpress, varid_press))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "PRESS", varid_press))
        CALL nf(nf90_get_var(ncid_physc, varid_press, press1, &
          start = (/ waypct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_press, press2, &
          start = (/ waypct /)))

        !CALL nf(nf90_inq_varid(ncid_physc, "RELHUM", varid_relhum))

        !mz_hr_20080519+
        IF (l_spechum) THEN
          CALL nf(nf90_inq_varid(ncid_physc, vspechum, varid_relhum))
        ELSE
          CALL nf(nf90_inq_varid(ncid_physc, vrelhum, varid_relhum))
        ENDIF
        !mz_hr_20071031-
        CALL nf(nf90_get_var(ncid_physc, varid_relhum, relhum1, &
          start = (/ waypct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_relhum, relhum2, &
          start = (/ waypct /)))

        CALL nf(nf90_inq_varid(ncid_physc, vlat, varid_lat))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "LAT", varid_lat))
        CALL nf(nf90_get_var(ncid_physc, varid_lat, lat1, &
          start = (/ waypct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lat, lat2, &
          start = (/ waypct /)))

        CALL nf(nf90_inq_varid(ncid_physc, vlon, varid_lon))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "LON", varid_lon))
        CALL nf(nf90_get_var(ncid_physc, varid_lon, lon1, &
          start = (/ waypct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lon, lon2, &
          start = (/ waypct /)))

        !mz_hr_20080226+ dirty here since read for JVAL!
        CALL nf(nf90_inq_varid(ncid_jval, "J_NO2", varid_jno2))
        CALL nf(nf90_get_var(ncid_jval, varid_jno2, jno21, &
          start = (/ waypct-1 /)))
        CALL nf(nf90_get_var(ncid_jval, varid_jno2, jno22, &
          start = (/ waypct /)))
        !mz_hr_20080226-

        !mz_hr_20071219+ in case data contains lon up to 360deg_east
        IF (ABS(360._dp - lon1) .LE. TINY_DP) THEN
          lon1 = 0._dp
        ENDIF
        IF (ABS(360._dp - lon2) .LE. TINY_DP) THEN
          lon2 = 0._dp
        ENDIF
        !mz_hr_20071219-

        EXIT
      ENDIF
    ENDDO

    time_step_len_orig = time_step_len

    ! set to 1 during first get_physc_data, where model_time = model_start
    !   and only time_step_len needs to be updated

    p_old  = -1._dp ! recognized as 'no previous value' in get_physc_data
    T_old  = -1._dp ! recognized as 'no previous value' in get_physc_data

    ! model_start =  6912000.00
    ! model_end   =  7084800.00

    !mz_hr_20071029+
    ! init physc and calculate cair b4 x0 is called by mecca_init
    temp   = temp1
    press  = press1
    relhum = relhum1
    IF (l_spechum) THEN
      spechum = relhum
      relhum = spec2relhum(status, spechum, temp, press)
      IF (status/=0) STOP
      IF (spechum >= 1.0_dp) THEN
        WRITE(*,*) 'Warning get_model_start_end: specific humidity &
                    & >= 1.0_dp kg/kg: ', spechum
        WRITE(*,*) '=> relhum = ', relhum
      ENDIF
    ELSE
      IF (relhum >= 1.0_dp) THEN
        WRITE(*,*) 'Warning get_model_start_end: relative humidity &
                    & >= 1.0_dp: ', relhum
      ENDIF
    ENDIF
    degree_lat = lat1
    degree_lon = lon1
    cair = (N_A/1.E6) * press1 / (R_gas*temp1) ! cair = c(air) in [molec/cm3]
    !mz_hr_20071029-

    !mz_hr_20080227+
    x_j_no2 = jno21
    !print *, 'get_model: x_j_no2 = ', x_j_no2
    !mz_hr_20080227-

  END SUBROUTINE get_model_start_end

  !***************************************************************************

  SUBROUTINE get_physc_data(z_ccorr)

    !mz_hr_20071219+
    USE messy_main_constants_mem, ONLY: pi, TINY_DP
    !USE messy_main_constants_mem, ONLY: pi
    !mz_hr_20071219-
    !mz_hr_20080124+ already used for whole module
    !USE caaba_mem,                ONLY: model_time
    !mz_hr_20080124-

    IMPLICIT NONE
    SAVE

    !INTRINSIC TINY!mz_hr_20071219 not needed

    ! I/O
    REAL(dp), INTENT(OUT)   :: z_ccorr

    !LOCAL
    REAL(dp)        :: ratio1, ratio2
    REAL(dp)        :: lontmp

    IF (l_next_trajp) THEN
      ! stop at next trajectory point
      temp       = temp2
      press      = press2
      relhum     = relhum2
      degree_lat = lat2
      degree_lon = lon2
      !mz_hr_20080227+
      x_j_no2    = jno22
      !print *, 'get_physc, trajp: x_j_no2 = ', x_j_no2
      !mz_hr_20080227-

      ! end of trajectory not yet reached?
      IF (waypct < len_time) THEN
        ! waypt count up, shift in slots
        waypct  = waypct + 1
        time1   = time2
        temp1   = temp2
        press1  = press2
        relhum1 = relhum2
        lat1    = lat2
        lon1    = lon2
        !mz_hr_20080227+
        jno21   = jno22
        !mz_hr_20080227-
        CALL nf(nf90_get_var(ncid_physc, varid_time, time2, &
          start = (/ waypct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_temp, temp2, &
          start = (/ waypct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_press, press2, &
          start = (/ waypct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_relhum, relhum2, &
          start = (/ waypct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lat, lat2, start = (/ waypct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lon, lon2, start = (/ waypct /)))
        !mz_hr_20080227+
        CALL nf(nf90_get_var(ncid_jval, varid_jno2, jno22, start = (/ waypct /)))
        !mz_hr_20080227-
        !mz_hr_20071219+
        IF (ABS(360._dp - lon2) .LE. TINY_DP) THEN
          print *, 'lon2 = 360 deg east, set to 0'
          lon2 = 0._dp
        ENDIF
        !mz_hr_20071219-
        time2 = time2 * tuf
      ENDIF
    ELSE ! next regular integration point => interpolation needed
      intpct = intpct + 1
      ! calculate contribution of slot1 and slot2
      ratio2 = (model_time - time1)/(time2 - time1)
      ratio1 = 1._dp - ratio2
       
      temp    = ratio1 * temp1   + ratio2 * temp2
      press   = ratio1 * press1  + ratio2 * press2
      relhum  = ratio1 * relhum1 + ratio2 * relhum2
      degree_lat = ratio1 * lat1    + ratio2 * lat2
      !mz_hr_20080227+
      x_j_no2 = ratio1 * jno21   + ratio2 * jno22
      !print *, 'get_physc, no trajp: x_j_no2 = ', x_j_no2
      !mz_hr_20080227-
      !mz_hr_20071219+
      IF ((lon2 - lon1) .LE. -300._dp) THEN
        ! crossing 0 meridian from West to East
        lontmp = lon2 + 360._dp
        degree_lon = ratio1 * lon1 + ratio2 * lontmp
        IF (degree_lon .GE. 360) degree_lon = degree_lon - 360._dp
      ELSEIF ((lon2 - lon1) .GE. 300._dp) THEN
      ! crossing 0 meridian from East to West
        lontmp = lon1 + 360._dp
        degree_lon = ratio1 * lontmp + ratio2 * lon2
        IF (degree_lon .GE. 360) degree_lon = degree_lon - 360._dp
      ELSE
        ! not crossing at all
        degree_lon = ratio1 * lon1 + ratio2 * lon2
      ENDIF
      !mz_hr_20071219-
    ENDIF

    !mz_hr_20071219+
    ! important for 1st calculation of ccorr (T_old, p_old are init with -1)
    !IF (T_old < TINY(0._dp) .AND. p_old < TINY(0._dp)) THEN
    IF (T_old < 0._dp .AND. p_old < 0._dp) THEN
    !mz_hr_20071219-
      T_old = temp
      p_old = press
    ENDIF
    z_ccorr = (press * T_old)/(p_old * temp)
    p_old   = press
    T_old   = temp

    IF (intpct*time_step_len_orig+model_start < time2) THEN
      l_next_trajp = .FALSE.
      time_step_len = intpct*time_step_len_orig+model_start - model_time
      !print *, 'get_data: next point is a kpp point!'
      !print *, 'get_data: current time_step_len = ', time_step_len
    ELSEIF (intpct*time_step_len_orig+model_start > time2) THEN
      l_next_trajp = .TRUE.
      time_step_len = time2 - model_time
      !print *, 'get_data: next point is a waypoint!'
      !print *, 'get_data: current time_step_len = ', time_step_len
    ELSE
      l_next_trajp = .TRUE.
      time_step_len = time2 - model_time
      ! intpoint AND traj point, treated as traj point => increment intpct
      intpct = intpct + 1
      !print *, 'get_physc_data: next point is a waypoint + kpp point!'
      !print *, 'get_physc_data: intpct artificially increased to ', intpct
      !print *, 'get_physc_data: current time_step_len = ', time_step_len
    ENDIF

  END SUBROUTINE get_physc_data

  !***************************************************************************
!mz_hr_20080228+
!  SUBROUTINE eval_time_units !from caaba_mem: (time_string)
!
!    USE messy_main_tools, ONLY: strcrack
!
!    IMPLICIT NONE
!
!    INTRINSIC TRIM, ADJUSTL
!
!    ! LOCAL
!    INTEGER                      :: nosub
!    !INTEGER                      :: gyear, gmonth, gday, ghour, gmin, gsec
!    !mz_hr_20080125+
!    CHARACTER(LEN=STRLEN_MEDIUM) :: tunit, helpvar
!    !CHARACTER(LEN=STRLEN_MEDIUM) :: date, time, tunit
!    !mz_hr_20080125-
!    CHARACTER(LEN=STRLEN_MEDIUM), DIMENSION(:), POINTER :: field
!
!    ! crack time_unit string into field of nosub substrings
!    CALL strcrack(TRIM(time_string), " ", field, nosub)
!    IF ((nosub /= 3) .AND. (nosub /= 4)) THEN
!      WRITE(*,*) 'Error in messy_mecca_traject_box: '// &
!        'time_string in incompatible format!'
!      STOP
!    ENDIF
!
!    ! determine time unit factor tuf <time>*tuf=time(s)
!    tunit = TRIM(ADJUSTL(field(1)))
!    SELECT CASE (tunit)
!    CASE ('seconds')
!      tuf = 1.0_dp
!    CASE ('SECONDS')
!      tuf = 1.0_dp
!    CASE ('minutes')
!      tuf = 60.0_dp
!    CASE ('MINUTES')
!      tuf = 60.0_dp
!    CASE ('hours')
!      tuf = 3600.0_dp
!    CASE ('HOURS')
!      tuf = 3600.0_dp
!    CASE ('days')
!      tuf = 86400.0_dp
!    CASE ('DAYS')
!      tuf = 86400.0_dp
!    CASE DEFAULT
!      WRITE(*,*) 'Error in eval_time_units: time unit not readable!'
!      STOP
!    END SELECT
!
!    odate  = field(3)
!    ! processing time
!    IF (nosub == 4) THEN
!      otime  = field(4)
!      !mz_hr_20080125+ done in messy_main_control_cb
!      !WRITE(*,*) 'Time origin:   ', TRIM(odate), ' ', TRIM(otime)
!      !mz_hr_20080125-
!      CALL strcrack(otime, ":", field, nosub)
!      IF (nosub /= 3) THEN
!        WRITE(*,*) 'Error in messy_mecca_traject.f90: '// &
!          'Time in time_string in incompatible format!'
!        STOP
!      ENDIF
!      helpvar      = field(1)(1:2)
!      READ(helpvar, *) start_hr
!      helpvar      = field(2)(1:2)
!      READ(helpvar, *) start_min
!      helpvar      = field(3)(1:2)
!      READ(helpvar, *) start_sec
!    ELSE
!      start_hr  = 0
!      start_min = 0
!      start_sec = 0
!      !mz_hr_20080125+
!      otime = '00:00:00'
!      !WRITE(*,*) 'Time origin:', odate, '00:00:00'
!      !mz_hr_20080125-
!    ENDIF
!
!    ! processing date
!    CALL strcrack(odate, "-", field, nosub)
!    IF (nosub /= 3) THEN
!      WRITE(*,*) 'Error in messy_mecca_traject.f90: '// &
!        'Date in time_string in incompatible format!'
!      STOP
!    ENDIF
!    helpvar      = field(1)(1:4)
!    READ(helpvar, *) start_yr
!    helpvar      = field(2)(1:2)
!    READ(helpvar, *) start_mon
!    helpvar      = field(3)(1:2)
!    READ(helpvar, *) start_day
!
!    !start_juldate = gregor2julian(start_yr, start_mon, start_day, start_hr, &
!    !  start_min, start_sec)
!
!    ! CALL julian2gregor(start_juldate, gyear, gmonth, gday, ghour, gmin, gsec)
!
!  END SUBROUTINE eval_time_units
!mz_hr_20080228-
  !***************************************************************************
!mz_hr_20080226+ moved to messy_main_tools as spec2relhum(z_spechum, z_temp, z_press)
!  REAL(dp) FUNCTION spec2relhum(z_temp, z_press)
!
!    ! calculates relative humidity from specific humidity
!    ! [Jacobson, Fundamentals of Atmospheric Modeling, CamUnivPress, 1999]
!
!    ! molar mass of water (vapour) / molar mass of dry air
!    USE messy_main_constants_mem, ONLY: MM_eps, TINY_DP
!    USE caaba_mem,                ONLY: spechum
!
!    IMPLICIT NONE
!
!    ! e funct, natural log, absolute value
!    INTRINSIC EXP, LOG, ABS
!
!    !I/O
!    REAL(dp), INTENT(IN) :: z_temp, z_press
!
!    !LOCAL
!    REAL(dp) :: omega_v, omega_vs, p_vs, expon
!
!    ! NOW input for spechum must be in kg/kg
!
!    ! convert spechum to mass mixing ratio of water in dry air
!    !mz_hr_20071219+
!    !IF (1._dp - spechum < TINY_DP) THEN
!    IF (ABS(1._dp - spechum) < TINY_DP) THEN
!      WRITE(*,*) 'Error in messy_traject_box: division by 0 between&
!        & trajectory waypoints ', waypct-1, 'and ', waypct
!      STOP
!    ENDIF
!
!    omega_v = spechum/(1._dp-spechum)
!    !print *, 'spec2relhum: omega_v = ', omega_v
!
!    ! calc sat vap press of H2O [Pa]
!    !print *, 'mtrajbox: temp = ', temp
!    expon = 6816._dp * (1._dp/273.15_dp-1._dp/z_temp) + 5.1309_dp * LOG(273.15_dp/z_temp)
!    !print *, 'spec2relhum: expon = ', expon
!    p_vs  = 611.2_dp * EXP( expon )
!
!    ! calc saturation mass mixing ratio of H2O vapor over liquid surface
!    omega_vs = MM_eps * p_vs / (z_press - p_vs)
!    !print *, 'spec2relhum: omega_vs = ', omega_vs
!
!    ! calc relhum, def by World Meteorological Organization WMO in %
!    !mz_hr_20071026+
!    spec2relhum = omega_v / omega_vs
!    !spec2relhum = 100._dp * omega_v / omega_vs
!    !mz_hr_20071026-
!    !print *, 'spec2relhum: relhum = omega_v/omega_vs = ', spec2relhum
!
!    IF (spec2relhum > 1._dp) THEN
!      WRITE(*,*) 'WARNING! spec2relhum: calculated rel. humidity exceeds&
!        & 100% between trajectory waypoints ', waypct-1, &
!        ' and ', waypct
!      WRITE(*,*) 'rel. humidity:', spec2relhum, '(spec. humidity = ', spechum, 'press = ', z_press, 'temp = ', z_temp, ')'
!    ELSEIF (spec2relhum < 0._dp) THEN
!      WRITE(*,*) 'ERROR! spec2relhum: negative calculated rel. humidity&
!        & between trajectory waypoints ', waypct-1, 'and', waypct
!      WRITE(*,*) 'exit from caaba'
!      STOP
!    ENDIF
!
!  END FUNCTION spec2relhum
!mz_hr_20080226-
  !*****************************************************************************

  SUBROUTINE traject_init

    USE caaba_mem, ONLY: input_physc, input_jval
    USE caaba_io,  ONLY: open_input_file

    IMPLICIT NONE

    INTRINSIC TRIM

    ! open external trajectory netcdf file
    CALL open_input_file(ncid_physc, input_physc)
    CALL open_input_file(ncid_jval, input_jval)

    ! runlast to crop start of traj
    CALL get_model_start_end()

    model_time = model_start
    startday   = model_start / OneDay

    ! open output file mecca_traject.nc:
    ! additional result file for external trajectory input

    ! localtime unit: 33 chars 14+10(yyyy-mm-dd)+1(space)+8(hh:mm:ss)

    IF (l_spechum) THEN
      !mz_hr_20080131+ lon-lat swapped, caaba_messy vars added
      !mz_hr_20080227+ external J_NO2 written out
      !mz_hr_20080229 local year, month, day, ... written out
      IF (l_input_jval) THEN
        CALL open_output_file(ncid_traj, 'caaba_traject',       &
          (/ 'lon_tr   ', 'lat_tr   ', 'press    ', 'temp     ', &
             'relhum   ', 'spechum  ', 'sza      ', 'J_NO2_x  ', &
             'localtime', 'year_loc ', 'month_loc', 'day_loc  ', &
             'hour_loc ', 'min_loc  ', 'sec_loc  ' /), &
          (/ 'degrees_east                     ', &
             'degrees_north                    ', &
             'Pa                               ', &
             'K                                ', &
             '                                 ', &
             'kg/kg                            ', &
             'deg                              ', &
             '1/s                              ', &
             !mz_hr_20080229+
             TRIM(time_string)                  , & 
             !mz_hr_20080229-
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ' /) )
             !TRIM('seconds since '//TRIM(odate)//' '//TRIM(otime))/) )
      ELSE
        CALL open_output_file(ncid_traj, 'caaba_traject',       &
          (/ 'lon_tr   ', 'lat_tr   ', 'press    ', 'temp     ', &
             'relhum   ', 'spechum  ', 'sza      ', 'localtime', &
             'year_loc ', 'month_loc', 'day_loc  ', 'hour_loc ', &
             'min_loc  ', 'sec_loc  ' /), &
          (/ 'degrees_east                     ', &
             'degrees_north                    ', &
             'Pa                               ', &
             'K                                ', &
             '                                 ', &
             'kg/kg                            ', &
             'deg                              ', &
             !mz_hr_20080229+
             TRIM(time_string)                  , &
             !mz_hr_20080229-
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ' /) )
             !TRIM('seconds since '//TRIM(odate)//' '//TRIM(otime))/) )
      ENDIF
    ELSE
      IF (l_input_jval) THEN
        CALL open_output_file(ncid_traj, 'caaba_traject',       &
          (/ 'lon_tr   ', 'lat_tr   ', 'press    ', 'temp     ', &
             'relhum   ', 'sza      ', 'J_NO2_x  ', 'localtime', &
             'year_loc ', 'month_loc', 'day_loc  ', 'hour_loc ', &
             'min_loc  ', 'sec_loc  ' /), &
          (/ 'degrees_east                     ', &
             'degrees_north                    ', &
             'Pa                               ', &
             'K                                ', &
             '                                 ', &
             'deg                              ', &
             '1/s                              ', &
             !mz_hr_20080229+
             TRIM(time_string)                  , &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ' /) )
             !mz_hr_20080229-
             !TRIM('seconds since '//TRIM(odate)//' '//TRIM(otime))/) )
      ELSE
        CALL open_output_file(ncid_traj, 'caaba_traject',       &
          (/ 'lon_tr   ', 'lat_tr   ', 'press    ', 'temp     ', &
             'relhum   ', 'sza      ', 'localtime', 'year_loc ', &
             'month_loc', 'day_loc  ', 'hour_loc ', 'min_loc  ', &
             'sec_loc  ' /), &
          (/ 'degrees_east                     ', &
             'degrees_north                    ', &
             'Pa                               ', &
             'K                                ', &
             '                                 ', &
             'deg                              ', &
             !mz_hr_20080229+
             TRIM(time_string)                  , &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ', &
             '                                 ' /) )
             !mz_hr_20080229-
             !TRIM('seconds since '//TRIM(odate)//' '//TRIM(otime))/) )
      ENDIF
      !mz_hr_20080227-
      !mz_hr_20080131-
    ENDIF

  END SUBROUTINE traject_init

  !*****************************************************************************

  SUBROUTINE traject_physc

    IMPLICIT NONE

    CALL update_trajectory

  END SUBROUTINE traject_physc

  !*****************************************************************************

  SUBROUTINE traject_result

    USE caaba_mem,  ONLY: localtime, percent_done, degree_sza, & !mz_hr_20071219
                          lyear, lmonth, lday, lmin, lhour, lsec !mz_hr_20080229
    !mz_hr_20080219+
    USE messy_main_constants_mem, ONLY: FLAGGED_BAD
    !USE messy_main_constants_mem, ONLY: FLAGGED_BAD_SP
    !mz_hr_20080219-

    IMPLICIT NONE

    !mz_hr_20080125+
    IF (percent_done > 0.) THEN
      IF (l_spechum) THEN
        !mz_hr_20080227+ if external J_NO2 then written out
        IF (l_input_jval) THEN
          !mz_hr_20080131+ lon-lat swapped, caaba_messy vars added
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, spechum, &
              degree_sza, x_j_no2, (localtime/tuf), REAL(lyear,dp), &
              REAL(lmonth,dp), REAL(lday,dp), REAL(lhour,dp),       &
              REAL(lmin,dp), REAL(lsec,dp) /))
            !(/degree_lon, degree_lat, press, temp, relhum, spechum, degree_sza, x_j_no2, localtime/))
        ELSE
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, spechum, &
              degree_sza, (localtime/tuf), REAL(lyear,dp), REAL(lmonth,dp), &
              REAL(lday,dp), REAL(lhour,dp), REAL(lmin,dp), REAL(lsec,dp) /))
        ENDIF
      ELSE
        IF (l_input_jval) THEN
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, degree_sza, &
              x_j_no2, (localtime/tuf), REAL(lyear,dp), REAL(lmonth,dp), &
              REAL(lday,dp), REAL(lhour,dp), REAL(lmin,dp), REAL(lsec,dp) /))
        ELSE
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, degree_sza, &
              (localtime/tuf), REAL(lyear,dp), REAL(lmonth,dp), &
               REAL(lday,dp), REAL(lhour,dp), REAL(lmin,dp), REAL(lsec,dp) /))
        ENDIF
      ENDIF
    ELSE ! 1st output, localtime not defined yet
      IF (l_spechum) THEN
        IF (l_input_jval) THEN
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, spechum, &
              FLAGGED_BAD, x_j_no2, FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, &
              FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD /))
        ELSE
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, spechum, &
              FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, &
              FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD /))
        ENDIF
      ELSE
        IF (l_input_jval) THEN
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, FLAGGED_BAD, &
              x_j_no2, FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, &
              FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD /))
        ELSE
          CALL write_output_file(ncid_traj, model_time,  &
            (/degree_lon, degree_lat, press, temp, relhum, FLAGGED_BAD, &
              FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD, &
              FLAGGED_BAD, FLAGGED_BAD, FLAGGED_BAD /))
        ENDIF
          !mz_hr_20080131-
      ENDIF
      !mz_hr_20080227-
    ENDIF
    !mz_hr_20080125-

  END SUBROUTINE traject_result

  !*****************************************************************************

  SUBROUTINE traject_finish

    CALL close_file(ncid_physc)
    CALL close_file(ncid_traj)
    CALL close_file(ncid_jval)

  END SUBROUTINE traject_finish

  !*****************************************************************************

  SUBROUTINE update_trajectory

    !mz_hr_20080226+
    USE messy_main_tools,  ONLY: psat, spec2relhum
    !USE messy_main_tools,  ONLY: psat
    !mz_hr_20080226-

    IMPLICIT NONE

    INTEGER :: status

    ! LOCAL
    REAL(dp) :: ccorr = 0._dp ! concentration correction due to p and T changes

    ! get trajectory data
    CALL get_physc_data(ccorr)
    !print *, 'update_trajectory: ccorr  = ', ccorr

    IF (l_spechum) THEN ! spechum and relhum written to messy_traject.nc
      spechum = relhum
      !mz_hr_20080226+
      relhum = spec2relhum(status, spechum, temp, press)
      IF (status/=0) STOP
      !relhum = spec2relhum(temp, press)
      !mz_hr_20080226-
      IF (spechum >= 1.0_dp) THEN
        WRITE(*,*) 'Warning update_trajectory: specific humidity &
                    & >= 1.0_dp kg/kg: ', spechum
        WRITE(*,*) '=> relhum = ', relhum
      ENDIF
    ELSE
      IF (relhum >= 1.0_dp) THEN
        WRITE(*,*) 'Warning update_trajectory: relative humidity &
                    & >= 1.0_dp: ', relhum
      ENDIF
    ENDIF
    zmbl    = 1000._dp     ! height of boundary layer [m]
    ! adjust concentrations to altered temperature and pressure
    C(:) = C(:) * ccorr
    cair = (N_A/1.E6) * press / (R_gas*temp) ! cair = c(air) in [mcl/cc]
    ! do not use ccorr for water, but calculate it directly:
    c(ind_H2O) = cair * relhum * psat(temp) / press

  END SUBROUTINE update_trajectory

  !***************************************************************************

END MODULE messy_traject_box

!*****************************************************************************
