! Time-stamp: <2010-02-16 15:52:14 sander>

! This file contains routines needed for box-trajectory procedures.
! It can only be used if the netcdf library is available.

! Author: Hella Riede, MPCH Mainz, 2006-2008

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
  USE caaba_mem,          ONLY: tuf, time_string, relhum,          &
                                degree_lat, degree_lon,            &
                                model_time, model_start, model_end,&
                                runtime, runlast, l_spechum, &
                                spechum, &
                                time_step_len, startday, &
                                !mz_hr_20081121+zmbl, time_step_len, startday, &
                                c, cair, temp, press, x_j_no2, l_input_jval
  !mz_hr_20081130 USE messy_mecca_kpp,    ONLY: ind_H2O

  IMPLICIT NONE

  ! LOCAL netCDF vars
  INTEGER :: dimid_time, len_time  ! dimension ID time, no. time values
  INTEGER :: varid_time, varid_press, varid_relhum, varid_temp, &
             varid_lat, varid_lon, varid_jno2
  INTEGER :: ncid_physc, ncid_traj, ncid_jval

  ! LOCAL variables
  INTEGER  :: trajpct  ! trajectory point counter
  INTEGER  :: intpct  = 1  ! integr. point count 1:next is 1st after start
  REAL(dp) :: time_step_len_orig ! to keep original time step after time step is varied
  LOGICAL  :: l_next_trajp !mz_hr_20081130 not necessary anymore = .FALSE.
  !mz_hr_20081201+
  REAL(dp) :: ratio1, ratio2
  REAL(dp) :: lontmp
  !mz_hr_20081201-

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

CONTAINS

  !***************************************************************************

  SUBROUTINE get_model_start_end()

    USE caaba_mem,                ONLY: l_ext_runtime, vlat, vlon, vpress, &
                                        vtemp, vrelhum, vspechum
    USE messy_main_constants_mem, ONLY: TINY_DP
    USE messy_main_tools,         ONLY: spec2relhum
    USE messy_main_timer,         ONLY: eval_time_str

    IMPLICIT NONE
    SAVE !mz_hr_20081130: needed? 

    INTRINSIC :: ABS, TRIM

    ! LOCAL
    INTEGER :: status

    ! number of time values
    CALL nf(nf90_inq_dimid(ncid_physc, "TIME", dimid_time)) ! dimid of time?
    CALL nf(nf90_inquire_dimension(ncid_physc, dimid_time, len = len_time))

    ! time units
    CALL nf(nf90_inq_varid(ncid_physc, "TIME", varid_time)) ! varid of time?
    CALL nf(nf90_get_att(ncid_physc, varid_time, "units", time_string))

    ! eval time units string
    !mz_hr_20080228+
    CALL eval_time_str(status, time_string, tuf) ! used from caaba_mem
    IF (status/=0) STOP
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
      ! start runlast days before model_end
      model_start = model_end - runlast * OneDay
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
    ELSE
      ! runtime in days determined by trajectory data
      runtime = (model_end - model_start)/OneDay
    ENDIF

    ! if the user specifies runtime and runlast in namelist,
    ! an inner section of the trajectory is chosen

    ! find out between which trajpts starting point is
    DO trajpct = 2, len_time, 1

      CALL nf(nf90_get_var(ncid_physc, varid_time, time2, &
          start = (/ trajpct /)))
      time2 = time2 * tuf

      IF (time2 > model_start) THEN
        ! initialize 1st and 2nd value slot of each trajectory var
        CALL nf(nf90_get_var(ncid_physc, varid_time, time1, &
          start = (/ trajpct-1 /)))
        time1 = time1 * tuf

        !mz_hr_20080612+ inserted TRIM() for external variables
        CALL nf(nf90_inq_varid(ncid_physc, TRIM(vtemp), varid_temp))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "TEMP", varid_temp))
        CALL nf(nf90_get_var(ncid_physc, varid_temp, temp1, &
          start = (/ trajpct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_temp, temp2, &
          start = (/ trajpct /)))

        CALL nf(nf90_inq_varid(ncid_physc, TRIM(vpress), varid_press))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "PRESS", varid_press))
        CALL nf(nf90_get_var(ncid_physc, varid_press, press1, &
          start = (/ trajpct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_press, press2, &
          start = (/ trajpct /)))

        !CALL nf(nf90_inq_varid(ncid_physc, "RELHUM", varid_relhum))

        !mz_hr_20080519+
        IF (l_spechum) THEN
          CALL nf(nf90_inq_varid(ncid_physc, TRIM(vspechum), varid_relhum))
        ELSE
          CALL nf(nf90_inq_varid(ncid_physc, TRIM(vrelhum), varid_relhum))
        ENDIF
        !mz_hr_20071031-
        CALL nf(nf90_get_var(ncid_physc, varid_relhum, relhum1, &
          start = (/ trajpct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_relhum, relhum2, &
          start = (/ trajpct /)))

        CALL nf(nf90_inq_varid(ncid_physc, TRIM(vlat), varid_lat))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "LAT", varid_lat))
        CALL nf(nf90_get_var(ncid_physc, varid_lat, lat1, &
          start = (/ trajpct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lat, lat2, &
          start = (/ trajpct /)))

        CALL nf(nf90_inq_varid(ncid_physc, TRIM(vlon), varid_lon))
        !mz_hr_20080519 CALL nf(nf90_inq_varid(ncid_physc, "LON", varid_lon))
        CALL nf(nf90_get_var(ncid_physc, varid_lon, lon1, &
          start = (/ trajpct-1 /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lon, lon2, &
          start = (/ trajpct /)))

        ! mz_hr_20080226+ dirty here since read for JVAL!
        IF (l_input_jval) THEN !mz_hr_20080808
          CALL nf(nf90_inq_varid(ncid_jval, "J_NO2", varid_jno2))
          CALL nf(nf90_get_var(ncid_jval, varid_jno2, jno21, &
            start = (/ trajpct-1 /)))
          CALL nf(nf90_get_var(ncid_jval, varid_jno2, jno22, &
            start = (/ trajpct /)))
        ENDIF
        !mz_hr_20080226-

        !mz_hr_20071219+ in case data contains lon up to 360deg_east
        ! -> set to 0 instead of 360
        IF (ABS(360._dp - lon1) .LE. TINY_DP) THEN
          print *, 'lon1 = 360 deg east, set to 0'
          lon1 = 0._dp
        ENDIF
        IF (ABS(360._dp - lon2) .LE. TINY_DP) THEN
          print *, 'lon2 = 360 deg east, set to 0'
          lon2 = 0._dp
        ENDIF
        !mz_hr_20071219-

        EXIT
      ENDIF
    ENDDO

    time_step_len_orig = time_step_len

    ! set to 1 during first get_physc_data, where model_time = model_start
    !   and only time_step_len needs to be updated

    !mz_hr_20081130+ HHH artificial values not needed anymore?
    !p_old  = -1._dp ! recognized as 'no previous value' in get_physc_data
    !T_old  = -1._dp 
    !mz_hr_20081130-

    !mz_hr_20071029+
    ! init physc and calculate cair b4 x0 is called by mecca_init
    !mz_hr_20081130+ interpolate to model_start
    !temp   = temp1
    !press  = press1
    !relhum = relhum1
    !degree_lat = lat1
    !degree_lon = lon1
    !x_j_no2 = jno21
    ratio2 = (model_start - time1)/(time2 - time1)
    ratio1 = 1._dp - ratio2
    temp       = ratio1 * temp1 + ratio2 * temp2
    press      = ratio1 * press1 + ratio2 * press2
    relhum     = ratio1 * relhum1 + ratio2 * relhum2
    degree_lat = ratio1 * lat1 + ratio2 * lat2
    IF ((lon2 - lon1) .LT. -180._dp) THEN
      ! crossing 0 meridian from West to East
      lontmp = lon2 + 360._dp
      degree_lon = ratio1 * lon1 + ratio2 * lontmp
      IF (degree_lon .GE. 360) degree_lon = degree_lon - 360._dp
    ELSEIF ((lon2 - lon1) .GT. 180._dp) THEN
    ! crossing 0 meridian from East to West
      lontmp = lon1 + 360._dp
      degree_lon = ratio1 * lontmp + ratio2 * lon2
      IF (degree_lon .GE. 360) degree_lon = degree_lon - 360._dp
    ELSE
      degree_lon = ratio1 * lon1 + ratio2 * lon2
    ENDIF
    IF (l_input_jval) THEN
      x_j_no2 = ratio1 * jno21   + ratio2 * jno22
    ENDIF
    !mz_hr_20081130-
    IF (l_spechum) THEN
      spechum = relhum
      relhum = spec2relhum(status, spechum, temp, press)
      IF (status/=0) THEN
        WRITE(*,*) 'Error get_model_start_end: spec2relhum'
        STOP
      ENDIF
    ELSE
      IF ((relhum >= 1.0_dp) .AND. (relhum <= 1.1_dp)) THEN
        WRITE(*,*) 'Warning get_model_start_end: relative humidity &
                    & >= 1 (< 1.1) : ', relhum
      ELSEIF (relhum < 0._dp) THEN
        WRITE(*,*) 'Error get_model_start_end: relative humidity < 0 : ', &
                    relhum
        STOP
      ELSEIF (relhum > 1.1_dp) THEN
        WRITE(*,*) 'Error get_model_start_end: relative humidity > 1.1 : ', &
                    relhum
        STOP
      ENDIF
    ENDIF

    !print *, "temp  = ", temp
    !print *, "press = ", press
    !mz_hr_20081130+
    cair = (N_A/1.E6) * press / (R_gas*temp) ! cair = c(air) in [molec/cm3]
    !cair = (N_A/1.E6) * press1 / (R_gas*temp1) ! cair = c(air) in [molec/cm3]
    !mz_hr_20081130-
    !mz_hr_20071029-

    !mz_hr_20081130+
    T_old = temp
    p_old = press
    !mz_hr_20081130-

  END SUBROUTINE get_model_start_end

  !***************************************************************************

  SUBROUTINE get_physc_data(z_ccorr)

    USE messy_main_constants_mem, ONLY: pi, TINY_DP
    USE messy_main_tools, ONLY: spec2relhum

    IMPLICIT NONE
    SAVE

    ! I/O
    REAL(dp), INTENT(OUT)   :: z_ccorr

    ! LOCAL
    INTEGER :: status

    print *, ''
    print *, 'get_data: model_time = ', model_time
    ! calculate time step for kpp
    IF (intpct*time_step_len_orig+model_start < time2) THEN
      l_next_trajp = .FALSE.
      time_step_len = intpct*time_step_len_orig+model_start - model_time
      print *, 'next point kpp, time_step_len = ', time_step_len
    ELSEIF (intpct*time_step_len_orig+model_start > time2) THEN
      l_next_trajp = .TRUE.
      time_step_len = time2 - model_time
      print *, 'next point traj, time_step_len = ', time_step_len
    ELSE
      l_next_trajp = .TRUE.
      time_step_len = time2 - model_time
      intpct = intpct + 1
      print *, 'next point traj + kpp, time_step_len = ', time_step_len
    ENDIF
    print *, 'get_data: model_time+time_step_len = ', model_time+time_step_len

    IF (l_next_trajp) THEN
      ! stop at next trajectory point
      temp       = temp2
      press      = press2
      relhum     = relhum2
      degree_lat = lat2
      degree_lon = lon2
      !mz_hr_20081130+
      IF (l_input_jval) THEN
      !mz_hr_20081130-
        x_j_no2    = jno22
      ENDIF

      ! end of trajectory not yet reached?
      !mz_hr_20081126+
      IF (trajpct >= len_time) THEN ! end of trajectory already reached
        WRITE(*,*) 'Info get_physc_data: end of trajectory reached, '
        WRITE(*,*) '  no shift in interpolation slots'
      !mz_hr_20081126-
      ELSE
        ! trajpt count up, shift in slots
        trajpct  = trajpct + 1
        time1   = time2
        temp1   = temp2
        press1  = press2
        relhum1 = relhum2
        lat1    = lat2
        lon1    = lon2
        !mz_hr_20080227+
        IF (l_input_jval) THEN !mz_hr_20081125 
          jno21   = jno22
        ENDIF
        !mz_hr_20080227-
        CALL nf(nf90_get_var(ncid_physc, varid_time, time2, &
          start = (/ trajpct /)))
        time2 = time2 * tuf
        CALL nf(nf90_get_var(ncid_physc, varid_temp, temp2, &
          start = (/ trajpct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_press, press2, &
          start = (/ trajpct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_relhum, relhum2, &
          start = (/ trajpct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lat, lat2, start = (/ trajpct /)))
        CALL nf(nf90_get_var(ncid_physc, varid_lon, lon2, start = (/ trajpct /)))
        !mz_hr_20071219+
        IF (ABS(360._dp - lon2) .LE. TINY_DP) THEN
          print *, 'lon2 = 360 deg east, set to 0'
          lon2 = 0._dp
        ENDIF
        !mz_hr_20071219-
        IF (l_input_jval) THEN !mz_hr_20080808 
          CALL nf(nf90_get_var(ncid_jval, varid_jno2, jno22, start = (/ trajpct /)))
        ENDIF
      ENDIF
      print *, 'time1 = ', time1, 'time2 = ', time2
    ELSE ! next regular integration point => interpolation needed
      intpct = intpct + 1
      ! calculate contribution of slot1 and slot2
      !mz_hr_20081201+
      ratio2 = ((model_time+time_step_len) - time1)/(time2 - time1)
      !ratio2 = (model_time - time1)/(time2 - time1)
      !mz_hr_20081201-
      ratio1 = 1._dp - ratio2
      print *, '(model_time+time_step_len) = ', model_time+time_step_len, 'time1 = ', time1, 'time2 = ', time2
      print *, 'ratio1 = ', ratio1, 'ratio2 = ', ratio2
       
      temp    = ratio1 * temp1   + ratio2 * temp2
      press   = ratio1 * press1  + ratio2 * press2
      relhum  = ratio1 * relhum1 + ratio2 * relhum2
      degree_lat = ratio1 * lat1    + ratio2 * lat2
      IF ((lon2 - lon1) .LT. -180._dp) THEN
      !mz_hr_20081117-
        ! crossing 0 meridian from West to East
        lontmp = lon2 + 360._dp
        degree_lon = ratio1 * lon1 + ratio2 * lontmp
        IF (degree_lon .GE. 360) degree_lon = degree_lon - 360._dp
      !mz_hr_20081117+
      ELSEIF ((lon2 - lon1) .GT. 180._dp) THEN
      !mz_hr_20081117-
      ! crossing 0 meridian from East to West
        lontmp = lon1 + 360._dp
        degree_lon = ratio1 * lontmp + ratio2 * lon2
        IF (degree_lon .GE. 360) degree_lon = degree_lon - 360._dp
      ELSE
        degree_lon = ratio1 * lon1 + ratio2 * lon2
      ENDIF
      !mz_hr_20081130+
      IF (l_input_jval) THEN
      !mz_hr_20081130-
        x_j_no2 = ratio1 * jno21   + ratio2 * jno22
      ENDIF
    ENDIF

    !mz_hr_20081130+
    IF (l_spechum) THEN ! spechum and relhum written to messy_traject.nc
      spechum = relhum
      relhum = spec2relhum(status, spechum, temp, press)
      IF (status/=0) THEN
        WRITE(*,*) 'Error get_physc_data: spec2relhum'
        STOP
      ENDIF
    ELSE
      IF ((relhum >= 1.0_dp) .AND. (relhum <= 1.1_dp)) THEN
        WRITE(*,*) 'Warning update_trajectory: relative humidity &
                    & >= 1 (< 1.1) : ', relhum
      ELSEIF (relhum < 0._dp) THEN
        WRITE(*,*) 'Error update_trajectory: relative humidity < 0 : ', &
                    relhum
        STOP
      ELSEIF (relhum > 1.1_dp) THEN
        WRITE(*,*) 'Error update_trajectory: relative humidity > 1.1 : ', &
                    relhum
        STOP
      ENDIF
    ENDIF
    !mz_hr_20081130-

    !mz_hr_20081130+ outdated
    !mz_hr_20071219+
    ! T_old and p_old are init with -1
    ! z_ccorr is 1 for 1st calculation
    !IF (T_old < 0._dp .AND. p_old < 0._dp) THEN
    !mz_hr_20071219-
    !  T_old = temp
    !  p_old = press
    !ENDIF
    !mz_hr_20081130-
    z_ccorr = (press * T_old)/(p_old * temp)
    p_old   = press
    T_old   = temp

  END SUBROUTINE get_physc_data

  !***************************************************************************

  SUBROUTINE traject_init

    USE caaba_mem, ONLY: input_physc, input_jval
    USE caaba_io,  ONLY: open_input_file

    IMPLICIT NONE

    INTRINSIC TRIM

    ! open external trajectory netcdf file
    CALL open_input_file(ncid_physc, input_physc)
    IF (l_input_jval) THEN !mz_hr_20080808
      CALL open_input_file(ncid_jval, input_jval)
    ENDIF

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
        !mz_hr_20090828+
        !CALL open_output_file(ncid_traj, 'caaba_traject',       &
        CALL open_output_file(ncid_traj, 'caaba_messy',       &
        !mz_hr_20090828-
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
        !mz_hr_20090828+
        !CALL open_output_file(ncid_traj, 'caaba_traject',       &
        CALL open_output_file(ncid_traj, 'caaba_messy',       &
        !mz_hr_20090828-
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
        !mz_hr_20090828+
        !CALL open_output_file(ncid_traj, 'caaba_traject',       &
        CALL open_output_file(ncid_traj, 'caaba_messy',       &
        !mz_hr_20090828-
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
        !mz_hr_20090828+
        !CALL open_output_file(ncid_traj, 'caaba_traject',       &
        CALL open_output_file(ncid_traj, 'caaba_messy',       &
        !mz_hr_20090828-
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
    IF (l_input_jval) THEN !mz_hr_20080808
      CALL close_file(ncid_jval)
    ENDIF

  END SUBROUTINE traject_finish

  !*****************************************************************************

  SUBROUTINE update_trajectory

    USE messy_main_tools,  ONLY: psat, psatf

    IMPLICIT NONE


    ! LOCAL
    REAL(dp) :: ccorr ! p, T changge -> concentration correction (cair2/cair1)
    !mz_hr_20081125 b4: ccorr = 0._dp

    ! get trajectory data, return correction factor for concentrations
    CALL get_physc_data(ccorr)
    !print *, 'update_trajectory: ccorr  = ', ccorr

    !zmbl    = 1000._dp     ! height of boundary layer [m] !mz_hr_20081121: see caaba.f90
    ! adjust concentrations to altered temperature and pressure
    C(:) = C(:) * ccorr
    cair = (N_A/1.E6) * press / (R_gas*temp) ! cair = c(air) in [mcl/cc]
    !mz_hr_20081130: c(ind_H2O) only calc in mecca_init/mecca_physc

  END SUBROUTINE update_trajectory

  !***************************************************************************

END MODULE messy_traject_box

!*****************************************************************************
