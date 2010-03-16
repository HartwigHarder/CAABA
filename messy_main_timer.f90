MODULE messy_main_timer

  ! MESSy
  USE messy_main_constants_mem, ONLY: dp, OneDay, STRLEN_ULONG

  IMPLICIT NONE
  PRIVATE
  SAVE

  CHARACTER(LEN=*), PUBLIC, PARAMETER :: modstr='timer'
  CHARACTER(LEN=*), PUBLIC, PARAMETER :: modver='0.1'

  ! CALENDER TYPE (per default Julian)
  INTEGER, PARAMETER, PUBLIC :: CAL_JULIAN = 0
  INTEGER, PARAMETER, PUBLIC :: CAL_360D   = 1
  !
  INTEGER, PUBLIC :: CAL_TYPE = 0

  CHARACTER(len=3), PARAMETER, PUBLIC :: CMONTHS(12) = &
       (/ 'Jan','Feb','Mar','Apr','May','Jun',&
          'Jul','Aug','Sep','Oct','Nov','Dec' /)

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! DATE MANAGEMENT
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  LOGICAL, PUBLIC :: LDEBUG2 = .FALSE.
  LOGICAL, PUBLIC :: LDEBUG  = .FALSE.
!  LOGICAL, PUBLIC :: LDEBUG  = .TRUE.

  TYPE, PUBLIC :: time_days 
     !
     ! relative calendar date and time format
     !
     ! time_days [structure]
     !   day    [integer]  (day in calendar, -2147483648 ..... 2147483647
     !                      approx. +/-5.8 Mio. years)
     !   second [integer]  (seconds of day, 0,...,86399)
     !
     !PRIVATE
     LOGICAL :: init   = .FALSE.
     INTEGER :: day    = 0
     INTEGER :: second = 0
  END TYPE time_days

  ! DEFINE SPECIFIC INFORMATION about SIMULATION STATE
  INTEGER, PUBLIC   :: INIT_STEP = 0
  REAL(dp), PUBLIC  :: delta_time    = -999._dp ! BM model time step
  REAL(dp), PUBLIC  :: time_step_len = 0._dp    ! leap frog BM model time step

  LOGICAL,PUBLIC    :: lstart  = .TRUE.   ! .TRUE. for the first time step
  LOGICAL,PUBLIC    :: lfirst_cycle = .TRUE. ! .TRUE. during first rerun cycle
  LOGICAL,PUBLIC    :: lresume = .FALSE.  ! .TRUE. during rerun step
  LOGICAL,PUBLIC    :: lbreak  = .FALSE.  ! .TRUE. at end of one time segment
  LOGICAL,PUBLIC    :: lstop   = .FALSE.  ! .TRUE. during the last time step

  LOGICAL,PUBLIC    :: l_rerun  = .FALSE.  ! 
  LOGICAL,PUBLIC    :: L_TRIGGER_RESTART = .FALSE.
  ! no_cycles: DUMMY needed for adjustment to ECHAM5 rerun structure
  INTEGER, PUBLIC :: NO_CYCLES = 9999
  LOGICAL, PUBLIC :: LABORT = .FALSE.

  INTEGER, PUBLIC :: YEAR,MONTH,DAY,HOUR,MINUTE,SECOND
  INTEGER, PUBLIC :: YEAR_START,MONTH_START,DAY_START &
           , HOUR_START,MINUTE_START,SECOND_START
  INTEGER, PUBLIC :: YEAR_NEXT,MONTH_NEXT,DAY_NEXT &
           , HOUR_NEXT,MINUTE_NEXT,SECOND_NEXT
  INTEGER, PUBLIC :: current_time_step

  INTEGER, PUBLIC :: JULIAN_DATE_START
  INTEGER, PUBLIC :: DAYOFYEAR        ! day of year [day] ! mz_ab_20080309

  ! DEFINE SPECIFIC DATE INFORMATION
  TYPE(time_days),PUBLIC  :: start_date         ! start date
  TYPE(time_days),PUBLIC  :: stop_date          ! stop date
  TYPE(time_days),PUBLIC  :: resume_date        ! rerun date

  TYPE(time_days),PUBLIC  ::  previous_date     ! date at (time - delta_time)
  TYPE(time_days),PUBLIC  ::   current_date     ! date at (time)
  TYPE(time_days),PUBLIC  ::      next_date     ! date at (time + delta_time)

  TYPE(time_days), PUBLIC :: rerun_stop_date  

  PUBLIC :: date_set
  PUBLIC :: date_get
  !PRIVATE :: date_get_components
  !PRIVATE :: date_set_components
  PUBLIC :: add_date
  PUBLIC :: copy_date
  PUBLIC :: if_less
  PUBLIC :: if_equal
  PUBLIC :: is_init
  PUBLIC :: print_date
  PUBLIC :: print_date_components
  ! -----------------------------------------------------------------
  ! HELPER ROUTINES FOR DATE CONVERSIONS / TIME DISTANCE CALCULATIONS
  ! -----------------------------------------------------------------
  !
  PUBLIC :: MonthLength         ! function to calculate the length of a year
  !                             ! in days
  PUBLIC :: JulianMonthLength   ! function to calculate the length of a 
  !                             ! julian month in days
  PUBLIC :: YearLength          ! function to calculate the length of a year
  !                             ! in days
  PUBLIC :: JulianYearLength    ! function to calculate the length of a 
  !                             ! julian year in days
  PUBLIC :: YearDay             ! function to calculate the current number of a 
  !                             ! day in the current year 
  PUBLIC :: julian_day          ! function to calculate the Julian day
  !
  PUBLIC :: time_span_s         ! time difference [s] between two greg.dates
  PUBLIC :: time_span_d         ! time difference [d] between two greg.dates
  !
  PUBLIC :: gregor2julian       ! convert gregorian date + time to julian date
  PUBLIC :: julian2gregor       ! convert julian date to gregorian date + time
  !                             ! humidity (WMO definition)
  PUBLIC :: utc2lt 
  PUBLIC :: eval_time_str       ! evaluate netcdf time string
  !
  ! --------------------------------------------------------------
  ! INTERFACE ROUTINES FOR TIMER EXTERNAL USE
  ! --------------------------------------------------------------
  ! +++ TRANSPORT NAMELIST INFORMATION IN BOTH DIRECTIONS
  INTERFACE timer_set_date
     MODULE PROCEDURE timer_set_date_str
     MODULE PROCEDURE timer_set_date_myd
     MODULE PROCEDURE timer_set_date_str_ds ! mz_ak_20090526
     MODULE PROCEDURE timer_set_date_myd_ds ! mz_ak_20090526
  END INTERFACE
  PUBLIC :: timer_set_date

  INTERFACE timer_get_date
     MODULE PROCEDURE timer_get_date_str
     MODULE PROCEDURE timer_get_date_myd
  END INTERFACE
  PUBLIC :: timer_get_date

  PUBLIC :: timer_set_calendar
  PUBLIC :: timer_get_calendar
  PUBLIC :: timer_set_delta_time
  PUBLIC :: timer_get_delta_time
  PUBLIC :: timer_set_lresume
  PUBLIC :: timer_get_lresume         ! mz_ak_20090723
  PUBLIC :: timer_set_no_cycles       ! mz_ak_20090602
  PUBLIC :: timer_get_no_cycles       ! mz_ak_20090723
  PUBLIC :: timer_set_labort
  PUBLIC :: timer_get_labort
  ! --- TRANSPORT NAMELIST INFORMATION IN BOTH DIRECTIONS

  PUBLIC :: timer_add_date
  PUBLIC :: timer_set_time_step_len   ! mz_pj_20090414

!!$  PUBLIC :: timer_read_nml_ctrl

CONTAINS

  !--------------------------------------------------------------------------

  SUBROUTINE date_set(day, second, time)
    
    INTEGER, INTENT(IN) :: day
    INTEGER, INTENT(IN) :: second
    TYPE(time_days), INTENT(OUT) :: time

    time %day    = day
    time %second = second
    time%init    =.TRUE.

  END SUBROUTINE date_set

  !--------------------------------------------------------------------------
  
  SUBROUTINE date_set_components(nyr,nmo,ndy,nhr,nmin,nsec,i_time)
    
    IMPLICIT NONE
    INTRINSIC :: AINT, INT

    INTEGER, INTENT(IN) :: nyr,nmo,ndy,nhr,nmin,nsec
    TYPE(time_days), INTENT(OUT) :: i_time
    REAL(dp) :: julday, julfrac, zsec, julianday
    INTEGER  :: kday, isec
    
    SELECT CASE(CAL_TYPE)
    CASE (CAL_JULIAN)
       julianday = gregor2julian(nyr,nmo,ndy,nhr,nmin,nsec)
       Julday = AINT(gregor2julian(nyr,nmo,ndy,nhr,nmin,nsec))
       Julfrac = julianday- Julday      

       IF (LDEBUG2) write (*,*) 'set', julday, julfrac
       IF (julday  < 0.0_dp) THEN
          kday = INT(julday -0.0001_dp)
          IF (julfrac < -0.5_dp)       kday = kday - 1
       ELSE 
          kday = INT(julday+0.0001_dp)
          IF (julfrac > 0.0_dp) THEN
             IF (.NOT.(julfrac < 0.5_dp)) kday = kday + 1
          ELSE IF (julfrac < -0.5_dp) THEN
             kday = kday - 1
          ELSE IF (.NOT.(julfrac < 0.5_dp)) THEN
             kday = kday + 1
          END IF
       END IF
       
       IF  (julfrac < -0.5_dp) THEN
          zsec = julfrac + 1.5_dp
       ELSE IF (julfrac < 0.5_dp) THEN
          zsec = julfrac + 0.5_dp
       ELSE
          zsec = julfrac - 0.5_dp
       END IF
       IF (LDEBUG2) write (*,*) 'set2', zsec
       isec = INT((zsec+0.00001_dp)*Oneday)
       
       i_time %day    = kday
       i_time %second = isec
       IF (LDEBUG2) write (*,*) 'set3', zsec, isec, kday

    CASE (CAL_360D)

       i_time %second = nhr*3600 + nmo*60 + nsec
       i_time %day    = 360*nyr + (nmo-1)*30 + (ndy-1)
       
    END SELECT
    
    i_time %init   = .TRUE.
    
  END SUBROUTINE date_set_components

  !==========================================================================

  SUBROUTINE date_get(time, day, second, ierr)

    IMPLICIT NONE
    INTRINSIC :: PRESENT
    
    TYPE(time_days), INTENT(in)    :: time
    INTEGER, OPTIONAL, INTENT(OUT) :: day
    INTEGER, OPTIONAL, INTENT(OUT) :: second
    INTEGER, OPTIONAL, INTENT(OUT) :: ierr

    IF (.NOT. time%init) THEN
       IF (PRESENT(ierr)) ierr = 3435
       RETURN
    END IF

    IF (PRESENT(day))      day  = time%day
    IF (PRESENT(second)) second = time%second

    IF (PRESENT(ierr))     ierr = 0

  END SUBROUTINE date_get

  !==========================================================================

  SUBROUTINE date_get_components(time, year, month, day, hour&
       , minute, second, ierror)

    IMPLICIT NONE
    INTRINSIC :: AINT, INT, MOD, PRESENT, REAL

    TYPE (time_days)  ,INTENT(in)   :: time
    INTEGER           ,INTENT(out)  :: year, month, day, hour                   
    INTEGER ,OPTIONAL ,INTENT(out)  :: minute, second
    INTEGER, OPTIONAL, INTENT(out)  :: ierror

    REAL(dp)   :: juldate, julfrac, julday
    INTEGER    :: mn,se,rest, iday
    REAL(dp)   :: zsecs

    IF (.NOT.time%init) THEN
       IF (PRESENT(ierror)) ierror = 3435
       RETURN
    ENDIF

    SELECT CASE(CAL_TYPE)
    CASE (CAL_JULIAN)
      ! remapping of julian day (adjustment at 12UTC) 
      ! and day with adjustment at 00UTC
      !
      zsecs = REAL(time %second,dp)
      iday  = time %day
      julfrac = (zsecs+0.000001_dp)/Oneday - 0.5_dp
      
      IF ((time %day < 0).AND. (zsecs > 0.5_dp)) THEN
        iday     = iday + 1
        julfrac = zsecs - 1.5_dp
      ELSE IF ((time%day > 0) .AND. (zsecs < 0.5_dp)) THEN
        iday     = iday - 1
        julfrac  = zsecs + 0.5_dp
      END IF
      IF (iday < 0) THEN
        julday = AINT(REAL(iday,dp)-0.0001_dp)
      ELSE
        julday = AINT(REAL(iday,dp)+0.0001_dp)
      END IF

      IF (LDEBUG2) write (*,*) 'get1', time%day, time%second, julday, julfrac
      juldate = julday + julfrac
      IF (LDEBUG2) write (*,*) 'get2',juldate
      CALL julian2gregor(juldate,year, month, day, hour, mn, se)
      IF (LDEBUG2) write (*,*) 'get', year, month, day, hour, mn, se
   CASE (CAL_360D)

       IF (time%day < 0) THEN
          year = INT(time%day/360-1)
          rest  = MOD(time%day,360)
          month = 12-rest/30
          day = 30-(MOD(rest,30)+1)
       ELSE
          year  = time%day/360
          rest  = MOD(time%day, 360)
          month = rest/30+1
          day   = MOD(rest,30)+1
       ENDIF
      
       hour = time%second/3600
       mn = MOD(time%second,3600)/60
       se = MOD(time%second,60)
      
    END SELECT
   
    IF (PRESENT(minute)) minute = mn
    IF (PRESENT(second)) second = se

    IF (PRESENT(ierror)) ierror = 0

  END SUBROUTINE date_get_components

  !==========================================================================

  SUBROUTINE add_date (days, seconds, my_day, ierr)

  ! add a number of days and seconds to a given date and time
  ! adopted from mo_time_conversion (ECHAM5)

    IMPLICIT NONE
    INTRINSIC :: INT, REAL, PRESENT

    INTEGER,          INTENT(in)    :: days, seconds
    TYPE (time_days), INTENT(inout) :: my_day
    INTEGER, OPTIONAL, INTENT(out)  :: ierr

    !CHARACTER(LEN=*), PARAMETER :: substr='main_timer: add_date'
    INTEGER   :: idays, isecs

    IF (PRESENT(ierr)) ierr=0

    IF (.NOT. my_day%init) THEN
       IF (PRESENT(ierr)) ierr=3435
       RETURN
    ENDIF

    isecs = seconds + my_day%second
    IF (isecs < 0) THEN
       idays = INT((REAL(isecs,dp)-0.001_dp)/OneDay)
    ELSE
       idays = INT((REAL(isecs,dp)+0.001_dp)/OneDay)
    END IF
    isecs = isecs - idays*INT(OneDay)
    idays = my_day%day + days + idays

    IF (isecs < 0) THEN
       isecs = INT(OneDay) + isecs
       idays = idays - 1
    END IF

    my_day %day    = idays
    my_day %second = isecs

  END SUBROUTINE add_date

  !--------------------------------------------------------------------------

    SUBROUTINE copy_date(date1,date2, ierr)

      IMPLICIT NONE
      INTRINSIC :: PRESENT
      
      TYPE(time_days), INTENT(IN)  :: date1 
      TYPE(time_days), INTENT(OUT) :: date2
      INTEGER, OPTIONAL, INTENT(OUT) :: ierr
     
      IF (PRESENT(ierr)) ierr = 0
      
      IF (.NOT. date1%init) THEN
         IF (PRESENT(ierr)) ierr = 3435
         RETURN
      ENDIF

      date2%day   =date1%day
      date2%second=date1%second
      date2%init  = .TRUE.

    END SUBROUTINE copy_date

   !-------------------------------------------------------------
    
    SUBROUTINE print_date (day,ierr, mess)

      IMPLICIT NONE
      INTRINSIC :: PRESENT, TRIM

      ! print out the date/time information
      TYPE (time_days),           INTENT(in)  :: day
      INTEGER,                    INTENT(OUT) :: ierr
      CHARACTER(len=*), OPTIONAL, INTENT(out) :: mess
      
      ! LOCAL
      !CHARACTER(LEN=*), PARAMETER :: substr='main_timer print_date'
      CHARACTER(LEN=STRLEN_ULONG) :: message_text
      INTEGER                     :: iday, isec

      CALL date_get(day,iday,isec,ierr)
      IF (ierr /= 0) RETURN

      SELECT CASE(CAL_TYPE)
      CASE (CAL_JULIAN)      
         WRITE(message_text,'(a,i8,a,i8)') &
              'modified Julian day (00 UT adjusted): ', &
              iday,' seconds: ', isec
      CASE (CAL_360D)
         WRITE(message_text,'(a,i8,a,i8)') &
              '360 day year day (00 UT based): ', &
              iday,' seconds: ', isec
      END SELECT
      
      IF (PRESENT(mess)) THEN
         mess = TRIM(message_text)
      ELSE
         WRITE(*,*) message_text
      END IF
      
      ierr = 0

  END SUBROUTINE print_date

 !-----------------------------------------------------------

  SUBROUTINE print_date_components (day, ierr, mess)

    IMPLICIT NONE
    INTRINSIC :: PRESENT, TRIM

    TYPE (time_days),           INTENT(IN)   :: day
    INTEGER,                    INTENT(OUT)  :: ierr
    CHARACTER(len=STRLEN_ULONG), OPTIONAL, INTENT(OUT) :: mess

    ! LOCAL
    !CHARACTER(LEN=*), PARAMETER :: substr='main_timer print_date'
    CHARACTER(LEN=STRLEN_ULONG) :: message_text
    INTEGER                     :: yr,mo,dy,hr,mn,se

    ierr = 0
    
    CALL date_get_components(day,yr,mo,dy,hr,mn,se,ierr)
    IF (ierr /= 0) RETURN
    
    WRITE(message_text,'(i2,a,a3,a,i8,3(a,i2.2))') &
         dy,'. ',CMONTHS(mo),' ',yr,' ',hr,':',mn,':',se

    IF (PRESENT(mess)) THEN
       mess = TRIM(message_text)
    ELSE
       write (*,*) TRIM(message_text)
    END IF
    
    ierr = 0

  END SUBROUTINE print_date_components

   !-------------------------------------------------------------------

    SUBROUTINE if_less (date1, date2, lless, ierr)
      
      IMPLICIT NONE
      INTRINSIC :: PRESENT

      TYPE (time_days), INTENT(IN)   :: date1
      TYPE (time_days), INTENT(IN)   :: date2
      LOGICAL, INTENT(OUT)           :: lless
      INTEGER, OPTIONAL, INTENT(OUT) :: ierr
      
      IF ((.NOT.date1%init) .OR. (.NOT.date2%init)) THEN
         IF (PRESENT(ierr)) ierr =3435
         RETURN
      ENDIF
      
      IF ( (date1%day == date2%day .AND. date1%second >= date2%second) &
         .OR. (date1%day > date2%day) ) THEN
         lless = .FALSE.
      ELSE
         lless = .TRUE.
      END IF
      
      IF (PRESENT(ierr)) ierr = 0

    END SUBROUTINE if_less

    !-------------------------------------------------------------------

    SUBROUTINE if_equal (date1, date2, leq, ierr)
      
      IMPLICIT NONE
      INTRINSIC :: PRESENT

      TYPE (time_days), INTENT(IN)   :: date1
      TYPE (time_days), INTENT(IN)   :: date2
      LOGICAL, INTENT(OUT)           :: leq
      INTEGER, OPTIONAL, INTENT(OUT) :: ierr
      
      IF ((.NOT.date1%init) .OR. (.NOT.date2%init)) THEN
         IF (PRESENT(ierr)) ierr =3435
         RETURN
      ENDIF
      
      IF ( (date1%day == date2%day) .AND. (date1%second == date2%second)) THEN
         leq =.TRUE.
      ELSE
         leq = .FALSE.
      END IF
      
      IF (PRESENT(ierr)) ierr = 0

    END SUBROUTINE if_equal

    !-------------------------------------------------------------------------

    SUBROUTINE is_init(date,linit)
      
      TYPE(time_days), INTENT(IN) :: date
      LOGICAL, INTENT(OUT)        :: linit

      linit = date%init

    END SUBROUTINE is_init

 ! ---------------------------------------------------------------------
 ! HELPER ROUTINES FOR DATE CONVERSIONS / TIME DISTANCE CALCULATIONS
 ! ---------------------------------------------------------------------

 ! ---------------------------------------------------------------------  
    INTEGER FUNCTION MonthLength(ky, km)

      IMPLICIT NONE

      INTEGER, INTENT(IN) :: ky, km

      SELECT CASE(CAL_TYPE)
      CASE(CAL_JULIAN)
         MonthLength = JulianMonthLength(ky, km)
      CASE(CAL_360D)
         MonthLength = 30
      CASE DEFAULT
         MonthLength = 0
      END SELECT

    END FUNCTION MonthLength
    ! -----------------------------------------------------------------------  

 ! ---------------------------------------------------------------------  
    INTEGER FUNCTION JulianMonthLength(ky, km)
      !+
      !
      ! Get_JulianMonLen [function, integer]
      !    get the length of a months in a Julian year
      !    (
      !    year  [integer] input (Calendar year)
      !    month [integer] input (month of the year)
      !    )
      !
      !-
      IMPLICIT NONE
      INTRINSIC :: MOD

      INTEGER, INTENT(in) :: km, ky

      INTEGER :: idmax

      SELECT CASE(km)
      CASE(1,3,5,7,8,10,12);  idmax = 31
      CASE(4,6,9,11);                  idmax = 30
      CASE(2)
         IF ( (MOD(ky,4)==0 .AND. MOD(ky,100)/=0) .OR. MOD(ky,400)==0 ) THEN
            ! leap year found
            idmax = 29
         ELSE
            idmax = 28
         END IF

      CASE DEFAULT
         idmax = 0

      END SELECT
      JulianMonthLength = idmax

    END FUNCTION JulianMonthLength
    ! -----------------------------------------------------------------------  

    ! -----------------------------------------------------------------------  
    INTEGER FUNCTION YearLength(yr)

      INTEGER, OPTIONAL, INTENT(IN) :: yr

         SELECT CASE(CAL_TYPE)
         CASE(CAL_JULIAN)
            IF (PRESENT(yr)) THEN
               YearLength= JulianYearLength(yr)
            ELSE
               YearLength= 365.2422_dp
            ENDIF
         CASE(CAL_360D)
               YearLength=360
         END SELECT

    END FUNCTION YearLength
    ! -----------------------------------------------------------------------  

    ! -----------------------------------------------------------------------  
    INTEGER FUNCTION JulianYearLength(yr)

      INTEGER, INTENT(IN) :: yr

      IF (yr == 1582) THEN
         JulianYearLength = 355
      ELSE IF ( (MOD(yr,4)==0 .AND. MOD(yr,100)/=0) .OR. MOD(yr,400)==0 ) THEN
         JulianYearLength = 366
      ELSE
         JulianYearLength = 365
      END IF

    END FUNCTION JulianYearLength
    ! -----------------------------------------------------------------------  

    ! -----------------------------------------------------------------------
    FUNCTION YearDay (date) RESULT (dayno)   

    ! returns the day of the year for a given date
    ! the seconds of the day are contained fractional

    TYPE(time_days) :: date    ! evaluate for this date
    REAL(dp)        :: dayno

    ! LOCAL
    INTEGER         :: yr, mo, dy, hr, mi, se
    INTEGER         :: nseconds, day, day01, day_diff
    REAL(DP)        :: frac
    INTEGER         :: status

    CALL timer_get_date(status,date,yr,mo,dy,hr,mi,se)
    nseconds = 3600*hr+ 60* mi + se

    SELECT CASE (CAL_TYPE)
    CASE (CAL_JULIAN)
      ! Julian date for current date
      day      = Julian_day(REAL(dy,dp),mo,yr)
      ! Julian date for 1st January (same year)
      day01    = Julian_day(REAL(1,dp),01,yr)
      day_diff = day-day01 +1 
      frac = (REAL(nseconds,dp)+0.000001_dp)/OneDay
      dayno = REAL(day_diff,dp) + frac
    CASE (CAL_360D)
      day   = 360*yr+(mo-1)*30+(dy-1)
      frac  = REAL(nseconds,dp)/OneDay
      dayno = REAL(day,dp) + frac
    END SELECT

  END FUNCTION YearDay
  ! ----------------------------------------------------------------------------

  ! -----------------------------------------------------------------------
  FUNCTION julian_day(DD, MM, YY)
    
    !
    ! [x] = the greatest integer that does not exceed x.
    !       For example, [-1.5]=-2. This is sometimes called the floor
    !       function (for example in C/C++). 
    ! INT(x) = [x] NOTE: some computer languages have a different definition.
    ! FIX(x) = the number x without its fraction. For example, FIX(-1.5)=-1 
    ! x\y    = FIX(x/y) 
    !
    ! 2.3.1 Gregorian Date to Julian Day Number
    ! For a Gregorian Date specified as day D (a real number),
    ! month M (an integer with January = 1), and year Y (any integer),
    ! the Julian Day Number JD can be calculated as follows: 
    ! IF M < 3 THEN 
    !       M = M + 12 
    !       Y=Y-1 
    ! END IF 
    ! JD = D + (153 * M - 457) \ 5 + 365 * Y + [Y / 4] - [Y / 100] + 
    !      [Y / 400] + 1721118.5
    !
    
    IMPLICIT NONE
    
    INTRINSIC :: FLOOR, REAL, INT
    
    ! I/O
    REAL(DP)             :: julian_day
    REAL(dp), INTENT(IN) :: DD
    INTEGER,  INTENT(IN) :: MM, YY
    
    ! LOCAL
      REAL(dp) :: D
      INTEGER  :: M, Y
      
      D = DD
      IF (MM < 3) THEN
         M = MM + 12
         Y = YY - 1
      ELSE
         M = MM
         Y = YY
      END IF
      
      julian_day = D &
           + INT(REAL((153 * M - 457), DP) / 5.0_DP) &
           + 365.0_DP * REAL(Y, DP) &
           + FLOOR(REAL(Y, DP) / 4.0_DP) &
           - FLOOR(REAl(Y, DP) / 100.0_DP) &
           + FLOOR(REAL(Y, DP) / 400.0_DP) &
           + 1721118.5_DP
      
    END FUNCTION julian_day
    ! ------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    SUBROUTINE time_span_s(dts           &
         , yy1, mo1, dy1, hr1, mi1, se1  &
         , yy2, mo2, dy2, hr2, mi2, se2 )


      IMPLICIT NONE

      INTRINSIC :: INT

      ! I/O
      INTEGER, INTENT(OUT) :: dts ! dime span [s]
      INTEGER, INTENT(IN)  :: yy1, mo1, dy1, hr1, mi1, se1
      INTEGER, INTENT(IN)  :: yy2, mo2, dy2, hr2, mi2, se2

      ! LOCAL
      REAL(dp) :: day_1, day_2

      day_1 = gregor2julian(yy1, mo1, dy1, hr1, mi1, se1)
      day_2 = gregor2julian(yy2, mo2, dy2, hr2, mi2, se2)

      dts = NINT(86400.0_dp * (day_2 - day_1))

    END SUBROUTINE time_span_s
    !-------------------------------------------------------------------------

! mz_pj_20090519+
    !-------------------------------------------------------------------------
    SUBROUTINE time_span_d(dtd           &
         , yy1, mo1, dy1, hr1, mi1, se1  &
         , yy2, mo2, dy2, hr2, mi2, se2 )


      IMPLICIT NONE

      INTRINSIC :: INT

      ! I/O
      REAL(dp), INTENT(OUT) :: dtd ! dime span [d]
      INTEGER,  INTENT(IN)  :: yy1, mo1, dy1, hr1, mi1, se1
      INTEGER,  INTENT(IN)  :: yy2, mo2, dy2, hr2, mi2, se2

      ! LOCAL
      REAL(dp) :: day_1, day_2

      day_1 = gregor2julian(yy1, mo1, dy1, hr1, mi1, se1)
      day_2 = gregor2julian(yy2, mo2, dy2, hr2, mi2, se2)

      dtd = (day_2 - day_1)

    END SUBROUTINE time_span_d
    !-------------------------------------------------------------------------
! mz_pj_20090519-

    ! ------------------------------------------------------------------------ 
    FUNCTION gregor2julian(YY, MM, DD, hr, mi, se) RESULT(julian_date)

      ! calculates Julian day and Julian date, output: Julian date
      ! [x]    = The greatest integer that does not exceed x, e.g., [-1.5]=-2.
      !          This is sometimes called the floor function, e.g., in C/C++.
      ! INT(x) = [x] NOTE: some computer languages have a different definition.
      !          HERE: INT = FIX
      ! FIX(x) = the number x without its fraction, e.g., FIX(-1.5)=-1.
      !          HERE: FIX = INT
      ! x\y    = FIX(x/y)
      !
      ! Gregorian Date to Julian Day Number
      ! The Julian Day Count is a uniform count of days from a remote epoch
      ! in the past:
      !  -4712     January 1,   12 hours Greenwich Mean Time 
      !                                  (Julian proleptic Calendar)
      ! = 4713 BCE January 1,   12 hours GMT                 
      !                                  (Julian proleptic Calendar)
      ! = 4714 BCE November 24, 12 hours GMT                 
      !                                  (Gregorian proleptic Calendar)).
      ! At this instant, the Julian Day Number is 0.
      ! see also: http://aa.usno.navy.mil/data/docs/JulianDate.html
      !
      ! For a Gregorian Date specified as
      !   day D   (a real number),
      !   month M (an integer with January = 1), and
      !   year Y  (any integer),
      ! the Julian Day Number JD can be calculated as follows:
      ! IF M < 3 THEN
      !   M = M + 12
      !   Y=Y-1
      ! END IF
      ! JD= D + (153*M - 457)\5 + 365*Y + [Y/4] - [Y/100] + [Y/400] + 1721118.5

      IMPLICIT NONE

      INTRINSIC :: FLOOR, REAL, INT

      ! I/O
      REAL(DP)             :: julian_day, julian_date
      INTEGER,  INTENT(IN) :: DD, MM, YY, hr, mi, se

      ! LOCAL
      INTEGER  :: D, M, Y

      D = DD
      IF (MM < 3) THEN
         M = MM + 12
         Y = YY - 1
      ELSE
         M = MM
         Y = YY
      END IF

      julian_day  = D &
           + INT(REAL((153 * M - 457), DP) / 5.0_DP) &
           + 365.0_DP * REAL(Y, DP) &
           + FLOOR(REAL(Y, DP) / 4.0_DP) &
           - FLOOR(REAL(Y, DP) / 100.0_DP) &
           + FLOOR(REAL(Y, DP) / 400.0_DP) &
           + 1721118.5_DP

      julian_date = REAL(julian_day, dp)                  &
           + REAL(hr, DP) / 24.0_DP                & ! hour day fraction
           + REAL(mi, DP) / (60.0_DP * 24.0_DP)    & ! minute day fraction
           ! seconds day fraction
      + REAL(se, DP) / (60.0_DP * 60.0_DP * 24.0_DP)  

    END FUNCTION gregor2julian
    ! ---------------------------------------------------------------------  

    ! ---------------------------------------------------------------------  
    SUBROUTINE julian2gregor(jdate, year, month, day, hrs, mins, secs)

      ! Here julian is input as a Julian Date, and the routine outputs
      ! iyyy (year), mm (month), id (day), hh (hour), min (minute), and
      ! ss (second) of the Gregorian date
      !
      ! adapted from 'Numerical Recipes in Fortran'

      IMPLICIT NONE
      INTRINSIC :: INT, REAL

      ! I/O
      INTEGER, INTENT(OUT) :: year, month, day, hrs, mins, secs
      REAL(dp)             :: jdate

      ! LOCAL
      INTEGER, PARAMETER :: IGREG = 2299161
      INTEGER            :: julian, ja,alpha,jb,jc,jd,je
      REAL(dp)           :: fract

      ! add 0.5 to julian date to make the decimals 0 at midnight, not noon
      fract  = jdate + 0.5_dp
      julian = INT(fract)

      ! treat fraction to calculate time
      fract = fract - REAL(julian,dp)  ! day fraction

      fract = fract * 24.0_dp ! hour fraction
      hrs   = INT(fract)

      fract = (fract - hrs) * 60.0_dp
      mins  = INT(fract)

      fract = (fract - mins) * 60.0_dp 
      secs  = INT(fract)

      fract = fract - REAL(INT(fract),dp)
      IF (fract >= 0.5_dp) THEN
         secs = secs + 1
      END IF
      IF (secs >= 60) THEN
         mins = mins + 1
         secs = secs - 60
      END IF
      IF (mins >= 60) THEN
         hrs  = hrs + 1
         mins = mins - 60
      END IF

      ! treat integer julian day number for date
      IF (julian >= IGREG) THEN 
         ! correction because of cross-over to Gregorian Calendar in 1582
         alpha = int( ( (julian-1867216) - 0.25_dp) / 36524.25_dp )
         ja    = julian + 1 + alpha - int(0.25_dp*alpha)
      ELSE IF (julian < 0) THEN 
         ! make day number pos by adding int number of Julian centuries,
         ! subtract off at the end
         ja = julian + 36525 * ( 1 - julian/36525 )
      ELSE
         ja = julian
      END IF

      jb = ja + 1524
      jc = int(6680.0_dp + ( (jb-2439870) - 122.1_dp) / 365.25_dp)
      jd = 365 * jc + int(0.25_dp*jc)
      je = int( (jb-jd) / 30.6001_dp )

      day   = jb - jd - int(30.6001_dp*je)

      month = je - 1
      IF (month > 12) month = month - 12

      year = jc - 4715
      IF (month > 2)  year = year - 1
      IF (year <= 0)  year = year - 1
      if (julian < 0) year = year - 100 * ( 1 - julian / 36525)

    END SUBROUTINE julian2gregor
    ! ---------------------------------------------------------------------  

    ! ---------------------------------------------------------------------
    FUNCTION utc2lt(status, model_time, degree_lon)

      USE messy_main_constants_mem, ONLY: OneDay

      IMPLICIT NONE

      ! I/O
      REAL(dp)              :: utc2lt
      INTEGER,  INTENT(OUT) :: status
      REAL(dp), INTENT(IN)  :: model_time
      REAL(dp), INTENT(IN)  :: degree_lon
      ! LOCAL
      REAL(dp) :: dateback

      status = 0

      IF ( (degree_lon < 0.0_dp) .OR. (degree_lon >= 360.0_dp) ) THEN
         WRITE(*,*) 'ERROR in utc2lt: Longitude out of bounds (lon = ', &
              degree_lon, ')!'
         status = 1
         RETURN
      ELSE IF (degree_lon <= 180.0_dp) THEN
         dateback = 0._dp
      ELSE IF (degree_lon > 180.0_dp) THEN
         dateback = 1._dp
      ENDIF

      ! [s]       [s]           [°]    [°/day]                [s/day]
      utc2lt = model_time + (degree_lon/360.0_dp - dateback) * OneDay

    END FUNCTION utc2lt
    ! ---------------------------------------------------------------------

  ! ------------------------------------------------------------------
  ! ROUTINES FOR TIMER EXTERNAL USE
  ! ------------------------------------------------------------------

  SUBROUTINE timer_set_date_str(status, strflag, yr, mo, dy, hr, mi, se)

    IMPLICIT NONE

    ! I/O
    INTEGER,          INTENT(OUT) :: status
    CHARACTER(LEN=*), INTENT(IN)  :: strflag
    INTEGER,          INTENT(IN)  :: yr, mo, dy, hr, mi, se


    SELECT CASE(strflag)
    CASE('start')
       CALL date_set_components(yr, mo, dy, hr, mi, se, start_date)

       ! SET DATE COMPONENTS
       YEAR_START   = yr
       MONTH_START  = mo
       DAY_START    = dy
       HOUR_START   = hr
       MINUTE_START = mi
       SECOND_START = se

       ! SET JULIAN START DATE
       JULIAN_DATE_START = &
            INT(gregor2julian( YEAR_START,MONTH_START, DAY_START &
            , HOUR_START,MINUTE_START,SECOND_START ))

    CASE('previous')
       CALL date_set_components(yr, mo, dy, hr, mi, se, previous_date)
    CASE('current')
       CALL date_set_components(yr, mo, dy, hr, mi, se, current_date)
    CASE('next')
       CALL date_set_components(yr, mo, dy, hr, mi, se, next_date)
    CASE('stop')
       CALL date_set_components(yr, mo, dy, hr, mi, se, stop_date)
    CASE('resume')
       CALL date_set_components(yr, mo, dy, hr, mi, se, resume_date)
!!$    CASE('rerun_stop')
!!$       CALL date_set_components(yr, mo, dy, hr, mi, se, rerun_stop_date)
    CASE DEFAULT
       status = 3443 ! unknown date 
       RETURN
    END SELECT

    status = 0
    
  END SUBROUTINE timer_set_date_str
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_set_date_myd(status, my_date, yr, mo, dy, hr, mi, se)

    IMPLICIT NONE

    ! I/O
    INTEGER,          INTENT(OUT) :: status
    TYPE(time_days),  INTENT(OUT) :: my_date
    INTEGER,          INTENT(IN)  :: yr, mo, dy, hr, mi, se


    CALL date_set_components(yr, mo, dy, hr, mi, se, my_date)
    status = 0

  END SUBROUTINE timer_set_date_myd
  ! ---------------------------------------------------------------------------

! mz_ak_20090526+
  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_set_date_str_ds(status, strflag, day, second)

    IMPLICIT NONE

    ! I/O
    INTEGER,          INTENT(OUT) :: status
    CHARACTER(LEN=*), INTENT(IN)  :: strflag
    INTEGER,          INTENT(IN)  :: day, second

    ! LOCAL 
    INTEGER  ::  yr, mo, dy, hr, mi, se

    SELECT CASE(strflag)
    CASE('start')
       CALL date_set(day, second, start_date)
       CALL date_get_components(start_date, yr, mo, dy, hr, mi, se, status)

       ! SET DATE COMPONENTS
       YEAR_START   = yr
       MONTH_START  = mo
       DAY_START    = dy
       HOUR_START   = hr
       MINUTE_START = mi
       SECOND_START = se

       ! SET JULIAN START DATE
       JULIAN_DATE_START = &
            INT(gregor2julian( YEAR_START,MONTH_START, DAY_START &
            , HOUR_START,MINUTE_START,SECOND_START ))

    CASE('previous')
       CALL date_set(day, second, previous_date)
    CASE('current')
       CALL date_set(day, second, current_date)
    CASE('next')
       CALL date_set(day, second, next_date)
    CASE('stop')
       CALL date_set(day, second, stop_date)
    CASE('resume')
       CALL date_set(day, second, resume_date)
!!$    CASE('rerun_stop')
!!$       CALL date_set_components(yr, mo, dy, hr, mi, se, rerun_stop_date)
    CASE DEFAULT
       status = 3443 ! unknown date 
       RETURN
    END SELECT

    status = 0
    
  END SUBROUTINE timer_set_date_str_ds
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_set_date_myd_ds(status, my_date, day, second)

    IMPLICIT NONE

    ! I/O
    INTEGER,          INTENT(OUT) :: status
    TYPE(time_days),  INTENT(OUT) :: my_date
    INTEGER,          INTENT(IN)  :: day, second


    CALL date_set(day, second, my_date)
    status = 0

  END SUBROUTINE timer_set_date_myd_ds
  ! ---------------------------------------------------------------------------
! mz_ak_20090526-

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_get_date_str(status, strflag, yr, mo, dy, hr, mi, se)

    IMPLICIT NONE

    ! I/O
    INTEGER,          INTENT(OUT) :: status
    CHARACTER(LEN=*), INTENT(IN)  :: strflag
    INTEGER,          INTENT(OUT) :: yr, mo, dy, hr, mi, se


    SELECT CASE(strflag)
    CASE('start')
       CALL date_get_components(start_date, yr, mo, dy, hr, mi, se, status)
    CASE('previous')
       CALL date_get_components(previous_date, yr, mo, dy, hr, mi, se, status)
    CASE('current')
       CALL date_get_components(current_date, yr, mo, dy, hr, mi, se, status)
    CASE('next')
       CALL date_get_components(next_date, yr, mo, dy, hr, mi, se, status)
    CASE('stop')
       CALL date_get_components(stop_date, yr, mo, dy, hr, mi, se, status)
    CASE('resume')
       CALL date_get_components(resume_date, yr, mo, dy, hr, mi, se, status)
    CASE('rerun_stop')
       CALL date_get_components(rerun_stop_date, yr, mo, dy, hr, mi, se &
            , status)
    CASE DEFAULT
       status = 3443 ! unknown date 
       RETURN
    END SELECT
    
    status = 0

  END SUBROUTINE timer_get_date_str
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_get_date_myd(status, my_date, yr, mo, dy, hr, mi, se)

    IMPLICIT NONE

    ! I/O
    INTEGER,          INTENT(OUT) :: status
    TYPE(time_days),  INTENT(IN)  :: my_date
    INTEGER,          INTENT(OUT) :: yr, mo, dy, hr, mi, se

    CALL date_get_components(my_date, yr, mo, dy, hr, mi, se, status)

  END SUBROUTINE timer_get_date_myd
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_set_calendar(status, strcal)

    IMPLICIT NONE

    ! I/O
    INTEGER,          INTENT(OUT) :: status
    CHARACTER(LEN=*), INTENT(IN)  :: strcal    

    SELECT CASE(strcal)
       CASE('julian')
          CAL_TYPE = CAL_JULIAN
       CASE('days360')
          CAL_TYPE = CAL_360D
       CASE DEFAULT
          status = 3444 ! unknown calendar type
          RETURN
    END SELECT

    status = 0

  END SUBROUTINE timer_set_calendar
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_get_calendar(status, ical)

    IMPLICIT NONE

    ! I/O
    INTEGER, INTENT(OUT) :: status
    INTEGER, INTENT(OUT) :: ical    

    ical = CAL_TYPE
    status = 0

  END SUBROUTINE timer_get_calendar
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_set_delta_time(status, dt)

    IMPLICIT NONE

    ! I/O
    INTEGER,  INTENT(OUT) :: status
    REAL(dp), INTENT(IN)  :: dt

    delta_time = dt
    
    status = 0

  END SUBROUTINE timer_set_delta_time
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_get_delta_time(status, dt)

    IMPLICIT NONE

    ! I/O
    INTEGER,  INTENT(OUT) :: status
    REAL(dp), INTENT(OUT)  :: dt

    dt = delta_time
    
    status = 0

  END SUBROUTINE timer_get_delta_time
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_add_date(status, add_seconds, iyr, imo, idy, ihr, imi, ise &
       , oyr, omo, ody, ohr, omi, ose)

    IMPLICIT NONE

    ! I/O
    INTEGER,  INTENT(OUT) :: status
    INTEGER,  INTENT(IN)  :: add_seconds ! seconds to add to date
    INTEGER,  INTENT(IN)  :: iyr, imo, idy, ihr, imi, ise
    INTEGER,  INTENT(OUT) :: oyr, omo, ody, ohr, omi, ose

    ! LOCAL
    TYPE(time_days)       :: my_date

    ! calculate date
    CALL date_set_components(iyr, imo, idy, ihr, imi, ise, my_date)

    CALL add_date(0, add_seconds, my_date, status)
    IF (status /= 0) RETURN

    CALL date_get_components(my_date, oyr, omo, ody, ohr, omi, ose)

    status = 0

  END SUBROUTINE timer_add_date
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_set_lresume

    IMPLICIT NONE

    lresume = .TRUE.

  END SUBROUTINE timer_set_lresume
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  SUBROUTINE timer_get_lresume(lr)

    IMPLICIT NONE

    LOGICAL, INTENT(OUT) :: lr

    lr = lresume

  END SUBROUTINE timer_get_lresume
  ! ---------------------------------------------------------------------------

  ! ---------------------------------------------------------------------------
  ! mz_pj_20090414+
  SUBROUTINE timer_set_time_step_len(l2tls)

    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: l2tls

    IF (lstart) THEN
       time_step_len = delta_time
    ELSE
       IF (l2tls) THEN
          time_step_len = delta_time
       ELSE
          time_step_len = 2.0_dp*delta_time
       ENDIF
    END IF

  END SUBROUTINE timer_set_time_step_len
  ! mz_pj_20090414-
  ! ---------------------------------------------------------------------------

  ! mz_ak_20090602+
  SUBROUTINE timer_set_no_cycles(ncyc)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: ncyc
    
    no_cycles = ncyc

  END SUBROUTINE timer_set_no_cycles
  ! mz_ak_20090602-

  ! ---------------------------------------------------------------------------

  SUBROUTINE timer_get_no_cycles(ncyc)

    IMPLICIT NONE

    INTEGER, INTENT(out) :: ncyc
    
    ncyc = no_cycles

  END SUBROUTINE timer_get_no_cycles

  ! ---------------------------------------------------------------------------

  SUBROUTINE timer_set_labort(la)

    IMPLICIT NONE

    LOGICAL, INTENT(IN) :: la
    
    labort = la

  END SUBROUTINE timer_set_labort

  ! ---------------------------------------------------------------------------

  SUBROUTINE timer_get_labort(la)

    IMPLICIT NONE

    LOGICAL, INTENT(OUT) :: la
    
    la = labort

  END SUBROUTINE timer_get_labort

  !===========================================================================
  
!!$  SUBROUTINE timer_read_nml_ctrl(status, iou)
!!$
!!$    ! MAIN_TIMER MODULE ROUTINE (CORE)
!!$    !
!!$    ! READ TIMER NAMELIST, CHECK IT, AND INITIALIZE GLOBAL VARIABLES
!!$    !
!!$    ! Author: Astrid Kerkweg, UNI-MZ, Apr 2008
!!$
!!$    USE messy_main_tools, ONLY: read_nml_open, read_nml_check, read_nml_close
!!$
!!$    IMPLICIT NONE
!!$
!!$    ! I/O
!!$    INTEGER, INTENT(OUT) :: status
!!$    INTEGER, INTENT(IN)  :: iou   ! logical I/O unit
!!$
!!$    ! LOCAL
!!$    CHARACTER(LEN=*), PARAMETER       :: substr='timer_read_nml_ctrl'
!!$    LOGICAL                           :: lex          ! file exists ?
!!$    INTEGER                           :: fstat        ! file status
!!$
!!$    NAMELIST /CTRL/  L_JULIAN
!!$
!!$    status = 1 ! ERROR ON RETURN
!!$
!!$    CALL read_nml_open(lex, substr, iou, 'CTRL', modstr)
!!$    IF (.not.lex) RETURN    ! <modstr>.nml does not exist
!!$
!!$    READ(iou, NML=CTRL, IOSTAT=fstat)
!!$    CALL read_nml_check(fstat, substr, iou, 'CTRL', modstr)
!!$    IF (fstat /= 0) RETURN  ! error while reading namelist
!!$
!!$    ! DIAGNOSE NAMELIST AND SET GLOBAL SWITCHES
!!$    CALL read_nml_close(substr, iou, modstr)
!!$
!!$    status = 0
!!$
!!$  END SUBROUTINE timer_read_nml_ctrl

!mz_hr_20080228+
! ---------------------------------------------------------------------
  ! crack common netcdf time string format into usable bits
  ! example: 'seconds since 2000-01-01 00:00:00'

  SUBROUTINE eval_time_str(status, z_time_string, z_tuf, z_year, z_month, &
                           z_day, z_hour, z_min, z_sec)

    USE messy_main_constants_mem, ONLY: STRLEN_VLONG, STRLEN_MEDIUM
    USE messy_main_tools,         ONLY: strcrack
    IMPLICIT NONE

    INTRINSIC TRIM, ADJUSTL

    ! I/O
    INTEGER, INTENT(OUT) :: status
    INTEGER, INTENT(OUT), OPTIONAL :: z_year, z_month, z_day, z_hour, z_min, z_sec
    REAL(DP), INTENT(OUT) :: z_tuf
    CHARACTER(LEN=STRLEN_VLONG), INTENT(IN) :: z_time_string

    ! LOCAL
    INTEGER                      :: nosub
    CHARACTER(LEN=STRLEN_MEDIUM) :: tunit, helpvar, odate, otime
    CHARACTER(LEN=STRLEN_MEDIUM), DIMENSION(:), POINTER :: field => NULL()

    status = 0

    ! crack time_unit string into field of nosub substrings
    CALL strcrack(TRIM(z_time_string), " ", field, nosub)
    IF ((nosub /= 3) .AND. (nosub /= 4)) THEN
      WRITE(*,*) 'ERROR eval_time_str: '// &
        'time_string in incompatible format!'
      status = 1
      RETURN
    ENDIF

    ! determine time unit factor tuf <time>*tuf=time(s)
    tunit = TRIM(ADJUSTL(field(1)))
    SELECT CASE (tunit)
    CASE ('seconds')
      z_tuf = 1.0_dp
    CASE ('SECONDS')
      z_tuf = 1.0_dp
    CASE ('Seconds')
      z_tuf = 1.0_dp
    CASE ('minutes')
      z_tuf = 60.0_dp
    CASE ('MINUTES')
      z_tuf = 60.0_dp
    CASE ('Minutes')
      z_tuf = 60.0_dp
    CASE ('hours')
      z_tuf = 3600.0_dp
    CASE ('HOURS')
      z_tuf = 3600.0_dp
    CASE ('Hours')
      z_tuf = 3600.0_dp
    CASE ('days')
      z_tuf = 86400.0_dp
    CASE ('DAYS')
      z_tuf = 86400.0_dp
    CASE ('Days')
      z_tuf = 86400.0_dp
    CASE DEFAULT
      WRITE(*,*) 'eval_time_str: ERROR '// &
        'time_string unit not recognized!'
      status = 1
      RETURN
    END SELECT

    ! needs to be here, 'field' modified for time
    odate  = field(3)

    ! processing time
    IF (nosub == 4) THEN
      otime  = field(4)
      CALL strcrack(otime, ":", field, nosub)
      IF (nosub /= 3) THEN
        WRITE(*,*) 'eval_time_str: ERROR '// &
          'time_string in incompatible format!'
        status = 1
        RETURN
      ENDIF
      helpvar      = field(1)(1:2)
      IF (PRESENT(z_hour)) READ(helpvar, *) z_hour
      helpvar      = field(2)(1:2)
      IF (PRESENT(z_min))  READ(helpvar, *) z_min
      helpvar      = field(3)(1:2)
      IF (PRESENT(z_sec))  READ(helpvar, *) z_sec
    ELSE
      IF (PRESENT(z_hour)) z_hour = 0
      IF (PRESENT(z_min))  z_min  = 0
      IF (PRESENT(z_sec))  z_sec  = 0
      otime  = '00:00:00'
    ENDIF

    ! processing date
    CALL strcrack(odate, "-", field, nosub)
    IF (nosub /= 3) THEN
      WRITE(*,*) 'eval_time_str: ERROR'// &
        ' Date in time_string in incompatible format!'
      status = 1
      RETURN
    ENDIF
    helpvar      = field(1)(1:4)
    IF (PRESENT(z_year))  READ(helpvar, *) z_year
    helpvar      = field(2)(1:2)
    IF (PRESENT(z_month)) READ(helpvar, *) z_month
    helpvar      = field(3)(1:2)
    IF (PRESENT(z_day))   READ(helpvar, *) z_day

    IF (ASSOCIATED(field)) THEN
      DEALLOCATE(field)
      NULLIFY(field)
    ENDIF

  END SUBROUTINE eval_time_str
! ---------------------------------------------------------------------
!mz_hr_20080228-

  END MODULE MESSY_MAIN_TIMER
