! **********************************************************************
MODULE messy_main_import_tseries
! **********************************************************************

  ! MESSy
  USE messy_main_constants_mem,   ONLY: DP

  IMPLICIT NONE
  PRIVATE

  TYPE t_tseries_ctrl
     CHARACTER(LEN=15) :: method = ''  
     INTEGER           :: i0     = 0   !  interpolation (0=off, 1=on)
     REAL(DP)          :: const  = 0.0_dp ! constant
     !                                 !  cyclic,       transient,  stationary
     INTEGER           :: i1     = 0   !  start_month   offset      month
     INTEGER           :: i2     = 0   !  year           ---        year
     INTEGER           :: i3     = 0   !  no. of steps   ---        ---
  END TYPE t_tseries_ctrl

  TYPE t_tseries_data
     INTEGER                                 :: npar = 1 ! at least 1 for const
     REAL(DP), DIMENSION(:),         POINTER :: grid => NULL()
     REAL(DP), DIMENSION(:,:,:,:,:), POINTER :: data => NULL()
     INTEGER                                 :: flag = 0
  END TYPE t_tseries_data
  PUBLIC :: t_tseries_ctrl, t_tseries_data

  INTERFACE get_tseries_data
     ! get pointer to current time series data
     MODULE PROCEDURE get_tseries_data_ptr
     ! get current time series data
     MODULE PROCEDURE get_tseries_data_ipo
  END INTERFACE
  PUBLIC :: read_tseries_data   ! read time series data from ASCII File
  PUBLIC :: clean_tseries_data  ! free memory
  PUBLIC :: get_tseries_data    ! get current time series data

CONTAINS

! ---------------------------------------------------------------------
  SUBROUTINE read_tseries_data(status, modstr, fname, iou &
       , tseries, fill)

    USE messy_main_blather, ONLY: start_message, end_message
    IMPLICIT NONE

    INTRINSIC :: LBOUND, UBOUND, TRIM

    ! I/O
    INTEGER,                INTENT(OUT)           :: status
    CHARACTER(LEN=*),       INTENT(IN)            :: modstr
    CHARACTER(LEN=*),       INTENT(IN)            :: fname
    INTEGER,                INTENT(IN)            :: iou
    TYPE(t_tseries_data),    INTENT(OUT)           :: tseries
    REAL(DP),               INTENT(IN), OPTIONAL  :: fill

    ! LOCAL
    CHARACTER(LEN=*), PARAMETER :: substr = 'read_tseries_data'
    LOGICAL  :: lex, lopn
    INTEGER  :: iout
    INTEGER  :: fstat
    INTEGER  :: year1, year2
    INTEGER  :: nm, nd, nh1, nh2
    INTEGER  :: y,m,d,h
    REAL(DP), DIMENSION(:), ALLOCATABLE :: par
    INTEGER  :: i

    ! INIT
    status = 1
    CALL start_message(modstr, 'DATA IMPORT', substr)

    WRITE(*,*) substr,': OPENING FILE ',TRIM(fname)

    IF (ASSOCIATED(tseries%data)) DEALLOCATE(tseries%data)
    NULLIFY(tseries%data)
    IF (ASSOCIATED(tseries%grid)) DEALLOCATE(tseries%grid)
    NULLIFY(tseries%grid)

    ! CHECK DATASET
    INQUIRE(file=TRIM(fname), exist=lex, opened=lopn, number=iout)
    IF (.NOT.lex) THEN
       WRITE(*,*) substr,': FILE DOES NOT EXIST (',TRIM(fname),')'
       RETURN ! ERROR
    END IF
    IF (lopn) THEN
       WRITE(*,*) substr,': FILE ',TRIM(fname),' ALREADY  OPEN ON UNIT ',iout
       RETURN ! ERROR
    END IF

    ! OPEN DATASET
    OPEN(unit=iou,file=TRIM(fname))

    ! READ DATA
    READ(iou,*) ! header (line 1)
    READ(iou,*) ! header (line 2)
    READ(iou,*) ! header (line 3)
    READ(iou,*) ! header (line 4): note on syntax
    READ(iou,*) tseries%flag, year1, year2, tseries%npar   ! line 5
    i = 5
    WRITE(*,*) substr,': FIRST YEAR IS ',year1
    WRITE(*,*) substr,': LAST  YEAR IS ',year2
    WRITE(*,*) substr,': NUMBER OF PARAMTERES IS ',tseries%npar

    SELECT CASE(tseries%flag)
    CASE(1) 
       ! annually
       WRITE(*,*) substr,': TIME RESOLUTION IS annually'
       nm = 1
       nd = 1
       nh1 = 0
       nh2 = 0
    CASE(2) 
       ! monthly
       WRITE(*,*) substr,': TIME RESOLUTION IS monthly'
       nm = 12
       nd = 1
       nh1 = 0
       nh2 = 0
    CASE(3) 
       ! daily
       WRITE(*,*) substr,': TIME RESOLUTION IS daily'
       nm = 12
       nd = 31
       nh1 = 0
       nh2 = 0
    CASE(4) 
       ! hourly
       WRITE(*,*) substr,': TIME RESOLUTION IS hourly'
       nm = 12
       nd = 31
       nh1 = 0
       nh2 = 23
    CASE DEFAULT
       !
       WRITE(*,*) substr, &
            ': UNKNOWN TIME RESOLUTION FLAG (',TRIM(fname),')',tseries%flag
       RETURN
       !
    END SELECT

    ! GRID MEMORY
    ALLOCATE(tseries%grid(tseries%npar))
    READ(iou,*) ! header (line 6)
    READ(iou,*) tseries%grid(:) ! line 7
    i = i + 2
    WRITE(*,*) substr,': AXIS IS ',tseries%grid(:)

    READ(iou,*) ! header (line 8)
    i = i + 1

    ! DATA MEMORY
    ALLOCATE(tseries%data(year1:year2, nm, nd, nh1:nh2, tseries%npar))
    WRITE(*,*) substr,': MEMORY ALLOCATION ', &
         LBOUND(tseries%data),' - ',UBOUND(tseries%data)
    IF (PRESENT(fill)) THEN
       tseries%data(:,:,:,:,:) = fill
       WRITE(*,*) substr,': PRE-FILLED WITH   ',fill
    ELSE
       tseries%data(:,:,:,:,:) = 0.0_dp
    END IF

    ALLOCATE(par(tseries%npar))

    DO
       i = i+1

       SELECT CASE(tseries%flag)
       CASE(1)
          READ(iou, *, IOSTAT=fstat) y, par
          tseries%data(y,1,1,0,:) = par(:)
       CASE(2)
          READ(iou, *, IOSTAT=fstat) y, m, par
          tseries%data(y,m,1,0,:) = par(:)
       CASE(3)
          READ(iou, *, IOSTAT=fstat) y, m, d, par
          tseries%data(y,m,d,0,:) = par(:)
       CASE(4)
          READ(iou, *, IOSTAT=fstat) y, m, d, h, par
          tseries%data(y,m,d,h,:) = par(:)
       END SELECT

       IF (fstat < 0) THEN
          WRITE(*,*) substr,': END OF FILE REACHED; ',i,' LINES READ'
          EXIT
       ELSEIF (fstat > 0) THEN
          WRITE(*,*) substr,': READ ERROR IN LINE ',i
          RETURN ! ERROR
       END IF

    END DO

    ! CLOSE DATASET
    CLOSE(iou)

    DEALLOCATE(par)

    status = 0

    CALL end_message(modstr, 'DATA IMPORT', substr)

  END SUBROUTINE read_tseries_data
! ---------------------------------------------------------------------

! ---------------------------------------------------------------------
  SUBROUTINE clean_tseries_data(tseries)

    IMPLICIT NONE

    ! I/O
    TYPE(t_tseries_data)                           :: tseries ! INTENT(INOUT)

    tseries%npar = 1

    IF (ASSOCIATED(tseries%grid)) DEALLOCATE(tseries%grid)
    NULLIFY(tseries%grid)

    IF (ASSOCIATED(tseries%data)) DEALLOCATE(tseries%data)
    NULLIFY(tseries%data)

    tseries%flag = 0

  END SUBROUTINE clean_tseries_data
! ---------------------------------------------------------------------

! ---------------------------------------------------------------------
  SUBROUTINE get_tseries_data_ptr(status, callstr, tseries &
       , yr, mo, dy, hr, val)

    IMPLICIT NONE

    INTRINSIC :: UBOUND, LBOUND, MIN

    ! I/O
    INTEGER,                 INTENT(OUT) :: status
    CHARACTER(LEN=*),        INTENT(IN)  :: callstr
    TYPE(t_tseries_data),     INTENT(IN)  :: tseries
    INTEGER,                 INTENT(IN)  :: yr, mo, dy, hr
    REAL(DP), DIMENSION(:),  POINTER     :: val        ! INTENT(OUT)

    ! LOCAL
    INTEGER, DIMENSION(5) :: lb, ub
    INTEGER :: m,d,h

    status = 1

    lb = LBOUND(tseries%data)
    ub = UBOUND(tseries%data)

    IF ( (yr < lb(1)) .OR. (yr > ub(1)) ) THEN
       ! ERROR
       WRITE(*,*) TRIM(callstr),': YEAR OUT OF RANGE ',yr &
            , '(',lb(1), ' - ', ub(1),')'
       RETURN
    ENDIF

    m = MIN(ub(2),mo)    ! 1 for                 annual data
    d = MIN(ub(3),dy)    ! 1 for        monthly, annual data
    h = MIN(ub(4),hr)    ! 0 for daily, monthly, annual data

    val => tseries%data(yr,m,d,h,:)

    status = 0

  END SUBROUTINE get_tseries_data_ptr
! ---------------------------------------------------------------------

! ---------------------------------------------------------------------
  SUBROUTINE get_tseries_data_ipo(status, callstr, time_control, tseries &
       , yr, mo, dy, hr, val, rflag, rng)

    USE messy_main_timer, ONLY: JulianMonthLength

    IMPLICIT NONE

    INTRINSIC :: UBOUND, LBOUND, MIN, MOD, ASSOCIATED

    ! I/O
    INTEGER,                 INTENT(OUT) :: status
    CHARACTER(LEN=*),        INTENT(IN)  :: callstr
    TYPE(t_tseries_ctrl),    INTENT(IN)  :: time_control
    TYPE(t_tseries_data),     INTENT(IN)  :: tseries
    INTEGER,                 INTENT(IN)  :: yr, mo, dy, hr ! date/time
    REAL(DP), DIMENSION(:),  POINTER     :: val          ! INTENT(OUT)
    REAL(DP), DIMENSION(:),  POINTER     :: rflag        ! INTENT(OUT)
    REAL(DP), DIMENSION(2),  INTENT(IN), OPTIONAL :: rng ! valid range

    ! LOCAL
    CHARACTER(LEN=*), PARAMETER :: substr = 'get_tseries_data_ipo'
    REAL(DP), DIMENSION(:), POINTER :: val1 => NULL(), val2 => NULL()
    INTEGER            :: pyr, pmo, pdy ! present date
    INTEGER            :: ml            ! length of month
    INTEGER            :: ioff          ! offset
    INTEGER            :: ioff1, ioff2  ! offset
    REAL               :: wgm1, wgm2    ! weight of month 1 and 2
    INTEGER            :: y1            ! 1st year
    INTEGER            :: nyr, nmo      ! only for interpolation
    INTEGER            :: idx, nidx     ! start index
    INTEGER            :: i             ! loop counter

    ! SET TIME
    pyr = yr
    pmo = mo
    pdy = dy

    IF (ASSOCIATED(val)) THEN
       IF (SIZE(val) /= tseries%npar) THEN
          ! ERROR
          WRITE(*,*) substr,' (',TRIM(callstr),'): ARRAY SIZE MISMATCH ' &
               , '(',SIZE(val), ' /= ',tseries%npar,')'
          status = 1 ! ERROR
          RETURN
       END IF
    ELSE
       ALLOCATE(val(tseries%npar))
    END IF

    IF (ASSOCIATED(rflag)) THEN
       IF (SIZE(rflag) /= tseries%npar) THEN
          ! ERROR
          WRITE(*,*) substr,' (',TRIM(callstr),'): ARRAY SIZE MISMATCH ' &
               , '(',SIZE(rflag), ' /= ',tseries%npar,')'
          status = 1 ! ERROR
          RETURN
       END IF
    ELSE
       ALLOCATE(rflag(tseries%npar))
    END IF

    SELECT CASE(TRIM(time_control%method))
       !
    CASE('constant')
       !
       val(:)   = time_control%const
       rflag(:) = 1.0_dp
       status = 0
       RETURN
       !
    CASE('transient')
       !
       y1 = LBOUND(tseries%data,1)
       idx = (yr - y1)*12 + mo
       !
       ! ADD OFFSET
       ioff = time_control%i1
       !
       IF (time_control%i0 == 1) THEN
          ! INTERPOLATION
          IF (pdy <=15) THEN
             ioff = ioff - 1
          END IF
       END IF
       !
       pyr = y1 + (idx-1+ioff)/12
       pmo = MOD(idx-1+ioff,12) + 1
       nyr = y1 + (idx+ioff)/12
       nmo = MOD(idx+ioff,12) + 1
       !
    CASE('stationary')
       !
       ! SET TIME
       pmo = time_control%i1  ! month
       pyr = time_control%i2  ! year
       nyr = pyr
       nmo = pmo
       !
    CASE('cyclic')
       !
       y1   = LBOUND(tseries%data,1)
       !
       ! OFFSET BETWEEN START DATA YEAR AND 1st YEAR IN DATA SET
       ioff1 = (time_control%i2-y1)*12  
       !
       ! OFFSET BETWEEN CURRENT MODEL YEAR AND START DATA YEAR
       ioff2 = (yr-time_control%i1)*12
       !
       IF (time_control%i0 == 1) THEN
          ! INTERPOLATION
          IF (pdy <=15) THEN
             ioff2 = ioff2 - 1
          END IF
       END IF
       !
       idx  = MOD(ioff2 + mo - 1, time_control%i3) + ioff1 + 1
       nidx = MOD(ioff2 + mo, time_control%i3) + ioff1 + 1
       !
       pyr = y1 + (idx-1)/12
       pmo = MOD(idx-1,12) + 1
       !
       nyr = y1 + (nidx-1)/12
       nmo = MOD(nidx-1,12) + 1
       !
    CASE DEFAULT
       !
       WRITE(*,*) substr,' (',TRIM(callstr),'): UNKNOWN TIME CONTROL METHOD ' &
            ,TRIM(time_control%method)
       status = 1 ! ERROR
       RETURN
       !
    END SELECT

    CALL get_tseries_data_ptr(status, callstr, tseries &
         , pyr, pmo, pdy, hr, val1)
    IF (status /= 0) RETURN
    
    interpolate: IF (time_control%i0 == 1) THEN
       
       SELECT CASE(tseries%flag)
          !
       CASE (1,3,4)
          !
          WRITE(*,*) substr,' (',TRIM(callstr),'): INTERPOLATION FOR '//&
               &'annual/daily/hourly data not yet implemented'
          status = 1 ! ERROR
          RETURN
          !
       CASE (2)
          !
          ! monthly -> linear interpolation
          !
          !       The monthly mean is assumed to be centered on the 15th
          !       day of the month. Days 1 to 15 are interpolated from the
          !       monthly means of the actual month and the month before,
          !       days above 16 are interpolated from the monthly means of
          !       the actual month and the following month.
          !         imm = index of actual month,
          !         im1 = index of 1st month, wgm1 = weight of 1st month
          !         im2 = index of 2nd month, wgm2 = weight of 2nd month
          !
          CALL get_tseries_data_ptr(status, callstr, tseries &
               , nyr, nmo, pdy, hr, val2)
          IF (status /= 0) RETURN
          !
          ! GET LENGHT (in days) OF THE MONTH
          ml = JulianMonthLength(pyr, pmo)
          !
          IF (pdy <=15) THEN
             wgm2 = REAL((pdy + 15)) / REAL(ml)
             wgm1 = 1.0 - wgm2
          ELSE
             wgm2 = REAL((pdy - 15)) / REAL(ml)
             wgm1 = 1.0 - wgm2
          END IF
          !
       CASE DEFAULT
          !
          WRITE(*,*) substr,&
               ': *** ERROR *** UNKNOWN TIME RESOLUTION FLAG ',tseries%flag
          status = 1 ! ERROR
          RETURN
          !
       END SELECT
       
       val(:) = wgm1 * val1(:) + wgm2 * val2(:)
       
    ELSE
       
       val(:) =  val1(:)
       val2   => val(:)
       
    END IF interpolate
    
    rflag(:) = 1.0_dp
    IF (PRESENT(rng)) THEN
       DO i=1, tseries%npar
          IF ( (val1(i) < rng(1)) .OR. (val1(i) > rng(2)) .OR. &
               (val2(i) < rng(1)) .OR. (val2(i) > rng(2)) ) &
               rflag(i) = 0.0_dp
       END DO
    END IF

    NULLIFY(val1)
    NULLIFY(val2)
    
    status = 0
    
  END SUBROUTINE get_tseries_data_ipo
  ! ---------------------------------------------------------------------

! **********************************************************************
END MODULE messy_main_import_tseries
! **********************************************************************
