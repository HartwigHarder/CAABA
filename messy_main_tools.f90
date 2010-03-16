! ************************************************************************
MODULE messy_main_tools
! ************************************************************************

  ! MESSy-SMCL tools
  !
  ! Author: Patrick Joeckel, MPICH, Mainz, 2003-2009
  !         Hella Riede    , MPICH, Mainz, Sep 2009
  !         Rolf Sander,     MPICH, Mainz, 2006-2008

  USE messy_main_constants_mem, ONLY: SP, DP, STRLEN_LONG, STRLEN_VLONG, &
                                      STRLEN_MEDIUM

  IMPLICIT NONE
  PRIVATE

  ! mz_rs_20081222+
  ! 1D array also for integer:
  TYPE PTR_1D_ARRAY_INT
     INTEGER, DIMENSION(:), POINTER :: PTR
  END TYPE PTR_1D_ARRAY_INT
  ! mz_rs_20081222-

  TYPE PTR_1D_ARRAY
     REAL(DP), DIMENSION(:), POINTER :: PTR
  END TYPE PTR_1D_ARRAY

  TYPE PTR_2D_ARRAY
     REAL(DP), DIMENSION(:,:), POINTER :: PTR
  END TYPE PTR_2D_ARRAY

  TYPE PTR_3D_ARRAY
     REAL(DP), DIMENSION(:,:,:), POINTER :: PTR
  END TYPE PTR_3D_ARRAY

  TYPE PTR_4D_ARRAY
     REAL(DP), DIMENSION(:,:,:,:), POINTER :: PTR
  END TYPE PTR_4D_ARRAY

  TYPE PTR_5D_ARRAY
     REAL(DP), DIMENSION(:,:,:,:,:), POINTER :: PTR
  END TYPE PTR_5D_ARRAY

  PUBLIC :: PTR_1D_ARRAY, PTR_2D_ARRAY, PTR_3D_ARRAY &
          , PTR_4D_ARRAY, PTR_5D_ARRAY, PTR_1D_ARRAY_INT

  ! mz_ht_20040414+
  ! FOR LOOKUP TABLE INITIALIZATION (CONVECTION at al.)
  INTEGER,  PARAMETER, PUBLIC :: jptlucu1 =  50000  ! lookup table lower bound
  INTEGER,  PARAMETER, PUBLIC :: jptlucu2 = 400000  ! lookup table upper bound
  ! table - e_s*Rd/Rv
  REAL(dp), TARGET, SAVE,  PUBLIC :: tlucua(jptlucu1:jptlucu2)
  ! table - for derivative calculation
  REAL(dp), TARGET, SAVE,  PUBLIC :: tlucub(jptlucu1:jptlucu2)
  ! table - l/cp
  REAL(dp), TARGET, SAVE,  PUBLIC :: tlucuc(jptlucu1:jptlucu2)
  ! table
  REAL(dp), TARGET, SAVE,  PUBLIC :: tlucuaw(jptlucu1:jptlucu2)
  ! mz_ht_20040414-

  ! mz_rs_20060110+
  INTERFACE str
     MODULE PROCEDURE str_logical
     MODULE PROCEDURE str_integer
     MODULE PROCEDURE str_real_sp
     MODULE PROCEDURE str_real_dp
  END INTERFACE
  PUBLIC :: str
  ! mz_rs_20060110-
  PUBLIC :: psat
  PUBLIC :: psatf    ! calculates saturation vapor press of H2O [Pa]

  INTERFACE iso2ind
     MODULE PROCEDURE iso2ind_1d
     MODULE PROCEDURE iso2ind_2d
  END INTERFACE

  INTERFACE ind2val
     MODULE PROCEDURE ind2val_1d
     MODULE PROCEDURE ind2val_2d
  END INTERFACE

  ! SUBROUTINES
  PUBLIC :: read_nml_open       ! Utilities ...
  PUBLIC :: read_nml_check      ! ... to simplify ...
  PUBLIC :: read_nml_close      ! ... namelist input

  PUBLIC :: iso2ind             ! find index
  PUBLIC :: ind2val             ! find value at index level
  PUBLIC :: int2str             ! convert integer to string
  PUBLIC :: strcrack            ! cracking strings into parts
  PUBLIC :: nn_index            ! look for nearest neighbour(s) in list
  ! mz_ap_20070913+
  PUBLIC :: ns_index            ! look for sourrounding neighbour(s) in list
  ! mz_ap_20070913-
  ! mz_ht_20040414+
  PUBLIC :: init_convect_tables ! lookup table for convection et al. 
  ! mz_ht_20040414-
  PUBLIC :: match_wild          ! compare strings with wildcards
  PUBLIC :: str2chob            ! convert string to channel/object list
  PUBLIC :: bilin_weight        ! weights for bilinear interpolation
  !mz_hr_20080229+
  PUBLIC :: ucase               ! turn a string into all uppercase
  PUBLIC :: spec2relhum         ! conversion from specific to relative
  PUBLIC :: rel2spechum         ! conversion from relative to specific 
  !mz_hr_20080229-
  PUBLIC :: find_next_free_unit

CONTAINS

! -----------------------------------------------------------------------
  SUBROUTINE read_nml_open(lex, substr, iou, nmlstr, modstr)

    USE messy_main_blather, ONLY: start_message
    IMPLICIT NONE
    INTRINSIC :: TRIM

    ! I/O
    LOGICAL, INTENT(OUT)                   :: lex       ! file exists ?
    CHARACTER(LEN=*), INTENT(IN)           :: substr    ! calling routine
    INTEGER,          INTENT(IN)           :: iou       ! unit
    CHARACTER(LEN=*), INTENT(IN)           :: nmlstr    ! namelist
    CHARACTER(LEN=*), INTENT(IN)           :: modstr    ! module name


    CALL start_message(TRIM(modstr), 'INITIALISATION', substr)

    ! CHECK IF FILE EXISTS
    INQUIRE(file=TRIM(modstr)//'.nml', exist=lex)
    IF (.NOT.lex) THEN
       WRITE(*,*) '*** WARNING: FILE '''//TRIM(modstr)//'.nml'&
            &//'''  NOT FOUND !'
       RETURN
    END IF

    ! OPEN FILE
    OPEN(iou,file=TRIM(modstr)//'.nml')
    WRITE(*,*) 'Reading namelist '''//TRIM(nmlstr)//''''//&
         &' from '''//TRIM(modstr)//'.nml',''' (unit ',iou,') ...'

  END SUBROUTINE read_nml_open
! -----------------------------------------------------------------------

! -----------------------------------------------------------------------
  SUBROUTINE read_nml_check(fstat, substr, iou, nmlstr, modstr)

    IMPLICIT NONE
    INTRINSIC :: TRIM

    ! I/O
    INTEGER, INTENT(IN)                    :: fstat     ! file status
    CHARACTER(LEN=*), INTENT(IN)           :: substr    ! calling routine
    INTEGER,          INTENT(IN)           :: iou       ! unit
    CHARACTER(LEN=*), INTENT(IN)           :: nmlstr    ! namelist
    CHARACTER(LEN=*), INTENT(IN)           :: modstr    ! module name

    IF (fstat /= 0) THEN
       WRITE(*,*) '*** ERROR: READ ERROR in NAMELIST '''//TRIM(nmlstr)//''''&
            &//' in FILE '''//TRIM(modstr)//'.nml'//''' !'
       CLOSE(iou)
    ELSE
       WRITE(*,*) ' ... OK !'
    END IF

  END SUBROUTINE read_nml_check
! -----------------------------------------------------------------------

! -----------------------------------------------------------------------
  SUBROUTINE read_nml_close(substr, iou, modstr)

    USE messy_main_blather, ONLY: end_message
    IMPLICIT NONE
    INTRINSIC :: TRIM

    ! I/O
    CHARACTER(LEN=*), INTENT(IN)           :: substr    ! calling routine
    INTEGER,          INTENT(IN)           :: iou       ! unit
    CHARACTER(LEN=*), INTENT(IN)           :: modstr    ! module name

    CLOSE(iou)

    CALL end_message(TRIM(modstr), 'INITIALISATION', substr)

  END SUBROUTINE read_nml_close
! -----------------------------------------------------------------------

  ! -----------------------------------------------------------------------
  SUBROUTINE iso2ind_1d(field, iso, k, f, lrev)
    
    ! ISOSURFACE TO INDEX
    ! OUTPUT IS THE LEVEL INDEX FOR A GIVEN ISOSURFACE
    ! NOTE:
    !   THIS ROUTINE IS WORKING PROPERLY ONLY IF THE 'FIELD'
    !   IS MONOTONIC
    !   (e.g., POTENTIAL TEMPERATURE, PRESSURE, POTENTIAL VORTICITY, ...)
    ! METHOD:
    !  lrev = .false. (default) -> SEARCH FROM LOW INDEX TO HIGH INDEX
    !  lrev = .true.            -> SEARCH FROM HIGH INDEX TO LOW INDEX
    !  ADJUST INDEX
    !  OPTIONAL: FRACTION OF BOX 'BELOW' ISO-SURFACE
    
    IMPLICIT NONE
    INTRINSIC :: NINT, ABS, MAX, MIN, PRESENT, REAL, SIZE, TINY

    ! I/O
    REAL, DIMENSION(:), INTENT(IN)            :: field  ! input field
    REAL,               INTENT(IN)            :: iso    ! isosurface value
    INTEGER ,           INTENT(OUT)           :: k      ! isosurface level
    REAL,               INTENT(OUT), OPTIONAL :: f      ! fraction in layer
    LOGICAL,            INTENT(IN),  OPTIONAL :: lrev   ! reverse order ?

    ! LOCAL
    INTEGER :: nk, jk, dk
    ! mz_rs_20080224: dk changed to integer
    REAL    :: zf
    LOGICAL :: llrev
    INTEGER :: jstart, jstop, jstep

    k = 0
    nk = SIZE(field)    

    IF (PRESENT(lrev)) THEN
       llrev = lrev
    ELSE
       llrev = .FALSE. ! default
    END IF

    IF (llrev) THEN
       jstart = nk
       jstop  = 2
       jstep = -1
    ELSE
       jstart = 2
       jstop  = nk
       jstep = 1
    END IF

    DO jk = jstart, jstop, jstep
       IF ( ( (iso >= field(jk-1)) .AND. (iso <= field(jk)) ) .OR. &
            ( (iso <= field(jk-1)) .AND. (iso >= field(jk)) ) ) THEN
          k=jk
          EXIT
       END IF
    END DO

    IF ( k == 0 ) THEN   ! NOT FOUND
       IF (llrev) THEN
          k = 2
          IF (PRESENT(f)) f = 1.0
       ELSE
          k = nk
          IF (PRESENT(f)) f = 0.0
       END IF
       RETURN
    END IF

    ! ADJUST INDEX
    ! CALCULATE FRACTION OF BOX 'BELOW' ISO-SURFACE
    !
    ! METHOD: LINEAR INTERPOLATION
    !
    ! THE FOLLOWING CONDITION MUST ALWAYS BE .TRUE.,
    ! SINCE THE FIRST LEVEL WITH 
    !    FIELD(k-1) <= ISO <= FLIELD(k)
    ! OR
    !    FIELD(k-1) >= ISO >= FLIELD(k)
    ! IS SEARCHED
    !
    IF ( ABS( field(k) - field(k-1) ) > TINY(0.0) ) THEN
       zf = ABS( (iso-field(k-1)) / (field(k)-field(k-1)) )    ! e [0,1)
    ELSE
       zf = 0.5  ! SHOULD BE NEVER REACHED !!!
    END IF
    
    zf = MIN(1.,zf)
    zf = MAX(0.,zf)
    
    ! dk = INT(zf+0.5)
    dk = NINT(zf)
    ! zf e [0,0.5] -> dk = 0 -> ONE LEVEL ABOVE (-1)
    ! zf e (0.5,1) -> dk = 1 -> KEEP LEVEL

    k = k - 1 + dk
    
    ! CALCULATE FRACTION OF BOX 'BELOW' ISO-SURFACE
    ! ONE LEVEL ABOVE (dk = 0) -> zf e [0, 0.5]
    !                          -> f  e [0.5, 0]
    !       EXAMPLE: zf  = 0   -> ISO AT BOX MID         -> FRACT. = 0.5
    !                zf  = 0.5 -> ISO AT LOWER INTERFACE -> FRACT. = 0.0
    ! KEEP LEVEL      (dk = 1) -> zf e (0.5, 1)
    !                          -> f  e (1, 0.5)
    !       EXAMPLE: zf  = 0.5 -> ISO AT UPPER INTERFACE -> FRACT. = 1.0
    !                zf  = 1   -> ISO AT BOX MID         -> FRACT. = 0.5
    
    !                            FOR dk=1            FOR dk=0
    IF (PRESENT(f)) f = (1.5-zf)*REAL(dk) + (0.5-zf)*REAL(1-dk)
     
  END SUBROUTINE iso2ind_1d
! -----------------------------------------------------------------------

! -----------------------------------------------------------------------
  SUBROUTINE iso2ind_2d(kproma, field, iso, k, f, lrev)
    
    ! as iso2ind_1d, however for vectors ...
    
    IMPLICIT NONE
    INTRINSIC :: NINT, ABS, MAX, MIN, PRESENT, REAL, SIZE, TINY

    ! I/O
    INTEGER,                 INTENT(IN)            :: kproma
    REAL,    DIMENSION(:,:), INTENT(IN)            :: field ! input field
    REAL,    DIMENSION(:),   INTENT(IN)            :: iso   ! isosurface value
    INTEGER, DIMENSION(:),   INTENT(OUT)           :: k     ! isosurface level
    REAL,    DIMENSION(:),   INTENT(OUT), OPTIONAL :: f     ! fraction in layer
    LOGICAL,                 INTENT(IN),  OPTIONAL :: lrev  ! reverse order ?

    ! LOCAL
    INTEGER :: nk, jk, jl, dk
    ! mz_rs_20080224: dk changed to integer
    REAL    :: zf
    LOGICAL :: llrev
    INTEGER :: jstart, jstop, jstep
    INTEGER, DIMENSION(kproma) :: ilfound

    k = 0
    ilfound(:) = 0
    nk = SIZE(field,2)    

    IF (PRESENT(lrev)) THEN
       llrev = lrev
    ELSE
       llrev = .FALSE. ! default
    END IF

    IF (llrev) THEN
       jstart = nk
       jstop  = 2
       jstep = -1
    ELSE
       jstart = 2
       jstop  = nk
       jstep = 1
    END IF

    DO jk = jstart, jstop, jstep
       DO jl = 1, kproma
          IF ( ilfound(jl) == 1) CYCLE
          IF ( ( (iso(jl) >= field(jl,jk-1)) .AND. &
               (iso(jl) <= field(jl,jk)) ) .OR. &
               ( (iso(jl) <= field(jl,jk-1)) .AND. &
               (iso(jl) >= field(jl,jk)) ) ) THEN
             k(jl) = jk
             ilfound(jl) = 1 
          END IF
       END DO
    END DO

    DO jl = 1, kproma

       IF ( k(jl) == 0 ) THEN   ! NOT FOUND
          IF (llrev) THEN
             k(jl) = 2
             IF (PRESENT(f)) f(jl) = 1.0
          ELSE
             k(jl) = nk
             IF (PRESENT(f)) f(jl) = 0.0
          END IF

       ELSE
          
          ! ADJUST INDEX
          ! CALCULATE FRACTION OF BOX 'BELOW' ISO-SURFACE
          !
          ! METHOD: LINEAR INTERPOLATION
          !
          ! THE FOLLOWING CONDITION MUST ALWAYS BE .TRUE.,
          ! SINCE THE FIRST LEVEL WITH 
          !    FIELD(k-1) <= ISO <= FLIELD(k)
          ! OR
          !    FIELD(k-1) >= ISO >= FLIELD(k)
          ! IS SEARCHED
          !
          IF ( ABS( field(jl,k(jl)) - field(jl,k(jl)-1) ) > TINY(0.0) ) THEN
             zf = ABS( (iso(jl)-field(jl,k(jl)-1)) / &
                  (field(jl,k(jl))-field(jl,k(jl)-1)) )    ! e [0,1)
          ELSE
             zf = 0.5  ! SHOULD BE NEVER REACHED !!!
          END IF
    
          zf = MIN(1.,zf)
          zf = MAX(0.,zf)
    
          ! dk = INT(zf+0.5)
          dk = NINT(zf)
          ! zf e [0,0.5] -> dk = 0 -> ONE LEVEL ABOVE (-1)
          ! zf e (0.5,1) -> dk = 1 -> KEEP LEVEL
    
          k(jl) = k(jl) - 1 + dk
    
          ! CALCULATE FRACTION OF BOX 'BELOW' ISO-SURFACE
          ! ONE LEVEL ABOVE (dk = 0) -> zf e [0, 0.5]
          !                          -> f  e [0.5, 0]
          !       EXAMPLE: zf  = 0   -> ISO AT BOX MID         -> FRACT. = 0.5
          !                zf  = 0.5 -> ISO AT LOWER INTERFACE -> FRACT. = 0.0
          ! KEEP LEVEL      (dk = 1) -> zf e (0.5, 1)
          !                          -> f  e (1, 0.5)
          !       EXAMPLE: zf  = 0.5 -> ISO AT UPPER INTERFACE -> FRACT. = 1.0
          !                zf  = 1   -> ISO AT BOX MID         -> FRACT. = 0.5

          !                            FOR dk=1            FOR dk=0
          IF (PRESENT(f)) f(jl) = (1.5-zf)*REAL(dk) + (0.5-zf)*REAL(1-dk)
          
       END IF

    END DO
     
  END SUBROUTINE iso2ind_2d
! -----------------------------------------------------------------------

! -----------------------------------------------------------------------
  SUBROUTINE ind2val_1d(val, field, k, f)

    ! CONVERT INDEX (AND FRACTION 'BELOW') IN A MONOTONOUS FIELD
    ! TO THE VALUE
    ! METHOD:
    !   - PICK OUT INDEX
    !   - LINEAR INTERPOLATION BETWEEN NEIGHBOURS, IF f IS PRESENT
    
    IMPLICIT NONE
    INTRINSIC :: PRESENT, SIZE

    ! I/O
    REAL,                   INTENT(OUT)          :: val   ! value
    REAL,     DIMENSION(:), INTENT(IN)           :: field ! field
    INTEGER,                INTENT(IN)           :: k     ! level
    REAL,                   INTENT(IN), OPTIONAL :: f     ! fraction
    
    ! LOCAL
    INTEGER :: nk
    REAL    :: ri, gf
    
    nk = SIZE(field)
    
    IF (PRESENT(f)) THEN
       ri  = 0.5 - f           ! e (-0.5,0.5) -> (top, bottom) of box
       IF (ri >= 0.0) THEN
          IF (k == nk) THEN
             gf  = (field(k)-field(k-1))
          ELSE
             gf  = (field(k+1)-field(k))
          END IF
       ELSE
          IF (k == 1) THEN
             gf  = (field(k+1)-field(k))
          ELSE
             gf  = (field(k)-field(k-1))
          END IF
       END IF
       !
       val = field(k) + ri * gf
    ELSE
       val = field(k)
    END IF
    
  END SUBROUTINE ind2val_1d
! -----------------------------------------------------------------------

! -----------------------------------------------------------------------
  SUBROUTINE ind2val_2d(kproma, val, field, k, f)

    ! as ind2val_1d, however for vectors ...
    
    IMPLICIT NONE
    INTRINSIC :: PRESENT, SIZE

    ! I/O
    INTEGER,                 INTENT(IN)           :: kproma
    REAL,    DIMENSION(:),   INTENT(OUT)          :: val   ! value
    REAL,    DIMENSION(:,:), INTENT(IN)           :: field ! field
    INTEGER, DIMENSION(:),   INTENT(IN)           :: k     ! level
    REAL,    DIMENSION(:),   INTENT(IN), OPTIONAL :: f     ! fraction
    
    ! LOCAL
    INTEGER :: jl, nk
    REAL    :: ri, gf
    
    nk = SIZE(field,2)
    
    IF (PRESENT(f)) THEN
       DO jl = 1, kproma
          ri  = 0.5 - f(jl)       ! e (-0.5,0.5) -> (top, bottom) of box
          IF (ri >= 0.0) THEN
             IF (k(jl) == nk) THEN
                gf  = (field(jl,k(jl))-field(jl,k(jl)-1))
             ELSE
                gf  = (field(jl,k(jl)+1)-field(jl,k(jl)))
             END IF
          ELSE
             IF (k(jl) == 1) THEN
                gf  = (field(jl,k(jl)+1)-field(jl,k(jl)))
             ELSE
                gf  = (field(jl,k(jl))-field(jl,k(jl)-1))
             END IF
          END IF
          !
          val(jl) = field(jl,k(jl)) + ri * gf
       END DO
    ELSE
       DO jl = 1, kproma
          val(jl) = field(jl,k(jl))
       END DO
    END IF
    
  END SUBROUTINE ind2val_2d
! -----------------------------------------------------------------------

! ---------------------------------------------------------------------
  SUBROUTINE int2str(str, ii, cpad, cerr)
    
    IMPLICIT NONE
    INTRINSIC :: MOD, LEN, PRESENT
    
    ! I/O
    CHARACTER(LEN=*), INTENT(OUT)          :: str   ! STRING
    INTEGER,          INTENT(IN)           :: ii    ! INTEGER
    CHARACTER,        INTENT(IN), OPTIONAL :: cpad  ! CHAR FOR PADDING
    CHARACTER,        INTENT(IN), OPTIONAL :: cerr  ! CHAR FOR ERROR
    
    ! LOCAL
    INTEGER              :: n, zi, zn, k
    INTEGER              :: rest
    CHARACTER            :: zcpad
    
    IF (PRESENT(cpad)) THEN
       zcpad = cpad
    ELSE
       zcpad = '0'      ! DEFAULT PADDING
    END IF
    
    n  = LEN(str)
    zi = ii
    zn = n
    
    DO
       rest = MOD(zi, 10)
       zi   = zi/10
       WRITE(str(zn:zn),'(i1)') rest
       zn = zn - 1
       IF (zi == 0) EXIT
       IF (zn == 0) EXIT
    END DO
    
    IF (PRESENT(cerr)) THEN
       IF ( (zn == 0) .AND. (zi /= 0) ) THEN
          DO k = 1, n
             WRITE(str(k:k),'(a1)') cerr
          END DO
       END IF
    END IF
    
    DO k = zn, 1, -1
       WRITE(str(k:k),'(a1)') zcpad
    END DO
    
  END SUBROUTINE int2str
! ---------------------------------------------------------------------

! ---------------------------------------------------------------------

  ! mz_rs_20060110+
  ! various types of the function str:

  CHARACTER(LEN=5) FUNCTION str_logical(zlogical, fmt)
    ! create string from logical
    IMPLICIT NONE
    INTRINSIC :: PRESENT
    LOGICAL, INTENT(in) :: zlogical
    CHARACTER(LEN=*), INTENT(in), OPTIONAL :: fmt
    IF (PRESENT(fmt)) THEN
      WRITE(str_logical,fmt) zlogical
    ELSE
      IF (zlogical) THEN
        str_logical = 'TRUE '
      ELSE
        str_logical = 'FALSE'
      ENDIF
    ENDIF
  END FUNCTION str_logical

  CHARACTER(LEN=STRLEN_LONG) FUNCTION str_integer(zinteger, fmt)
    ! create string from integer
    IMPLICIT NONE
    INTRINSIC :: ADJUSTL, PRESENT
    INTEGER, INTENT(in) :: zinteger
    CHARACTER(LEN=*), INTENT(in), OPTIONAL :: fmt
    IF (PRESENT(fmt)) THEN
      WRITE(str_integer,fmt) zinteger
    ELSE
      WRITE(str_integer,*) zinteger
      str_integer = ADJUSTL(str_integer) ! remove leading spaces
    ENDIF
  END FUNCTION str_integer

  CHARACTER(LEN=STRLEN_LONG) FUNCTION str_real_sp(zreal_sp, fmt)
    ! create string from real_sp
    IMPLICIT NONE
    INTRINSIC :: ADJUSTL, PRESENT
    REAL(sp), INTENT(in) :: zreal_sp
    CHARACTER(LEN=*), INTENT(in), OPTIONAL :: fmt
    IF (PRESENT(fmt)) THEN
      WRITE(str_real_sp,fmt) zreal_sp
    ELSE
      WRITE(str_real_sp,*) zreal_sp
      str_real_sp = ADJUSTL(str_real_sp) ! remove leading spaces
    ENDIF
  END FUNCTION str_real_sp

  CHARACTER(LEN=STRLEN_LONG) FUNCTION str_real_dp(zreal_dp, fmt)
    ! create string from real_dp
    IMPLICIT NONE
    INTRINSIC :: ADJUSTL, PRESENT
    REAL(dp), INTENT(in) :: zreal_dp
    CHARACTER(LEN=*), INTENT(in), OPTIONAL :: fmt
    IF (PRESENT(fmt)) THEN
      WRITE(str_real_dp,fmt) zreal_dp
    ELSE
      WRITE(str_real_dp,*) zreal_dp
      str_real_dp = ADJUSTL(str_real_dp) ! remove leading spaces
    ENDIF
  END FUNCTION str_real_dp
  ! mz_rs_20060110-

! ---------------------------------------------------------------------

! ---------------------------------------------------------------------
  SUBROUTINE strcrack(str, ch, el, n)

    ! strcrack = string crack

    ! Split the string <str> into small pieces which are separated by
    ! the character <ch>. Delete trailing spaces from the resulting <n>
    ! pieces, then put them into the array <el>.

    IMPLICIT NONE
    INTRINSIC :: ADJUSTL, ASSOCIATED, INDEX, LEN_TRIM, TRIM

    ! I/O
    CHARACTER(LEN=*),               INTENT(IN)  :: str ! string 2b cracked
    CHARACTER,                      INTENT(IN)  :: ch  ! separating char
    CHARACTER(LEN=*), DIMENSION(:), POINTER     :: el  ! field of substrings
    INTEGER,                        INTENT(OUT) :: n   ! # of substrings
    
    ! LOCAL
    INTEGER :: idx1, idx2, i
    
    ! INIT
    IF (ASSOCIATED(el)) DEALLOCATE(el)
    NULLIFY(el)
    n = 0
    
    ! EMPTY STRING
    IF ( (TRIM(str) == '') .OR. (TRIM(str) == ch) ) RETURN
    
    idx1 = 0
    idx2 = 0
    DO 
       idx1 = idx2 + 1
       IF (idx1 > LEN_TRIM(str(:))) EXIT
       IF (INDEX(TRIM(str(idx1:)), ch) == 0) THEN
          idx2 = LEN_TRIM(str(:)) + 1
       ELSE
          idx2 = idx2 + INDEX(TRIM(str(idx1:)), ch)
       END IF
       IF (idx1 == idx2) CYCLE
       
       n = n + 1
       
    END DO
    
    ! ALLOCATE SPACE
    ALLOCATE(el(n))
    DO i=1, n
       el(i) = ''
    END DO
    
    n = 0
    idx1 = 0
    idx2 = 0
    DO     
       idx1 = idx2 + 1
       IF (idx1 > LEN_TRIM(str(:))) EXIT
       IF (INDEX(TRIM(str(idx1:)), ch) == 0) THEN
          idx2 = LEN_TRIM(str(:)) + 1
       ELSE
          idx2 = idx2 + INDEX(TRIM(str(idx1:)), ch)
       END IF
       IF (idx1 == idx2) CYCLE
       
       n = n + 1
       
!       el(n) = str(idx1:idx2-1)
       el(n) = ADJUSTL(str(idx1:idx2-1))
       
    END DO

  END SUBROUTINE strcrack
! ---------------------------------------------------------------------  

! ---------------------------------------------------------------------
! mz_ap_20070913+
  SUBROUTINE ns_index(list, value, idx, idx2, l_periodic)

    IMPLICIT NONE
    INTRINSIC :: ABS, PRESENT, SIZE

    ! I/O
    REAL(DP), DIMENSION(:), INTENT(IN)            :: list
    REAL(DP),               INTENT(IN)            :: value
    INTEGER,                INTENT(OUT)           :: idx
    INTEGER,                INTENT(OUT), OPTIONAL :: idx2
    LOGICAL,                INTENT(IN),  OPTIONAL :: l_periodic 
    ! list(i) < value < list(i+1)
    ! l_periodic : periodic boundaries

    ! LOCAL
    INTEGER     :: n, i

    ! INIT
    n = SIZE(list)
    idx = 0

    ! we suppouse that both lists are ordered
    IF (value<list(1)) THEN
      DO i=1, n
         IF (value <= list(i)) THEN
            idx = i
         END IF
      END DO
    ELSE
      DO i=1, n
         IF (value >= list(i)) THEN
            idx = i
         END IF
      END DO
    END IF
    IF (PRESENT(idx2)) THEN
       IF (idx == n) THEN
          IF (PRESENT(l_periodic)) THEN
            idx2 = 1
          ELSE
            idx2 = n
          ENDIF
       ELSE
          idx2 = idx+1
       ENDIF
    END IF
    IF (idx == 0 ) THEN
      IF (PRESENT(l_periodic)) THEN 
        idx=n
        IF (PRESENT(idx2)) idx2=1
      ELSE 
        idx=1
        IF (PRESENT(idx2)) idx2=2
      ENDIF
    ENDIF

  END SUBROUTINE ns_index
! mz_ap_20070913-
! --------------------------------------------------------------------

! ---------------------------------------------------------------------
  SUBROUTINE nn_index(list, value, idx, idx2)

    IMPLICIT NONE
    INTRINSIC :: ABS, PRESENT, SIZE

    ! I/O
    REAL(DP), DIMENSION(:), INTENT(IN)            :: list
    REAL(DP),               INTENT(IN)            :: value
    INTEGER,                INTENT(OUT)           :: idx
    INTEGER,                INTENT(OUT), OPTIONAL :: idx2

    ! LOCAL
    INTEGER     :: n, i
    REAL(DP)    :: dmin

    ! INIT
    n = SIZE(list)
    idx = 0
    dmin = ABS(list(1) - list(n))
    DO i=1, n
       IF (ABS(list(i)-value) <= dmin) THEN
          dmin = ABS(list(i)-value)
          idx = i
       END IF
    END DO

    ! NOTE: FOR IDX2 IT IS ASSUMED THAT THE LIST IS ORDERED
    IF (PRESENT(idx2)) THEN
       IF (idx == 1) THEN
          idx2 = 2
          RETURN
       END IF
       IF (idx == n) THEN
          idx2 = n-1
          RETURN
       END IF
       IF ( ABS(list(idx+1)-value) <= ABS(list(idx-1)-value) ) THEN
          idx2 = idx+1
       ELSE
          idx2 = idx-1
       END IF
    END IF

  END SUBROUTINE nn_index
! --------------------------------------------------------------------

! ---------------------------------------------------------------------  
  ! mz_ht_20042510+
  SUBROUTINE init_convect_tables

    ! Lookup tables for convective adjustment code
    !
    ! D. Salmond, CRAY (UK), August 1991, original code

    USE messy_main_constants_mem, ONLY: rd, rv, tmelt, cpd => cp_air &
                                      , alv, als ! op_pj_20100209

    IMPLICIT NONE
    INTRINSIC :: EXP, LOG
                       
    REAL(dp), PARAMETER :: zavl1 = -6096.9385_dp
    REAL(dp), PARAMETER :: zavl2 =    21.2409642_dp
    REAL(dp), PARAMETER :: zavl3 =    -2.711193_dp
    REAL(dp), PARAMETER :: zavl4 =     1.673952_dp
    REAL(dp), PARAMETER :: zavl5 =     2.433502_dp 

    REAL(dp), PARAMETER :: zavi1 = -6024.5282_dp
    REAL(dp), PARAMETER :: zavi2 =    29.32707_dp
    REAL(dp), PARAMETER :: zavi3 =     1.0613868_dp
    REAL(dp), PARAMETER :: zavi4 =    -1.3198825_dp
    REAL(dp), PARAMETER :: zavi5 =    -0.49382577_dp   
    
! op_pj_20100209+
!!$    REAL(dp), PARAMETER :: alv   = 2.5008e6_dp ! latent heat for
!!$    !                                          ! vaporisation in J/kg
!!$    REAL(dp), PARAMETER :: als   = 2.8345e6_dp ! latent heat for
!!$    !                                          ! sublimation in J/kg
! op_pj_20100209-
    ! Constants used for computation of saturation mixing ratio
    ! over liquid water (*c_les*) or ice(*c_ies*)
    REAL(dp),PARAMETER :: c3les = 17.269_dp           ! 
    REAL(dp),PARAMETER :: c3ies = 21.875_dp           ! 
    REAL(dp),PARAMETER :: c4les = 35.86_dp            ! 
    REAL(dp),PARAMETER :: c4ies = 7.66_dp             ! 
    REAL(dp),PARAMETER :: c5les = c3les*(tmelt-c4les) ! 
    REAL(dp),PARAMETER :: c5ies = c3ies*(tmelt-c4ies) ! 
   
    REAL(dp) :: z5alvcp, z5alscp, zalvdcp, zalsdcp
    REAL(dp) :: ztt, zldcp
!!$    REAL(dp) :: zcvm3, zcvm4, zcvm5
    REAL(dp) :: zcvm4, zcvm5
    REAL(dp) :: zavm1, zavm2, zavm3, zavm4, zavm5

    INTEGER :: it

    z5alvcp = c5les*alv/cpd
    z5alscp = c5ies*als/cpd

    zalvdcp = alv/cpd
    zalsdcp = als/cpd

    DO it = jptlucu1, jptlucu2
      ztt = 0.001_dp*it
      IF ((ztt-tmelt) > 0.0_dp) THEN
!!$        zcvm3 = c3les
        zcvm4 = c4les
        zcvm5 = z5alvcp
        zldcp = zalvdcp
        zavm1 = zavl1
        zavm2 = zavl2
        zavm3 = zavl3
        zavm4 = zavl4
        zavm5 = zavl5
      ELSE
!!$        zcvm3 = c3ies
        zcvm4 = c4ies
        zcvm5 = z5alscp
        zldcp = zalsdcp
        zavm1 = zavi1
        zavm2 = zavi2
        zavm3 = zavi3
        zavm4 = zavi4
        zavm5 = zavi5
      END IF
      tlucuc(it)  = zldcp
      tlucua(it)  = EXP((zavm1/ztt+zavm2+zavm3*0.01_dp* &
           ztt+zavm4*ztt*ztt*1.e-5_dp+zavm5*LOG(ztt)))*rd/rv
      tlucub(it)  = zcvm5*(1.0_dp/(ztt-zcvm4))**2
      tlucuaw(it) = EXP((zavl1/ztt+zavl2+zavl3*0.01_dp* &
           ztt+zavl4*ztt*ztt*1.e-5_dp+zavl5*LOG(ztt)))*rd/rv
    END DO
    
  END SUBROUTINE init_convect_tables
  ! mz_ht_20042510-
! ---------------------------------------------------------------------  

! ---------------------------------------------------------------------  
LOGICAL FUNCTION match_wild(pattern, string)

   ! compare given string for match to pattern which may
   ! contain wildcard characters:
   ! "?" matching any one character, and
   ! "*" matching any zero or more characters.
   ! Both strings may have trailing spaces which are ignored.
   ! Authors: Clive Page, userid: cgp  domain: le.ac.uk, 2003 (original code)
   !          Rolf Sander, 2005 (bug fixes and pattern preprocessing)
   ! Minor bug fixed by Clive Page, 2005 Nov 29.

   ! This program is free software; you can redistribute it and/or modify
   ! it under the terms of the GNU General Public License as published by
   ! the Free Software Foundation; either version 2 of the License, or
   ! (at your option) any later version.
   !
   ! This program is distributed in the hope that it will be useful,
   ! but WITHOUT ANY WARRANTY; without even the implied warranty of
   ! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   ! GNU General Public License for more details.
   !
   ! You should have received a copy of the GNU General Public License
   ! along with this program; if not, write to the Free Software
   ! Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 
   ! 02110-1301  USA

   IMPLICIT NONE
   INTRINSIC :: INDEX, LEN, LEN_TRIM, REPEAT

   CHARACTER(LEN=*), INTENT(IN) :: pattern ! pattern may contain * and ?
   CHARACTER(LEN=*), INTENT(IN) :: string  ! string to be compared
   INTEGER :: lenp, lenp2, lens, n, p2, p, s
   INTEGER :: n_question, n_asterisk

   CHARACTER(LEN=LEN(pattern)) :: pattern2

   lens = LEN_TRIM(string)
   lenp = LEN_TRIM(pattern)

   ! If the pattern is empty, always return true
   IF (lenp == 0) THEN
     match_wild = .TRUE.
     RETURN
   ENDIF

   ! The pattern must be preprocessed. All consecutive occurences of
   ! one or more question marks ('?') and asterisks ('*') are sorted and
   ! compressed. The result is stored in pattern2.

   pattern2(:)=''
   p  = 1 ! current position in pattern
   p2 = 1 ! current position in pattern2
   DO
     IF ((pattern(p:p) == '?').OR.(pattern(p:p) == '*')) THEN
       ! a special character was found in the pattern
       n_question = 0
       n_asterisk = 0
       DO WHILE (p <= lenp)
         ! count the consecutive question marks and asterisks
         IF ((pattern(p:p) /= '?').AND.(pattern(p:p) /= '*')) EXIT
         IF (pattern(p:p) == '?') n_question = n_question + 1
         IF (pattern(p:p) == '*') n_asterisk = n_asterisk + 1
         p = p + 1
       ENDDO
       IF (n_question>0) THEN ! first, all the question marks
         pattern2(p2:p2+n_question-1) = REPEAT('?',n_question)
         p2 = p2 + n_question
       ENDIF
       IF (n_asterisk>0) THEN ! next, the asterisk (only one!)
         pattern2(p2:p2) = '*'
         p2 = p2 + 1
       ENDIF
     ELSE
       ! just a normal character
       pattern2(p2:p2) = pattern(p:p)
       p2 = p2 + 1
       p = p + 1
     ENDIF
     IF (p > lenp) EXIT
   ENDDO
   lenp2 = LEN_TRIM(pattern2)

   ! The modified wildcard in pattern2 is compared to the string:

   p2 = 1
   s = 1
   match_wild = .FALSE.
   DO
     IF (pattern2(p2:p2) == '?') THEN
       ! accept any char in string
       p2 = p2 + 1
       s = s + 1
     ELSEIF (pattern2(p2:p2) == "*") THEN
       p2 = p2 + 1
       IF (p2 > lenp2) THEN
         ! anything goes in rest of string
         match_wild = .TRUE.
         EXIT ! .TRUE.
       ELSE
         ! search string for char at p2
         n = INDEX(string(s:), pattern2(p2:p2))
         IF (n == 0) EXIT  ! .FALSE.
         s = n + s - 1
       ENDIF
     ELSEIF (pattern2(p2:p2) == string(s:s)) THEN
       ! single char match
       p2 = p2 + 1
       s = s + 1
     ELSE
       ! non-match
       EXIT ! .FALSE.
     ENDIF
     IF (p2 > lenp2 .AND. s > lens) THEN
       ! end of both pattern2 and string
       match_wild = .TRUE.
       EXIT ! .TRUE.
     ENDIF

     IF (s > lens .AND. p2 == lenp) THEN
       IF(pattern2(p2:p2) == "*") THEN
         ! "*" at end of pattern2 represents an empty string
         match_wild = .TRUE.
         EXIT ! .TRUE.
       END IF
     ENDIF
     IF (p2 > lenp2 .OR. s > lens) THEN
       ! end of either pattern2 or string
       EXIT ! .FALSE.
     ENDIF
   ENDDO

END FUNCTION match_wild
! ---------------------------------------------------------------------  

! ----------------------------------------------------------------------
  SUBROUTINE str2chob(status, str, n, c, o)

    USE messy_main_constants_mem, ONLY: STRLEN_ULONG

    IMPLICIT NONE
    INTRINSIC :: ASSOCIATED, TRIM

    ! I/O
    INTEGER,           INTENT(OUT)               :: status
    CHARACTER(LEN=*),  INTENT(IN)                :: str
    INTEGER,           INTENT(OUT)               :: n
    CHARACTER(LEN=*),  DIMENSION(:), POINTER     :: c
    CHARACTER(LEN=*),  DIMENSION(:), POINTER     :: o

    ! LOCAL
    INTEGER                                            :: nc, no, two, j, i
    CHARACTER(LEN=STRLEN_ULONG), DIMENSION(:), POINTER :: tmp1 => NULL()
    CHARACTER(LEN=STRLEN_ULONG), DIMENSION(:), POINTER :: tmp2 => NULL()
    CHARACTER(LEN=STRLEN_ULONG), DIMENSION(:), POINTER :: tmp3 => NULL()

    ! INIT
    IF (ASSOCIATED(c)) DEALLOCATE(c)
    IF (ASSOCIATED(o)) DEALLOCATE(o)
    NULLIFY(c)
    NULLIFY(o)
    n = 0
    status = 1 ! ERROR

    ! COUNT OBJECTS
    CALL strcrack(TRIM(str), ';', tmp1, nc)           ! CHANNEL BLOCKS
    DO i=1, nc
       CALL strcrack(TRIM(tmp1(i)), ':', tmp2, two)   ! ONE CHANNEL PER BLOCK
       IF (two /= 2) RETURN      ! ERROR: 0 or more than 1 ':' in string
       CALL strcrack(TRIM(tmp2(2)), ',', tmp3, no)    ! OBJECTS PER CHANNEL
       n = n + no
    END DO

    ! ALLOCATE SPACE
    ALLOCATE(c(n))
    ALLOCATE(o(n))
    ! INIT
    DO i=1,n
       c(i) = ''
       o(i) = ''
    END DO

    ! PARSE STRING
    n = 0
    CALL strcrack(TRIM(str), ';', tmp1, nc)           ! CHANNEL BLOCKS
    DO i=1, nc
       CALL strcrack(TRIM(tmp1(i)), ':', tmp2, two)   ! ONE CHANNEL PER BLOCK
       CALL strcrack(TRIM(tmp2(2)), ',', tmp3, no)    ! OBJECTS PER CHANNEL
       DO j=1, no
          n = n + 1
          c(n) = TRIM(tmp2(1))
          o(n) = TRIM(tmp3(j))
       END DO
    END DO   

    IF (ASSOCIATED(tmp1)) THEN
      DEALLOCATE(tmp1) ; NULLIFY(tmp1)
    ENDIF
    IF (ASSOCIATED(tmp2)) THEN
      DEALLOCATE(tmp2) ; NULLIFY(tmp2)
    ENDIF
    IF (ASSOCIATED(tmp3)) THEN
      DEALLOCATE(tmp3) ; NULLIFY(tmp3)
    ENDIF

    status  = 0  ! NO ERROR

  END SUBROUTINE str2chob
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
  SUBROUTINE bilin_weight(vn, v, w)
    
    IMPLICIT NONE

    ! I/O
    REAL(dp), DIMENSION(4, 2), INTENT(IN)  :: vn ! neighbours
    REAL(dp), DIMENSION(2),    INTENT(IN)  :: v  ! position
    REAL(dp), DIMENSION(4),    INTENT(OUT) :: w  ! weight

    ! LOCAL
    REAL(DP) :: t, u

    t = (v(1) - vn(1,1)) / (vn(2,1) - vn(1,1))
    u = (v(2) - vn(1,2)) / (vn(3,2) - vn(1,2))

    w(1) = (1-t)*(1-u)
    w(2) = t*(1-u)
    w(3) = t*u
    w(4) = (1-t)*u

  END SUBROUTINE bilin_weight
! ----------------------------------------------------------------------

! ---------------------------------------------------------------------
! mz_rs_20070327+
  REAL(dp) FUNCTION psat(T)
    IMPLICIT NONE
    REAL(dp), INTENT(in) :: T ! temperature [K]
    ! psat = saturation partial pressure of gaseous H2O [Pa]
    psat = 6.982616467E+5_dp & 
           - 1.888612677E+4_dp*T    + 2.132971127E+2_dp*T**2 &
           - 1.288405237E+0_dp*T**3 + 4.393186046E-3_dp*T**4 &
           - 8.023554873E-6_dp*T**5 + 6.136820929E-9_dp*T**6
  END FUNCTION psat
! mz_rs_20070327-
! ---------------------------------------------------------------------

! mz_hr_20070906+
! ---------------------------------------------------------------------
  REAL(dp) FUNCTION psatf(z_temp)
  ! source: Mark Z. Jacobson, Fundamentals of Atmospheric Modeling,
  !         Cambridge University Press, 2000, page 32 
  !         ISBN: 0-521-637-171

    IMPLICIT NONE

    REAL(dp), INTENT(in) :: z_temp ! temperature [K]

    ! calc sat vap press of H2O [Pa]
    psatf = 611.2_dp * EXP( 6816._dp * (1._dp/273.15_dp-1._dp/z_temp) &
            + 5.1309_dp * LOG(273.15_dp/z_temp) )

  END FUNCTION psatf
! ---------------------------------------------------------------------
!mz_hr_20080226-

!mz_hr_20080221+
  ! ---------------------------------------------------------------------
  SUBROUTINE ucase(string)
    ! from book Stephen Chapman "F90/95"

    IMPLICIT NONE

    INTRINSIC LGE, LLE, ACHAR, IACHAR

    ! I/O
    CHARACTER(LEN=*),        INTENT(INOUT)  :: string

    ! LOCAL
    INTEGER :: i 
    INTEGER :: length

    ! get len of str
    length = LEN(string)

    ! shift lower case to upper case
    DO i=1, length
      IF (LGE(string(i:i),'a') .AND. LLE(string(i:i),'z')) THEN
        string(i:i) = ACHAR(IACHAR(string(i:i)) - 32)
      ENDIF
    ENDDO
    
  END SUBROUTINE ucase
! ---------------------------------------------------------------------
!mz_hr_20080221-

!mz_hr_20080226+
  ! ---------------------------------------------------------------------
  REAL(dp) FUNCTION spec2relhum(status, z_spechum, z_temp, z_press)

    ! calculates relative humidity from specific humidity
    ! [Jacobson, Fundamentals of Atmospheric Modeling, CamUnivPress, 1999]

    ! molar mass of water (vapour) / molar mass of dry air
    USE messy_main_constants_mem, ONLY: MM_eps, TINY_DP, FLAGGED_BAD

    IMPLICIT NONE

    ! e funct, natural log, absolute value
    INTRINSIC EXP, LOG, ABS

    !I/O
    INTEGER, INTENT(OUT) :: status
    REAL(dp), INTENT(IN) :: z_spechum    ! kg/kg
    REAL(dp), INTENT(IN) :: z_temp       ! K
    REAL(dp), INTENT(IN) :: z_press      ! Pa

    !LOCAL
    REAL(dp) :: omega_v, omega_vs

    status = 0

    ! convert spechum to mass mixing ratio of water in dry air
    IF (ABS(1._dp - z_spechum) < TINY_DP) THEN
      WRITE(*,*) 'Error spec2relhum: spechum = 1, division by 0'
      spec2relhum = FLAGGED_BAD
      status = 1
      RETURN
    !mz_hr_20081125+
    ELSEIF (z_spechum > 1._dp) THEN
      WRITE(*,*) 'Error spec2relhum: spechum > 1 : ', z_spechum
      spec2relhum = FLAGGED_BAD
      status = 1
      RETURN
    ELSEIF (z_spechum < 0._dp) THEN
      WRITE(*,*) 'Error spec2relhum: spechum < 0 : ', z_spechum
      spec2relhum = FLAGGED_BAD
      status = 1
      RETURN
    !mz_hr_20081125-
    ENDIF

    omega_v = z_spechum/(1._dp-z_spechum)
    omega_vs = MM_eps * psatf(z_temp) / (z_press - psatf(z_temp))

    ! calc relhum, def by World Meteorological Organization WMO
    spec2relhum = omega_v / omega_vs

    IF ((spec2relhum >= 1._dp) .AND. (spec2relhum <= 1.1_dp)) THEN
      WRITE(*,*) 'Warning spec2relhum: calculated rel. humidity >= 1 (< 1.1) : ', &
        spec2relhum
    ELSEIF ((spec2relhum > 1.1_dp)) THEN
      WRITE(*,*) 'Error spec2relhum: calculated rel. humidity > 1.1 : ', &
        spec2relhum
      spec2relhum = FLAGGED_BAD
      status = 1
      RETURN
    ELSEIF (spec2relhum < 0._dp) THEN
      WRITE(*,*) 'Error spec2relhum: calculated rel. humidity < 0 : ', &
        spec2relhum
      spec2relhum = FLAGGED_BAD
      status = 1
      RETURN
    ENDIF

  END FUNCTION spec2relhum
! --------------------------------------------------------------------- 
!mz_hr_20080226-

  ! ---------------------------------------------------------------------
  REAL(dp) FUNCTION rel2spechum(status, z_relhum, z_temp, z_press)

    ! calculates specific humidity from relative humidity
    ! [Jacobson, Fundamentals of Atmospheric Modeling, CamUnivPress, 1999]

    ! molar mass of water (vapour) / molar mass of dry air
    USE messy_main_constants_mem, ONLY: MM_eps, TINY_DP, FLAGGED_BAD

    IMPLICIT NONE

    ! e funct, natural log, absolute value
    INTRINSIC EXP, LOG, ABS

    !I/O
    INTEGER, INTENT(OUT) :: status
    REAL(dp), INTENT(IN) :: z_relhum    ! 
    REAL(dp), INTENT(IN) :: z_temp       ! K
    REAL(dp), INTENT(IN) :: z_press      ! Pa

    !LOCAL
    REAL(dp) ::  omega_vs

    status = 0

    omega_vs = MM_eps * psatf(z_temp) / (z_press - psatf(z_temp))

    ! calc relhum, def by World Meteorological Organization WMO in %
    rel2spechum = 1._dp/(1._dp/(z_relhum * omega_vs) + 1._dp)

 
  END FUNCTION rel2spechum
! --------------------------------------------------------------------- 

  !---------------------------------------------------------------------
  FUNCTION find_next_free_unit(istart,istop) RESULT(unit)

    ! I/O
    INTEGER :: istart, istop, unit

    ! LOCAL
    LOGICAL        :: found, opened
    INTEGER        :: i
    CHARACTER(256) :: info

    found = .FALSE.
    DO i=istart,istop
       INQUIRE(unit=i,opened=opened)
       IF (.NOT.opened) THEN
          unit = i
          found = .TRUE.
          EXIT
       END IF
    END DO

    IF (.NOT. found) THEN
       WRITE(info,'(a,i2.2,a,i2.2,a)') &
         'No unit in range <',istart,':',istop,'> free.'
       !CALL error_bi(info,'find_next_free_unit')
    END IF

  END FUNCTION find_next_free_unit
!-------------------------------------------------------------------------

! ************************************************************************
END MODULE messy_main_tools
! ************************************************************************
