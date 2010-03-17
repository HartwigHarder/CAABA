!*****************************************************************************

! Time-stamp: <2009-01-12 00:35:10 sander>
! Author: R. Sander, based on random number generator in
! messy_attila.f90 by P. Joeckel, M. Traub, and A. Pozzer

!*****************************************************************************

MODULE messy_main_rnd

  USE messy_main_constants_mem, ONLY: DP
  USE messy_main_tools,         ONLY: PTR_1D_ARRAY_INT

  IMPLICIT NONE

  PRIVATE
  PUBLIC  :: rnd_init   ! initialize
  PUBLIC  :: rnd_number    ! generate array of random numbers
  PUBLIC  :: rnd_finish ! deallocate

  INTEGER, PARAMETER, PUBLIC :: &
    RND_F90       = 0, & ! Fortran90 intrinisc      (architecture   dependent)
    RND_MTW       = 1, & ! Mersenne Twister MT19937 (architecture independent)
    RND_LUX       = 2, & ! Luxury                   (architecture independent)
    RND_F90_GAUSS = 3, & ! Gauss normal distribution, based on F90
    RND_MTW_GAUSS = 4, & ! Gauss normal distribution, based on MTW
    RND_LUX_GAUSS = 5    ! Gauss normal distribution, based on LUX

  ! The following variables are for different random number series,
  ! each with their own id:
  INTEGER, PARAMETER :: ID_MAX  = 20 ! max number of series
  INTEGER :: nstate(ID_MAX)     =  0 ! dimension of state vector
  INTEGER :: rnd_method(ID_MAX)
  TYPE(PTR_1D_ARRAY_INT), DIMENSION(ID_MAX) :: state

CONTAINS

  !***************************************************************************

  SUBROUTINE rnd_init(status, id, method, pseed)

    USE messy_main_rnd_mtw, ONLY: init_genrand, mt, mti, mtinit
    USE messy_main_rnd_lux, ONLY: rluxgo, isdext
    IMPLICIT NONE

    INTEGER,           INTENT(OUT) :: status
    INTEGER,           INTENT(OUT) :: id
    INTEGER,           INTENT(IN)  :: method
    INTEGER, OPTIONAL, INTENT(IN)  :: pseed
    REAL :: randomseed
    INTEGER :: seed

    WRITE(*,*)
    WRITE(*,*) 'Initializing random number generator:'
    status = 0 ! ok

    ! define seed:
    IF (PRESENT(pseed)) THEN
      seed = pseed
      WRITE(*,*) 'seed   = ', seed
    ELSE
      CALL RANDOM_SEED
      CALL RANDOM_NUMBER(randomseed)
      seed = INT(HUGE(1)*randomseed)
      WRITE(*,*) 'seed   = ', seed, ' (computer-generated)'
    ENDIF

    ! assign an id to this series of random numbers:
    DO id=1,ID_MAX
      IF (nstate(id)==0) EXIT ! exit do loop
    ENDDO
    WRITE(*,*) 'id     = ', id
    IF (id>ID_MAX) THEN
      WRITE(*,*) 'ERROR: Too many series. Increase ID_MAX!'
      status = 1
      RETURN
    ENDIF

    ! allocate memory for the state array, and set the seed:
    rnd_method(id) = method
    WRITE(*,'(A)', ADVANCE='NO') ' method = '
    SELECT CASE(rnd_method(id))
    CASE(RND_F90,RND_F90_GAUSS)
      CALL RANDOM_SEED(put=SPREAD(seed,1,4))
      WRITE(*,'(A)', ADVANCE='NO') 'Fortran90 intrinisc'
      CALL RANDOM_SEED(size = nstate(id))
      ALLOCATE(state(id)%ptr(nstate(id)))
      state(id)%ptr(:) = seed
    CASE(RND_MTW,RND_MTW_GAUSS)
      WRITE(*,'(A)', ADVANCE='NO') 'Mersenne Twister MT19937'
      nstate(id) = SIZE(mt)+1
      ALLOCATE(state(id)%ptr(nstate(id)))
      CALL init_genrand(seed)
      mti = SIZE(mt)+1
      state(id)%ptr(1:624) = INT(mt(1:624))
      state(id)%ptr(625)   = INT(mti)
    CASE(RND_LUX,RND_LUX_GAUSS)
      WRITE(*,'(A)', ADVANCE='NO') 'Luxury'
      nstate(id) = SIZE(isdext)
      ALLOCATE(state(id)%ptr(nstate(id)))
      CALL rluxgo(4,seed,0,0)
      state(id)%ptr(:) = INT(isdext(:))
    CASE default
      WRITE(*,*) 'ERROR: unknown method id = ', rnd_method(id)
      status = 2
    END SELECT

    IF ((rnd_method(id)==RND_F90_GAUSS) .OR. &
      (rnd_method(id)==RND_MTW_GAUSS) .OR. (rnd_method(id)==RND_LUX_GAUSS)) THEN
      WRITE(*,*) ' (Gauss normal distribution)'
    ELSE
      WRITE(*,*) ''
    ENDIF

    WRITE(*,*) 'nstate = ', nstate(id)

  END SUBROUTINE rnd_init

  !***************************************************************************

  SUBROUTINE rnd_seed(id, size, put, get)

    IMPLICIT NONE
    INTEGER,           INTENT(IN)                :: id
    INTEGER, OPTIONAL, INTENT(OUT)               :: size
    INTEGER, OPTIONAL, INTENT(IN),  DIMENSION(:) :: put
    INTEGER, OPTIONAL, INTENT(OUT), DIMENSION(:) :: get

    IF (PRESENT(size)) THEN
      size = nstate(id)
    ENDIF

    IF (PRESENT(put)) THEN
      state(id)%ptr(:) = put(:)
    ENDIF

    IF (PRESENT(get)) THEN
      get(:) = state(id)%ptr(:)
    ENDIF

  END SUBROUTINE rnd_seed

  !***************************************************************************

  SUBROUTINE rnd_number(id, harvest, get)

    USE messy_main_rnd_mtw, ONLY: genrand_res53, mt, mti, WI
    USE messy_main_rnd_lux, ONLY: isdext, ranlux, rluxgo
    IMPLICIT NONE
    INTRINSIC :: random_seed, random_number

    INTEGER, INTENT(IN) :: id
    REAL(DP), DIMENSION(:), INTENT(OUT)           :: harvest
    INTEGER,  DIMENSION(:), INTENT(OUT), OPTIONAL :: get
    INTEGER :: j, n
    REAL, DIMENSION(:), ALLOCATABLE :: temp_array
    REAL(DP) :: p, q, y1, y2 ! for Marsaglia polar method

    n = SIZE(harvest)
    ! ------------------------------------------------------------------------
    SELECT CASE(rnd_method(id))
    ! ------------------------------------------------------------------------
    CASE(RND_F90)
      CALL RANDOM_SEED(put = state(id)%ptr(:))
      CALL RANDOM_NUMBER(harvest)
      CALL RANDOM_SEED(get = state(id)%ptr(:))
    ! ------------------------------------------------------------------------
    CASE(RND_MTW)
      mt(1:624) = INT(state(id)%ptr(1:624), WI)
      mti       = INT(state(id)%ptr(625),   WI)
      DO j = 1, n
        harvest(j) = REAL(genrand_res53(), DP)
      ENDDO
      state(id)%ptr(1:624) = INT(mt(1:624))
      state(id)%ptr(625)   = INT(mti)
    ! ------------------------------------------------------------------------
    CASE(RND_LUX)
      isdext(:) = INT(state(id)%ptr(:))
      ALLOCATE(temp_array(n))
      CALL ranlux(temp_array, n)
      harvest(:) = REAL(temp_array(:), DP)
      DEALLOCATE(temp_array)
      state(id)%ptr(:) = INT(isdext(:))
    ! ------------------------------------------------------------------------
    CASE(RND_F90_GAUSS)
      ! create normal distribution with Marsaglia polar method, see:
      ! http://de.wikipedia.org/wiki/Polar-Methode
      DO j=1,n
        DO
          CALL RANDOM_NUMBER (y1) ! produce a number between 0 and 1
          CALL RANDOM_NUMBER (y2) ! produce a number between 0 and 1
          q = (2.*y1-1)**2 + (2.*y2-1)**2
          IF (q<=1) EXIT
          ! if q>1, repeat calculation with new y1, y2
        ENDDO
        p = SQRT((-2.*LOG(q)/q))
        harvest(j)=(2.*y1-1.) * p
        !z2=(2.*y2-1.) * p ! z2 is not used here
      ENDDO
    ! ------------------------------------------------------------------------
    CASE(RND_MTW_GAUSS)
      ! create normal distribution with Marsaglia polar method, see:
      ! http://de.wikipedia.org/wiki/Polar-Methode
      mt(1:624) = INT(state(id)%ptr(1:624), WI)
      mti       = INT(state(id)%ptr(625),   WI)
      DO j = 1, n
        DO
          y1 = REAL(genrand_res53(), DP) ! produce a number between 0 and 1
          y2 = REAL(genrand_res53(), DP) ! produce a number between 0 and 1
          q = (2.*y1-1)**2 + (2.*y2-1)**2
          IF (q<=1) EXIT
          ! if q>1, repeat calculation with new y1, y2
        ENDDO
        p = SQRT((-2.*LOG(q)/q))
        harvest(j)=(2.*y1-1.) * p
        !z2=(2.*y2-1.) * p ! z2 is not used here
      ENDDO
      state(id)%ptr(1:624) = INT(mt(1:624))
      state(id)%ptr(625)   = INT(mti)
    CASE default
      WRITE(*,*) 'ERROR: unknown id'
    ! ------------------------------------------------------------------------
    END SELECT
    ! ------------------------------------------------------------------------

    IF (PRESENT(get)) THEN
      get(:) = state(id)%ptr(:)
    ENDIF

  END SUBROUTINE rnd_number

  !***************************************************************************

  SUBROUTINE rnd_finish(id)

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: id

    DEALLOCATE(state(id)%ptr)
    nstate(id) = 0 ! reset to make id available again

  END SUBROUTINE rnd_finish

  !***************************************************************************

END MODULE messy_main_rnd

!*****************************************************************************
