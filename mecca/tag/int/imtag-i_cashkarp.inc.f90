! >>>>> CASH-KARP RK integration scheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    SUBROUTINE cashkarp(IVI,IDI,t,h,IVO,IER)   
    
      IMPLICIT NONE
    
      REAL(dp), INTENT(IN)  :: h, t           
      REAL(dp), INTENT(IN)  :: IVI({%NTSPEC},{%NISO}), IDI({%NTSPEC},{%NISO})
      REAL(dp), INTENT(OUT) :: IVO({%NTSPEC},{%NISO}), IER({%NTSPEC},{%NISO})
    
  ! using the 5th-order Cash-Karp Runge-Kutta method to advance the solution over an 
  ! interval h and return the incremented variables as IVO; also return an estimate 
  ! of the IVO local truncation error IER using the embedded fourth-order method
  ! 
  ! input: 
  ! :values for {%NTSPEC} variables IVI and their derivatives IDI known at t
  ! h: advance interval
  !
  ! output: 
  ! IVO and IER: vectors of new values and estimated error
  !
  ! iderivs returns the right-hand side derivatives using isotopologues concentrations 
  ! from IVT and PTs

      REAL(dp)            :: IVT({%NTSPEC},{%NISO})      ! temp

      REAL(dp), DIMENSION({%NTSPEC},{%NISO}) :: AK2, AK3, AK4, AK5, AK6
      REAL(dp), PARAMETER ::  A2 = 0.2_dp, &
                              A3 = 0.3_dp, &
                              A4 = 0.6_dp, &
                              A5 = 1.0_dp, &
                              A6 = 0.875_dp, &
                             B21 = 0.2_dp, &
                             B31 = 3.0_dp/40.0_dp, &
                             B32 = 9.0_dp/40.0_dp, &
                             B41 = 0.3_dp, &
                             B42 = -0.9_dp, &
                             B43 = 1.2_dp, &
                             B51 = -11.0_dp/54.0_dp, &
                             B52 = 2.5_dp, &
                             B53 = -70.0_dp/27.0_dp, &
                             B54 = 35.0_dp/27.0_dp, &
                             B61 = 1631.0_dp/55296.0_dp, &
                             B62 = 175.0_dp/512.0_dp, &
                             B63 = 575.0_dp/13824.0_dp, &
                             B64 = 44275.0_dp/110592.0_dp, &
                             B65 = 253.0_dp/4096.0_dp, &
                              C1 = 37.0_dp/378.0_dp, &
                              C3 = 250.0_dp/621.0_dp, &
                              C4 = 125.0_dp/594.0_dp, &
                              C6 = 512.0_dp/1771.0_dp, &
                             DC1 = C1-2825.0_dp/27648.0_dp, &
                             DC3 = C3-18575.0_dp/48384.0_dp, &
                             DC4 = C4-13525.0_dp/55296.0_dp, &
                             DC5 = -277.0_dp/14336.0_dp, &
                             DC6 = C6-0.25

    ! First step.
      IVT(:,:) = IVI(:,:)+h*(B21*IDI(:,:))
      CALL iderivs(t+A2*h,IVT,AK2)        ! Second step.

      IVT(:,:) = IVI(:,:)+h*(B31*IDI(:,:)+B32*AK2(:,:))
      CALL iderivs(t+A3*h,IVT,AK3)        ! Third step.

      IVT(:,:) = IVI(:,:)+h*(B41*IDI(:,:)+B42*AK2(:,:)+B43*AK3(:,:))
      CALL iderivs(t+A4*h,IVT,AK4)        ! Fourth step.

      IVT(:,:) = IVI(:,:)+h*(B51*IDI(:,:)+B52*AK2(:,:)+B53*AK3(:,:)+B54*AK4(:,:))
      CALL iderivs(t+A5*h,IVT,AK5)        ! Fifth step.

      IVT(:,:) = IVI(:,:)+h*(B61*IDI(:,:)+B62*AK2(:,:)+B63*AK3(:,:)+B64*AK4(:,:)+B65*AK5(:,:))
      CALL iderivs(t+A6*h,IVT,AK6)        ! Sixth step.
    
    ! output:

    ! accumulating increments with proper weights
      IVO(:,:) = IVI(:,:)+h*(C1*IDI(:,:)+C3*AK3(:,:)+C4*AK4(:,:)+C6*AK6(:,:))    
    
    ! Estimate error as difference between fourth and fifth order methods.
      IER(:,:) = h*(DC1*IDI(:,:)+DC3*AK3(:,:)+DC4*AK4(:,:)+DC5*AK5(:,:)+DC6*AK6(:,:))
    
!     RETURN
    
    END SUBROUTINE cashkarp
    
  ! ---------------------------------------------------------------------------

  ! stepper calls the cashkarp to take a Cash-Karp Runge-Kutta step:

    SUBROUTINE stepper(IVIO,IDI,t,htry,eps,ESC,hdid,hnext) ! +n,derivs
  
      IMPLICIT NONE
  
      REAL(dp), INTENT(INOUT) :: t, IVIO({%NTSPEC},{%NISO})
      REAL(dp), INTENT(OUT)   :: hdid, hnext
      REAL(dp), INTENT(IN)    :: IDI({%NTSPEC},{%NISO}), &
                                 ESC({%NTSPEC},{%NISO}), &      ! error scale
                                 htry, eps
  
    ! fifth-order Runge-Kutta stepper with monitoring of local truncation error to 
    ! ensure accuracy and adjust stepsize
    !
    ! input: 
    ! :{%NTSPEC}x{%NISO}-size vectors of variables IVIO and its derivatives IDI at 
    ! the starting value of the independent variable t
    ! htry: the stepsize to be attempted htry
    ! eps: required accuracy eps
    ! ESC: vector against which the error is scaled
    !
    ! output: 
    ! :IVIO and t are replaced by their new values
    ! hdid: stepsize that was actually accomplished
    ! hnext: estimated next stepsize
  
      INTEGER             :: i, j
      REAL(dp)            :: errmax, h, htemp, tnew

      REAL(dp)            :: IVT({%NTSPEC},{%NISO})      ! temp
      REAL(dp)            :: IER({%NTSPEC},{%NISO})      ! error
      
      REAL(dp), PARAMETER :: SAFETY = 0.9_dp, &
                              PGROW = -0.2_dp, &
                             PSHRNK = -0.25_dp, &
                             ERRCON = (5.0_dp/SAFETY)**(1.0_dp/PGROW)  ! 1.89E-4_dp !
    
      ! The value ERRCON equals , see use below.
    
      h = htry           ! setting stepsize to the initial trial value
    
   77 CALL cashkarp(IVIO,IDI,t,h,IVT,IER)      ! taking a step
  
      errmax = 0.0_dp                          ! evaluating accuracy
      DO j = 1, {%NISO} 
        DO i = 1, {%NTSPEC} 
          errmax = MAX(errmax,DABS(IER(i,j)/ESC(i,j)))
    ENDDO
      ENDDO
    
      errmax = errmax/eps                      ! scaling relative to required tolerance
    
      IF (errmax .GT. 1.0_dp) THEN             ! truncation error is too large, reduce stepsize
    
        htemp = SAFETY * h * (errmax**PSHRNK)
        h = SIGN(MAX(DABS(htemp),0.1_dp*DABS(h)),h)     ! Not more than a factor of 10
        tnew = t + h
    
        IF (tnew .EQ. t) pause 'stepper: stepsize underflow'
        GOTO 77                                ! For another try.
  
      ELSE                                     ! Step succeeded. Computing the size of next step.
  
        IF (errmax .GT. ERRCON) THEN
          hnext = SAFETY * h * (errmax**PGROW)
        ELSE                                   ! No more than a factor of 5 increase.
          hnext = 5.0_dp * h
        ENDIF
    
        hdid = h
        t = t + h
    
        IVIO = IVT                             ! IVT() is returned from cashkarp
    
        RETURN
  
      ENDIF
    
    END SUBROUTINE stepper
  
  ! ---------------------------------------------------------------------------

    SUBROUTINE rkckintegrate(IVIO,t1,t2,eps,h1,hmin,nok,nbad)
    
      REAL(dp), INTENT(INOUT) :: IVIO({%NTSPEC},{%NISO})  ! in-out values
      REAL(dp), INTENT(IN)    :: t1, t2, eps, h1, hmin
  
      INTEGER, INTENT(OUT)    :: nok, nbad
  
      REAL(dp)                :: IVT({%NTSPEC},{%NISO})      ! temp vars
      REAL(dp)                :: IDT({%NTSPEC},{%NISO})      ! temp der
      REAL(dp)                :: IER({%NTSPEC},{%NISO})      ! error
      REAL(dp)                :: ESC({%NTSPEC},{%NISO})      ! error scale
  
      INTEGER, PARAMETER :: MAXSTP = 10000, &
                            ATINY =  1.e-30 ! TINY(dp) !
  
    ! Runge-Kutta driver with adaptive stepsize control
    ! 
    ! input:
    ! integrating the starting values IVIO()
    ! from t1 to t2 with accuracy eps
    ! storing intermediate results in the common block /path/.
    ! h1 should be set as a guessed first stepsize
    ! hmin as the minimum allowed stepsize (can be zero). 
    !
    ! output:
    ! nok and nbad are the number of good and bad (but retried and fixed) steps taken
    ! and IVIO is replaced by values at the end of the integration interval
    
      INTEGER                 :: i, j, nstp, nact
      REAL(dp)                :: t, h, hdid, hnext
    
      t = t1
      h = DSIGN(h1,t2-t1)
      nok = 0
      nbad = 0
    
      IVT = IVIO                               ! IVT - operational vector

      DO nstp = 1, MAXSTP                      ! take at most MAXSTP steps
  
        CALL iderivs(t,IVT,IDT)
      
        ! scaling (used for monitoring accuracy)
        ! can be modifed if there is a need

        ESC(:,:) = DABS(IVT(:,:)) + DABS(h*IDT(:,:)) + ATINY
    
        ! if stepsize can overshoot, decreasing
        IF ((t+h-t2)*(t+h-t1) .GT. 0.0_dp) h = t2 - t  
        
        CALL stepper(IVT,IDT,t,h,eps,ESC,hdid,hnext)
  
        IF (hdid .EQ. h) THEN
          nok = nok+1
        ELSE
          nbad = nbad+1
        ENDIF
    
        IF ((t-t2)*(t2-t1) .GE. 0.) THEN      ! done?
          
          IVIO = IVT                          ! output values
          RETURN                              ! normal exit
  
        ENDIF
  
        IF (DABS(hnext) .LT. hmin) pause 'rkckintegrate: stepsize smaller than minimum'
        h = hnext
      
      ENDDO       ! nstp cycle
  
      pause 'rkckintegrate: too many steps'
  
      RETURN
  
    END SUBROUTINE rkckintegrate

! <<<<< CASH-KARP RK integration scheme <<<<<<<<<<<<<<<<<<<<<<<<<<<<<