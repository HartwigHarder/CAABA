      
    SUBROUTINE pzextr(iest, xest, yest, yz, dy, nv)
  
      INTEGER, INTENT(IN)  :: iest, nv
      REAL,    INTENT(IN)  :: xest, yest(nv)
      REAL,    INTENT(OUT) :: yz(nv), dy(nv), 
      INTEGER, PARAMETER   :: IMAX = 13, NMAX = {%NTSPEC} ! 50

    ! Use polynomial extrapolation to evaluate nv functions at x = 0 by fitting a polynomial to a
    ! sequence of estimates with progressively smaller values x = xest, and corresponding function
    ! vectors yest(1:nv). This call is number iest in the sequence of calls. Extrapolated
    ! function values are output as yz(1:nv), and their estimated error is output as dy(1:nv).
    ! Parameters: Maximum expected value of iest is IMAX; of nv is NMAX.

      INTEGER :: j, k1
      REAL    :: delta, f1, f2, q, d(NMAX), qcol(NMAX,IMAX), x(IMAX)
      SAVE       qcol, x

      x(iest) = xest                  ! Save current independent variable.
      
      DO j = 1, nv
        dy(j) = yest(j)
        yz(j) = yest(j)
      ENDDO
      
      IF (iest == 1) THEN                 ! Store rst estimate in rst column.
        DO j = 1, nv
          qcol(j,1) = yest(j)
        ENDDO
      ELSE
        DO j = 1, nv
          d(j) = yest(j)
        ENDDO
        DO k1 = 1, iest-1
          delta = 1.0_dp/(x(iest-k1)-xest)
          f1 = xest*delta
          f2 = x(iest-k1)*delta
          DO j = 1, nv                  ! Propagate tableau 1 diagonal more.
            q = qcol(j,k1)
            qcol(j,k1) = dy(j)
            delta = d(j)-q
            dy(j) = f1*delta
            d(j) = f2*delta
            yz(j) = yz(j)+dy(j)
          ENDDO
        ENDDO
        DO j = 1, nv
          qcol(j,iest) = dy(j)
        ENDDO
      ENDIF

    ! return
  
    END SUBROUTINE pzextr



    SUBROUTINE rzextr(iest,xest,yest,yz,dy,nv)
     
      INTEGER iest,nv,IMAX,NMAX
      REAL xest,dy(nv),yest(nv),yz(nv)
      PARAMETER (IMAX=13,NMAX=50)
     
    ! Exact substitute for pzextr, but uses diagonal rational function extrapolation instead of
    ! polynomial extrapolation.
     
      INTEGER j,k
      REAL b,b1,c,ddy,v,yy,d(NMAX,IMAX),fx(IMAX),x(IMAX)
      SAVE d,x
  
      x(iest) = xest                   ! Save current independent variable.

      IF (iest == 1) THEN
        DO 11 j = 1,nv
          yz(j) = yest(j)
          d(j,1) = yest(j)
          dy(j) = yest(j)
        ENDDO 11
      ELSE
        DO 12 k = 1,iest-1
          fx(k+1) = x(iest-k)/xest
        ENDDO 12
        DO 14 j = 1,nv                   ! Evaluate next diagonal in tableau.
          yy = yest(j)
          v = d(j,1)
          c = yy
          d(j,1) = yy
          DO 13 k = 2,iest
            b1 = fx(k)*v
            b = b1-c
            IF (b /= 0.) THEN
              b = (c-v)/b
              ddy = c*b
              c = b1*b
            ELSE              ! Care needed to avoid division by 0.
              ddy = v
            ENDIF
          IF (k /= iest) v = d(j,k)
            d(j,k) = ddy
            yy = yy+ddy
          ENDDO 13
          dy(j) = ddy
          yz(j) = yy
        ENDDO 14
      ENDIF

    ! return

    END





    SUBROUTINE stifbs(y, dydx, nv, x, htry, eps, yscal, hdid, hnext, derivs)

      INTEGER ::  nv, NMAX, KMAXX, IMAX
      REAL    ::  eps, hdid, hnext, htry, x, dydx(nv), y(nv), yscal(nv), SAFE1, SAFE2, REDMAX, REDMIN, TINY, SCALMX
      EXTERNAL derivs
      PARAMETER (NMAX = 50, KMAXX = 7, IMAX = KMAXX+1, SAFE1 = .25, SAFE2 = .7, REDMAX = 1.e-5, REDMIN = .7, TINY = 1.e-30, SCALMX = .1)
!     C USES derivs, jacobn, simpr, pzextr
 
    ! Semi-implicit extrapolation step for integrating stiff o.d.e., with monitoring of local truncation
    ! error to adjust stepsize. Input are the dependent variable vector y(1:n) and its
    ! derivative dydx(1:n) at the starting value of the independent variable x. Also input are
    ! the stepsize to be attempted htry, the required accuracy eps, and the vector yscal(1:n)
    ! against which the error is scaled. On output, y and x are replaced by their new values, hdid
    ! is the stepsize that was actually accomplished, and hnext is the estimated next stepsize.
    ! derivs is a user-supplied subroutine that computes the derivatives of the right-hand side
    ! with respect to x, while jacobn (a fixed name) is a user-supplied subroutine that computes
    ! the Jacobi matrix of derivatives of the right-hand side with respect to the components of y.
    ! Be sure to set htry on successive steps to the value of hnext returned from the previous
    ! step, as is the case if the routine is called by odeint.
 
      INTEGER :: i, iq, k, kk, km, kmax, kopt, nvold, nseq(IMAX)
      REAL    :: eps1, epsold, errmax, fact, h, red, scale, work, wrkmin, xest, xnew, &
                 a(IMAX), alf(KMAXX,KMAXX), dfdx(NMAX), dfdy(NMAX,NMAX), err(KMAXX), yerr(NMAX), ysav(NMAX), yseq(NMAX)
      LOGICAL :: first, reduct
 
      SAVE a, alf, epsold, first, kmax, kopt, nseq, nvold, xnew
 
      DATA first/.TRUE./, epsold/-1./, nvold/-1/
      DATA nseq /2,6,10,14,22,34,50,70/                ! Sequence is different from bsstep.
 
      IF (eps /= epsold .OR. nv /= nvold) THEN             ! Reinitialize also if nv has changed.
        hnext = -1.e29
        xnew = -1.e29
        eps1 = SAFE1*eps
        a(1) = nseq(1)+1
   
        DO k = 1, KMAXX
          a(k+1) = a(k)+nseq(k+1)
        ENDDO
   
        DO iq = 2, KMAXX
          DO k = 1, iq-1
            alf(k,iq) = eps1**((a(k+1)-a(iq+1))/((a(iq+1)-a(1)+1.)*(2*k+1)))
          ENDDO
        ENDDO

        epsold = eps
        nvold = nv                                   ! Save nv.
        a(1) = nv+a(1)                      ! Add cost of Jacobian evaluations to work coefficients

        DO k = 1, KMAXX
          a(k+1) = a(k)+nseq(k+1)
        ENDDO

        DO kopt = 2, KMAXX-1
          IF ( a(kopt+1) .GT. (a(kopt)*alf(kopt-1,kopt)) ) GOTO 1
        ENDDO
      1 kmax = kopt
      ENDIF

      h = htry

      DO 16 i = 1, nv
        ysav(i) = y(i)
      ENDDO 16

      CALL jacobn(x, y, dfdx, dfdy, nv, nmax)       ! Evaluate Jacobian.

      IF (h /= hnext .OR. x /= xnew) THEN
        first = .TRUE.
        kopt = kmax
      ENDIF
      reduct = .FALSE.

    2 DO 18 k = 1, kmax
        xnew = x+h
        IF (xnew == x) PAUSE 'stepsize underflow in stifbs'

$$      CALL simpr(ysav, dydx, dfdx, dfdy, nmax, nv, x, h, nseq(k), yseq, derivs)       ! Semi-implicit midpoint rule.

        xest = (h/nseq(k))**2                       ! The rest of the routine is identical to bsstep.
        CALL pzextr(k, xest, yseq, y, yerr, nv)

        IF (k /= 1) THEN
          errmax = TINY

          DO 17 i = 1, nv
            errmax = max(errmax, abs(yerr(i)/yscal(i)))
          ENDDO 17

          errmax = errmax/eps
          km = k-1
          err(km) = (errmax/SAFE1)**(1./(2*km+1))
        ENDIF

        IF (k /= 1.and.(k.ge.kopt-1 .OR. first)) THEN
          IF (errmax.lt.1.) GOTO 4
          IF (k == kmax .OR. k == kopt+1) THEN
            red = SAFE2/err(km)
            GOTO 3
          ELSE IF (k == kopt) THEN
            IF (alf(kopt-1, kopt).lt.err(km)) THEN
              red = 1./err(km)
              GOTO 3
            ENDIF
          ELSE IF (kopt == kmax) THEN
            IF (alf(km, kmax-1).lt.err(km)) THEN
              red = alf(km, kmax-1)*SAFE2/err(km)
              GOTO 3
            ENDIF
          ELSE IF (alf(km, kopt).lt.err(km)) THEN
                 red = alf(km, kopt-1)/err(km)
                 GOTO 3
               ENDIF
        ENDIF

      ENDDO 18

    3 red = min(red, REDMIN)
      red = max(red, REDMAX)
      h = h*red
      reduct = .TRUE.
      GOTO 2
  
    4 x = xnew
      hdid = h
      first = .FALSE.
      wrkmin = 1.e35

      DO 19 kk = 1, km
        fact = max(err(kk), SCALMX)
        work = fact*a(kk+1)
        IF (work.lt.wrkmin) THEN
          scale = fact
          wrkmin = work
          kopt = kk+1
        ENDIF
      ENDDO 19

      hnext = h/scale
      IF (kopt.ge.k.and.kopt /= kmax.and..not.reduct) THEN
        fact = max(scale/alf(kopt-1,kopt), SCALMX)
        IF (a(kopt+1)*fact.le.wrkmin) THEN
          hnext = h/fact
          kopt = kopt+1
        ENDIF
      ENDIF

    ! return

    END SUBROUTINE stifbs

