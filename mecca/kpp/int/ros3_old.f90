MODULE KPP_ROOT_Integrator

  USE KPP_ROOT_Parameters, ONLY: NVAR, NFIX, NSPEC, LU_NONZERO
  USE KPP_ROOT_Global
  IMPLICIT NONE
  PUBLIC
  SAVE
  
  CHARACTER(LEN=50) :: IERR_NAME = ''
  !REAL(dp) :: t_before, t_after
      
CONTAINS

      SUBROUTINE INTEGRATE( TIN, TOUT, &
        ICNTRL_U, RCNTRL_U, ISTATUS_U, RSTATUS_U, IERR_U )

      IMPLICIT NONE

      KPP_REAL, INTENT(IN) :: TIN  ! Start Time
      KPP_REAL, INTENT(IN) :: TOUT ! End Time
      ! Optional input parameters and statistics
      INTEGER,  INTENT(IN),  OPTIONAL :: ICNTRL_U(20)
      KPP_REAL, INTENT(IN),  OPTIONAL :: RCNTRL_U(20)
      INTEGER,  INTENT(OUT), OPTIONAL :: ISTATUS_U(20)
      KPP_REAL, INTENT(OUT), OPTIONAL :: RSTATUS_U(20)
      INTEGER,  INTENT(OUT), OPTIONAL :: IERR_U

      INTEGER    INFO(5)

      INFO(1) = 0
      STEPMIN=1.E-37 ! mz_ak_20040707
      STEPMAX = 4800.

!        call cpu_time(t_before)
        CALL ROS3(NVAR,TIN,TOUT,STEPMIN,STEPMAX, &
                         0.0001_dp,VAR,ATOL,RTOL, &
                         Info)
!        call cpu_time(t_after)
        !print *, 'cpu time for call ros3: ',t_after-t_before

      END SUBROUTINE INTEGRATE

      
      SUBROUTINE ROS3(N,T,Tnext,Hmin,Hmax,Hstart, &
                         y,AbsTol,RelTol, &
                         Info)

!       L-stable Rosenbrock 3(2), with 
!     strongly A-stable embedded formula for error control.  
!
!     All the arguments aggree with the KPP syntax.
!
!  INPUT ARGUMENTS:
!     y = Vector of (NVAR) concentrations, contains the
!         initial values on input
!     [T, Tnext] = the integration interval
!     Hmin, Hmax = lower and upper bounds for the selected step-size.
!          Note that for Step = Hmin the current computed
!          solution is unconditionally accepted by the error
!          control mechanism.
!     AbsTol, RelTol = (NVAR) dimensional vectors of 
!          componentwise absolute and relative tolerances.
!     FUN = name of routine of derivatives. KPP syntax.
!          See the header below.
!     JAC_SP = name of routine that computes the Jacobian, in
!          sparse format. KPP syntax. See the header below.
!     Info(1) = 1  for  autonomous   system
!             = 0  for nonautonomous system 
!
!  OUTPUT ARGUMENTS:
!     y = the values of concentrations at Tend.
!     T = equals Tend on output.
!     Info(2) = # of FUN calls.
!     Info(3) = # of JAC_SP calls.
!     Info(4) = # of accepted steps.
!     Info(5) = # of rejected steps.
!    
!     Adrian Sandu, April 1996
!     The Center for Global and Regional Environmental Research

      USE KPP_ROOT_Jacobiansp
      USE KPP_ROOT_LinearAlgebra, ONLY: kppsolve

      IMPLICIT NONE !mz_rs_20030728

      ! mz_rs_20030728+
      KPP_REAL dround, gam, c21, c31, c32, b1, b2, b3, d1, d2, d3, a21
      KPP_REAL a31, a32, alpha2, alpha3, g1, g2, g3, tau, x1, x2, x3, ytol
      INTEGER    ier
      ! mz_rs_20030728-
      KPP_REAL     K1(NVAR), K2(NVAR), K3(NVAR)
      KPP_REAL     F1(NVAR), JAC(LU_NONZERO)
      KPP_REAL     Hmin,Hmax,Hstart,ghinv,uround
      KPP_REAL     y(NVAR), ynew(NVAR)
      KPP_REAL     AbsTol(NVAR), RelTol(NVAR)
      KPP_REAL     T, Tnext, H, Hold, Tplus
      KPP_REAL     ERR, factor, facmax
      INTEGER    n,nfcn,njac,Naccept,Nreject,i,j
      INTEGER    Info(5)
      LOGICAL    IsReject,Autonom
      ! EXTERNAL   FUN, JAC_SP

      ! print *,'N,T,Tnext,Hmin,Hmax,Hstart=', &
      !   N,T,Tnext,Hmin,Hmax,Hstart

!     Initialization of counters, etc.
      Autonom = Info(1) .EQ. 1
      uround = 1.e-15
      dround = SQRT(uround)
      H = MAX(1.e-8_dp, Hstart) ! mz_rs_20040830: dp added
      Tplus = T
      IsReject = .false.
      Naccept  = 0
      Nreject  = 0
      Nfcn     = 0
      Njac     = 0
      gam=   .43586652150845899941601945119356e+00
      c21=  -.10156171083877702091975600115545e+01
      c31=   .40759956452537699824805835358067e+01
      c32=   .92076794298330791242156818474003e+01
       b1=   .10000000000000000000000000000000e+01
       b2=   .61697947043828245592553615689730e+01
       b3=  -.42772256543218573326238373806514e+00
       d1=   .50000000000000000000000000000000e+00
       d2=  -.29079558716805469821718236208017e+01
       d3=   .22354069897811569627360909276199e+00
       a21 = 1.e0
       a31 = 1.e0
       a32 = 0.e0
       alpha2 = gam
       alpha3 = gam
       g1=   .43586652150845899941601945119356e+00
       g2=   .24291996454816804366592249683314e+00
       g3=   .21851380027664058511513169485832e+01


! === Starting the time loop ===      
 10    continue  
       Tplus = T + H
       if ( Tplus .gt. Tnext ) then
          H = Tnext - T
          Tplus = Tnext
       end if

!       call cpu_time(t_before)
       call JacTemplate(NVAR, T, y, JAC)
!       call cpu_time(t_after)
       !print *, 'cpu time for call JacTemplate: ',t_after-t_before
       Njac = Njac+1
       gHinv = -1.0e0/(gam*H)
       do 15 j=1,LU_NONZERO
         JAC(j) = -JAC(j) 
 15    continue
       do 20 j=1,NVAR
         JAC(LU_DIAG(j)) = JAC(LU_DIAG(j)) - gHinv
 20    continue

!print *, 'nvar=',nvar
!print *, 'jac=',jac

!       call cpu_time(t_before)
       call DECOMP (NVAR, JAC, ier)
!       call cpu_time(t_after)
       !print *, 'cpu time for call DECOMP: ',t_after-t_before

       if (ier.ne.0) then
!print *,'h,hmin=',h,hmin
         if ( H.gt.Hmin) then
            H = 5.0e-1*H
            go to 10
         else
            print *,'IER <> 0, H=',H
            stop
         end if      
       end if  
       
!       call cpu_time(t_before)

       call FunTemplate(NVAR, T, y, F1)

! ====== NONAUTONOMOUS CASE ===============
       IF (.not. Autonom) THEN
         ! mz_rs_20040830: dp added to next line:
         tau = sign(dround*max( 1.0e-6_dp, abs(T) ), T)
         call FunTemplate(NVAR, T+tau, y, K2)
         nfcn=nfcn+1
         do 30 j = 1,NVAR
           K3(j) = ( K2(j)-F1(j) )/tau
 30      continue
 
! ----- STAGE 1 (NONAUTONOMOUS) -----
         x1 = g1*H
         do 35 j = 1,NVAR
           K1(j) =  F1(j) + x1*K3(j)
 35      continue
         call kppsolve (JAC, K1)
      
! ----- STAGE 2 (NONAUTONOMOUS) -----
       do 40 j = 1,NVAR
         ynew(j) = y(j) + K1(j) 
 40    continue
       call FunTemplate(NVAR, T+gam*H, ynew, F1)
       nfcn=nfcn+1
       x1 = c21/H
       x2 = g2*H
       do 45 j = 1,NVAR
         K2(j) = F1(j) + x1*K1(j) + x2*K3(j)
 45    continue
       call kppsolve (JAC, K2)
       
! ----- STAGE 3  (NONAUTONOMOUS) -----
       x1 = c31/H
       x2 = c32/H
       x3 = g3*H
       do 50 j = 1,NVAR
         K3(j) = F1(j) + x1*K1(j) + x2*K2(j) + x3*K3(j)
 50    continue
       call kppsolve (JAC, K3)


! ====== AUTONOMOUS CASE ===============
       ELSE

! ----- STAGE 1 (AUTONOMOUS) -----
         do 60 j = 1,NVAR
           K1(j) =  F1(j) 
 60      continue
         call kppsolve (JAC, K1)
      
! ----- STAGE 2 (AUTONOMOUS) -----
       do 65 j = 1,NVAR
         ynew(j) = y(j) + K1(j) 
 65    continue
       call FunTemplate(NVAR, T + gam*H, ynew, F1)
       nfcn=nfcn+1
         x1 = c21/H
         do 70 j = 1,NVAR
           K2(j) = F1(j) + x1*K1(j) 
 70      continue
         call kppsolve (JAC, K2)
       
! ----- STAGE 3  (AUTONOMOUS) -----
       x1 = c31/H
       x2 = c32/H
       do 90 j = 1,NVAR
         K3(j) = F1(j) + x1*K1(j) + x2*K2(j)
 90    continue
       call kppsolve (JAC, K3)

       END  IF ! Autonomous

! ---- The Solution ---

       do 120 j = 1,NVAR
         ynew(j) = y(j) + b1*K1(j) + b2*K2(j) + b3*K3(j) 
 120   continue


! ====== Error estimation ========

        ERR=0.e0
        do 130 i=1,NVAR
           ytol = AbsTol(i) + RelTol(i)*abs(ynew(i))
           ERR=ERR+((d1*K1(i)+d2*K2(i)+d3*K3(i))/ytol)**2
 130    continue      
        ERR = MAX( uround, SQRT( ERR/NVAR ) )

! ======= Choose the stepsize ===============================

        factor = 0.9/ERR**(1.e0/3.e0)
        if (IsReject) then
            facmax=1.0
        else
            facmax=10.0
        end if 
        factor = MAX( 1.0e-1_dp, min(factor,facmax) ) ! mz_rs_20040830: dp added
        Hold = H
        H = min( Hmax, MAX(Hmin,factor*H) )    

! ======= Rejected/Accepted Step ============================

        if ( (ERR.gt.1).and.(Hold.gt.Hmin) ) then
          IsReject = .true.
          Nreject  = Nreject+1
        else
          IsReject = .false.
          do 140 i=1,NVAR
             y(i)  = ynew(i)
 140      continue 
          T = Tplus     
          Naccept = Naccept+1
        end if

!       call cpu_time(t_after)
       !print *, 'cpu time for rest of time loop: ',t_after-t_before
! ======= End of the time loop ===============================
      if ( T .lt. Tnext ) go to 10
 
     
      
! ======= Output Information =================================
!print *,'Nfcn,Njac,Naccept,Nreject=',Nfcn,Njac,Naccept,Nreject
      Info(2) = Nfcn
      Info(3) = Njac
      Info(4) = Naccept
      Info(5) = Nreject
      
      END SUBROUTINE ROS3

  
  
        SUBROUTINE FunTemplate(N, T, Y, P)

        USE KPP_ROOT_Global, ONLY: FIX, RCONST, TIME
        USE KPP_ROOT_Rates, ONLY: Update_SUN
        USE KPP_ROOT_Function, ONLY: Fun
        IMPLICIT NONE !mz_rs_20030728
        INTEGER N !mz_rs_20030728
        KPP_REAL   T, Told
        KPP_REAL   Y(NVAR), P(NVAR)
        Told = TIME
        TIME = T
        CALL UPDATE_SUN()
        ! CALL UPDATE_PHOTO() ! mz_rs_20021113: subroutine call switched off
        CALL Fun( Y, FIX, RCONST, P )
        TIME = Told
        END SUBROUTINE FUNTEMPLATE

        SUBROUTINE JacTemplate(N, T, Y, J)

        USE KPP_ROOT_Global, ONLY: FIX, RCONST, TIME
        USE KPP_ROOT_Rates, ONLY: Update_SUN
        USE KPP_ROOT_Jacobian, ONLY: Jac_SP
        IMPLICIT NONE !mz_rs_20030728
        INTEGER N !mz_rs_20030728
        KPP_REAL   Told, T
        KPP_REAL   Y(NVAR), J(LU_NONZERO)
        Told = TIME
        TIME = T
        CALL UPDATE_SUN()
        ! CALL UPDATE_PHOTO() ! mz_rs_20021113: subroutine call switched off
        CALL Jac_SP( Y, FIX, RCONST, J )
        TIME = Told
        END SUBROUTINE JacTemplate

      subroutine DECOMP(N,V,IER)

      USE messy_mecca_kpp_Jacobiansp

      IMPLICIT NONE ! mz_rs_20030728

      REAL(dp) V(LU_NONZERO), W(NSPEC)
      integer k, kk, j, jj
      integer N,IER ! mz_rs_20030728
      REAL(dp) a 

      IER = 0
      do k=1,N
        ! mz_rs_20040202: don't check if real value == 0
        ! if ( V( LU_DIAG(k) ) .eq. 0. ) then
        if ( ABS(V(LU_DIAG(k))) < TINY(a) ) then
!print *,'k,LU_DIAG(k),V(LU_DIAG(k))=',k,LU_DIAG(k),V(LU_DIAG(k))
            IER = k
            return
        end if
        do kk = LU_CROW(k), LU_CROW(k+1)-1
              W( LU_ICOL(kk) ) = V(kk)
        end do
        do kk = LU_CROW(k), LU_DIAG(k)-1
            j = LU_ICOL(kk)
            a = -W(j) / V( LU_DIAG(j) )
            W(j) = -a
            do jj = LU_DIAG(j)+1, LU_CROW(j+1)-1
               W( LU_ICOL(jj) ) = W( LU_ICOL(jj) ) + a*V(jj)
            end do
         end do
         do kk = LU_CROW(k), LU_CROW(k+1)-1
            V(kk) = W( LU_ICOL(kk) )
         end do
      end do
      end subroutine decomp

END MODULE KPP_ROOT_Integrator
