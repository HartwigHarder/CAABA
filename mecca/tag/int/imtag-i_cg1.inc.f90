! >>>>> CG(1) method (Logg, Eriksson, Johnston; CFEC) >>>>>>>>>>>>>>>>>>>>>>>>>>

    {%TAG}_ISPAR = 0.0_dp

  ! ----- computing first step size -----

    CG1_MAXSTEP = TSL
  
  ! first guess: from last successful guess or from scratch
    IF ( last_first .LE. 0.0_dp) THEN
      delta = TSL / 5.0_dp             ! 20% of TSL
    ELSE
      delta = last_first * 1.20_dp     ! 20% of previous successful
    ENDIF                              
  
  ! trials
    DO s = 1, CG1_MAXTRY

    ! get solution at (done,+delta)
      CALL step_CG1(I,delta,IP)            ! IP = solution(I,delta) -- proposed solution

    ! computing resudial
      CALL iderivs_atom(IP,ID,F)           ! ID = derivative(IP)
      rc = MAX( MAXVAL( ( IP%A-I%A )/delta - ID%A ), &
                MAXVAL( ( IP%R-I%R )/delta - ID%R ) )

    ! estimating suitability
      IF ( ((delta*lev) .LT. 1.0_dp) .AND. ((delta*delta*rc) .LT. TOL) ) EXIT

    ! trying smaller timestep  
      delta = delta * 0.5_dp

    ENDDO
    
  ! firs step estimation trials number
    {%TAG}_ISPAR = s - 1
  ! storing successful estimation
    last_first = delta
    
    IF (s .GE. CG1_MAXTRY) THEN
      print *,'{%TAG}_integrate: CG1 failed to estimate first timestep'
      pause 'press any key'
    ! EXIT HERE
    ENDIF

  ! ----- integration loop -----
  
    damp = .FALSE.
    p_damp = .FALSE.

    delta_prev = delta
    done = 0.0_dp

    steps = 0
    DO WHILE (done .LT. TSL)

      done = done + delta
      steps = steps + 1
#ifdef DEBUG
      print *,'{%TAG}_integrate_CG1: doing step: ',steps,' @ done:',done,' [delta]: ',delta
#endif
  
      IF (damp) THEN
      
      ! --- performing forward Euler damping ---
        niter = 1
        CALL iderivs_atom(I,ID,F)            ! ID = derivative(I)
        I%A = I%A + delta * ID%A
        I%R = I%R + delta * ID%R
        steps2damp = steps2damp - 1
      
      ELSE
      
      ! --- regular CG1 step ---
        CALL step_CG1(I,delta,IP)            ! IP = solution(I,delta) -- proposed solution
                                             ! step_CG1 calculates I0 = derivative at current time (done)
#ifdef INT_CG1_PLAIN
        IF (damp) THEN
          IF (delta .LT. CG1_MAXSTEP) THEN
            CG1_MAXSTEP = delta
          ENDIF
          damp = .FALSE.
        ENDIF
#endif
        IF (damp) THEN
  
        ! prepairing for damping
          p_damp = .TRUE.
        
          steps2damp = MAX( INT( LOG(delta_prev*lev)+1.0_dp ), 2 )
#ifdef DEBUG
          print *,'{%TAG}_integrate_CG1: damping at ( ',done,' ), lev = ',lev,', steps2damp = ',steps2damp
#endif
        ! redoing current step:

        ! rolling back
          done = done - delta
          delta = delta_prev

        ! skipping solution obtained in IP
        ! skipping new rc, ed calculation
        
        ENDIF  ! IF (damp) #2

      ENDIF    ! IF (damp) #1
      
      IF ( .NOT.(p_damp) ) THEN
  
      ! --- computing resudials ---
    
      ! IP = solution(done,+delta)
      ! ID = derivative(done,+delta)

        IT%A = ( IP%A - I%A ) / delta
        IT%R = ( IP%R - I%R ) / delta

      ! continuous
        CALL iderivs_atom(IP,I0,F)           ! I0 = derivative(IP)
        rc = MAX( MAXVAL( IT%A - I0%A ), &
                  MAXVAL( IT%R - I0%R ) )

!!    ! diagnostic from Logg
!!    ! discrete resudial
!!      I0%A = 0.5_dp * ( I%A + IP%A )
!!      I0%R = 0.5_dp * ( I%R + IP%R )
!!
!!      CALL iderivs_atom(I0,I0,F)
!!      rd = MAX( MAXVAL( IT%A - I0%A ), &
!!                MAXVAL( IT%R - I0%R ) )

      ! continue with damping?
        IF ( steps2damp .EQ. 0 ) damp = .FALSE.

      ! --- redoing the step if it was not OK ---
        IF ( ( ((rc*delta) .GT. (2.0_dp*TOL)) .AND. (steps .GT. 1) ) &
#ifdef NTC_CG1_PLAIN
           .AND. .FALSE. &
#endif
                           ) THEN
        ! rolling back
          done = done - delta
          delta = delta_prev
          {%TAG}_IREJCT = {%TAG}_IREJCT + 1
#ifdef DEBUG
          print *,'{%TAG}_integrate_CG1: rejecting step: ',steps,'  @ ',done
#endif
        ELSE
          ! accepted step, advancing
          I = IP
        ENDIF

      ELSE    ! if not p_damp
        p_damp = .FALSE.
      ENDIF

    ! --- computing new stepsize ---
      delta_prev = delta
      IF ( rc .GT. 0.0_dp ) THEN
        dk = TOL/rc
      ELSE
        dk = CG1_MAXSTEP
      ENDIF
      
    ! a simple regulator
      dk = (1.0_dp + CG1_SIMREG) * dk * delta / (CG1_SIMREG * dk + delta)
      
    ! overriding timestep in case of damping
      IF (damp) THEN
        tmp = CG1_PRKLAM/lev
        IF (tmp .LT. dk) dk = tmp
      ENDIF
      
    ! checking too large step
      IF ( dk .GT. CG1_MAXSTEP ) dk = CG1_MAXSTEP

    ! checking TSL overshoot (tricking with +20% of new estimated stepsize)
      IF ( (done + dk*1.20_dp) .GT. TSL ) dk = (TSL - done)
      
      delta = dk

    ENDDO

  ! performance infomation
    {%TAG}_ISTEP = steps
  ! {%TAG}_IREJCT is updated within

! <<<<< CG(1) method (Logg, Eriksson, Johnston; CFEC) <<<<<<<<<<<<<<<<<<<<<<<<<<