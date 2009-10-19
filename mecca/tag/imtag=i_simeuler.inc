! >>>>> Simple Euler integration scheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

  ! partitioning = 1/nstep
    part = 1.0_dp / DBLE(nstep)

    {%TAG}_NREJCT = 0   ! # of rejected species
    {%TAG}_NSTEP  = 0   ! # of calculation steps made
    
    delta = part        ! assume first suggested delta = partitioning
    reject = .FALSE.; reject_last = .FALSE.
    done = 0.0_dp
    nok = 0; nbad = 0

    DO WHILE (done .LT. 1.0_dp)

#ifdef DEBUG    
      print *, 'step = ',{%TAG}_NSTEP+1,'   done = ',done,'   delta = ',delta
#endif

      IF (.NOT. reject) THEN
      ! normal case
      !
      ! saving tracers (except T{%ATOM})
        ISAVE = ISO{%ATOM} 
      ELSE
      ! overshooting case, perivous step was rejected
      !
      ! restoring tracers and calculating again with a new delta
        ISO{%ATOM} = ISAVE 
      ENDIF

    ! calculating new isotopologues budgets

      CALL iderivs(0.0_dp, ISO{%ATOM}, IDER)
      ISO{%ATOM}(:,:) = ISO{%ATOM}(:,:) + delta * IDER(:,:)

    ! overshoot check
      reject = .FALSE.
      DO n = 1, {%NISO}
        DO s = 1, {%NTSPEC}
          IF ( ISO{%ATOM}(s,n) .LT. 0.0_dp )  THEN
          ! step is rejected
            reject = .TRUE.
            reject_last = .TRUE.
  
          ! choosing new, smaller delta from all overshooted:
          !
          ! to reach >0, delta should be scaled down,
          ! new delta <= -delta * P/(I-P), among all isotopologues
          !
            delta = DMIN1(delta, 1.0_dp-done, &
                          -CLOSE_FAC * delta * ISAVE(s,n) &
                         / ( ISO{%ATOM}(s,n) - ISAVE(s,n) ) )
          ENDIF
        ENDDO
      ENDDO

    ! prepairing new step
      IF (.NOT. reject) THEN
        done = done + delta

      ! assume next suggested delta = partitioning,
      ! or the rest if part goes beyond the calculation interval (1.0)
        IF (reject_last) THEN
          delta = DMIN1(delta, 1.0_dp-done)
          reject_last = .FALSE.
        ELSE
          delta = delta * FAR_FAC
      delta = DMIN1(delta, 1.0_dp-done)
        ENDIF

        nok = nok + 1
      ELSE
        nbad = nbad + 1
      ENDIF

    ENDDO        !   (WHILE)

    {%TAG}_NSTEP = nok + nbad
    {%TAG}_NREJCT = {%TAG}_NREJCT / {%TAG}_NSTEP

! <<<<< Simple Euler integration scheme <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<