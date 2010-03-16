! >>>>> Simple Euler integration scheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

    {%TAG}_IREJCT = 0   ! # of rejected species
    {%TAG}_ISTEP  = 0   ! # of calculation steps made
    {%TAG}_ISPAR  = 0   ! # of pre-CLOSE_FAC overshoots
    
  ! first timestep
!   IF (delta_last .EQ. 0.0_dp) THEN
      delta = TSL ! / DBLE(nstep)
!   ELSE
!     delta = delta_last
!   ENDIF

    reject = .FALSE.; reject_last = .FALSE.
    done = 0.0_dp
    nok = 0; nbad = 0

    IR = I

    DO WHILE (done .LT. TSL)

#ifdef DEBUG    
      print *, 'step = ',{%TAG}_NSTEP+1,'   done = ',done,'   delta = ',delta
#endif

    ! advancing solution
      CALL iderivs_atom(I,ID,F)        ! I` -> ID
      I0%A = I%A + delta * ID%A
      I0%R = I%R + delta * ID%R

    ! error control      
      reject = .FALSE.
      maxerr = 0.0_dp
      DO n = 1, {%NSPEC}
        tot = SUM(I0%A(n,:))
        IF ( tot .GT. 0.0_dp ) THEN
          DO k = 2, {%NISO}
            drR = DABS( (IR%R(n,k)/SUM(IR%A(n,:))) - (I%R(n,k)/SUM(I%A(n,:))) ) ! * delta
            dr0 = DABS( (I0%R(n,k)/SUM(I0%A(n,:))) - (I%R(n,k)/SUM(I%A(n,:))) ) ! * delta
            drE = DABS( drR - dr0 ) * delta
            IF (  drE .GT. RTOL  ) THEN
              reject = .TRUE.
              reject_last = .TRUE.
            ENDIF
            maxerr = DMAX1( maxerr, RTOL / drE )
          ENDDO
        ENDIF
      ENDDO

    ! prepairing new step
      delta_last = delta

      IF (.NOT. reject) THEN

      ! accepted step
        IR = I
        I = I0
        done = done + delta

      ! if the last step was not rejected, assume greater next delta
      ! else keep
        IF (reject_last) THEN
          reject_last = .FALSE.
        ELSE
          tmp = delta * (maxerr**(PSHRNK))  ! * CLOSE_FAC
          delta = DMAX1(delta * FAR_FAC, tmp)
        ENDIF

        nok = nok + 1
      ELSE
        
      ! choosing new, smaller delta from "overshoot"
      ! plus soome safe stepback
        tmp =  delta * (maxerr**(PGROW))  ! * CLOSE_FAC
        delta = DMAX1(delta * 0.1_dp, tmp)

        nbad = nbad + 1
      ENDIF

    ! TSL overshoot check
      delta = DMIN1(delta, TSL-done)

    ENDDO        !   (WHILE)

    {%TAG}_ISTEP = nok
    {%TAG}_IREJCT = nbad

! <<<<< Simple Euler integration scheme <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<