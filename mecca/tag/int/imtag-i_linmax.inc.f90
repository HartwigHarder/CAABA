! >>>>> LINEAR-MATRIX integration scheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! Based on Bloch [1990], needs reconfiguration

  ! ----- stepping -----

  ! assessing values from the major jacobian

    delta = 0.0_dp
    DO n = 1, {%NTSPEC}
!     comp = MAXVAL(J(n,:,1), MASK=(J(n,:,1) .GT. 0.0_dp)) / &
!            MINVAL(J(n,:,1), MASK=(J(n,:,1) .GT. 0.0_dp))
      IF (C(TRPTIND(n)) .GT. 0.0_dp) &
        comp = MAXVAL(J(n,:,1), MASK=(J(n,:,1) .GT. 0.0_dp)) / &
               MINVAL(J(n,:,1), MASK=(J(n,:,1) .GT. 0.0_dp)) / C(TRPTIND(n))
      IF (comp .GT. delta) delta = comp
    ENDDO

  ! choosing the number of steps

!    steps = INT(2*1.0E5 * delta**2.71)
!    steps = INT(5.0E3 * done)

!    steps = ABS(INT(delta))   ! 250

    steps = 25

!PRINT *,steps

    IF (steps .LT. 25) steps = 25
    IF (steps .GT. 1000) steps = 1000
    
    delta = TSL / steps

/*
  ! preparing major and minor jac. matrices for integration
    DO n = 1, {%NTSPEC}
      Z(n) = J(n,n,1)
    ENDDO
    
    DO n = 1, {%NTSPEC}
      DO s = 1, {%NTSPEC}
        J(n,s,:) = J(n,s,:) / (1.0_dp - delta * Z(s))
      ENDDO
    ENDDO
*/

    DO s = 1, steps 

    ! advancing solution
      CALL iderivs_atom(I,I0,F)        ! I -> I0
      I%A = I%A + delta * I0%A
      I%R = I%R + delta * I0%R
      
    ! filtering negative concentrations 
      DO n = 1, {%NTSPEC}
        DO k = 1, {%NISO}    
          IF (I%A(n,k) .LT. 0.0_dp) THEN
            print *,'{%TAG}_integrate: ',TRIM(SPC_NAMES(RT{%A}IND(n))),', class #',k,&
                    ', abundant is negative = ',I%A(n,k)
            I%A(n,k) = 0.0_dp
          ENDIF
        ENDDO
        DO k = 2, {%NISO}    
          IF (I%R(n,k) .LT. 0.0_dp) THEN
            print *,'{%TAG}_integrate: ',TRIM(SPC_NAMES(RT{%A}IND(n))),', class #',k,&
                    ', rare is negative = ',I%R(n,k)
            I%R(n,k) = 0.0_dp
          ENDIF
        ENDDO
      ENDDO
      
    ENDDO

  ! performance info
    {%TAG}_ISTEP = steps
    {%TAG}_IREJCT = -1

! <<<<< LINEAR-MATRIX integration scheme <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<