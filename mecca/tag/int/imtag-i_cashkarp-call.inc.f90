! >>>>> CASH-KARP RK integration scheme >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
                                                                              
  ! calling integrator                                                        
    CALL rkckintegrate(ISO{%ATOM}, &                                          
                       0.0_dp, 1.0_dp, 1.0E-7_dp, &                           
                       1.0_dp/DBLE(nstep), 0.0_dp, &                          
                       nok, nbad)                                             
                                                                              
  ! run-control parameters                                                    
    {%TAG}_ISTEP  = nok          ! # of successful steps made
    {%TAG}_IREJCT = nbad         ! # of rejected steps
                                                                              
! <<<<< CASH-KARP RK integration scheme <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<