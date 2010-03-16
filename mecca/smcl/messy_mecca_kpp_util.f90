! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Auxiliary Routines File
! 
! Generated by KPP-2.2.1_rs5 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : messy_mecca_kpp_Util.f90
! Time                 : Tue Mar 16 16:04:25 2010
! Working directory    : /home/sander/e2/messy_2.3zp_rs_mim2/messy/mbm/caaba/mecca
! Equation file        : messy_mecca_kpp.kpp
! Output root filename : messy_mecca_kpp
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE messy_mecca_kpp_Util

  USE messy_mecca_kpp_Parameters
  IMPLICIT NONE

CONTAINS



! User INLINED Utility Functions

! from xmecca:
SUBROUTINE initialize_indexarrays
  USE messy_mecca_kpp_global     ! ind_XYZ_a(:) arrays
  USE messy_mecca_kpp_parameters ! ind_XYZ_a## scalars
  IMPLICIT NONE
END SUBROUTINE initialize_indexarrays

! End INLINED Utility Functions

! Utility Functions from KPP_HOME/util/util
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! UTIL - Utility functions
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! ****************************************************************
!                            
! InitSaveData - Opens the data file for writing
!   Parameters :                                                  
!
! ****************************************************************

      SUBROUTINE InitSaveData ()

      USE messy_mecca_kpp_Parameters

      open(10, file='messy_mecca_kpp.dat')

      END SUBROUTINE InitSaveData

! End of InitSaveData function
! ****************************************************************

! ****************************************************************
!                            
! SaveData - Write LOOKAT species in the data file 
!   Parameters :                                                  
!
! ****************************************************************

      SUBROUTINE SaveData ()

      USE messy_mecca_kpp_Global
      USE messy_mecca_kpp_Monitor

      INTEGER i

      WRITE(10,999) (TIME-TSTART)/3600.D0,  &
                   (C(LOOKAT(i))/CFACTOR, i=1,NLOOKAT)
999   FORMAT(E24.16,100(1X,E24.16))

      END SUBROUTINE SaveData

! End of SaveData function
! ****************************************************************

! ****************************************************************
!                            
! CloseSaveData - Close the data file 
!   Parameters :                                                  
!
! ****************************************************************

      SUBROUTINE CloseSaveData ()

      USE messy_mecca_kpp_Parameters

      CLOSE(10)

      END SUBROUTINE CloseSaveData

! End of CloseSaveData function
! ****************************************************************

! ****************************************************************
!                            
! GenerateMatlab - Generates MATLAB file to load the data file 
!   Parameters : 
!                It will have a character string to prefix each 
!                species name with.                                                 
!
! ****************************************************************

      SUBROUTINE GenerateMatlab ( PREFIX )

      USE messy_mecca_kpp_Parameters
      USE messy_mecca_kpp_Global
      USE messy_mecca_kpp_Monitor

      
      CHARACTER(LEN=8) PREFIX 
      INTEGER i

      open(20, file='messy_mecca_kpp.m')
      write(20,*) 'load messy_mecca_kpp.dat;'
      write(20,990) PREFIX
990   FORMAT(A1,'c = messy_mecca_kpp;')
      write(20,*) 'clear messy_mecca_kpp;'
      write(20,991) PREFIX, PREFIX
991   FORMAT(A1,'t=',A1,'c(:,1);')
      write(20,992) PREFIX
992   FORMAT(A1,'c(:,1)=[];')

      do i=1,NLOOKAT
        write(20,993) PREFIX, SPC_NAMES(LOOKAT(i)), PREFIX, i
993     FORMAT(A1,A6,' = ',A1,'c(:,',I2,');')
      end do
      
      CLOSE(20)

      END SUBROUTINE GenerateMatlab

! End of GenerateMatlab function
! ****************************************************************


! ****************************************************************
!                            
! tag2num - convert equation tags to kpp reaction number
!   Arguments :
!      id        - string with the equation tag
!
! ****************************************************************

ELEMENTAL INTEGER FUNCTION tag2num ( id )

  USE messy_mecca_kpp_Monitor, ONLY: EQN_TAGS

  CHARACTER(LEN=*), INTENT(IN) :: id
  INTEGER i

  tag2num = 0 ! mz_rs_20050115
  DO i = 1, SIZE(EQN_TAGS)
    IF (TRIM(EQN_TAGS(i)) == TRIM(id)) THEN
      tag2num = i ! mz_rs_20050115
      EXIT
    ENDIF
  END DO

END FUNCTION tag2num

! End of tag2num function
! ****************************************************************

! End Utility Functions from KPP_HOME/util/util
! End of UTIL function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Shuffle_user2kpp - function to copy concentrations from USER to KPP
!   Arguments :
!      V_USER    - Concentration of variable species in USER's order
!      V         - Concentrations of variable species (local)
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Shuffle_user2kpp ( V_USER, V )

! V_USER - Concentration of variable species in USER's order
  REAL(kind=dp) :: V_USER(NVAR)
! V - Concentrations of variable species (local)
  REAL(kind=dp) :: V(NVAR)

  V(12) = V_USER(1)
  V(5) = V_USER(2)
  V(26) = V_USER(3)
  V(7) = V_USER(4)
  V(9) = V_USER(5)
  V(28) = V_USER(6)
  V(29) = V_USER(7)
  V(19) = V_USER(8)
  V(8) = V_USER(9)
  V(2) = V_USER(11)
  V(1) = V_USER(12)
  V(27) = V_USER(13)
  V(30) = V_USER(14)
  V(25) = V_USER(15)
  V(6) = V_USER(16)
  V(17) = V_USER(17)
  V(18) = V_USER(18)
  V(15) = V_USER(19)
  V(22) = V_USER(20)
  V(21) = V_USER(21)
  V(14) = V_USER(22)
  V(20) = V_USER(23)
  V(13) = V_USER(24)
  V(24) = V_USER(25)
  V(10) = V_USER(26)
  V(16) = V_USER(27)
  V(3) = V_USER(28)
  V(11) = V_USER(29)
      
END SUBROUTINE Shuffle_user2kpp

! End of Shuffle_user2kpp function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Shuffle_kpp2user - function to restore concentrations from KPP to USER
!   Arguments :
!      V         - Concentrations of variable species (local)
!      V_USER    - Concentration of variable species in USER's order
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Shuffle_kpp2user ( V, V_USER )

! V - Concentrations of variable species (local)
  REAL(kind=dp) :: V(NVAR)
! V_USER - Concentration of variable species in USER's order
  REAL(kind=dp) :: V_USER(NVAR)

  V_USER(1) = V(12)
  V_USER(2) = V(5)
  V_USER(3) = V(26)
  V_USER(4) = V(7)
  V_USER(5) = V(9)
  V_USER(6) = V(28)
  V_USER(7) = V(29)
  V_USER(8) = V(19)
  V_USER(9) = V(8)
  V_USER(11) = V(2)
  V_USER(12) = V(1)
  V_USER(13) = V(27)
  V_USER(14) = V(30)
  V_USER(15) = V(25)
  V_USER(16) = V(6)
  V_USER(17) = V(17)
  V_USER(18) = V(18)
  V_USER(19) = V(15)
  V_USER(20) = V(22)
  V_USER(21) = V(21)
  V_USER(22) = V(14)
  V_USER(23) = V(20)
  V_USER(24) = V(13)
  V_USER(25) = V(24)
  V_USER(26) = V(10)
  V_USER(27) = V(16)
  V_USER(28) = V(3)
  V_USER(29) = V(11)
      
END SUBROUTINE Shuffle_kpp2user

! End of Shuffle_kpp2user function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! GetMass - compute total mass of selected atoms
!   Arguments :
!      CL        - Concentration of all species (local)
!      Mass      - value of mass balance
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE GetMass ( CL, Mass )

! CL - Concentration of all species (local)
  REAL(kind=dp) :: CL(NSPEC)
! Mass - value of mass balance
  REAL(kind=dp) :: Mass(1)

      
END SUBROUTINE GetMass

! End of GetMass function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE messy_mecca_kpp_Util

