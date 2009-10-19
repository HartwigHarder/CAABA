! ==============================================================================
! {%CMODEL}_tag_box
! generated: {%TIMEDATE}
!
! This module is generated by imtag utility
!
! inter-configuration driver module: interface routines
! level: boxmodel
!
! {$TAG_INFO} ! this is a template for isotopes configuration
!
! [Gromov, MPI-C, 2007-2008]
! ==============================================================================

! - general tagging parameters (as conditional defines) ------------------------

#include "{%CMODEL}_tag_parameters.inc"

! -----------------------------------------------------------------------------


MODULE {%CMODEL}_tag_box

  USE messy_mecca_kpp ! dp, nreact, nspec, ind_*, SPC_NAMES, EQN_TAGS

  USE {%CMODEL}_tag        ! link main inter-conf. kinetics driver module

! configurations linked
! {$CONF_LIST} [%  USE {%CMODEL}_#_box%]

  IMPLICIT NONE

  PUBLIC tag_emis
  PUBLIC tag_process
  PUBLIC tag_init
  PUBLIC tag_result
  PUBLIC tag_finish

! =============================================================================

CONTAINS

! -----------------------------------------------------------------------------

  SUBROUTINE tag_x0
  
    IMPLICIT NONE

! {$CONF_LIST} [%    CALL #_x0%]

#ifdef DEBUG
    print *,'tag_x0: passed'
#endif 

  END SUBROUTINE tag_x0



! -----------------------------------------------------------------------------

  SUBROUTINE tag_emis
 
    IMPLICIT NONE
    
  ! TODO: emissions code for all configurations can be introduced here

  ! z.B.
  ! #ifdef TAG_IC
  !   tag_IC_emis(tag_IC_CH4, 1.0E-9*cair, -52.6_dp+2.6_dp)
  ! #endif
  
#ifdef DEBUG
    print *,'tag_emis: passed'
#endif 

  END SUBROUTINE tag_emis



! -----------------------------------------------------------------------------

  SUBROUTINE tag_process(TSL, nstep, C, press, cair, temp)

    IMPLICIT NONE

    ! I/O
    REAL(dp), INTENT(IN) :: C(:)
    REAL(dp), INTENT(IN) :: press
    REAL(dp), INTENT(IN) :: cair
    REAL(dp), INTENT(IN) :: temp
    
    REAL(dp), INTENT(IN) :: TSL
    INTEGER, INTENT(IN)  :: nstep

  ! call the kinetics driver to integrate all the configurations
    CALL tag_integrate(TSL, nstep, C, press, cair, temp)

  ! updating derived values for boxmodel
! {$CONF_LIST} [%    CALL #_calctotals(C)\n    CALL #_calcdeltas\n%]

#ifdef DEBUG
    print *,'tag_process: passed'
#endif 

  END SUBROUTINE tag_process



! -----------------------------------------------------------------------------

  SUBROUTINE tag_init

    IMPLICIT NONE

    CALL tag_x0

! {$CONF_LIST} [%    CALL #_init%]

#ifdef DEBUG
    print *,'tag_init: passed'
#endif 

  END SUBROUTINE tag_init



! -----------------------------------------------------------------------------

  SUBROUTINE tag_result(model_time)
  
    IMPLICIT NONE
    
    REAL(dp), INTENT(IN) :: model_time
    
! {$CONF_LIST} [%    CALL #_result(model_time)%]

#ifdef DEBUG
    print *,'tag_result: passed'
#endif 

  ! initializing passive tracers
    CALL tag_resetPTs(C)

  END SUBROUTINE tag_result



! ---------------------------------------------------------------------------

  SUBROUTINE tag_finish

! {$CONF_LIST} [%    CALL #_finish%]

#ifdef DEBUG
    print *,'tag_finish: passed'
#endif 

  END SUBROUTINE tag_finish



! ---------------------------------------------------------------------------

END MODULE {%CMODEL}_tag_box

! ***************************************************************************

