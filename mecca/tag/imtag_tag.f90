! ==============================================================================
! {%CMODEL}_tag
! generated: {%TIMEDATE}
!
! this module is generated by imtag utility
!
! inter-configuration driver module: kinetics
! level: smcl
!
! {$TAG_INFO} ! this is a template for isotopes configuration
!
! [Gromov, MPI-C, 2007-2008]
! ==============================================================================

! - general tagging parameters (as conditional defines) ------------------------

#include "{%CMODEL}_tag_parameters.inc"

! ------------------------------------------------------------------------------


MODULE {%CMODEL}_tag

  USE messy_mecca_kpp, RNSPEC => NSPEC ! dp, ...
  
! common flow calculation, utils
  USE {%CMODEL}_tag_common

! configurations linked
! {$CONF_LIST} [%  USE {%CMODEL}_#%]

  IMPLICIT NONE
  
  PUBLIC tag_integrate

! ==============================================================================

CONTAINS

! ------------------------------------------------------------------------------

  SUBROUTINE tag_integrate(TSL, C, press, cair, temp)

#ifdef FUDGE
    USE caaba_mem, ONLY: model_time, model_start, time_step_len  ! in s
! {$CONF_LIST} [%    USE {%CMODEL}_#_box, ONLY: #_fudge%]
#endif

    IMPLICIT NONE

    ! I/O
    REAL(dp), INTENT(INOUT) :: C(:)  ! INOUT is due to the PTs scaling and init
    REAL(dp), INTENT(IN) :: press
    REAL(dp), INTENT(IN) :: cair
    REAL(dp), INTENT(IN) :: temp
  
    REAL(dp), INTENT(IN) :: TSL      ! TSL = times_step_len value    

  ! important: scaling PTs according to TSL -> get lin. approx. rates
    CALL tag_convertPTs_lrates(C, TSL)

#ifdef INTERFLOW
  ! calculating total molecules flow for all configurations
    CALL tag_flow_calc(C, TSL)
#endif

  ! integrating each configuration
! {$CONF_LIST} [%    CALL #_integrate(TSL, C, press, cair, temp)%]

#ifdef FUDGE
  IF ((model_time - model_start) .LT. 50.*time_step_len) THEN
! {$CONF_LIST} [%    CALL #_fudge%]
  ENDIF
#endif

  ! initializing passive tracers
    CALL tag_resetPTs(C)

  END SUBROUTINE tag_integrate
  

END MODULE {%CMODEL}_tag

! ******************************************************************************

