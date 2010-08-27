! -*- f90 -*-
! ==============================================================================
! messy_mecca_tag
! generated: {%TIMEDATE}
!
! This module is generated by imtag utility
!
! inter-configuration module: kinetics
!
! {$TAG_INFO} ! this is a template for isotopes configuration
!
! [Gromov, MPI-C, 2007-2008]
! ==============================================================================

! - general tagging parameters (as conditional defines) ------------------------

! INCLUDE 'messy_mecca_tag_parameters.inc'

! ------------------------------------------------------------------------------


MODULE messy_mecca_tag_box

  USE messy_mecca_kpp ! dp, ...
  
! common flow calculation, utils
! USE messy_mecca_tag_common

! configurations linked
! {$CONF_LIST} [%  USE messy_mecca_#%]

  IMPLICIT NONE

  PUBLIC mecca_tag_emis
  PUBLIC mecca_tag_prepare
  PUBLIC mecca_tag_process
  PUBLIC mecca_tag_resetPTs
  PUBLIC mecca_tag_init
  PUBLIC mecca_tag_result
  PUBLIC mecca_tag_finish

! ==============================================================================

CONTAINS

! ------------------------------------------------------------------------------

  SUBROUTINE mecca_tag_emis
    IMPLICIT NONE
  END SUBROUTINE mecca_tag_emis
  
  SUBROUTINE mecca_tag_prepare(C)
    IMPLICIT NONE
    REAL(dp), INTENT(IN)  :: C(:)
  END SUBROUTINE mecca_tag_prepare

  SUBROUTINE mecca_tag_process(TSL, C, press, cair, temp)

    USE messy_main_constants_mem, ONLY: dp

    IMPLICIT NONE

    ! I/O
    REAL(dp), INTENT(IN)  :: C(:)
    REAL(dp), INTENT(IN)  :: press
    REAL(dp), INTENT(IN)  :: cair
    REAL(dp), INTENT(IN)  :: temp  
    REAL(dp), INTENT(IN) :: TSL      ! TSL = times_step_len value    

  END SUBROUTINE mecca_tag_process
  
  SUBROUTINE mecca_tag_resetPTs
    IMPLICIT NONE
  END SUBROUTINE mecca_tag_resetPTs

  SUBROUTINE mecca_tag_init
    IMPLICIT NONE
  END SUBROUTINE mecca_tag_init

  SUBROUTINE mecca_tag_result(model_time)
    IMPLICIT NONE
    REAL(dp), INTENT(IN) :: model_time
  END SUBROUTINE mecca_tag_result

  SUBROUTINE mecca_tag_finish
    IMPLICIT NONE
  END SUBROUTINE mecca_tag_finish


END MODULE messy_mecca_tag_box

! ******************************************************************************

