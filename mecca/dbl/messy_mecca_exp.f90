!=============================================================================
! experiment utils unit for boxmodel simulations with MECCA:
! - mixing of the species concentrations with give background values
! - 0D chem-nudging (using the EHCAM/MECCA EVAL run output)
! - emission of a certain species from offline emission data from E5/M1
!   (prepared using icogesb scripts)
!
! * requires mo_netcdf module from mecca >= v2.2 release, or
! * requires netcdf module from caaba >= v2.4 release
!
! [Gromov, MPIC, 2007-2008]
!=============================================================================

#define noDEBUG

#include "messy_mecca_exp.inc"

MODULE messy_mecca_exp

  USE messy_mecca_kpp            ! dp, ...
  USE messy_main_constants_mem, ONLY: R_gas, N_A
  USE netcdf

  IMPLICIT NONE

  INTEGER :: ncid_nudge, ncid_offlemb

CONTAINS


!*******************************************************************************

! copied from old mo_netcdf module from MECCA releases for compatibility reasons

  SUBROUTINE nf(status) ! turns nf90_* function into subroutine + checks status
    INTEGER :: status
    IF (status /= nf90_noerr) THEN
      WRITE (*,*) 'netcdf error: ', nf90_strerror(status)
      STOP
    ENDIF
  END SUBROUTINE nf


! = pseudo-mixing ==============================================================

  SUBROUTINE pmix(TSL, dilF, ind, mix2_amount)

  ! pseudo-mixing of species ind_d with background concentration mix2_amount 
  ! within TSL timestep with dilF dilution factor [1/s]

    USE caaba_mem, ONLY: C

    IMPLICIT NONE
    
    REAL(dp), INTENT(IN) :: TSL, dilF
    INTEGER, INTENT(IN)  :: ind
    REAL(dp), INTENT(IN) :: mix2_amount

  ! pseudo-mixing of ind
    C(ind) = C(ind) + ( mix2_amount - C(ind) ) * TSL * dilF
    
  END SUBROUTINE pmix

! = nudging ====================================================================

  SUBROUTINE nudge_init(filedata_nc)  ! , clat, clon)
  
    IMPLICIT NONE

    CHARACTER(*), INTENT(IN)      :: filedata_nc
!    REAL(dp), INTENT(OUT)         :: clat, clon
    INTEGER                       :: varid

    ! open nc with eval19 data for reading 
    PRINT *, 'nudge_init: ouvre de fichier: '//filedata_nc//'.nc'
    CALL nf(nf90_open(filedata_nc//'.nc', nf90_nowrite, ncid_nudge))

    ! fixer les coordonnees geographiques,

    ! latitude
!    CALL nf(nf90_inq_varid(ncid_nudge, "CLAT", varid))
!    CALL nf(nf90_get_var(ncid_nudge, varid, clat, start = (/1/)))

    ! longitude
!    CALL nf(nf90_inq_varid(ncid_nudge, "CLON", varid))
!    CALL nf(nf90_get_var(ncid_nudge, varid, clon, start = (/1/)))

!    PRINT *, ' nouvelles cordonnees geographiques: ',clat,'N ',clon,' E'
    
  END SUBROUTINE nudge_init

  !-----------------------------------------------------------------------------

  SUBROUTINE nudge_close
  
    IMPLICIT NONE

    CALL nf(nf90_close(ncid_nudge))

  END SUBROUTINE nudge_close

  !-----------------------------------------------------------------------------

  SUBROUTINE nudge_spec(model_time, rel2nc_start, model2nc_scale, spec_list_ind, nudc_list)
  
    USE messy_mecca_kpp_monitor, ONLY: SPC_NAMES
    USE messy_main_constants_mem, ONLY: R_gas, N_A, STRLEN_SHORT, STRLEN_MEDIUM
    USE netcdf

    USE caaba_mem, ONLY: C, temp, cair, press

    IMPLICIT NONE

    INTEGER, INTENT(IN)           :: spec_list_ind(:)      ! liste des substances
    REAL(dp), INTENT(IN)          :: nudc_list(:)          ! avec les coefficients de nudge
    
    REAL(dp), INTENT(IN)          :: model_time, rel2nc_start, model2nc_scale
!                                                              ^ how many model steps fits into 1 nc step
!                                                                for monthly data should be = 365.24 / 12, z.B.
    REAL(dp)                      :: frac, dum
    INTEGER                       :: timestep_nc, s


  ! calculating corresponding time frame in nc and fraction for linear interpolation
    timestep_nc = 1 + INT( ( model_time - rel2nc_start ) / model2nc_scale )
    frac = model_time / model2nc_scale - INT( model_time / model2nc_scale )

!    PRINT *, 'nudge_spec (',model_time,',',rel2nc_start,' (',model2nc_scale,'): nc pas # ',timestep_nc,', frac = ',frac

    ! prendre la temperature 
    dum = get_val_i("TM1")
    if ( dum .LT. 400 ) temp = get_val_i("TM1")      ! sorting UNDEF through dum...
    cair = (N_A/1.E6) * press / (R_gas*temp) ! cair = c(air) in [mcl/cc]

#ifdef DEBUG
    PRINT *,'nudging the temperature: ',temp,' K'
#endif    

    ! et le reste des substances
    DO s = 1, UBOUND(spec_list_ind,1)
#ifdef DEBUG
      PRINT *,'nudging ',TRIM(SPC_NAMES(spec_list_ind(s))),' (',nudc_list(s),'): ', &
        C(spec_list_ind(s))/cair*1E9,' -> ', &
        get_val_i(SPC_NAMES(spec_list_ind(s)))*1E9,' => ',&
        ( 1.0_dp - nudc_list(s) ) * C(spec_list_ind(s))/cair*1e9 + &
          nudc_list(s) * ( get_val_i(SPC_NAMES(spec_list_ind(s)))*1e9 )
#endif    
      C(spec_list_ind(s)) = ( 1.0_dp - nudc_list(s) ) * C(spec_list_ind(s)) + &
                            nudc_list(s) * ( get_val_i(SPC_NAMES(spec_list_ind(s))) * cair )
    ENDDO                          
    
! spec_list_ind:
!
! (/ ind_CH4, ind_CO, ind_CO2, ind_HCHO, ind_OH, ind_HO2, &
!    ind_NO, ind_NO2, ind_NO3, ind_N2O, ind_N2O5, ind_O3, &
!    ind_CL2, ind_SO2, ind_CH3O2, ind_CH3CHO, ind_CH3OH, ind_CH3OOH, &
!    ind_C2H4, ind_C2H6, ind_C3H6, ind_C3H8, ind_NC4H10, &
!    ind_ISO2, ind_C5H8, ind_PA, ind_PAN, ind_MVKO2, ind_LMEKO2 /)

  CONTAINS
  
    REAL(dp) FUNCTION get_val_i(varname)
    
      IMPLICIT NONE
      
      CHARACTER(*), INTENT(IN)    :: varname
      INTEGER                     :: varid
      REAL(dp)                    :: val_ava, val_sui
  
      IF ( nf90_inq_varid(ncid_nudge, varname, varid) == nf90_noerr ) THEN
        CALL nf(nf90_get_var(ncid_nudge, varid, val_ava, start = (/timestep_nc/)))
        CALL nf(nf90_get_var(ncid_nudge, varid, val_sui, start = (/timestep_nc+1/)))
        ! l'iterpolation simple (linéaire)
        get_val_i = val_ava + frac * (val_sui - val_ava)
      ELSE  
        ! variable not found
!        PRINT *, "get_val_i(",varname,"): not found"
        get_val_i = 0.0_dp    ! just in case
      ENDIF
      
    END FUNCTION get_val_i

  END SUBROUTINE nudge_spec
  
! = offline emission from E5/M1 data ===========================================

  SUBROUTINE offlemb_init(filedata_nc)
  
    IMPLICIT NONE

    CHARACTER(*), INTENT(IN)      :: filedata_nc
    INTEGER                       :: varid

  ! open nc with eval19 offlem data for reading 
    PRINT *, 'offlemb_init: ouvre de fichier: '//filedata_nc//'.nc'
    CALL nf(nf90_open(filedata_nc//'.nc', nf90_nowrite, ncid_offlemb))

  END SUBROUTINE offlemb_init

  !-----------------------------------------------------------------------------

  SUBROUTINE offlemb_close
  
    IMPLICIT NONE

    CALL nf(nf90_close(ncid_offlemb))

  END SUBROUTINE offlemb_close

  !-----------------------------------------------------------------------------

! performs offline emission based on E5/M1 eval19 offlem setup 
! using data prepared by icogesb  scripts

! doubling configurations switches

  SUBROUTINE offlemb_perform(model_time, rel2nc_start, model2nc_scale, fct)
  
    USE messy_mecca_kpp_monitor, ONLY: SPC_NAMES
    USE caaba_mem,               ONLY: C

    USE netcdf

#ifdef dbl_IC
    USE messy_mecca_dbl_IC_box
#endif
#ifdef dbl_FCF
    USE messy_mecca_dbl_FCF_box
#endif
#ifdef dbl_FCB
    USE messy_mecca_dbl_FCB_box
#endif


#ifdef tag_IC
    USE messy_mecca_tag_IC_box
#endif
#ifdef dbl_IO
    USE messy_mecca_dbl_IO_box
#endif
#ifdef tag_IO
    USE messy_mecca_tag_IO_box
#endif
#ifdef dbl_O3F
    USE messy_mecca_dbl_O3F_box
#endif


    IMPLICIT NONE

    REAL(dp), INTENT(IN)   :: model_time, rel2nc_start, model2nc_scale, fct
!                                                             ^ how many model steps fits into 1 nc step
!                                                               for monthly data should be = 365.24 / 12, z.B.

    INTEGER                :: varid, timestep_nc
    REAL(dp)               :: val_ava, val_sui, val_cur, val_tot, frac
    CHARACTER(LEN=12)      :: spcname, req

  ! exp "undefined" flag
    REAL(dp), PARAMETER    :: eUNDEF = -1E33_dp

  ! - data section -------------------------------------------------------------

  ! specs list   (to search for in emission file)
    INTEGER            :: iSIL
    INTEGER, PARAMETER :: nSIL = 17
    INTEGER            :: SIL(nSIL) = &
      (/ ind_CH4, &
         ind_CO, &
         ind_HCHO, &
         ind_CH3OH, &
         ind_HCOOH, &
         ind_C2H4, &
         ind_C2H6, &
         ind_C3H6, &
         ind_C3H8, &
         ind_NC4H10, &
         ind_CH3CHO, &
         ind_CH3COCH3, &
         ind_CH3CO2H, &
         ind_MEK, &
         ind_NO, &
         ind_SO2, &
         ind_C5H8 /)     

  ! Dummy is instead of CO

  ! emission classes
    INTEGER            :: iOEC
    INTEGER, PARAMETER :: nOEC = 7
    CHARACTER(LEN=10)  :: OEC(nOEC) = (/ "BB   ", &
                                         "BF   ", &
                                         "FF   ", &
                                         "L43  ", &
                                         "LAND ", &
                                         "OCE  ", &
                                         "SHIPS" /)

  ! warn: xBB turns BB emissions off

  !        BB          BF        FF         L43        LAND       OCE      SHIPS

  ! corresponding 13C signatures for classes emissions
  ! area: highNH
    REAL(dp), PARAMETER :: S13C(nOEC,nSIL) = RESHAPE( &
       (/ &
          eUNDEF, -35.00_dp,    eUNDEF, -53.00_dp,    eUNDEF,    eUNDEF, -27.50_dp,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp, -27.00_dp, -13.50_dp, -27.50_dp,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp, -27.00_dp,    eUNDEF,    eUNDEF,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp, -27.00_dp,    eUNDEF,    eUNDEF,  &
       -24.50_dp, -27.50_dp, -22.20_dp, -27.50_dp, -27.00_dp, -20.00_dp, -27.50_dp,  &
       -24.50_dp, -27.50_dp, -27.40_dp, -27.50_dp,    eUNDEF, -20.00_dp, -27.50_dp,  &
       -24.50_dp, -27.50_dp, -25.20_dp, -27.50_dp, -27.00_dp,    eUNDEF, -27.50_dp,  &
       -24.50_dp, -27.50_dp, -27.70_dp, -27.50_dp,    eUNDEF, -20.00_dp, -27.50_dp,  &
       -24.50_dp, -27.50_dp, -30.60_dp, -27.50_dp,    eUNDEF, -20.00_dp, -27.50_dp,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp, -27.00_dp,    eUNDEF,    eUNDEF,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp, -27.00_dp,    eUNDEF,    eUNDEF,  &
       -24.50_dp, -27.50_dp, -27.50_dp, -27.50_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &
          eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,  &
          eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,  &
          eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF, -27.50_dp,    eUNDEF,    eUNDEF   &
        /), (/ nOEC, nSIL /) )

  ! corresponding 18O signatures for classes emissions
  ! area: highNH
    REAL(dp), PARAMETER :: S18O(nOEC,nSIL) = RESHAPE( &
       (/ &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,  &         ! ind_CH4, &
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,  -8.00_dp,   0.00_dp,  23.50_dp,   &        ! ind_CO, &
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_HCHO, &
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,   &        ! ind_CH3OH, &
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,   &        ! ind_HCOOH, &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C2H4, &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C2H6, &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C3H6, &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C3H8, &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_NC4H10, &
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &         ! ind_CH3CHO, 
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,  &         ! ind_CH3COCH3
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,  &         ! ind_CH3CO2H,
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &         ! ind_MEK, &
       17.15_dp,  17.20_dp,  23.50_dp,  17.20_dp,   0.00_dp,    eUNDEF,  23.50_dp,  &         ! ind_NO, &
         eUNDEF,  17.20_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,  23.50_dp,  &         ! ind_SO2, &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF   &         ! ind_C5H8 /) 
        /), (/ nOEC, nSIL /) )

!#  ! corresponding 18O signatures for classes emissions
!#  ! area: highNH
!#    REAL(dp), PARAMETER :: S18O(nOEC,nSIL) = RESHAPE( &
!#       (/ &
!#         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,  &         ! ind_CH4, &
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,  -8.00_dp,   0.00_dp,  23.50_dp,   &        ! ind_CO, &
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_HCHO, &
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,   &        ! ind_CH3OH, &
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,   &        ! ind_HCOOH, &
!#         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C2H4, &
!#         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C2H6, &
!#         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C3H6, &
!#         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_C3H8, &
!#         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &        ! ind_NC4H10, &
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &         ! ind_CH3CHO, 
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,  &         ! ind_CH3COCH3
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,  -8.00_dp,    eUNDEF,    eUNDEF,  &         ! ind_CH3CO2H,
!#       17.15_dp,   0.00_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &         ! ind_MEK, &
!#       17.15_dp,  23.50_dp,  23.50_dp,  17.20_dp,   0.00_dp,    eUNDEF,  23.50_dp,  &         ! ind_NO, &
!#         eUNDEF,  23.50_dp,  23.50_dp,  17.20_dp,    eUNDEF,    eUNDEF,  23.50_dp,  &         ! ind_SO2, &
!#         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF   &         ! ind_C5H8 /) 
!#        /), (/ nOEC, nSIL /) )

    REAL(dp), PARAMETER :: S17Ocap(nOEC,nSIL) = RESHAPE( &
       (/ &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &
         0.0_dp,   0.00_dp,    0.0_dp,    0.00_dp,  0.00_dp,   0.00_dp,    0.0_dp,   &
         0.0_dp,   0.00_dp,    0.0_dp,    0.00_dp,   eUNDEF,    eUNDEF,    eUNDEF,   &
         0.0_dp,   0.00_dp,    0.0_dp,    0.00_dp,  0.00_dp,    eUNDEF,    eUNDEF,   &
         0.0_dp,   0.00_dp,    0.0_dp,    0.00_dp,  0.00_dp,    eUNDEF,    eUNDEF,   &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,   &
         0.0_dp,   0.00_dp,    0.0_dp,   0.00_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &
         0.0_dp,   0.00_dp,    0.0_dp,   0.00_dp,   0.00_dp,    eUNDEF,    eUNDEF,  &
         0.0_dp,   0.00_dp,    0.0_dp,   0.00_dp,   0.00_dp,    eUNDEF,    eUNDEF,  &
         0.0_dp,   0.00_dp,    0.0_dp,   0.00_dp,    eUNDEF,    eUNDEF,    eUNDEF,  &
         0.0_dp,   0.00_dp,    0.0_dp,    0.0_dp,   0.00_dp,    eUNDEF,    0.0_dp,  &
         eUNDEF,   0.00_dp,    0.0_dp,    0.0_dp,    eUNDEF,    eUNDEF,    0.0_dp,  &
         eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF,    eUNDEF   &
        /), (/ nOEC, nSIL /) )


  ! - code section -------------------------------------------------------------

  ! calculating corresponding time frame in nc and fraction for linear interpolation
    timestep_nc = 1 + INT( ( model_time - rel2nc_start ) / model2nc_scale )
    frac = model_time / model2nc_scale - INT( model_time / model2nc_scale )

#ifdef DEBUG
  ! PRINT *, 'offlemb (',model_time,',',rel2nc_start,' (',model2nc_scale,'): nc pas # ',timestep_nc,', frac = ',frac
#endif

  ! species cycle
    DO iSIL = 1, nSIL

      IF ( SIL(iSIL) .EQ. 0 ) CYCLE

      val_tot = 0.0_dp       ! total emission of the species (inter-class sum)

    ! classes cycle
      DO iOEC = 1, nOEC 

      ! acquiring variable with name SPEC_CLASS, z.B.  CH4_BB,
      ! if it exists in the datafile, processing
      
        spcname = TRIM(SPC_NAMES(SIL(iSIL)))

        IF (spcname == "NO") spcname = "NOX"        ! for NO we're searching for NOX emission record
      
        IF ( nf90_inq_varid(ncid_offlemb, TRIM(spcname)//"_"//OEC(iOEC), varid) == nf90_noerr ) THEN

          CALL nf(nf90_get_var(ncid_offlemb, varid, val_ava, start = (/ cyc(timestep_nc,   1, 12, 12) /)))
          CALL nf(nf90_get_var(ncid_offlemb, varid, val_sui, start = (/ cyc(timestep_nc+1, 1, 12, 12) /)))
          
        ! l'iterpolation simple (linéaire)
          val_cur = val_ava + frac * (val_sui - val_ava)
          
 !         IF (spcname == "C5H8") val_cur = val_cur * 20        ! isop emission scale
!          IF (spcname == "NOX") val_cur = val_cur * 0        ! isop emission scale
        ! val_cur = val_cur / 9.448_dp        ! scaling emissions to a factor of 9.448 (NH versus remNH in CO)
          val_cur = val_cur / 12_dp       ! scaling emissions for 70N
!          val_cur = val_cur / 7_dp        ! scaling emissions for 30N
          
          val_tot = val_tot + val_cur

#ifdef DEBUG
          PRINT *, TRIM(spcname)//"_"//OEC(iOEC),': (',varid,') ',val_sui,' <-> ',val_ava,' >> ',val_cur
#endif

        ! TODO: call here emission according to the class
      
#ifdef dbl_IC
        ! checking if the value for the class is assigned in input data
          IF (S13C(iOEC,iSIL) /=eUNDEF) THEN

          ! emitting 12C/13C mixture converted from m2 to cm2
            CALL dbl_IC_emis(SIL(iSIL), val_cur * fct / 1.0E4_dp, (/ S13C(iOEC,iSIL) /) )
!            print *,'#dbl_IC#: ',TRIM(SPC_NAMES(SIL(iSIL))), ' > ',TRIM(OEC(iOEC)), ':', S13C(iOEC,iSIL)
            
          ELSE
!            print *,'offlemb_perform: #dbl_IC# >eUNDEF signature for', & 
!                                                        ' spec: ',TRIM(SPC_NAMES(SIL(iSIL))), &
!                                                       ' class: ',TRIM(OEC(iOEC))
          ENDIF
#endif
#ifdef dbl_FCF
        ! emission class FF - n=3
          IF (iOEC == 3) THEN
          ! checking if the value for the class is assigned in input data
            IF (S13C(iOEC,iSIL) /= eUNDEF) THEN

            ! emitting FCF species converted from m2 to cm2
            CALL dbl_FCF_emis(SIL(iSIL), val_cur * fct / 1.0E4_dp, (/ 1.0_dp /) )
!              print *,'#dbl_FCF#: ',TRIM(SPC_NAMES(SIL(iSIL))), ' > ',TRIM(OEC(iOEC)), ':', val_cur * fct / 1.0E4_dp
            
            ELSE
!              print *,'offlemb_perform: #dbl_FCF# >eUNDEF signature for', & 
!                                                          ' spec: ',TRIM(SPC_NAMES(SIL(iSIL))), &
!                                                         ' class: ',TRIM(OEC(iOEC))
            ENDIF
          ENDIF  
#endif
#ifdef tag_IC
        ! checking if the value for the class is assigned in input data
          IF (S13C(iOEC,iSIL) /=eUNDEF) THEN

          ! emitting 12C/13C mixture converted from m2 to cm2
            CALL tag_IC_emis(SIL(iSIL), val_cur * fct / 1.0E4_dp, (/ S13C(iOEC,iSIL) /) )
!            print *,'#tag_IC#: ',TRIM(SPC_NAMES(SIL(iSIL))), ' > ',TRIM(OEC(iOEC)), ':', S13C(iOEC,iSIL)
            
          ELSE
!            print *,'offlemb_perform: #tag_IC# >eUNDEF signature for', & 
!                                                        ' spec: ',TRIM(SPC_NAMES(SIL(iSIL))), &
!                                                       ' class: ',TRIM(OEC(iOEC))
          ENDIF
#endif

#ifdef dbl_IO
        ! checking if the value for the class is assigned in input data
          IF (S18O(iOEC,iSIL) /=eUNDEF) THEN

          ! emitting 18O/17O/16O mixture converted from m2 to cm2
            CALL dbl_IO_emis(SIL(iSIL), val_cur * fct / 1.0E4_dp, &
                             (/ S17Ocap(iOEC,iSIL) + NMDF_O * S18O(iOEC,iSIL), S18O(iOEC,iSIL) /) )
!            print *,'#dbl_IO#: ',TRIM(SPC_NAMES(SIL(iSIL))), ' > ',TRIM(OEC(iOEC)), ':', S18O(iOEC,iSIL),' / ',S17Ocap(iOEC,iSIL)
            
          ELSE
!            print *,'offlemb_perform: #dbl_IO# >eUNDEF signature for', & 
!                                                        ' spec: ',TRIM(SPC_NAMES(SIL(iSIL))), &
!                                                       ' class: ',TRIM(OEC(iOEC))
          ENDIF
#endif
#ifdef tag_IO
        ! checking if the value for the class is assigned in input data
          IF (S18O(iOEC,iSIL) /=eUNDEF) THEN

          ! emitting 18O/17O/16O mixture converted from m2 to cm2
            CALL tag_IO_emis(SIL(iSIL), val_cur * fct / 1.0E4_dp, &
                             (/ S17Ocap(iOEC,iSIL) + NMDF_O * S18O(iOEC,iSIL), S18O(iOEC,iSIL) /) )
!            print *,'#tag_IO#: ',TRIM(SPC_NAMES(SIL(iSIL))), ' > ',TRIM(OEC(iOEC)), ':', S18O(iOEC,iSIL),' / ',S17Ocap(iOEC,iSIL)
            
          ELSE
!            print *,'offlemb_perform: #tag_IO# >eUNDEF signature for', & 
!                                                        ' spec: ',TRIM(SPC_NAMES(SIL(iSIL))), &
!                                                       ' class: ',TRIM(OEC(iOEC))
          ENDIF
#endif

        ENDIF
      
      ENDDO

  ! emission of the regular species
    IF (SIL(iSIL) /= 0) C(SIL(iSIL)) = C(SIL(iSIL)) + val_tot * fct / 1.0E4_dp   ! converting from m2 to cm2
 !   IF (SIL(iSIL) /= 0) PRINT *, spcname, ':', C(SIL(iSIL)), ' + ', val_tot   ! converting from m2 to cm2
      
    ENDDO

!    ! et le reste des substances
!    DO s = 1, UBOUND(spec_list_ind,1)
!      C(spec_list_ind(s)) = ( 1.0_dp - nudc ) * C(spec_list_ind(s)) + &
!                            nudc * ( get_val_i(SPC_NAMES(spec_list_ind(s))) * cair )
!    ENDDO                          
    
  CONTAINS

  ! cycling function between 1 and given parameter
    INTEGER FUNCTION cyc(cinp, cmin, cmax, cper)
    
       IMPLICIT NONE
       
       INTEGER, INTENT(IN) :: cinp, cmin, cmax, cper
       INTEGER             :: cn
       
       cn = cinp; DO WHILE (cn .LT. cmin); cn = cn + cper; ENDDO
       DO WHILE (cn .GT. cmax); cn = cn - cper; ENDDO; cyc = cn;
       
     END FUNCTION cyc

!  
!    REAL(dp) FUNCTION get_val_i(varname)
!    
!      IMPLICIT NONE
!      
!      CHARACTER(*), INTENT(IN)    :: varname
!      INTEGER                     :: varid
!      REAL(dp)                    :: val_ava, val_sui
!  
!      CALL nf(nf90_inq_varid(ncid_offlemb, varname, varid))
!      CALL nf(nf90_get_var(ncid_offlemb, varid, val_ava, start = (/timestep_nc/)))
!      CALL nf(nf90_get_var(ncid_offlemb, varid, val_sui, start = (/timestep_nc+1/)))
!      
!      
!    END FUNCTION get_val_i
!    
  
  END SUBROUTINE offlemb_perform

  !-----------------------------------------------------------------------------

END MODULE messy_mecca_exp




! speclist from icogesb:
!
! C2H4    
! C2H6    
! C3H6    
! C3H8    
! NC4H10   
! CH3CHO  
! CH3COCH3
! CH3CO2H 
! CH3OH   
! CH4     
! CO      
! HCHO    
! HCOOH   
! MEK     
! NOX     
! SO2     
! NMV - ?

!      (/ ind_CH4, &
!         ind_CO, &
!         ind_HCHO, &
!         ind_CH3OH, &
!         ind_HCOOH, &
!         ind_C2H4, &
!         ind_C2H6, &
!         ind_C3H6, &
!         ind_C3H8, &
!         ind_NC4H10, &
!         ind_CH3CHO, &
!         ind_CH3COCH3, &
!         ind_CH3CO2H, &
!         ind_MEK, &
!         ind_NO, &
!         ind_SO2 /)     
