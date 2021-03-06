! ==============================================================================
! {%CMODEL}_{%TAG}_box
! generated: {%TIMEDATE}
!
! this module is generated by imtag utility
!
! {%ATOM} isotopes tagging configuration: interface routines
! level: boxmodel
!
! {$TAG_INFO} ! this is a template for isotopes configuration
!
! [Gromov, MPIC, 2007-2008]
! ==============================================================================

! - general tagging parameters (as conditional defines) ------------------------

#include "{%CMODEL}_tag_parameters.inc"

! - configuration parameters ---------------------------------------------------

! {$CONF_PARAM}

! -----------------------------------------------------------------------------


MODULE {%CMODEL}_{%TAG}_box

  USE messy_mecca_kpp ! dp, nreact, nspec, ind_*, SPC_NAMES, EQN_TAGS
  USE caaba_io,       ONLY: open_output_file, write_output_file, close_file
  USE caaba_mem,      ONLY: C, cair, press

  USE {%CMODEL}_{%TAG}           ! configuration kinetics

! utilities for the isotopes ratios, consts
  USE {%CMODEL}_tag_common
!  , ONLY: &
!    isoRa, isofrac2r, isofrac2f, isofrac3r, isofrac3f, kierate, UNDEF

! netcdf handle for deltas, conc., etc. output
  INTEGER :: ncid_{%TAG}

! ----------------------------------------------------------------------------

! output array: parameters: ISTEP, IREJCT, ISPAR, NDEREV, NREJCT, NCOR2R (+6)
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! tagged carbons: 2 isotopologues and 1 delta (+TC) = (NSPEC+1)*(2+1)
!                 TC(regular),                      +1
!                 d0TC(reg), d0TC(iso), d0D13CTC,   +3
  REAL(dp)            :: I{%ATOM}OUT(({%NTSPEC}+1)*(2+1)+1+3+6)
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! tagged oxygen: 3 isotopologues and 3 deltas (+TO) = (NSPEC+1)*(3+3)
!                TO(regular),                       +1
!                d0TO(reg), d0TO(iso),
!                d0D17TO, d0D18TO, d0DC17OTO        +5
  REAL(dp)            :: I{%ATOM}OUT(({%NTSPEC}+1)*(3+3)+1+5+6)
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:A}
  REAL(dp)            :: I{%ATOM}OUT(6)
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:A}

! -----------------------------------------------------------------------------

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! reference standart ratio for 13C, V-PDB
  REAL(dp), PARAMETER :: VPDB_13C     = 1123.72E-05_dp

! delta-13, plus for total
  REAL(dp)            :: D13C({%NTSPEC}), D13CTC

! total budget verification
  REAL(dp)            :: TC0, D13CTC0
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! reference standart ratio for 17O, 18O, V-SMOW
  REAL(dp), PARAMETER :: VSMOW_17O    =  386.72E-06_dp  ! Assonov, 2003b, pc
  REAL(dp), PARAMETER :: VSMOW_18O    = 2005.20E-06_dp
! oxygen NMD isotopic fractionation parameter
  REAL(dp), PARAMETER :: NMDF_O        = 0.5281_dp      ! ??

! deltas-17-18 capital-17, plus for total
  REAL(dp)            :: D17O({%NTSPEC}), D18O({%NTSPEC}), DC17O({%NTSPEC}), &
                         D17OTO, D18OTO, DC17OTO

! total budget verification
  REAL(dp)            :: TO0, D17OTO0, D18OTO0, DC17OTO0
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

! -----------------------------------------------------------------------------

  PUBLIC
  
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
  PRIVATE VPDB_13C
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
  PRIVATE VSMOW_17O, VSMOW_18O ! , NMDF_O
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

  PUBLIC {%TAG}_x0
  PUBLIC {%TAG}_emis
  PUBLIC {%TAG}_depos
  PUBLIC {%TAG}_pmix
  PUBLIC {%TAG}_calcdeltas
  PUBLIC {%TAG}_init
  PUBLIC {%TAG}_result
  PUBLIC {%TAG}_finish
#ifdef FUDGE
  PUBLIC {%TAG}_fudge
#endif
  PUBLIC {%TAG}_resetPTs

! =============================================================================

CONTAINS

! -----------------------------------------------------------------------------

  SUBROUTINE {%TAG}_x0

    IMPLICIT NONE

    INTEGER :: i

! initializing isotopologues concentration according to "regular"

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}

! {$x0} [%2%]  (%    D13C({%TAG}_#) = $%)

#ifdef ZERO_TEST
    D13C(:) = 0.0_dp
#endif

#ifdef INIUNIT_DELTAPM
  ! de-permilizing :]
    D13C(:) = D13C(:) / 1000.0_dp

  ! regular concentrations by 12C, 13C fractions from deltas:
    I12C(:) = C({%RTIND}(:,0)) * isofrac2r(D13C(:), VPDB_13C, QT{%A}ATOM(:))
    I13C(:) = C({%RTIND}(:,0)) * isofrac2f(D13C(:), VPDB_13C, QT{%A}ATOM(:))
#endif
#ifdef INIUNIT_FRACMIN
  ! 12C, 13C through fraction and regular species:
    I12C(:) = C({%RTIND}(:,0)) * (1.0_dp - D13C(:))
    I13C(:) = C({%RTIND}(:,0)) * D13C(:)
#endif
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}

! {$x0} [%2%]  (%    D17O({%TAG}_#) = $%)

! {$x0} [%3%]  (%    D18O({%TAG}_#) = $%)

#ifdef ZERO_TEST
    D17O(:) = 0.0_dp
    D18O(:) = 0.0_dp
#endif

#ifdef INIUNIT_DELTAPM
  ! de-permilizing :]
    D17O(:) = D17O(:) / 1000.0_dp
    D18O(:) = D18O(:) / 1000.0_dp

  ! regular concentrations by 16O, 17O, 18O fractions from deltas:
    I16O(:) = C({%RTIND}(:,0)) * &
      isofrac3r(D17O(:), VSMOW_17O, D18O(:), VSMOW_18O, QT{%ATOM}ATOM(:))
    I17O(:) = C({%RTIND}(:,0)) * &
      isofrac3f(D17O(:), VSMOW_17O, D18O(:), VSMOW_18O, QT{%ATOM}ATOM(:))
    I18O(:) = C({%RTIND}(:,0)) * &
      isofrac3f(D18O(:), VSMOW_18O, D17O(:), VSMOW_17O, QT{%ATOM}ATOM(:))
#endif
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
#ifdef INIUNIT_FRACMIN
  ! this is in the development phase, careful
  ! getting values from the configuration setup
! {$x0} [%1%]  (%    ISO{%A}({%TAG}_#,1) = $%)
! {$x0} [%2%]  (%    ISO{%A}({%TAG}_#,2) = $%)
! {$x0} [%3%]  (%    ISO{%A}({%TAG}_#,3) = $%)
! {$x0} [%4%]  (%    ISO{%A}({%TAG}_#,4) = $%)
! {$x0} [%5%]  (%    ISO{%A}({%TAG}_#,5) = $%)

  ! ISO{%A}(:,:) through fractions and regular species:
    DO i = 2, {%NISO} 
      ISO{%A}(:,i) = C({%RTIND}(:,0)) * ISO{%A}(:,i)
    ENDDO
    ISO{%A}(:,1) = C({%RTIND}(:,0)) * (1.0_dp - SUM(ISO{%A}(:,2:{%NISO})))
#endif

#ifdef NULL_TEST
    ISO{%A}(:,1) = C({%RTIND}(:,0))
    ISO{%A}(:,2:{%NISO}) = 0.0_dp
#endif

  ! updating total {%ATOM} & delta(s) in the system
    CALL {%TAG}_calctotals(C)
    CALL {%TAG}_calcdeltas

  ! totals at the initialization point
    T{%ATOM}0 = SUM(T{%ATOM}(:))
  ! check if initialization is correct for totals (tagging vs. regular)
    IF (ABS(T{%ATOM}0 - T{%ATOM}_R) .GT. 100) THEN
      print *,'{%TAG}_x0: totals 100-atoms check failed ( reg: ',&
              T{%ATOM}_R, '/ tag: ',T{%ATOM}0,' )'
    ENDIF
    
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
    D13CTC0 = D13CTC
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
    D17OTO0 = D17OTO
    D18OTO0 = D18OTO
    DC17OTO0 = DC17OTO
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

    CALL {%TAG}_initialize

! #ifdef INT_KPP
!     C(ind_AONE) = 1.0_dp
! #endif

#ifdef DEBUG
    print *,'{%TAG}_x0: passed'
#endif 

  END SUBROUTINE {%TAG}_x0



! -----------------------------------------------------------------------------

  SUBROUTINE {%TAG}_emis(ind_t, amount, deltas)
 
    IMPLICIT NONE
    
    INTEGER,  INTENT(IN)    :: ind_t
    REAL(dp), INTENT(IN)    :: amount
    REAL(dp), INTENT(IN)    :: deltas(:)

  ! filtering possible dummies
    IF ((ind_t .LT. 1) .OR. (ind_t .GT. {%NSPEC})) RETURN

  ! uncomment to manage emission only through {%TAG}
!    C(RI{%ATOM}IND(ind_t,0)) = C(RI{%ATOM}IND(ind_t,0)) + amount 

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
    I12C(ind_t) = I12C(ind_t) + amount * &
      isofrac2r(deltas(1) / 1000.0_dp, VPDB_13C, QT{%ATOM}ATOM(ind_t))
    I13C(ind_t) = I13C(ind_t) + amount * &
      isofrac2f(deltas(1) / 1000.0_dp, VPDB_13C, QT{%ATOM}ATOM(ind_t))
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
    I16O(ind_t) = I16O(ind_t) + amount *  &
      isofrac3r(deltas(1) / 1000.0_dp, VSMOW_17O, &
                deltas(2) / 1000.0_dp, VSMOW_18O, &
                QT{%ATOM}ATOM(ind_t))
    I17O(ind_t) = I17O(ind_t) + amount *  &
      isofrac3f(deltas(1) / 1000.0_dp, VSMOW_17O, &
                deltas(2) / 1000.0_dp, VSMOW_18O, &
                QT{%ATOM}ATOM(ind_t))
    I18O(ind_t) = I18O(ind_t) + amount *  &
      isofrac3f(deltas(2) / 1000.0_dp, VSMOW_18O, &
                deltas(1) / 1000.0_dp, VSMOW_17O, &
                QT{%ATOM}ATOM(ind_t))
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

  END SUBROUTINE {%TAG}_emis



! -----------------------------------------------------------------------------

  SUBROUTINE {%TAG}_depos(ind_t, factor)

    IMPLICIT NONE

    INTEGER,  INTENT(IN)    :: ind_t
    REAL(dp), INTENT(IN)    :: factor

  ! filtering possible dummies
    IF ((ind_t .LT. 1) .OR. (ind_t .GT. {%NSPEC})) RETURN

  ! simple deposition routine, introduces no KIE during the deposition
    ISO{%A}(ind_t,:) = ISO{%A}(ind_t,:) * factor
    
  END SUBROUTINE {%TAG}_depos
  
  
  
! -----------------------------------------------------------------------------

  SUBROUTINE {%TAG}_pmix(TSL, dilF, ind_t, mix_amount, mix_deltas)
 
    IMPLICIT NONE

  ! pseudo-mixing of species ind_t with background concentration mix_amount of
  ! mix_deltas composition within TSL timestep with dilF dilution factor [1/s]
    
    REAL(dp), INTENT(IN)    :: TSL, dilF      ! timestep length, dilution factor 
    INTEGER,  INTENT(IN)    :: ind_t          ! spec. index
    REAL(dp), INTENT(IN)    :: mix_amount     ! backgr. concentration
    REAL(dp), INTENT(IN)    :: mix_deltas(:)  ! backgr. composition
    REAL(dp)                :: corr, tot

  ! filtering possible dummies
    IF ((ind_t .LT. 1) .OR. (ind_t .GT. {%NSPEC})) RETURN

  ! buget to correct to
    tot = SUM(ISO{%A}(ind_t,1:{%NISO}))
    corr = tot + ( mix_amount - tot ) * TSL * dilF

  ! emission of background iso-composition
    CALL {%TAG}_emis(ind_t, mix_amount * TSL * dilF, mix_deltas)
    tot = SUM(ISO{%A}(ind_t,1:{%NISO}))

  ! removal of current composition
    ISO{%A}(ind_t,1:{%NISO}) = ISO{%A}(ind_t,1:{%NISO}) / tot * corr

  END SUBROUTINE {%TAG}_pmix



! -----------------------------------------------------------------------------

  SUBROUTINE {%TAG}_calcdeltas
 
    IMPLICIT NONE

    INTEGER      :: i

  ! deltas fot total {%ATOM} are included
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
  ! calculating new delta-13C values

    WHERE (I12C(:) .GT. 0.0_dp)
      D13C(:) = delta2( I12C(:), I13C(:), VPDB_13C, QT{%ATOM}ATOM(:) )
    ELSEWHERE
      D13C(:) = UNDEF
    ENDWHERE

    IF (T12C .GT. 0.0_dp) THEN
      D13CTC = delta2( T12C, T13C, VPDB_13C, 1 )
    ELSE
      D13CTC = UNDEF
    ENDIF
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
  ! calculating new delta-17O, delta-18O, cap.delta-17O values

    WHERE (I16O(:) .GT. 0.0_dp)
      D17O(:)  = delta3( I16O(:), I17O(:), I18O(:), VSMOW_17O, QT{%ATOM}ATOM(:) )
      D18O(:)  = delta3( I16O(:), I18O(:), I17O(:), VSMOW_18O, QT{%ATOM}ATOM(:) )
      DC17O(:) = D17O(:) - NMDF_O * D18O(:)
    ELSEWHERE
      D17O(:)  = UNDEF; D18O(:)  = UNDEF; DC17O(:) = UNDEF
    ENDWHERE

    IF (T16O .GT. 0.0_dp) THEN
      D17OTO  = delta3( T16O, T17O, T18O, VSMOW_17O, 1 )
      D18OTO  = delta3( T16O, T18O, T17O, VSMOW_18O, 1 )
      DC17OTO = D17OTO - NMDF_O * D18OTO
    ELSE
      D17OTO  = UNDEF; D18OTO  = UNDEF; DC17OTO = UNDEF
    ENDIF
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

  END SUBROUTINE {%TAG}_calcdeltas
  


! -----------------------------------------------------------------------------

#ifdef FUDGE
! "fudging" of total isotopologues budget to "regular" species budget  

  SUBROUTINE {%TAG}_fudge

    IMPLICIT NONE

  ! getting from doubled to tagged

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! {$FUDGE} [%  I12C({%TAG}_#) = C(ind_I12#)%]
! {$FUDGE} [%  I13C({%TAG}_#) = C(ind_I13#)%]
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! {$FUDGE} [%  I16O({%TAG}_#) = C(ind_I16#)%]
! {$FUDGE} [%  I17O({%TAG}_#) = C(ind_I17#)%]
! {$FUDGE} [%  I18O({%TAG}_#) = C(ind_I18#)%]
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

  CALL {%TAG}_calctotals(C)
  CALL {%TAG}_calcdeltas
  
! #ifdef DEBUG
    print *,'{%TAG}_fudge: working'
! #endif 

  END SUBROUTINE {%TAG}_fudge
#endif

! -----------------------------------------------------------------------------
  
! production tracers initialization (reset) routine

  SUBROUTINE {%TAG}_resetPTs

    IMPLICIT NONE
    
! {$RxESET_PTs} [%C(ind_#)%] (%ind_#%)      <-- boxmodel syntax
! - currently disabled with use of TRPT{%ATOM}IND()

    C(TRPT{%ATOM}IND(:)) = 0.0_dp       ! <-- boxmodel syntax

#ifdef DEBUG
    print *,'{%TAG}_resetPTs: passed'
#endif 

  END SUBROUTINE {%TAG}_resetPTs



! -----------------------------------------------------------------------------

! output file for tagged tracers
  SUBROUTINE {%TAG}_init

    IMPLICIT NONE

! TODO: put additional tracers/variables+units after INIT_TRAC, INIT_UNIT

    CALL open_output_file(ncid_{%TAG}, 'caaba_mecca_{%TAG}', &
      (/   &                            ! names of the tracers to be saved
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! {$TAG_SPECS} [%I12#%]
       , &
! {$TAG_SPECS} [%I13#%]
       , &
! {$TAG_SPECS} [%d13#%]
       , &
{$ELSA}       'TC_R', 'I12TC', 'I13TC', 'd13TC' &
       , &
{$ELSA}       'd0TC_R', 'd0TC', 'd0D13TC' &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! {$TAG_SPECS} [%I16#%]
       , &
! {$TAG_SPECS} [%I17#%]
       , &
! {$TAG_SPECS} [%I18#%]
       , &
! {$TAG_SPECS} [%d18#%]
       , &
! {$TAG_SPECS} [%d17#%]
       , &
! {$TAG_SPECS} [%DC17#%]
       , &
{$ELSA}       'TO_R', 'I16TO', 'I17TO', 'I18TO', 'd18TO', 'd17TO', 'DC17TO' &
       , &
{$ELSA}       'd0TO_R', 'd0TO', 'd0d18TO', 'd0d17TO', 'd0DC17TO' &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
       , &                              ! - common part
{$ELSA}       'ISTEP', 'IREJCT', 'ISPAR', 'NDEREV', 'NREJCT', 'NCOR2R' &
         &
       /), (/   &                       ! units
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! {$TAG_SPECS} [%mol/mol%]
       , &
! {$TAG_SPECS} [%mol/mol%]
       , &
! {$TAG_SPECS} [%o/oo%]
       , &
{$ELSA}       'atoms', 'atoms', 'atoms', 'o/oo' &
       , &
{$ELSA}       'atoms', 'atoms', 'o/oo' &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! {$TAG_SPECS} [%mol/mol%]
       , &
! {$TAG_SPECS} [%mol/mol%]
       , &
! {$TAG_SPECS} [%mol/mol%]
       , &
! {$TAG_SPECS} [%o/oo%]
       , &
! {$TAG_SPECS} [%o/oo%]
       , &
! {$TAG_SPECS} [%o/oo%]
       , &
{$ELSA}       'atoms', 'atoms', 'atoms', 'atoms', 'o/oo', 'o/oo', 'o/oo' &
       , &
{$ELSA}       'atoms', 'atoms', 'o/oo', 'o/oo', 'o/oo' &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
       , &                              ! - common part
         &
{$ELSA}       'steps', 'steps', '?', 'calls', 'specs', 'frac' &
       /), (/   &                       ! captions
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! {$TAG_SPECS} [%@SR^1^2#%]
       , &
! {$TAG_SPECS} [%@SR^1^3#%]
       , &
! {$TAG_SPECS} [%@SGd@SR^1^3C(#)%]
       , &
{$ELSA}       '@SRTC_R (regular mech)', '@SRT^1^2C', '@SRT^1^3C', '@SGd@SR^1^3C(TC)' &
       , &
{$ELSA}       '@SGD@SR_t_0(TC_R)', '@SGD@SR_t_0(TC_T)', '@SGD@SR_t_0(@SGd@SR^1^3C(TC_T)) ' &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! {$TAG_SPECS} [%@SR^1^6#%]
       , &
! {$TAG_SPECS} [%@SR^1^7#%]
       , &
! {$TAG_SPECS} [%@SR^1^8#%]
       , &
! {$TAG_SPECS} [%@SGd@SR^1^8O(#)%]
       , &
! {$TAG_SPECS} [%@SGd@SR^1^7O(#)%]
       , &
! {$TAG_SPECS} [%@SGD@SR^1^7O(#)%]
       , &
{$ELSA}       '@SRTO_R (regular mech)', '@SRT^1^6O', '@SRT^1^7O', '@SRT^1^8O', &
{$ELSA}       '@SGd@SR^1^8O(TO)', '@SGd@SR^1^7O(TO)', '@SGD@SR^1^7O(TO)' &
       , &
{$ELSA}       '@SGD@SR_t_0(TO_R)', '@SGD@SR_t_0(TO_T)', &
{$ELSA}       '@SGD@SR_t_0(@SGd@SR^1^8O(TO_T))', '@SGD@SR_t_0(@SGd@SR^1^7O(TO_T))', '@SGD@SR_t_0(@SGD@SR^1^7O(TO_T))' &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
       , &                              ! - common part
{$ELSA}       '@ASI: # of iterations', &
{$ELSA}       '@ASI: # of rejected iter.', &
{$ELSA}       '@ASI: special param. (ISPAR)', &
{$ELSA}       '@ASN: # of derivative evals.', &
{$ELSA}       '@ASN: # of filtered', &
{$ELSA}       '@AScorrected to regular ' &
       /) )
  
#ifdef DEBUG
    print *,'{%TAG}_init: passed'
#endif 

  END SUBROUTINE {%TAG}_init



! -----------------------------------------------------------------------------

  SUBROUTINE {%TAG}_result(model_time)
  
    IMPLICIT NONE
    
    REAL(dp), INTENT(IN) :: model_time
    INTEGER              :: i
    
    I{%A}OUT(:) = UNDEF
    
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
    I{%A}OUT(1+{%NTSPEC}*0:{%NTSPEC}*1) = I12C(:) / cair
    I{%A}OUT(1+{%NTSPEC}*1:{%NTSPEC}*2) = I13C(:) / cair
    WHERE ( D13C(:) /= UNDEF )
      I{%A}OUT(1+{%NTSPEC}*2:{%NTSPEC}*3) = D13C(:) * 1000.0_dp
    ENDWHERE

  ! totals
    I{%A}OUT({%NTSPEC}*3+1) = TC_R
    I{%A}OUT({%NTSPEC}*3+2) = T12C
    I{%A}OUT({%NTSPEC}*3+3) = T13C
    I{%A}OUT({%NTSPEC}*3+4) = D13CTC * 1000.0_dp
    
  ! totals verification
    I{%A}OUT({%NTSPEC}*3+5) = TC_R-TC0                     ! d0TC_R   = TC_R - TC_R(t=0)
    I{%A}OUT({%NTSPEC}*3+6) = SUM(TC(:))-TC0               ! d0TC     = (T12C+T13C) - TC_R(t=0)
    I{%A}OUT({%NTSPEC}*3+7) = (D13CTC-D13CTC0) * 1000.0_dp ! d0D13CTC = D13CTC - D13CTC(t=0)   
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
    I{%A}OUT(1+{%NTSPEC}*0:{%NTSPEC}*1) = I16O(:) / cair
    I{%A}OUT(1+{%NTSPEC}*1:{%NTSPEC}*2) = I17O(:) / cair
    I{%A}OUT(1+{%NTSPEC}*2:{%NTSPEC}*3) = I18O(:) / cair
    WHERE ( D18O(:) /= UNDEF )
      I{%A}OUT(1+{%NTSPEC}*3:{%NTSPEC}*4) = D18O(:) * 1000.0_dp
    ENDWHERE      
    WHERE ( D17O(:) /= UNDEF )
      I{%A}OUT(1+{%NTSPEC}*4:{%NTSPEC}*5) = D17O(:) * 1000.0_dp
    ENDWHERE
    WHERE ( DC17O(:) /= UNDEF )
      I{%A}OUT(1+{%NTSPEC}*5:{%NTSPEC}*6) = DC17O(:) * 1000.0_dp
    ENDWHERE
    
  ! totals
    I{%A}OUT({%NTSPEC}*6+1) = TO_R
    I{%A}OUT({%NTSPEC}*6+2) = T16O
    I{%A}OUT({%NTSPEC}*6+3) = T17O
    I{%A}OUT({%NTSPEC}*6+4) = T18O
    I{%A}OUT({%NTSPEC}*6+5) = D18OTO * 1000.0_dp
    I{%A}OUT({%NTSPEC}*6+6) = D17OTO * 1000.0_dp
    I{%A}OUT({%NTSPEC}*6+7) = DC17OTO * 1000.0_dp

  ! totals verification
    I{%A}OUT({%NTSPEC}*6+8)  = TO_R-TO0                       ! d0TO_R    = TO_R - TO_R(t=0)
    I{%A}OUT({%NTSPEC}*6+9)  = SUM(TO(:))-TO0                 ! d0TO      = (T16O+T17O+T18O) - TO_R(t=0)
    I{%A}OUT({%NTSPEC}*6+10) = (D18OTO-D18OTO0) * 1000.0_dp   ! d0D18OTO  = D18OTO - D18OTO(t=0)
    I{%A}OUT({%NTSPEC}*6+11) = (D17OTO-D17OTO0) * 1000.0_dp   ! d0D18OTO  = D17OTO - D17OTO(t=0)
    I{%A}OUT({%NTSPEC}*6+12) = (DC17OTO-DC17OTO0) * 1000.0_dp ! d0DC17OTO = DC17OTO - DC18OTO(t=0)
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

    ! last 6 values are common parameters
    I{%A}OUT(UBOUND(I{%A}OUT)-5) = REAL({%TAG}_ISTEP)
    I{%A}OUT(UBOUND(I{%A}OUT)-4) = REAL({%TAG}_IREJCT)    
    I{%A}OUT(UBOUND(I{%A}OUT)-3) = {%TAG}_ISPAR
    I{%A}OUT(UBOUND(I{%A}OUT)-2) = REAL({%TAG}_NDEREV)
    I{%A}OUT(UBOUND(I{%A}OUT)-1) = REAL({%TAG}_NREJCT)
    
#ifdef C2R_FILTER
    I{%A}OUT(UBOUND(I{%A}OUT)-0) = REAL({%TAG}_NCOR2R)    
#else
    I{%A}OUT(UBOUND(I{%A}OUT)-0) = UNDEF
#endif

    CALL write_output_file(ncid_{%TAG}, model_time, I{%A}OUT)

#ifdef DEBUG
!    print *,'{%TAG}_result: working'
#endif 

  END SUBROUTINE {%TAG}_result



! ---------------------------------------------------------------------------

  SUBROUTINE {%TAG}_finish

    CALL close_file(ncid_{%TAG})

  END SUBROUTINE {%TAG}_finish



! - some cfg checks ---------------------------------------------------------

#ifndef INIUNIT_DELTAPM
#ifndef INIUNIT_FRACMIN
#ifndef ZERO_TEST
#ifndef NULL_TEST
 FATAL: initialization is not defined, check the parameters
#endif
#endif
#endif
#endif

! ---------------------------------------------------------------------------

END MODULE {%CMODEL}_{%TAG}_box

! ***************************************************************************

