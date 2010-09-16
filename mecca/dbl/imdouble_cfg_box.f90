! ==============================================================================
! {%DBL}_box
! generated: {%TIMEDATE}
!
! this module is automaticly generated by imdouble utility
! contains: some maintenance routines for budgeting configurations (isotopes)
! level: smil boxmodel
!
! {$DBL_INFO} ! this is a template file for imdouble utility
!
! [Gromov, MPIC, 2007-2008]
! ==============================================================================

! - general doubling parameters (as conditional defines) -----------------------

#include "{%CMODEL}_dbl_parameters.inc"

! ------------------------------------------------------------------------------

! {$CONF_PARAM}

MODULE {%CMODEL}_{%DBL}_box

  USE messy_mecca_kpp     ! dp, ... nreact, nspec, ind_*, SPC_NAMES, EQN_TAGS
  USE caaba_io,           ONLY: open_output_file, write_output_file, close_file
  USE caaba_mem,          ONLY: C, cair, press

  USE {%CMODEL}_dbl_common ! common routines
  USE {%CMODEL}_{%DBL}     ! SMCL routines

  IMPLICIT NONE

! netcdf handle for deltas, conc., etc. output
  INTEGER :: ncid_{%DBL}

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! reference standart ratio for 13C, V-PDB
  REAL(dp), PARAMETER :: VPDB_13C     = 1123.72E-05_dp
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! reference standart ratio for 17O, 18O, V-SMOW
  REAL(dp), PARAMETER :: VSMOW_17O    =  386.72E-06_dp  ! Assonov, 2003b, pc
  REAL(dp), PARAMETER :: VSMOW_18O    = 2005.20E-06_dp
! oxygen mass-independent fractionation factor
  REAL(dp), PARAMETER :: NMDF_O        = 0.5281_dp      ! beta: meteoric waters
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! output array: doubled carbons: 2 species and D13C (+TC)     = (NSPEC+1)*3
!                                TC(regular),                 +1
!                                d0TC(reg), d0TC(iso), d0D13CTC, +3
!                                NREJCT                       +1
  REAL(dp)            :: D{%ATOM}OUT(({%NSPEC}+1)*(2+1)+1+3+1)
  REAL(dp)            :: D13C({%NSPEC}), D13CTC
  
! total budget verification
  REAL(dp)            :: D13CTC0
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! output array: doubled oxygen: 3 species, D17O,D18O,DC17O (+TO) = (NSPEC+1)*6
!                               TO(regular),
!                               d0TO(reg),d0TO(iso),d0D17TO,d0D18TO,d0DC17OTO,
!                               NREJCT
  REAL(dp)            :: D{%ATOM}OUT(({%NSPEC}+1)*(3+3)+1+5+1)
  REAL(dp)            :: D17O({%NSPEC}), D18O({%NSPEC}), DC17O({%NSPEC}), &
                         D17OTO, D18OTO, DC17OTO

! total budget verification
  REAL(dp)            :: D17OTO0, D18OTO0, DC17OTO0
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:I*}

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:F*}
! totals
  REAL(dp)            :: D{%A}OUT(({%NSPEC})*{%NCLASS}+5)   ! +1
! class fractions
  REAL(dp)            :: F({%NSPEC},{%NCLASS})
  REAL(dp)            :: FT({%NCLASS})
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:F*}

! total budget verification
  REAL(dp)            :: T{%A}0

! -----------------------------------------------------------------------------

  PUBLIC {%DBL}_x0
  PUBLIC {%DBL}_emis
  PUBLIC {%DBL}_depos
  PUBLIC {%DBL}_pmix
  PUBLIC {%DBL}_postprocess
  PUBLIC {%DBL}_calcdeltas
  PUBLIC {%DBL}_init
  PUBLIC {%DBL}_result
  PUBLIC {%DBL}_finish

! ==============================================================================

CONTAINS

! ==============================================================================

  SUBROUTINE {%DBL}_x0
  
    IMPLICIT NONE

  ! tracers mixing ratios initialization (x0)

    INTEGER :: i

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CASE:REM}
! {$x0} [%n%]  (%    D13C({%TAG}_@) = $%)
! n - # of the class, i.e. 2 for d13 / 2 for d17, 3 for d18
! @ - species name; $ - init. value; # - class no
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CASE:REM}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! {$x0} [%2%]  (%    D13C({%TAG}_@) = $%)

#ifdef ZERO_TEST
    D13C(:) = 0.0_dp
#endif

#ifdef INIUNIT_DELTAPM
  ! 12C, 13C through delta and regular species:

    D13C(:) = D13C(:) / 1000.0_dp         ! de-permilizing

    C({%RSIND}(:,1)) = C({%RSIND}(:,0)) * &
      isofrac2r(D13C(:), VPDB_13C, {%NQATOM}(:))
    C({%RSIND}(:,2)) = C({%RSIND}(:,0)) * &
      isofrac2f(D13C(:), VPDB_13C, {%NQATOM}(:))
#endif
#ifdef INIUNIT_FRACMIN
  ! 12C, 13C through minor fraction and regular species:

     C({%RSIND}(:,1)) = C({%RSIND}(:,0)) * (1.0_dp - D13C(:))
     C({%RSIND}(:,2)) = C({%RSIND}(:,0)) * D13C(:)
#endif
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! {$x0} [%2%]  (%    D17O({%TAG}_@) = $%)

! {$x0} [%3%]  (%    D18O({%TAG}_@) = $%)

#ifdef ZERO_TEST
    D17O(:) = 0.0_dp
    D18O(:) = 0.0_dp
#endif

#ifdef INIUNIT_DELTAPM
  ! 16O, 17O, 18O through delta and regular species:

    D17O(:) = D17O(:) / 1000.0_dp         ! de-permilizing
    D18O(:) = D18O(:) / 1000.0_dp

    C({%RSIND}(:,1)) = C({%RSIND}(:,0)) * &
      isofrac3r(D17O(:), VSMOW_17O, D18O(:), VSMOW_18O, {%NQATOM}(:))
    C({%RSIND}(:,2)) = C({%RSIND}(:,0)) * &
      isofrac3f(D17O(:), VSMOW_17O, D18O(:), VSMOW_18O, {%NQATOM}(:))
    C({%RSIND}(:,3)) = C({%RSIND}(:,0)) * &
      isofrac3f(D18O(:), VSMOW_18O, D17O(:), VSMOW_17O, {%NQATOM}(:))
#endif
#ifdef INIUNIT_FRACMIN
  ! 16O, 17O, 18O through minor fractions and regular species:

    C({%RSIND}(:,1)) = C({%RSIND}(:,0)) * (1.0_dp - (D17O(:) + D18O(:)))
    C({%RSIND}(:,2)) = C({%RSIND}(:,0)) * D17O(:)
    C({%RSIND}(:,3)) = C({%RSIND}(:,0)) * D18O(:)
#endif
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:F*}
  ! initialising using fractions given in cfg

! {$x0} [%#%]  (%    C({%RSIND}({%TAG}_@,#)) = C({%RSIND}({%TAG}_@,0)) * $%)

-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:F*}

#ifdef NULL_TEST
  ! minors are initialized emptied

#ifndef CLASSES_1
    C({%RSIND}(:,1)) = C({%RSIND}(:,0))
    C({%RSIND}(:,2:{%NISO})) = 0.0_dp
#else
    C({%RSIND}(:,1)) = 0.0_dp
#endif

#endif

  ! updating total {%ATOM} in the system
    CALL {%DBL}_calctotals(C)
    CALL {%DBL}_calcdeltas
    
    T{%ATOM}0 = C(ind_{%CONF}T{%ATOM})

->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
    D13CTC0 = D13CTC
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
    D17OTO0 = D17OTO
    D18OTO0 = D18OTO
    DC17OTO0 = DC17OTO
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:I*}

  END SUBROUTINE {%DBL}_x0



! -----------------------------------------------------------------------------

  SUBROUTINE {%DBL}_emis(ind_r, amount, deltas)
 
    IMPLICIT NONE
    
    INTEGER,  INTENT(IN)    :: ind_r
    REAL(dp), INTENT(IN)    :: amount
    REAL(dp), INTENT(IN)    :: deltas(:)
    INTEGER                 :: ind_d
    
  ! getting doubling index
    CALL {%DBL}_ind_d(ind_r, ind_d)
    IF (ind_d .LT. 1) RETURN

! uncomment to manage emission only through {%DBL}
!    C({%RSIND}(ind_d,0)) = C({%RSIND(ind_d,0)) + amount 

->>- + isotopic part ++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
    ! 12C
    C({%RSIND}(ind_d,1)) = C({%RSIND}(ind_d,1)) + amount * &
      isofrac2r(deltas(1)/1000.0_dp, VPDB_13C, {%NQATOM}(ind_d))
    ! 13C
    C({%RSIND}(ind_d,2)) = C({%RSIND}(ind_d,2)) + amount * &
      isofrac2f(deltas(1)/1000.0_dp, VPDB_13C, {%NQATOM}(ind_d))
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
    C({%RSIND}(ind_d,1)) = C({%RSIND}(ind_d,1)) + amount *  &
      isofrac3r(deltas(1)/1000.0_dp, VSMOW_17O, &
                deltas(2)/1000.0_dp, VSMOW_18O, {%NQATOM}(ind_d))
    C({%RSIND}(ind_d,2)) = C({%RSIND}(ind_d,2)) + amount *  &
      isofrac3f(deltas(1)/1000.0_dp, VSMOW_17O, &
                deltas(2)/1000.0_dp, VSMOW_18O, {%NQATOM}(ind_d))
    C({%RSIND}(ind_d,3)) = C({%RSIND}(ind_d,3)) + amount *  &
      isofrac3f(deltas(2)/1000.0_dp, VSMOW_18O, &
                deltas(1)/1000.0_dp, VSMOW_17O, {%NQATOM}(ind_d))
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:I*}

->>- + fractional part ++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:F*}
  ! deltas represent fractions here
    C({%RSIND}(ind_d,:)) = C({%RSIND}(ind_d,:)) + amount * deltas(:)
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:F*}

  END SUBROUTINE {%DBL}_emis



! -----------------------------------------------------------------------------

  SUBROUTINE {%DBL}_depos(ind_r, factor)

    IMPLICIT NONE

    INTEGER,  INTENT(IN)    :: ind_r
    REAL(dp), INTENT(IN)    :: factor
    INTEGER                 :: ind_d

  ! getting doubling index
    CALL {%DBL}_ind_d(ind_r, ind_d)
    IF (ind_d .LT. 1) RETURN

  ! simple deposition routine, introduces no KIE during the deposition
    C({%RSIND}(ind_d,1:{%NISO})) = C({%RSIND}(ind_d,1:{%NISO})) * factor
    
  END SUBROUTINE {%DBL}_depos



! -----------------------------------------------------------------------------

  SUBROUTINE {%DBL}_pmix(TSL, dilF, ind_r, mix_amount, mix_deltas)
 
    IMPLICIT NONE

  ! pseudo-mixing of species ind_d with background concentration mix_amount of
  ! mix_deltas composition within TSL timestep with dilF dilution factor [1/s]
    
    REAL(dp), INTENT(IN)    :: TSL, dilF      ! timestep length, dilution factor 
    INTEGER,  INTENT(IN)    :: ind_r          ! reg. spec. index
    REAL(dp), INTENT(IN)    :: mix_amount     ! backgr. concentration
    REAL(dp), INTENT(IN)    :: mix_deltas(:)  ! backgr. deltas
    REAL(dp)                :: corr, tot
    INTEGER                 :: ind_d

  ! getting doubling index
    CALL {%DBL}_ind_d(ind_r, ind_d)
    IF (ind_d .LT. 1) RETURN

  ! buget to correct to
    tot = SUM(C({%RSIND}(ind_d,1:{%NCLASS})))
    corr = tot + ( mix_amount - tot ) * MIN( TSL * dilF, 1.0_dp )

  ! emission of background iso-composition
    CALL {%DBL}_emis(ind_r, mix_amount * TSL * dilF, mix_deltas)
    tot = SUM(C({%RSIND}(ind_d,1:{%NCLASS})))

  ! removal preserving current composition
    IF (tot .GT. 0.0_dp) THEN
      CALL {%DBL}_depos(ind_r, corr / tot)
    ELSE
      C({%RSIND}(ind_d,1:{%NCLASS})) = 0.0_dp
      print *,'{%DBL}_pmix(',TSL,' ,',dilF,' ,',trim(SPC_NAMES(ind_r)),' ,', &
                 mix_amount,' ,',mix_deltas,'): mixing to nothing/negative'
    ENDIF

  END SUBROUTINE {%DBL}_pmix



! -----------------------------------------------------------------------------

  SUBROUTINE {%DBL}_postprocess

    IMPLICIT NONE

    INTEGER  :: i
    REAL(dp) :: chkamnt

  ! calculating the number of specs falling below THRES
    {%DBL}_NREJCT = 0
    DO i = 1, {%NSPEC}
      chkamnt = SUM( C({%RSIND}(i,1:{%NISO})) )

      IF (chkamnt .LT. THRES) THEN
        {%DBL}_NREJCT = {%DBL}_NREJCT + 1
!       C(RDCIND(i,0:{%NISO})) = 0.0_dp ! UNDEF
      ENDIF
    ENDDO           ! ndspec cycle

  ! every-step deltas/totals update
    CALL {%DBL}_calctotals(C)
    CALL {%DBL}_calcdeltas

  END SUBROUTINE {%DBL}_postprocess



! -----------------------------------------------------------------------------

  SUBROUTINE {%DBL}_calcdeltas
 
    IMPLICIT NONE

    INTEGER  :: i
    REAL(dp) :: tot
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
    REAL(dp) :: f12C
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
    REAL(dp) :: f16O
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}

->>- + isotopic part ++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
  ! calculating new delta-13C values
    DO i = 1, {%NSPEC}
      IF (C({%RSIND}(i,1)) .GT. 0.0_dp) THEN
!      IF ( ( C({%RSIND}(i,1))+C({%RSIND}(i,2)) ) .GT. THRES) THEN
        D13C(i) = delta2( C({%RSIND}(i,1)), C({%RSIND}(i,2)), & 
                          VPDB_13C, {%NQATOM}(i) )
      ELSE
        D13C(i) = UNDEF / 1000.0_dp  ! correct UNDEF value for output in permil
      ENDIF
    ENDDO        ! NISPEC-cycle

    ! total carbon   
    IF (C(ind_ICTI12) /= 0.0_dp) THEN
      D13CTC = delta2( C(ind_ICTI12),C(ind_ICTI13),VPDB_13C,1 )
    ELSE
      D13CTC = UNDEF / 1000.0_dp
    ENDIF
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
  ! calculating new delta-17O, delta-18O, cap.delta-17O values
    DO i = 1, {%NSPEC}
      IF (C({%RSIND}(i,1)) .GT. 0.0_dp) THEN
!      IF ( ( C({%RSIND}(i,1))+C({%RSIND}(i,2))+C({%RSIND}(i,3)) ) .GT. THRES) THEN
        D17O(i)  = delta3( C({%RSIND}(i,1)), C({%RSIND}(i,2)), C({%RSIND}(i,3)), &
                           VSMOW_17O, {%NQATOM}(i) )
        D18O(i)  = delta3( C({%RSIND}(i,1)), C({%RSIND}(i,3)), C({%RSIND}(i,2)), &
                           VSMOW_18O, {%NQATOM}(i) )
        DC17O(i) = D17O(i) - NMDF_O * D18O(i)
      ELSE
        D17O(i)  = UNDEF / 1000.0_dp  ! correct UNDEF value for output in permil
        D18O(i)  = UNDEF / 1000.0_dp
        DC17O(i) = UNDEF / 1000.0_dp
      ENDIF
    ENDDO        ! NISPEC-cycle

    ! total oxygen
    IF (C(ind_IOTI16) /= 0.0_dp) THEN
      D17OTO  = delta3( C(ind_IOTI16), C(ind_IOTI17), C(ind_IOTI18), VSMOW_17O, 1 )
      D18OTO  = delta3( C(ind_IOTI16), C(ind_IOTI18), C(ind_IOTI17), VSMOW_18O, 1 )
      DC17OTO = D17OTO - NMDF_O * D18OTO
    ELSE
      D17OTO  = UNDEF / 1000.0_dp
      D18OTO  = UNDEF / 1000.0_dp
      DC17OTO = UNDEF / 1000.0_dp
    ENDIF
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:I*}

->>- + fractional part ++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:F*}
  ! calculating fractions here (!) w.r.t. original mech
    F(:,:) = UNDEF
#ifndef OPT_FTOT_WRTDBL
  ! total: careful, accounts for atom number in molecule
    tot = SUM( C({%RSIND}(:,0)) * {%NQATOM}(:) )
#else
    tot = SUM( SUM(C({%RSIND}(:,1:{%NCLASS})),DIM=2) * {%NQATOM}(:) )
#endif
    DO i = 1, {%NCLASS}
      WHERE (C({%RSIND}(:,0)) .NE. 0.0_dp)
        F(:,i) = C({%RSIND}(:,i)) / C({%RSIND}(:,0))
      ENDWHERE
    ! fraction of total
      FT(i) = SUM( C({%RSIND}(:,i)) * {%NQATOM}(:) ) / tot
    ENDDO
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:F*}

  END SUBROUTINE {%DBL}_calcdeltas
  


! -----------------------------------------------------------------------------

! output file for doubled species info
  SUBROUTINE {%DBL}_init

    IMPLICIT NONE

! TODO: put additional tracers/variables+units after INIT_TRAC, INIT_UNIT

    CALL open_output_file(ncid_{%DBL}, 'caaba_mecca_{%DBL}', &
      (/   &
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
! {$TAG_SPECS} [%I12@%]
       , &
! {$TAG_SPECS} [%I13@%]
       , &
! {$TAG_SPECS} [%d13@%]
       , &
{$ELSA}       'TC_R', 'I12TC', 'I13TC', 'd13TC' &
       , &
{$ELSA}       'd0TC_R', 'd0TC', 'd0D13TC' &
       , &
{$ELSA}       'NREJCT' &
       /), (/   &
! {$TAG_SPECS} [%mol/mol%]
       , &
! {$TAG_SPECS} [%mol/mol%]
       , &
! {$TAG_SPECS} [%o/oo%]
       , &
{$ELSA}       'atoms', 'atoms', 'atoms', 'o/oo' &
       , &
{$ELSA}       'atoms', 'atoms', 'o/oo' &
       , &
{$ELSA}       'specs' &
       /), (/   &
! {$TAG_SPECS} [%\@SR^1^2@%]
       , &
! {$TAG_SPECS} [%\@SR^1^3@%]
       , &
! {$TAG_SPECS} [%\@SGd\@SR^1^3C(@)%]
       , &
{$ELSA}       '@SRTC_R (regular mech)', '@SRT^1^2C', '@SRT^1^3C', '@SGd@SR^1^3C(TC)' &
       , &
{$ELSA}       '@SGD@SR_t_0(TC_R)', '@SGD@SR_t_0(TC_D)', '@SGD@SR_t_0(@SGd@SR^1^3C(TC_D)) ' &
       , &
{$ELSA}       '@SRnumber of rejected species' &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
! {$TAG_SPECS} [%I16@%]
       , &
! {$TAG_SPECS} [%I17@%]
       , &
! {$TAG_SPECS} [%I18@%]
       , &
! {$TAG_SPECS} [%d18@%]
       , &
! {$TAG_SPECS} [%d17@%]
       , &
! {$TAG_SPECS} [%DC17@%]
       , &
{$ELSA}       'TO_R', 'I16TO', 'I17TO', 'I18TO', &
{$ELSA}       'd18TO', 'd17TO', 'DC17TO' &
       , &
{$ELSA}       'd0TO_R', 'd0TO', &
{$ELSA}       'd0D18TO', 'd0D17TO', 'd0DC17TO' &
       , &
{$ELSA}       'NREJCT' &
       /), (/   &
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
{$ELSA}       'atoms', 'atoms', 'atoms', 'atoms', &
{$ELSA}       'o/oo', 'o/oo', 'o/oo' &
       , &
{$ELSA}       'atoms', 'atoms', &
{$ELSA}       'o/oo', 'o/oo', 'o/oo' &
       , &
{$ELSA}       'specs'  &
       /), (/   &
! {$TAG_SPECS} [%\@SR^1^6@%]
       , &
! {$TAG_SPECS} [%\@SR^1^7@%]
       , &
! {$TAG_SPECS} [%\@SR^1^8@%]
       , &
! {$TAG_SPECS} [%\@SGd\@SR^1^8O(@)%]
       , &
! {$TAG_SPECS} [%\@SGd\@SR^1^7O(@)%]
       , &
! {$TAG_SPECS} [%\@SGD\@SR^1^7O(@)%]
       , &
{$ELSA}       '@SRTO_R', '@SRT^1^6O', '@SRT^1^7O', '@SRT^1^8O', &
{$ELSA}       '@SGd@SR^1^8O(TO)', '@SGd@SR^1^7O(TO)', '@SGD@SR^1^7O(TO)' &
       , &
{$ELSA}       '@SGD@SR_t_0(TO_R)', '@SGD@SR_t_0(TO_D)', &
{$ELSA}       '@SGD@SR_t_0(@SGd@SR^1^8O(TO_D))', '@SGD@SR_t_0(@SGd@SR^1^7O(TO_D))', '@SGD@SR_t_0(@SGD@SR^1^7O(TO_D))' &
       , &
{$ELSA}       '@SRnumber of rejected species'  &
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:F*}
{$ELSA}       'T_R', 'T_1' &
       , &
{$ELSA}       'd0T_R', 'd0T_1' &
       , &
{$ELSA}       'NREJCT' &
       , &
! {$TAG_SPECS} [%f$_@%]
       /), (/   &
{$ELSA}       'mol/mol', 'mol/mol' &
       , &
{$ELSA}       'mol/mol', 'mol/mol' &
       , &
{$ELSA}       'steps' &
       , &
! {$TAG_SPECS} [%$_frac%]
       /), (/   &
{$ELSA}       '@SRT_R (regular mech)', '@SRT_1' &
       , &
{$ELSA}       '@SGD@SR_t_0(T_R)', '@SGD@SR_t_0(T_1)' &
       , &
{$ELSA}       '@SRnumber of rejected species' &
       , &
! {$TAG_SPECS} [%\@SRf($,@)%]
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:F*}
       /) )

  END SUBROUTINE {%DBL}_init



! -----------------------------------------------------------------------------

  SUBROUTINE {%DBL}_result(model_time)
  
    IMPLICIT NONE
    
    REAL(dp), INTENT(IN) :: model_time
    INTEGER              :: i, j
    
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:C}
    DO i = 1, {%NSPEC}
      D{%A}OUT(i)           = C({%RSIND}(i,1))/cair
      D{%A}OUT({%NSPEC}+i)   = C({%RSIND}(i,2))/cair
      D{%A}OUT({%NSPEC}*2+i) = D13C(i) * 1000.0_dp
    ENDDO

  ! totals
    D{%A}OUT({%NSPEC}*3+1) = C(ind_ICTC)
    D{%A}OUT({%NSPEC}*3+2) = C(ind_ICTI12)
    D{%A}OUT({%NSPEC}*3+3) = C(ind_ICTI13)
    D{%A}OUT({%NSPEC}*3+4) = D13CTC * 1000.0_dp

  ! totals verification
    D{%A}OUT({%NSPEC}*3+5) = C(ind_ICTC)-TC0                  ! d0TC_R   = TC_R - TC_R(t=0)
    D{%A}OUT({%NSPEC}*3+6) = (C(ind_ICTI12)+C(ind_ICTI13))-TC0  ! d0TC     = (T12C+T13C) - TC_R(t=0)
    D{%A}OUT({%NSPEC}*3+7) = (D13CTC-D13CTC0) * 1000.0_dp     ! d0D13CTC = D13CTC - D13CTC(t=0)   
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:C}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>ATOM:O}
    DO i = 1, {%NSPEC}
      D{%A}OUT(i)           = C({%RSIND}(i,1))/cair
      D{%A}OUT({%NSPEC}+i)   = C({%RSIND}(i,2))/cair
      D{%A}OUT({%NSPEC}*2+i) = C({%RSIND}(i,3))/cair
      D{%A}OUT({%NSPEC}*3+i) = D18O(i) * 1000.0_dp
      D{%A}OUT({%NSPEC}*4+i) = D17O(i) * 1000.0_dp
      D{%A}OUT({%NSPEC}*5+i) = DC17O(i) * 1000.0_dp
    ENDDO

  ! totals
    D{%A}OUT({%NSPEC}*6+1) = C(ind_IOTO)
    D{%A}OUT({%NSPEC}*6+2) = C(ind_IOTI16)
    D{%A}OUT({%NSPEC}*6+3) = C(ind_IOTI17)
    D{%A}OUT({%NSPEC}*6+4) = C(ind_IOTI18)
    D{%A}OUT({%NSPEC}*6+5) = D18OTO * 1000.0_dp
    D{%A}OUT({%NSPEC}*6+6) = D17OTO * 1000.0_dp
    D{%A}OUT({%NSPEC}*6+7) = DC17OTO * 1000.0_dp

  ! totals verification
    D{%A}OUT({%NSPEC}*6+8) = C(ind_IOTO)-TO0                   ! d0TO_R    = TO_R - TO_R(t=0)
    D{%A}OUT({%NSPEC}*6+9) = C(ind_IOTI16)+C(ind_IOTI17)+C(ind_IOTI18)-TO0  ! d0TO = (T16O+T17O+T18O) - TO_R(t=0)
    D{%A}OUT({%NSPEC}*6+10) = (D18OTO-D18OTO0) * 1000.0_dp    ! d0D18OTO  = D18OTO - D18OTO(t=0)
    D{%A}OUT({%NSPEC}*6+11) = (D17OTO-D17OTO0) * 1000.0_dp    ! d0D18OTO  = D17OTO - D17OTO(t=0)
    D{%A}OUT({%NSPEC}*6+12) = (DC17OTO-DC17OTO0) * 1000.0_dp  ! d0DC17OTO = DC17OTO - DC18OTO(t=0)

  ! last value is NREJCT
    D{%A}OUT(UBOUND(D{%A}OUT)) = REAL({%DBL}_NREJCT)
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<ATOM:O}
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:I*}
->>- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {>CONF:F*}
  ! totals
    D{%A}OUT(1) = C({%TDIND}(0))
    D{%A}OUT(2) = C({%TDIND}(1))
  ! totals verification
    D{%A}OUT(3) = C({%TDIND}(0))-T{%A}0
    D{%A}OUT(4) = C({%TDIND}(1))-T{%A}0
  ! NREJCT
    D{%A}OUT(5) = REAL({%DBL}_NREJCT)
  ! fractions
    DO j = 1, {%NCLASS}
      DO i = 1, {%NSPEC}
        D{%A}OUT(5+{%NSPEC}*(j-1)+i) = F(i,j)
      ENDDO
    ENDDO
-<<- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ {<CONF:F*}

    CALL write_output_file(ncid_{%DBL}, model_time, D{%A}OUT)

  END SUBROUTINE {%DBL}_result



! -----------------------------------------------------------------------------

  SUBROUTINE {%DBL}_finish

    CALL close_file(ncid_{%DBL})

  END SUBROUTINE {%DBL}_finish



! - some cfg cheks ------------------------------------------------------------

#ifndef INIUNIT_DELTAPM
#ifndef INIUNIT_FRACMIN
#ifndef ZERO_TEST
#ifndef NULL_TEST
 FATAL: initialization is not defined, check the parameters
#endif
#endif
#endif
#endif

! -----------------------------------------------------------------------------
  
END MODULE messy_mecca_{%DBL}_box

! *****************************************************************************
