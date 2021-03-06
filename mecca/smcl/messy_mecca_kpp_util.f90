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
! Time                 : Thu Aug 26 14:15:50 2010
! Working directory    : /home/caaba/caaba_2.7b/mecca
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

  V(322) = V_USER(1)
  V(5) = V_USER(2)
  V(323) = V_USER(3)
  V(438) = V_USER(4)
  V(434) = V_USER(5)
  V(395) = V_USER(6)
  V(325) = V_USER(8)
  V(1) = V_USER(9)
  V(437) = V_USER(10)
  V(436) = V_USER(11)
  V(328) = V_USER(12)
  V(343) = V_USER(13)
  V(424) = V_USER(14)
  V(360) = V_USER(15)
  V(362) = V_USER(16)
  V(363) = V_USER(17)
  V(361) = V_USER(18)
  V(333) = V_USER(19)
  V(344) = V_USER(20)
  V(415) = V_USER(21)
  V(334) = V_USER(22)
  V(433) = V_USER(23)
  V(381) = V_USER(24)
  V(2) = V_USER(25)
  V(329) = V_USER(26)
  V(330) = V_USER(27)
  V(404) = V_USER(28)
  V(364) = V_USER(29)
  V(326) = V_USER(30)
  V(414) = V_USER(31)
  V(348) = V_USER(32)
  V(435) = V_USER(33)
  V(349) = V_USER(34)
  V(338) = V_USER(35)
  V(350) = V_USER(36)
  V(419) = V_USER(37)
  V(366) = V_USER(38)
  V(396) = V_USER(39)
  V(374) = V_USER(40)
  V(336) = V_USER(41)
  V(403) = V_USER(42)
  V(430) = V_USER(43)
  V(375) = V_USER(44)
  V(423) = V_USER(45)
  V(367) = V_USER(46)
  V(377) = V_USER(47)
  V(368) = V_USER(48)
  V(369) = V_USER(49)
  V(432) = V_USER(50)
  V(417) = V_USER(51)
  V(331) = V_USER(52)
  V(405) = V_USER(53)
  V(370) = V_USER(54)
  V(407) = V_USER(55)
  V(337) = V_USER(56)
  V(379) = V_USER(57)
  V(409) = V_USER(58)
  V(380) = V_USER(59)
  V(339) = V_USER(60)
  V(389) = V_USER(61)
  V(388) = V_USER(62)
  V(428) = V_USER(63)
  V(406) = V_USER(64)
  V(352) = V_USER(65)
  V(383) = V_USER(66)
  V(324) = V_USER(67)
  V(340) = V_USER(68)
  V(421) = V_USER(69)
  V(397) = V_USER(70)
  V(390) = V_USER(71)
  V(384) = V_USER(72)
  V(358) = V_USER(73)
  V(394) = V_USER(74)
  V(393) = V_USER(75)
  V(420) = V_USER(76)
  V(398) = V_USER(77)
  V(422) = V_USER(78)
  V(399) = V_USER(79)
  V(353) = V_USER(80)
  V(426) = V_USER(81)
  V(354) = V_USER(82)
  V(425) = V_USER(83)
  V(400) = V_USER(84)
  V(355) = V_USER(85)
  V(372) = V_USER(86)
  V(373) = V_USER(87)
  V(401) = V_USER(88)
  V(402) = V_USER(89)
  V(356) = V_USER(90)
  V(412) = V_USER(91)
  V(413) = V_USER(92)
  V(327) = V_USER(93)
  V(392) = V_USER(94)
  V(347) = V_USER(95)
  V(410) = V_USER(96)
  V(359) = V_USER(97)
  V(335) = V_USER(98)
  V(357) = V_USER(99)
  V(411) = V_USER(100)
  V(351) = V_USER(101)
  V(385) = V_USER(102)
  V(346) = V_USER(103)
  V(418) = V_USER(104)
  V(332) = V_USER(105)
  V(386) = V_USER(106)
  V(427) = V_USER(107)
  V(387) = V_USER(108)
  V(408) = V_USER(109)
  V(429) = V_USER(110)
  V(371) = V_USER(111)
  V(431) = V_USER(112)
  V(378) = V_USER(113)
  V(345) = V_USER(114)
  V(382) = V_USER(115)
  V(341) = V_USER(116)
  V(391) = V_USER(117)
  V(365) = V_USER(118)
  V(416) = V_USER(119)
  V(376) = V_USER(120)
  V(342) = V_USER(121)
  V(3) = V_USER(317)
  V(4) = V_USER(318)
  V(6) = V_USER(319)
  V(7) = V_USER(320)
  V(8) = V_USER(321)
  V(9) = V_USER(322)
  V(10) = V_USER(323)
  V(11) = V_USER(324)
  V(12) = V_USER(325)
  V(13) = V_USER(326)
  V(14) = V_USER(327)
  V(15) = V_USER(328)
  V(16) = V_USER(329)
  V(17) = V_USER(330)
  V(18) = V_USER(331)
  V(19) = V_USER(332)
  V(20) = V_USER(333)
  V(21) = V_USER(334)
  V(22) = V_USER(335)
  V(23) = V_USER(336)
  V(24) = V_USER(337)
  V(25) = V_USER(338)
  V(26) = V_USER(339)
  V(27) = V_USER(340)
  V(28) = V_USER(341)
  V(29) = V_USER(342)
  V(30) = V_USER(343)
  V(31) = V_USER(344)
  V(32) = V_USER(345)
  V(33) = V_USER(346)
  V(34) = V_USER(347)
  V(35) = V_USER(348)
  V(36) = V_USER(349)
  V(37) = V_USER(350)
  V(38) = V_USER(351)
  V(39) = V_USER(352)
  V(40) = V_USER(353)
  V(41) = V_USER(354)
  V(42) = V_USER(355)
  V(43) = V_USER(356)
  V(44) = V_USER(357)
  V(45) = V_USER(358)
  V(46) = V_USER(359)
  V(47) = V_USER(360)
  V(48) = V_USER(361)
  V(49) = V_USER(362)
  V(50) = V_USER(363)
  V(51) = V_USER(364)
  V(52) = V_USER(365)
  V(53) = V_USER(366)
  V(54) = V_USER(367)
  V(55) = V_USER(368)
  V(56) = V_USER(369)
  V(57) = V_USER(370)
  V(58) = V_USER(371)
  V(59) = V_USER(372)
  V(60) = V_USER(373)
  V(61) = V_USER(374)
  V(62) = V_USER(375)
  V(63) = V_USER(376)
  V(64) = V_USER(377)
  V(65) = V_USER(378)
  V(66) = V_USER(379)
  V(67) = V_USER(380)
  V(68) = V_USER(381)
  V(69) = V_USER(382)
  V(70) = V_USER(383)
  V(71) = V_USER(384)
  V(72) = V_USER(385)
  V(73) = V_USER(386)
  V(74) = V_USER(387)
  V(75) = V_USER(388)
  V(76) = V_USER(389)
  V(77) = V_USER(390)
  V(78) = V_USER(391)
  V(79) = V_USER(392)
  V(80) = V_USER(393)
  V(81) = V_USER(394)
  V(82) = V_USER(395)
  V(83) = V_USER(396)
  V(84) = V_USER(397)
  V(85) = V_USER(398)
  V(86) = V_USER(399)
  V(87) = V_USER(400)
  V(88) = V_USER(401)
  V(89) = V_USER(402)
  V(90) = V_USER(403)
  V(91) = V_USER(404)
  V(92) = V_USER(405)
  V(93) = V_USER(406)
  V(94) = V_USER(407)
  V(95) = V_USER(408)
  V(96) = V_USER(409)
  V(97) = V_USER(410)
  V(98) = V_USER(411)
  V(99) = V_USER(412)
  V(100) = V_USER(413)
  V(101) = V_USER(414)
  V(102) = V_USER(415)
  V(103) = V_USER(416)
  V(104) = V_USER(417)
  V(105) = V_USER(418)
  V(106) = V_USER(419)
  V(107) = V_USER(420)
  V(108) = V_USER(421)
  V(109) = V_USER(422)
  V(110) = V_USER(423)
  V(111) = V_USER(424)
  V(112) = V_USER(425)
  V(113) = V_USER(426)
  V(114) = V_USER(427)
  V(115) = V_USER(428)
  V(116) = V_USER(429)
  V(117) = V_USER(430)
  V(118) = V_USER(431)
  V(119) = V_USER(432)
  V(120) = V_USER(433)
  V(121) = V_USER(434)
  V(122) = V_USER(435)
  V(123) = V_USER(436)
  V(124) = V_USER(437)
      
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

  V_USER(1) = V(322)
  V_USER(2) = V(5)
  V_USER(3) = V(323)
  V_USER(4) = V(438)
  V_USER(5) = V(434)
  V_USER(6) = V(395)
  V_USER(8) = V(325)
  V_USER(9) = V(1)
  V_USER(10) = V(437)
  V_USER(11) = V(436)
  V_USER(12) = V(328)
  V_USER(13) = V(343)
  V_USER(14) = V(424)
  V_USER(15) = V(360)
  V_USER(16) = V(362)
  V_USER(17) = V(363)
  V_USER(18) = V(361)
  V_USER(19) = V(333)
  V_USER(20) = V(344)
  V_USER(21) = V(415)
  V_USER(22) = V(334)
  V_USER(23) = V(433)
  V_USER(24) = V(381)
  V_USER(25) = V(2)
  V_USER(26) = V(329)
  V_USER(27) = V(330)
  V_USER(28) = V(404)
  V_USER(29) = V(364)
  V_USER(30) = V(326)
  V_USER(31) = V(414)
  V_USER(32) = V(348)
  V_USER(33) = V(435)
  V_USER(34) = V(349)
  V_USER(35) = V(338)
  V_USER(36) = V(350)
  V_USER(37) = V(419)
  V_USER(38) = V(366)
  V_USER(39) = V(396)
  V_USER(40) = V(374)
  V_USER(41) = V(336)
  V_USER(42) = V(403)
  V_USER(43) = V(430)
  V_USER(44) = V(375)
  V_USER(45) = V(423)
  V_USER(46) = V(367)
  V_USER(47) = V(377)
  V_USER(48) = V(368)
  V_USER(49) = V(369)
  V_USER(50) = V(432)
  V_USER(51) = V(417)
  V_USER(52) = V(331)
  V_USER(53) = V(405)
  V_USER(54) = V(370)
  V_USER(55) = V(407)
  V_USER(56) = V(337)
  V_USER(57) = V(379)
  V_USER(58) = V(409)
  V_USER(59) = V(380)
  V_USER(60) = V(339)
  V_USER(61) = V(389)
  V_USER(62) = V(388)
  V_USER(63) = V(428)
  V_USER(64) = V(406)
  V_USER(65) = V(352)
  V_USER(66) = V(383)
  V_USER(67) = V(324)
  V_USER(68) = V(340)
  V_USER(69) = V(421)
  V_USER(70) = V(397)
  V_USER(71) = V(390)
  V_USER(72) = V(384)
  V_USER(73) = V(358)
  V_USER(74) = V(394)
  V_USER(75) = V(393)
  V_USER(76) = V(420)
  V_USER(77) = V(398)
  V_USER(78) = V(422)
  V_USER(79) = V(399)
  V_USER(80) = V(353)
  V_USER(81) = V(426)
  V_USER(82) = V(354)
  V_USER(83) = V(425)
  V_USER(84) = V(400)
  V_USER(85) = V(355)
  V_USER(86) = V(372)
  V_USER(87) = V(373)
  V_USER(88) = V(401)
  V_USER(89) = V(402)
  V_USER(90) = V(356)
  V_USER(91) = V(412)
  V_USER(92) = V(413)
  V_USER(93) = V(327)
  V_USER(94) = V(392)
  V_USER(95) = V(347)
  V_USER(96) = V(410)
  V_USER(97) = V(359)
  V_USER(98) = V(335)
  V_USER(99) = V(357)
  V_USER(100) = V(411)
  V_USER(101) = V(351)
  V_USER(102) = V(385)
  V_USER(103) = V(346)
  V_USER(104) = V(418)
  V_USER(105) = V(332)
  V_USER(106) = V(386)
  V_USER(107) = V(427)
  V_USER(108) = V(387)
  V_USER(109) = V(408)
  V_USER(110) = V(429)
  V_USER(111) = V(371)
  V_USER(112) = V(431)
  V_USER(113) = V(378)
  V_USER(114) = V(345)
  V_USER(115) = V(382)
  V_USER(116) = V(341)
  V_USER(117) = V(391)
  V_USER(118) = V(365)
  V_USER(119) = V(416)
  V_USER(120) = V(376)
  V_USER(121) = V(342)
  V_USER(317) = V(3)
  V_USER(318) = V(4)
  V_USER(319) = V(6)
  V_USER(320) = V(7)
  V_USER(321) = V(8)
  V_USER(322) = V(9)
  V_USER(323) = V(10)
  V_USER(324) = V(11)
  V_USER(325) = V(12)
  V_USER(326) = V(13)
  V_USER(327) = V(14)
  V_USER(328) = V(15)
  V_USER(329) = V(16)
  V_USER(330) = V(17)
  V_USER(331) = V(18)
  V_USER(332) = V(19)
  V_USER(333) = V(20)
  V_USER(334) = V(21)
  V_USER(335) = V(22)
  V_USER(336) = V(23)
  V_USER(337) = V(24)
  V_USER(338) = V(25)
  V_USER(339) = V(26)
  V_USER(340) = V(27)
  V_USER(341) = V(28)
  V_USER(342) = V(29)
  V_USER(343) = V(30)
  V_USER(344) = V(31)
  V_USER(345) = V(32)
  V_USER(346) = V(33)
  V_USER(347) = V(34)
  V_USER(348) = V(35)
  V_USER(349) = V(36)
  V_USER(350) = V(37)
  V_USER(351) = V(38)
  V_USER(352) = V(39)
  V_USER(353) = V(40)
  V_USER(354) = V(41)
  V_USER(355) = V(42)
  V_USER(356) = V(43)
  V_USER(357) = V(44)
  V_USER(358) = V(45)
  V_USER(359) = V(46)
  V_USER(360) = V(47)
  V_USER(361) = V(48)
  V_USER(362) = V(49)
  V_USER(363) = V(50)
  V_USER(364) = V(51)
  V_USER(365) = V(52)
  V_USER(366) = V(53)
  V_USER(367) = V(54)
  V_USER(368) = V(55)
  V_USER(369) = V(56)
  V_USER(370) = V(57)
  V_USER(371) = V(58)
  V_USER(372) = V(59)
  V_USER(373) = V(60)
  V_USER(374) = V(61)
  V_USER(375) = V(62)
  V_USER(376) = V(63)
  V_USER(377) = V(64)
  V_USER(378) = V(65)
  V_USER(379) = V(66)
  V_USER(380) = V(67)
  V_USER(381) = V(68)
  V_USER(382) = V(69)
  V_USER(383) = V(70)
  V_USER(384) = V(71)
  V_USER(385) = V(72)
  V_USER(386) = V(73)
  V_USER(387) = V(74)
  V_USER(388) = V(75)
  V_USER(389) = V(76)
  V_USER(390) = V(77)
  V_USER(391) = V(78)
  V_USER(392) = V(79)
  V_USER(393) = V(80)
  V_USER(394) = V(81)
  V_USER(395) = V(82)
  V_USER(396) = V(83)
  V_USER(397) = V(84)
  V_USER(398) = V(85)
  V_USER(399) = V(86)
  V_USER(400) = V(87)
  V_USER(401) = V(88)
  V_USER(402) = V(89)
  V_USER(403) = V(90)
  V_USER(404) = V(91)
  V_USER(405) = V(92)
  V_USER(406) = V(93)
  V_USER(407) = V(94)
  V_USER(408) = V(95)
  V_USER(409) = V(96)
  V_USER(410) = V(97)
  V_USER(411) = V(98)
  V_USER(412) = V(99)
  V_USER(413) = V(100)
  V_USER(414) = V(101)
  V_USER(415) = V(102)
  V_USER(416) = V(103)
  V_USER(417) = V(104)
  V_USER(418) = V(105)
  V_USER(419) = V(106)
  V_USER(420) = V(107)
  V_USER(421) = V(108)
  V_USER(422) = V(109)
  V_USER(423) = V(110)
  V_USER(424) = V(111)
  V_USER(425) = V(112)
  V_USER(426) = V(113)
  V_USER(427) = V(114)
  V_USER(428) = V(115)
  V_USER(429) = V(116)
  V_USER(430) = V(117)
  V_USER(431) = V(118)
  V_USER(432) = V(119)
  V_USER(433) = V(120)
  V_USER(434) = V(121)
  V_USER(435) = V(122)
  V_USER(436) = V(123)
  V_USER(437) = V(124)
      
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

