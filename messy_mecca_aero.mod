G95 module created on Fri Jul 24 09:54:30 2009 from messy_mecca_aero.f90
If you edit this, you'll get what you deserve.
module-version 6
(() () () () () () () () () () () () () () () () () () () () ())

()

()

()

(('messy_mecca_kpp_global' (VARIABLE (REAL 8) 0 2 ((ARRAY (ELEMENT 1 (
CONSTANT (INTEGER 4) 0 '104') 1)))) 'messy_mecca_kpp_global' (VARIABLE (
REAL 8) 0 3 ((ARRAY (ELEMENT 1 (CONSTANT (INTEGER 4) 0 '1') 1))))) (
'messy_mecca_kpp_global' (VARIABLE (REAL 8) 0 2 ((ARRAY (ELEMENT 1 (
CONSTANT (INTEGER 4) 0 '1') 1)))) 'messy_mecca_kpp_global' (VARIABLE (
REAL 8) 0 4 ((ARRAY (ELEMENT 1 (CONSTANT (INTEGER 4) 0 '1') 1))))))

(5 'density' 'messy_mecca_aero' 1 ((PROCEDURE UNKNOWN MODULE-PROC DECL
NONE NONE FUNCTION ELEMENTAL) (REAL 8) 0 0 (6 NONE 7 NONE 8 NONE) () ''
() ())
9 'idt_brm' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE
NONE DIMENSION) (INTEGER 4) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1')
(CONSTANT (INTEGER 4) 0 '1')) '' () ())
10 'idt_brorg' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () () '' () ())
11 'idt_brsalt' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () () '' () ())
12 'idt_brsscap' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (INTEGER 4) 0 0 () () '' () ())
13 'idt_clm' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE DIMENSION) (INTEGER 4) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4)
0 '1') (CONSTANT (INTEGER 4) 0 '1')) '' () ())
14 'idt_hco3m' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE DIMENSION) (INTEGER 4) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4)
0 '1') (CONSTANT (INTEGER 4) 0 '1')) '' () ())
15 'idt_hp' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE
NONE DIMENSION) (INTEGER 4) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1')
(CONSTANT (INTEGER 4) 0 '1')) '' () ())
16 'idt_im' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE
NONE DIMENSION) (INTEGER 4) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1')
(CONSTANT (INTEGER 4) 0 '1')) '' () ())
17 'idt_io3m' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE DIMENSION) (INTEGER 4) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4)
0 '1') (CONSTANT (INTEGER 4) 0 '1')) '' () ())
18 'idt_nap' 'messy_mecca_aero' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE DIMENSION) (INTEGER 4) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4)
0 '1') (CONSTANT (INTEGER 4) 0 '1')) '' () ())
19 'layerthickness' 'messy_mecca_aero' 1 ((PROCEDURE UNKNOWN MODULE-PROC
DECL NONE NONE FUNCTION ELEMENTAL) (REAL 8) 0 0 (20 NONE 21 NONE) () ''
() ())
22 'mecca_aero_calc_k_ex' 'messy_mecca_aero' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (23 NONE 24
NONE 25 NONE 26 NONE 27 NONE 28 NONE 29 NONE 30 NONE 31 NONE 32 NONE 33
NONE) () '' () ())
34 'mecca_aero_diag' 'messy_mecca_aero' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (35 NONE 36
NONE 37 NONE 38 NONE 39 NONE 40 NONE) () '' () ())
41 'mecca_aero_henry' 'messy_mecca_aero' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (42 NONE 43
NONE) () '' () ())
44 'mecca_aero_trans_coeff' 'messy_mecca_aero' 1 ((PROCEDURE UNKNOWN
MODULE-PROC DECL NONE NONE SUBROUTINE) (PROCEDURE 0) 0 0 (45 NONE 46
NONE 47 NONE 48 NONE) () '' () ())
49 'submodstr' 'messy_mecca_aero' 1 ((PARAMETER UNKNOWN UNKNOWN UNKNOWN
NONE NONE) (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '10'))) 0 0 () (
CONSTANT (CHARACTER 1 ((CONSTANT (INTEGER 4) 0 '10'))) 0 10 'mecca_aero')
() '' () ())
48 'ykmt' '' 50 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (2 EXPLICIT (CONSTANT (INTEGER 4) 0 '1') (CONSTANT (
INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '0') (CONSTANT (INTEGER 4) 0
'106')) '' () ())
47 'zpress' '' 50 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8)
0 0 () () '' () ())
46 'ztemp' '' 50 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8)
0 0 () () '' () ())
45 'radius' '' 50 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1') (
CONSTANT (INTEGER 4) 0 '1')) '' () ())
43 'yhenry' '' 51 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '0') (
CONSTANT (INTEGER 4) 0 '106')) '' () ())
42 'ztemp' '' 51 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8)
0 0 () () '' () ())
40 'zmrac_brsscap' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY)
(REAL 8) 0 0 () () '' () ())
39 'zmrac_brorg' '' 52 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY)
(REAL 8) 0 0 () () '' () ())
38 'zmrac_brsalt' '' 52 ((VARIABLE INOUT UNKNOWN UNKNOWN NONE NONE DUMMY)
(REAL 8) 0 0 () () '' () ())
37 'zmr_brsscap' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
REAL 8) 0 0 () () '' () ())
36 'zmr_brorg' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
REAL 8) 0 0 () () '' () ())
35 'zmr_brsalt' '' 52 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
REAL 8) 0 0 () () '' () ())
33 'k_exf_brno3' '' 53 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE
DIMENSION DUMMY) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1')
(CONSTANT (INTEGER 4) 0 '1')) '' () ())
32 'k_exf_clno3' '' 53 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE
DIMENSION DUMMY) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1')
(CONSTANT (INTEGER 4) 0 '1')) '' () ())
31 'k_exf_n2o5' '' 53 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1') (
CONSTANT (INTEGER 4) 0 '1')) '' () ())
30 'k_exb' '' 53 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (2 EXPLICIT (CONSTANT (INTEGER 4) 0 '1') (
CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '1') (CONSTANT (
INTEGER 4) 0 '106')) '' () ())
29 'k_exf' '' 53 ((VARIABLE OUT UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (2 EXPLICIT (CONSTANT (INTEGER 4) 0 '1') (
CONSTANT (INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '1') (CONSTANT (
INTEGER 4) 0 '106')) '' () ())
28 'yhenry' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '0') (
CONSTANT (INTEGER 4) 0 '106')) '' () ())
27 'ykmt' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (2 EXPLICIT (CONSTANT (INTEGER 4) 0 '1') (CONSTANT (
INTEGER 4) 0 '1') (CONSTANT (INTEGER 4) 0 '0') (CONSTANT (INTEGER 4) 0
'106')) '' () ())
26 'c' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY) (
REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) '' () ())
25 'lwc' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) '' () ())
24 'xaer' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION DUMMY)
(REAL 8) 0 0 () (1 ASSUMED_SHAPE () ()) '' () ())
23 'loghet' '' 53 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DIMENSION
DUMMY) (LOGICAL 4) 0 0 () (1 ASSUMED_SHAPE () ()) '' () ())
21 'geopot_l' '' 54 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
REAL 8) 0 0 () () '' () ())
20 'geopot_u' '' 54 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (
REAL 8) 0 0 () () '' () ())
8 'sphum' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8)
0 0 () () '' () ())
7 'temp' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8) 0
0 () () '' () ())
6 'press' '' 55 ((VARIABLE IN UNKNOWN UNKNOWN NONE NONE DUMMY) (REAL 8)
0 0 () () '' () ())
4 'var' 'messy_mecca_kpp_global' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE DIMENSION) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0
'1') (CONSTANT (INTEGER 4) 0 '103')) '' () ())
3 'fix' 'messy_mecca_kpp_global' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN
NONE NONE DIMENSION) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0
'1') (CONSTANT (INTEGER 4) 0 '3')) '' () ())
2 'c' 'messy_mecca_kpp_global' 1 ((VARIABLE UNKNOWN UNKNOWN UNKNOWN NONE
NONE DIMENSION) (REAL 8) 0 0 () (1 EXPLICIT (CONSTANT (INTEGER 4) 0 '1')
(CONSTANT (INTEGER 4) 0 '106')) '' () ())
)

('density' 0 5 'idt_brm' 0 9 'idt_brorg' 0 10 'idt_brsalt' 0 11
'idt_brsscap' 0 12 'idt_clm' 0 13 'idt_hco3m' 0 14 'idt_hp' 0 15 'idt_im'
0 16 'idt_io3m' 0 17 'idt_nap' 0 18 'layerthickness' 0 19
'mecca_aero_calc_k_ex' 0 22 'mecca_aero_diag' 0 34 'mecca_aero_henry' 0
41 'mecca_aero_trans_coeff' 0 44 'submodstr' 0 49)
