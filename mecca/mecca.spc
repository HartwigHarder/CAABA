{This file was created automatically by xmecca, DO NOT EDIT!}
{xmecca was run on 2010-08-26 at 14:15:43 by caaba}
{***** START: gas-phase species from gas.spc *****}
#INCLUDE atoms

{ Species are sorted by elements in the following order:                      }
{ O,H,N,C,Cl,Br,I,S                                                           }
{ All peroxides are called ROOH, all peroxy radicals are called RO2           }

{ All species are defined here with #DEFVAR as VARIABLES. Some species        }
{ will be turned into FIXED species with #SETFIX in messy_mecca_kpp.kpp       }

#DEFVAR

{-----------------------------------------------------------------------------}
{--------------------------------- gas phase ---------------------------------}
{-----------------------------------------------------------------------------}

{------------------------------------- O -------------------------------------}

O1D           =  O                ; {@O(^1D)}            {O singlet D}
O3P           =  O                ; {@O(^3P)}            {O triplet P}
O2            = 2O                ; {@O_2}               {oxygen}
O3            = 3O                ; {@O_3}               {ozone}

{------------------------------------- H -------------------------------------}

H             =  H                ; {@H}                 {hydrogen atom}
H2            = 2H                ; {@H_2}               {hydrogen}
OH            =  H +  O           ; {@OH}                {hydroxyl radical}
HO2           =  H + 2O           ; {@HO_2}              {hydroperoxy radical}
H2O           = 2H +  O           ; {@H_2O}              {water}
H2O2          = 2H + 2O           ; {@H_2O_2}            {hydrogen peroxide}

{------------------------------------- N -------------------------------------}

N             =            N      ; {@N}                 {nitrogen atom}
N2            =           2N      ; {@N_2}               {nitrogen}
NH3           = 3H      +  N      ; {@NH_3}              {ammonia}
N2O           =       O + 2N      ; {@N_2O}              {nitrous oxide}
NO            =       O +  N      ; {@NO}                {nitric oxide}
NO2           =      2O +  N      ; {@NO_2}              {nitrogen dioxide}
NO3           =      3O +  N      ; {@NO_3}              {nitrogen trioxide}
N2O5          =      5O + 2N      ; {@N_2O_5}            {dinitrogen pentoxide}
HONO          =  H + 2O +  N      ; {@HONO}              {nitrous acid}
HNO3          =  H + 3O +  N      ; {@HNO_3}             {nitric acid}
HNO4          =  H + 4O +  N      ; {@HNO_4}             {peroxynitric acid}
NH2           = 2H      +  N      ; {@NH_2}              {}
HNO           =  H +  O +  N      ; {@HNO}               {}
NHOH          = 2H +  O +  N      ; {@NHOH}              {}
NH2O          = 2H +  O +  N      ; {@NH_2O}             {}
NH2OH         = 3H +  O +  N      ; {@NH_2OH}            {}

{------------------------------------- C -------------------------------------}

{1C}
CH3O2         =  C +  3H + 2O     ; {@CH_3O_2}           {methylperoxy radical}
CH3OH         =  C +  4H +  O     ; {@CH_3OH}            {methanol}
CH3OOH        =  C +  4H + 2O     ; {@CH_3OOH}           {methyl peroxide}
CH4           =  C +  4H          ; {@CH_4}              {methane}
CO            =  C       +  O     ; {@CO}                {carbon monoxide}
CO2           =  C       + 2O     ; {@CO_2}              {carbon dioxide}
HCHO          =  C +  2H +  O     ; {@HCHO}              {methanal (formaldehyde)}
HCOOH         =  C +  2H + 2O     ; {@HCOOH}             {formic acid}
LCARBON       =  C + IGNORE       ; {@LCARBON}           {lumped C1 species}

{2C}
C2H2          = 2C +  2H          ; {@C_2H_2}            {ethyne}
C2H4          = 2C +  4H          ; {@C_2H_4}            {ethene}
C2H5O2        = 2C +  5H + 2O     ; {@C_2H_5O_2}         {ethylperoxy radical}
C2H5OOH       = 2C +  6H + 2O     ; {@C_2H_5OOH}         {ethyl hydro peroxide}
C2H6          = 2C +  6H          ; {@C_2H_6}            {ethane}
CH3CHO        = 2C +  4H +  O     ; {@CH_3CHO}           {acetaldehyde}
CH3CO2H       = 2C +  4H + 2O     ; {@CH_3COOH}          {acetic acid}
CH3CO3        = 2C +  3H + 3O     ; {@CH_3C(O)OO}        {peroxy acetyl radical}
CH3CO3H       = 2C +  4H + 3O     ; {@CH_3C(O)OOH}       {peroxy acetic acid}
ETHGLY        = 2C +  6H + 2O     ; {@ETHGLY}            {HOCH2CH2OH}
ETHOHNO3      = 2C +  5H + 4O + N ; {@ETHOHNO3}          {HOCH2CH2ONO2}
GLYOX         = 2C +  2H + 2O     ; {@GLYOX}             {CHOCHO = glyoxal}
HCOCO2H       = 2C +  2H + 3O     ; {@HCOCO_2H}          {oxoethanoic acid}
HCOCO3        = 2C +   H + 4O     ; {@HCOCO_3}           {}
HCOCO3H       = 2C +  2H + 4O     ; {@HCOCO_3H}          {}
HOCH2CH2O     = 2C +  5H + 2O     ; {@HOCH_2CH_2O}       {}
HOCH2CH2O2    = 2C +  5H + 3O     ; {@HOCH_2CH_2O_2}     {}
HOCH2CHO      = 2C +  4H + 2O     ; {@HOCH_2CHO}         {glycolaldehyde}
HOCH2CO2H     = 2C +  4H + 3O     ; {@HOCH_2CO_2H}       {hydroxyethanoic acid}
HOCH2CO3      = 2C +  3H + 4O     ; {@HOCH_2CO_3}        {}
HOCH2CO3H     = 2C +  4H + 4O     ; {@HOCH_2CO_3H}       {}
HYETHO2H      = 2C +  6H + 3O     ; {@HYETHO2H}          {HOCH2CH2OOH}
PAN           = 2C +  3H + 5O + N ; {@PAN}               {CH3C(O)OONO2 = peroxyacetylnitrate}
PHAN          = 2C +  3H + 6O     ; {@PHAN}              {HOCH2C(O)OONO2}
{3C}
ACETOL        = 3C +  6H + 2O     ; {@CH_3COCH_2OH}      {HO-CH2-CO-CH3 = hydroxy acetone}
C3H6          = 3C +  6H          ; {@C_3H_6}            {propene}
C3H8          = 3C +  8H          ; {@C_3H_8}            {propane}
CH3COCH2O2    = 3C +  5H + 3O     ; {@CH_3COCH_2O_2}     {peroxyradical from acetone}
CH3COCH3      = 3C +  6H +  O     ; {@CH_3COCH_3}        {acetone}
HOCH2COCHO    = 3C +  4H + 3O     ; {@HOCH2COCHO}        {}
HOCH2COCO2H   = 3C +  4H + 4O     ; {@HOCH2COCO2H}       {}
HYPERACET     = 3C +  6H + 3O     ; {@CH_3COCH_2O_2H}    {hydroperoxide from CH3COCH2O2}
HYPROPO2      = 3C +  7H + 3O     ; {@HYPROPO2}          {CH3CH(O2)CH2OH}
HYPROPO2H     = 3C +  8H + 3O     ; {@HYPROPO2H}         {CH3CH(OOH)CH2OH}
IC3H7NO3      = 3C +  7H + 3O + N ; {@iC_3H_7ONO_2}      {i-propyl nitrate}
IC3H7O2       = 3C +  7H + 2O     ; {@iC_3H_7O_2}        {i-propylperoxy radical}
IC3H7OOH      = 3C +  8H + 2O     ; {@iC_3H_7OOH}        {i-propyl hydro peroxide}
MGLYOX        = 3C +  4H + 2O     ; {@MGLYOX}            {CH3COCHO = methylglyoxal}
NOA           = 3C +  5H + 4O + N ; {@NOA}               {CH3-CO-CH2ONO2 = nitro-oxy-acetone}
PR2O2HNO3     = 3C +  7H + 5O + N ; {@PR2O2HNO3}         {CH3-CH(OOH)-CH2ONO2}
PRONO3BO2     = 3C +  6H + 5O + N ; {@PRONO3BO2}         {CH3-CH(O2)-CH2ONO2}
{4C}
BIACET        = 4C +  6H + 2O     ; {@BIACET}            {CH3-CO-CO-CH3}
BIACETOH      = 4C +  6H + 3O     ; {@BIACETOH}          {CH3-CO-CO-CH2OH}
CO2H3CHO      = 4C +  5H + 3O     ; {@CO2H3CHO}          {CH3-CO-CH(OH)-CHO}
CO2H3CO3      = 4C +  5H + 5O     ; {@CO2H3CO3}          {CH3-CO-CH(OH)-C(O)O2}
CO2H3CO3H     = 4C +  6H + 5O     ; {@CO2H3CO3H}         {CH3-CO-CH(OH)-C(O)OOH}
HO12CO3C4     = 4C +  8H + 3O     ; {@HO12CO3C4}         {CH3-CO-CH(OH)-CH2OH}
LC4H9NO3      = 4C +  9H + 3O + N ; {@LC4H9NO3}          {NC4H9NO3 and SC4H9NO3}
LC4H9O2       = 4C +  9H + 2O     ; {@LC_4H_9O_2}        {CH3-CH2-CH(O2)-CH3 + CH3-CH2-CH2-CH2O2 MCM: NC4H9O2  and SC4H9O2}
LC4H9OOH      = 4C + 10H + 2O     ; {@LC_4H_9OOH}        {CH3-CH2-CH(OOH)-CH3 + CH3-CH2-CH2-CH2OOH MCM: NC4H9OOH and SC4H9OOH}
LHMVKABO2     = 4C +  7H + 4O     ; {@LHMVKABO2}         {HOCH2-CH(O2)-CO-CH3 + CH2(O2)-CH(OH)-CO-CH3}
LHMVKABOOH    = 4C +  8H + 4O     ; {@LHMVKABOOH}        {HOCH2-CH(OOH)-CO-CH3 + CH2(OOH)-CH(OH)-CO-CH3}
LMVKOHABO2    = 4C +  7H + 5O     ; {@LMVKOHABO2}        {HOCH2-CH(O2)-CO-CH2OH + CH2(O2)-CH(OH)-CO-CH2OH}
LMVKOHABOOH   = 4C +  8H + 5O     ; {@LMVKOHABOOH}       {HOCH2-CH(OOH)-CO-CH2OH + CH2(OOH)-CH(OH)-CO-CH2OH}
MACO2H        = 4C +  6H + 2O     ; {@MACO2H}            {CH2=C(CH3)COOH}
MACO3         = 4C +  5H + 3O     ; {@MACO3}             {CH2=C(CH3)C(O)O2}
MACO3H        = 4C +  6H + 2O     ; {@MACO3H}            {CH2=C(CH3)C(O)OOH}
MACR          = 4C +  6H +  O     ; {@MACR}              {CH2=C(CH3)CHO = methacrolein}
MACRO2        = 4C +  7H + 4O     ; {@MACRO2}            {HOCH2C(OO)(CH3)CHO}
MACROH        = 4C +  8H + 3O     ; {@MACROH}            {HOCH2C(OH)(CH3)CHO}
MACROOH       = 4C +  8H + 4O     ; {@MACROOH}           {HOCH2C(OOH)(CH3)CHO}
MEK           = 4C +  8H +  O     ; {@MEK}               {CH3-CO-CH2-CH3 = methyl ethyl ketone}
LMEKO2        = 4C +  7H + 3O     ; {@LMEKO2}            {CH3-CO-CH2-CH2-OO}
LMEKOOH       = 4C +  8H + 3O     ; {@LMEKOOH}           {CH3-CO-CH2-CH2-OOH}
MPAN          = 4C +  5H + 5O + N ; {@MPAN}              {CH2=C(CH3)C(O)OONO2 = peroxymethacryloyl nitrate ; peroxymethacrylic nitric anhydride}
MVK           = 4C +  6H +  O     ; {@MVK}               {CH3-CO-CH=CH2 = methyl vinyl ketone}
MVKOH         = 4C +  6H + 2O     ; {@MVKOH}             {CH2=CHC(=O)CH2OH}
NC4H10        = 4C + 10H          ; {@nC_4H_<10>}        {CH3-CH2-CH2-CH3 = n-butane}
{5C}
C59O2         = 5C +  9H + 5O     ; {@C59O2}             {HOCH2-CO-C(CH3)(O2)-CH2OH}
C59OOH        = 5C + 10H + 5O     ; {@C59OOH}            {HOCH2-CO-C(CH3)(OOH)-CH2OH}
C5H8          = 5C +  8H          ; {@C_5H_8}            {CH2=C(CH3)CH=CH2 = isoprene}
HCOC5         = 5C +  8H + 2O     ; {@HCOC5}             {HOCH2-CO-C(CH3)=CH2}
ISOPAOH       = 5C + 10H + 2O     ; {@ISOPAOH}           {HOCH2-C(CH3)=CH-CH2OH}
ISOPBNO3      = 5C +  9H + 4O + N ; {@ISOPBNO3}          {HOCH2-C(CH3)(ONO2)-CH=CH2}
ISOPBO2       = 5C +  9H + 3O     ; {@ISOPBO2}           {HOCH2-C(CH3)(O2)-CH=CH2}
ISOPBOH       = 5C + 10H + 2O     ; {@ISOPBOH}           {HOCH2-C(CH3)(OH)-CH=CH2}
ISOPBOOH      = 5C + 10H + 3O     ; {@ISOPBOOH}          {HOCH2-C(CH3)(OOH)-CH=CH2}
ISOPDNO3      = 5C +  9H + 4O + N ; {@ISOPDNO3}          {CH2=C(CH3)CH(ONO2)-CH2OH}
ISOPDO2       = 5C +  9H + 3O     ; {@ISOPDO2}           {CH2=C(CH3)CH(O2)-CH2OH}
ISOPDOH       = 5C + 10H + 2O     ; {@ISOPDOH}           {CH2=C(CH3)CH(OH)-CH2OH}
ISOPDOOH      = 5C + 10H + 3O     ; {@ISOPDOOH}          {CH2=C(CH3)CH(OOH)-CH2OH}
LC578O2       = 5C +  9H + 5O     ; {@LC578O2}           {HOCH2-CH(OH)C(CH3)(O2)-CHO + HOCH2-C(CH3)(O2)-CH(OH)-CHO}
LC578OOH      = 5C + 10H + 5O     ; {@LC578OOH}          {HOCH2-CH(OH)C(CH3)(OOH)-CHO + HOCH2-C(CH3)(OOH)-CH(OH)-CHO}
LC5PAN1719    = 5C +  7H + 6O + N ; {@LC5PAN1719}        {HOCH2-C(CH3)=CH-C(O)OONO2 + HOCH2-CH=C(CH3)C(O)OONO2}
LHC4ACCHO     = 5C +  8H + 2O     ; {@LHC4ACCHO}         {HOCH2-C(CH3)=CH-CHO + HOCH2-CH=C(CH3)-CHO}
LHC4ACCO2H    = 5C +  8H + 3O     ; {@LHC4ACCO2H}        {HOCH2-C(CH3)=CH-C(O)OH + HOCH2-CH=C(CH3)-C(O)OH}
LHC4ACCO3     = 5C +  7H + 4O     ; {@LHC4ACCO3}         {HOCH2-C(CH3)=CH-C(O)O2 + HOCH2-CH=C(CH3)-C(O)O2}
LHC4ACCO3H    = 5C +  8H + 4O     ; {@LHC4ACCO3H}        {HOCH2-C(CH3)=CH-C(O)OOH + HOCH2-CH=C(CH3)-C(O)OOH}
LISOPACNO3    = 5C + 10H + 4O + N ; {@LISOPACNO3}        {HOCH2-C(CH3)=CH-CH2ONO2 + HOCH2-CH=C(CH3)-CH2ONO2}
LISOPACO2     = 5C +  9H + 3O     ; {@LISOPACO2}         {HOCH2-C(CH3)=CH-CH2O2 + HOCH2-CH=C(CH3)-CH2O2}
LISOPACOOH    = 5C + 10H + 3O     ; {@LISOPACOOH}        {HOCH2-C(CH3)=CH-CH2OOH + HOCH2-CH=C(CH3)-CH2OOH}
LNISO3        = 5C + IGNORE   + N ; {@LNISO3}            {C510O2+NC4CO3 = CHO-CH(OH)-C(CH3)(O2)-CH2ONO2 + O2NOCH2-C(CH3)=CH-C(O)O2}
LNISOOH       = 5C + IGNORE   + N ; {@LNISOOH}           {CHO-CH(OH)-C(CH3)(OOH)-CH2ONO2 + O2NOCH2-C(CH3)=CH-C(O)OOH}
NC4CHO        = 5C +  7H + 4O + N ; {@NC4CHO}            {O2NOCH2-C(CH3)=CH-CHO}
NISOPO2       = 5C +  8H + 5O + N ; {@NISOPO2}           {O2NOCH2-C(CH3)=CH-CH2O2}
NISOPOOH      = 5C +  9H + 5O + N ; {@NISOPOOH}          {O2NOCH2-C(CH3)=CH-CH2OOH}

{------------------------------------- F -------------------------------------}

{------------------------------------- Cl ------------------------------------}

Cl            = Cl                ; {@Cl}                {chlorine atom}
Cl2           = 2Cl               ; {@Cl_2}              {chlorine}
ClO           = Cl + O            ; {@ClO}               {chlorine oxide}
HCl           = H + Cl            ; {@HCl}               {hydrochloric acid}
HOCl          = H + O + Cl        ; {@HOCl}              {hypochlorous acid}
Cl2O2         = 2Cl + 2O          ; {@Cl_2O_2}           {dichlorine dioxide}
OClO          = Cl + 2O           ; {@OClO}              {chlorine dioxide}
ClNO2         = Cl + 2O + N       ; {@ClNO_2}            {nitryl chloride}
ClNO3         = Cl + N + 3O       ; {@ClNO_3}            {chlorine nitrate}
CCl4          = C + 4Cl           ; {@CCl_4}             {tetrachloro methane}
CH3Cl         = C + 3H + Cl       ; {@CH_3Cl}            {chloromethane}
CH3CCl3       = 2C + 3H + 3Cl     ; {@CH_3CCl_3}         {1,1,1-trichloroethane = methyl chloroform = MCF}
CF2Cl2        = C + 2F + 2Cl      ; {@CF_2Cl_2}          {dichlorodifluoromethane = F12}
CFCl3         = C + F + 3Cl       ; {@CFCl_3}            {trichlorofluoromethane = F11}

{------------------------------------- Br ------------------------------------}

Br            = Br                ; {@Br}                {bromine atom}
Br2           = 2Br               ; {@Br_2}              {bromine}
BrO           = Br + O            ; {@BrO}               {bromine oxide}
HBr           = H + Br            ; {@HBr}               {hydrobromic acid}
HOBr          = H + O + Br        ; {@HOBr}              {hypobromous acid}
BrNO2         = Br + N + 2O       ; {@BrNO_2}            {nitryl bromide}
BrNO3         = Br + N + 3O       ; {@BrNO_3}            {bromine nitrate}
BrCl          = Br + Cl           ; {@BrCl}              {bromine chloride}
CH3Br         = Br + C +3H        ; {@CH_3Br}            {bromomethane}
CF3Br         = Br + 3F + C       ; {@CF_3Br}            {Halon 1301}
CF2ClBr       = Br + 2F + Cl + C  ; {@CF_2ClBr}          {Halon 1211}
CHCl2Br       = C + H + 2Cl + Br  ; {@CHCl_2Br}          {}
CHClBr2       = C + H + Cl + 2Br  ; {@CHClBr_2}          {}
CH2ClBr       = C + 2H + Cl + Br  ; {@CH_2ClBr}          {}
CH2Br2        = C + 2H + 2Br      ; {@CH_2Br_2}          {}
CHBr3         = C + H + 3Br       ; {@CHBr_3}            {}

{------------------------------------- I -------------------------------------}

I             = I                 ; {@I}                 {iodine atomic ground state}
I2            = 2I                ; {@I_2}               {molecular iodine}
IO            = I + O             ; {@IO}                {iodine monoxide radical}
OIO           = I + 2O            ; {@OIO}               {}
I2O2          = 2O + 2I           ; {@I_2O_2}            {}
HI            = H + I             ; {@HI}                {hydrogen iodide}
HOI           = H + O + I         ; {@HOI}               {hypoiodous acid}
HIO3          = H + I + 3O        ; {@HIO_3}             {}
INO2          = I + N + 2O        ; {@INO_2}             {iodine nitrite}
INO3          = I + N + 3O        ; {@INO_3}             {iodine nitrate}
CH3I          = C + 3H + I        ; {@CH_3I}             {iodomethane}
CH2I2         = C + 2H + 2I       ; {@CH_2I_2}           {diiodomethane}
C3H7I         = 3C + 7H + I       ; {@C_3H_7I}           {2-iodopropane}
ICl           = I + Cl            ; {@ICl}               {iodine chloride}
CH2ClI        = C + 2H + Cl + I   ; {@CH_2ClI}           {chloroiodomethane}
IBr           = I + Br            ; {@IBr}               {iodine bromide}

{------------------------------------- S -------------------------------------}

S             = S                 ; {@S}                 {sulfur atomic ground state}
SO            = S + O             ; {@SO}                {sulfur monoxide}
SO2           = S + 2O            ; {@SO_2}              {sulfur dioxide}
SH            = S + H             ; {@SH}                {}
H2SO4         = 2H + S + 4O       ; {@H_2SO_4}           {sulfuric acid}
CH3SO3H       = C + 4H + S + 3O   ; {@CH_3SO_3H}         {MSA: methane sulfonic acid}
DMS           = 2C + 6H + S       ; {@DMS}               {dimethyl sulfide}
DMSO          = 2C + 6H + S + O   ; {@DMSO}              {dimethyl sulfoxide: CH3SOCH3}
CH3SO2        = C + 3H + S + 2O   ; {@CH_3SO_2}          {}
CH3SO3        = C + 3H + S + 3O   ; {@CH_3SO_3}          {}
OCS           = C + S + O         ; {@OCS}               {}
SF6           = S + 6F            ; {@SF_6}              {sulfur hexaflouride}

{--------------------------------- Hg ----------------------------------------}

Hg            = Hg                ; {@Hg}                {}
HgO           = Hg + O            ; {@HgO}               {}
HgCl          = Hg + Cl           ; {@HgCl}              {}
HgCl2         = Hg + 2Cl          ; {@HgCl_2}            {}
HgBr          = Hg + Br           ; {@HgBr}              {}
HgBr2         = Hg + 2Br          ; {@HgBr_2}            {}
ClHgBr        = Hg + Cl + Br      ; {@ClHgBr}            {}
BrHgOBr       = Hg + O + 2Br      ; {@BrHgOBr}           {}
ClHgOBr       = Hg + O + Cl + Br  ; {@ClHgOBr}           {}

{--- mz_pj_20070209+}
{------------------------- Pseudo Aerosol ------------------------------------}
NO3m_cs       = N + 3O           ; {@NO_3^-\aq}         {}
Hp_cs         = H                ; {@H^+\aq}            {}
RGM_cs        = Hg               ; {@Hg\aq}             {from reactive gaseous Hg}
{--- mz_pj_20070209-}

{------------------------------- Dummies -------------------------------------}

IPART         = IGNORE           ; {@I_<part>}          {iodine particles}
Dummy         = IGNORE           ; {@Dummy}             {just a dummy}

{ mz_pj_20070621+}
{------------------------- O3 Budget Tracers (via eval2.3.rpl) ---------------}
O3s           = 3O               ; {@O_3(s)}            {strat. ozone}
LO3s          = IGNORE           ; {@LO_3(s)}           {lost strat. ozone}
{ mz_pj_20070621-}

{ mz_rs_20100227+}
{only for MIM1, not used in MIM2:}
LHOC3H6O2  = 3C + 7H + 3O     ; {@CH_3CH(O_2)CH_2OH} {hydroxyperoxyradical from propene+OH}
LHOC3H6OOH = 3C + 8H + 3O     ; {@CH_3CH(OOH)CH_2OH} {C3H6OHOOH = hydroxyhydroperoxides from C3H6}
ISO2       = 5C + 9H + 3O     ; {@ISO2}              {isoprene (hydroxy) peroxy radicals}
ISON       = IGNORE + N       ; {@ISON}              {organic nitrates from ISO2 and C5H8+NO3}
ISOOH      = 5C + 10H + 3O    ; {@ISOOH}             {isoprene (hydro) peroxides}
MVKO2      = 4C + 7H + 4O     ; {@MVKO2}             {MVK/MACR peroxy radicals}
MVKOOH     = 4C + 8H + 4O     ; {@MVKOOH}            {MVK hydroperoxides}
NACA       = 2C + 3H + 4O + N ; {@NACA}              {nitro-oxy acetaldehyde}
{ mz_rs_20100227-}
{***** END:   gas-phase species from gas.spc *****}
{**** START: aerosol species (phase 1) from aqueous.spc ****}
{-----------------------------------------------------------------------------}
{------------------------------ aerosol mode: 01# -----------------------------}
{-----------------------------------------------------------------------------}

{------------------------------- neutral species -----------------------------}

{------------------------------------- O -------------------------------------}

 O2_a01         = IGNORE; {@O_2\aq}          {oxygen}
 O3_a01         = IGNORE; {@O_3\aq}          {ozone}

{------------------------------------- H -------------------------------------}

 OH_a01         = IGNORE; {@OH\aq}           {hydroxyl radical}
 HO2_a01        = IGNORE; {@HO_2\aq}         {perhydroxyl radical}
 H2O_a01        = IGNORE; {@H_2O\aq}         {water}
 H2O2_a01       = IGNORE; {@H_2O_2\aq}       {hydrogen peroxide}

{------------------------------------- N -------------------------------------}

 NH3_a01        = IGNORE; {@NH_3\aq}         {ammonia}
 NO_a01         = IGNORE; {@NO\aq}           {nitric oxide}
 NO2_a01        = IGNORE; {@NO_2\aq}         {nitrogen dioxide}
 NO3_a01        = IGNORE; {@NO_3\aq}         {nitrogen trioxide}
 HONO_a01       = IGNORE; {@HONO\aq}         {nitrous acid}
 HNO3_a01       = IGNORE; {@HNO_3\aq}        {nitric acid}
 HNO4_a01       = IGNORE; {@HNO_4\aq}        {pernitric acid}
 N2O5_a01       = IGNORE; {@N_2O_5\aq}       {dinitrogen pentoxide}

{------------------------------------- C -------------------------------------}

{1C}
 CH3OH_a01      = IGNORE; {@CH_3OH\aq}       {methanol}
 HCOOH_a01      = IGNORE; {@HCOOH\aq}        {formic acid}
 HCHO_a01       = IGNORE; {@HCHO\aq}         {methanal (formaldehyde)}
 CH3O2_a01      = IGNORE; {@CH_3OO\aq}       {methylperoxy radical}
 CH3OOH_a01     = IGNORE; {@CH_3OOH\aq}      {}
 CO2_a01        = IGNORE; {@CO_2\aq}         {carbon dioxide}

{2C}
 CH3CO2H_a01    = IGNORE; {@CH_3COOH\aq}     {acetic acid}
 PAN_a01        = IGNORE; {@PAN\aq}          {peroxyacetylnitrate}
 C2H5O2_a01     = IGNORE; {@C_2H_5O_2\aq}    {ethylperoxy radical}
 CH3CHO_a01     = IGNORE; {@CH_3CHO\aq}      {acetaldehyde}

{3C}
 CH3COCH3_a01   = IGNORE; {@CH_3COCH_3\aq}   {acetone}

{------------------------------------- Cl ------------------------------------}

 Cl_a01         = IGNORE; {@Cl\aq}           {chlorine atom}
 Cl2_a01        = IGNORE; {@Cl_2\aq}         {molecular chlorine}
 HCl_a01        = IGNORE; {@HCl\aq}          {hydrogen chloride}
 HOCl_a01       = IGNORE; {@HOCl\aq}         {hypochlorous acid}

{------------------------------------- Br ------------------------------------}

 Br_a01         = IGNORE; {@Br\aq}           {bromine atom}
 Br2_a01        = IGNORE; {@Br_2\aq}         {molecular bromine}
 HBr_a01        = IGNORE; {@HBr\aq}          {hydrogen bromide}
 HOBr_a01       = IGNORE; {@HOBr\aq}         {hypobromous acid}
 BrCl_a01       = IGNORE; {@BrCl\aq}         {bromine chloride}

{------------------------------------- I -------------------------------------}

 I2_a01         = IGNORE; {@I_2\aq}          {molecular iodine}
 IO_a01         = IGNORE; {@IO\aq}           {iodine monoxide radical}
 HI_a01         = IGNORE; {@HI\aq}           {hydrogen iodide}
 HOI_a01        = IGNORE; {@HOI\aq}          {hypoiodous acid}
 ICl_a01        = IGNORE; {@ICl\aq}          {iodine chloride}
 IBr_a01        = IGNORE; {@IBr\aq}          {iodine bromide}
 HIO3_a01       = IGNORE; {@HIO_3\aq}        {iodic acid}

{------------------------------------- S -------------------------------------}

 SO2_a01        = IGNORE; {@SO_2\aq}         {sulfur dioxide}
 H2SO4_a01      = IGNORE; {@H_2SO_4\aq}      {sulfuric acid}
 DMS_a01        = IGNORE; {@DMS\aq}          {dimethyl sulfide: CH3SCH3}
 DMSO_a01       = IGNORE; {@DMSO\aq}         {dimethyl sulfoxide: CH3SOCH3}

{------------------------------------- Hg ------------------------------------}

 Hg_a01         = IGNORE; {@Hg\aq}           {mercury}
 HgO_a01        = IGNORE; {@HgO\aq}          {} 
 HgOH_a01       = IGNORE; {@HgOH\aq}         {}
 HgOHOH_a01     = IGNORE; {@Hg(OH)_2\aq}     {}
 HgOHCl_a01     = IGNORE; {@Hg(OH)Cl\aq}     {}
 HgCl2_a01      = IGNORE; {@HgCl_2\aq}       {}
 HgBr2_a01      = IGNORE; {@HgBr_2\aq}       {}
 HgSO3_a01      = IGNORE; {@HgSO_3\aq}       {}
 ClHgBr_a01     = IGNORE; {@ClHgBr\aq}       {}
 BrHgOBr_a01    = IGNORE; {@BrHgOBr\aq}      {}
 ClHgOBr_a01    = IGNORE; {@ClHgOBr\aq}      {}

{----------------------------------- ions ------------------------------------}

{------------------------------------- O -------------------------------------}

 O2m_a01        = IGNORE; {@O_2^-\aq}        {}
 OHm_a01        = IGNORE; {@OH^-\aq}         {}

{------------------------------------- H -------------------------------------}

 Hp_a01         = IGNORE; {@H^+\aq}          {}

{------------------------------------- N -------------------------------------}

 NH4p_a01       = IGNORE; {@NH_4^+\aq}       {ammonium}
 NO2m_a01       = IGNORE; {@NO_2^-\aq}       {nitrite}
 NO3m_a01       = IGNORE; {@NO_3^-\aq}       {nitrate}
 NO4m_a01       = IGNORE; {@NO_4^-\aq}       {peroxy nitrate}

{------------------------------------- C -------------------------------------}

{1C}
 CO3m_a01       = IGNORE; {@CO_3^-\aq}       {}
 HCOOm_a01      = IGNORE; {@HCOO^-\aq}       {formate}
 HCO3m_a01      = IGNORE; {@HCO_3^-\aq}      {hydrogen carbonate}

{2C}
 CH3COOm_a01    = IGNORE; {@CH_3COO^-\aq}    {acetate}

{------------------------------------- Cl ------------------------------------}

 Clm_a01        = IGNORE; {@Cl^-\aq}         {chloride}
 Cl2m_a01       = IGNORE; {@Cl_2^-\aq}       {}
 ClOm_a01       = IGNORE; {@ClO^-\aq}        {}
 ClOHm_a01      = IGNORE; {@ClOH^-\aq}       {}

{------------------------------------- Br ------------------------------------}

 Brm_a01        = IGNORE; {@Br^-\aq}         {bromide}
 Br2m_a01       = IGNORE; {@Br_2^-\aq}       {}
 BrOm_a01       = IGNORE; {@BrO^-\aq}        {}
 BrOHm_a01      = IGNORE; {@BrOH^-\aq}       {}
 BrCl2m_a01     = IGNORE; {@BrCl_2^-\aq}     {}
 Br2Clm_a01     = IGNORE; {@Br_2Cl^-\aq}     {}

{------------------------------------- I -------------------------------------}

 Im_a01         = IGNORE; {@I^-\aq}          {iodide}
 IO2m_a01       = IGNORE; {@IO_2^-\aq}       {}
 IO3m_a01       = IGNORE; {@IO_3^-\aq}       {iodate}
 ICl2m_a01      = IGNORE; {@ICl_2^-\aq}      {}
 IClBrm_a01     = IGNORE; {@IClBr^-\aq}      {}
 IBr2m_a01      = IGNORE; {@IBr_2^-\aq}      {}

{------------------------------------- S -------------------------------------}

 SO3m_a01       = IGNORE; {@SO_3^-\aq}       {}
 SO3mm_a01      = IGNORE; {@SO_3^<2->\aq}    {sulfite}
 SO4m_a01       = IGNORE; {@SO_4^-\aq}       {}
 SO4mm_a01      = IGNORE; {@SO_4^<2->\aq}    {sulfate}
 SO5m_a01       = IGNORE; {@SO_5^-\aq}       {}
 HSO3m_a01      = IGNORE; {@HSO_3^-\aq}      {hydrogen sulfite}
 HSO4m_a01      = IGNORE; {@HSO_4^-\aq}      {hydrogen sulfate}
 HSO5m_a01      = IGNORE; {@HSO_5^-\aq}      {}
 CH3SO3m_a01    = IGNORE; {@CH_3SO_3^-\aq}   {MSA anion}
 CH2OHSO3m_a01  = IGNORE; {@CH_2OHSO_3^-\aq} {}

{------------------------------------Hg---------------------------------------}

 Hgp_a01        = IGNORE; {@Hg^+\aq}              {}
 Hgpp_a01       = IGNORE; {@Hg^<2+>\aq}           {}
 HgOHp_a01      = IGNORE; {@HgOH^+\aq}            {}
 HgClp_a01      = IGNORE; {@HgCl^+\aq}            {}
 HgCl3m_a01     = IGNORE; {@HgCl_3^-\aq}          {}
 HgCl4mm_a01    = IGNORE; {@HgCl_4^<2->\aq}       {}
 HgBrp_a01      = IGNORE; {@HgBr^+\aq}            {}
 HgBr3m_a01     = IGNORE; {@HgBr_3^-\aq}          {}
 HgBr4mm_a01    = IGNORE; {@HgBr_4^<2->\aq}       {}
 HgSO32mm_a01   = IGNORE; {@Hg(SO_3)_2^<2->\aq}   {}

{-----------------------------------------------------------------------------}
{------------------------------------ dummies --------------------------------}
{-----------------------------------------------------------------------------}

 D1O_a01        = IGNORE; {@D_1O\aq}         {}
 D2O_a01        = IGNORE; {@D_2O\aq}         {}
 DAHp_a01       = IGNORE; {@DAH^+\aq}        {}
 DA_a01         = IGNORE; {@DA\aq}           {}
 DAm_a01        = IGNORE; {@DA^-\aq}         {}
 DGtAi_a01      = IGNORE; {@DGtAi\aq}        {}
 DGtAs_a01      = IGNORE; {@DGtAs\aq}        {}
 PROD1_a01      = IGNORE; {@PROD1\aq}        {}
 PROD2_a01      = IGNORE; {@PROD2\aq}        {}
 Nap_a01        = IGNORE; {@Na^+\aq}         {dummy cation}
{**** END:   aerosol species (phase 1) from aqueous.spc ****}
{SETFIX H2O_a* is done via xmecca}
#DEFVAR
{**** START: accumulated reaction rates ****}
RRG1000 = IGNORE ; {@RRG1000} {diagnostic tracer}
RRG1001 = IGNORE ; {@RRG1001} {diagnostic tracer}
RRG2100 = IGNORE ; {@RRG2100} {diagnostic tracer}
RRG2104 = IGNORE ; {@RRG2104} {diagnostic tracer}
RRG2105 = IGNORE ; {@RRG2105} {diagnostic tracer}
RRG2107 = IGNORE ; {@RRG2107} {diagnostic tracer}
RRG2109 = IGNORE ; {@RRG2109} {diagnostic tracer}
RRG2110 = IGNORE ; {@RRG2110} {diagnostic tracer}
RRG2111 = IGNORE ; {@RRG2111} {diagnostic tracer}
RRG2112 = IGNORE ; {@RRG2112} {diagnostic tracer}
RRG3101 = IGNORE ; {@RRG3101} {diagnostic tracer}
RRG3103 = IGNORE ; {@RRG3103} {diagnostic tracer}
RRG3106 = IGNORE ; {@RRG3106} {diagnostic tracer}
RRG3108 = IGNORE ; {@RRG3108} {diagnostic tracer}
RRG3109 = IGNORE ; {@RRG3109} {diagnostic tracer}
RRG3110 = IGNORE ; {@RRG3110} {diagnostic tracer}
RRG3200 = IGNORE ; {@RRG3200} {diagnostic tracer}
RRG3201 = IGNORE ; {@RRG3201} {diagnostic tracer}
RRG3202 = IGNORE ; {@RRG3202} {diagnostic tracer}
RRG3203 = IGNORE ; {@RRG3203} {diagnostic tracer}
RRG3204 = IGNORE ; {@RRG3204} {diagnostic tracer}
RRG3205 = IGNORE ; {@RRG3205} {diagnostic tracer}
RRG3206 = IGNORE ; {@RRG3206} {diagnostic tracer}
RRG3207 = IGNORE ; {@RRG3207} {diagnostic tracer}
RRG3208 = IGNORE ; {@RRG3208} {diagnostic tracer}
RRG3209 = IGNORE ; {@RRG3209} {diagnostic tracer}
RRG3210 = IGNORE ; {@RRG3210} {diagnostic tracer}
RRG3211 = IGNORE ; {@RRG3211} {diagnostic tracer}
RRG3212 = IGNORE ; {@RRG3212} {diagnostic tracer}
RRG3213 = IGNORE ; {@RRG3213} {diagnostic tracer}
RRG3214 = IGNORE ; {@RRG3214} {diagnostic tracer}
RRG3215 = IGNORE ; {@RRG3215} {diagnostic tracer}
RRG3216 = IGNORE ; {@RRG3216} {diagnostic tracer}
RRG3217 = IGNORE ; {@RRG3217} {diagnostic tracer}
RRG3218 = IGNORE ; {@RRG3218} {diagnostic tracer}
RRG3219 = IGNORE ; {@RRG3219} {diagnostic tracer}
RRG3220 = IGNORE ; {@RRG3220} {diagnostic tracer}
RRG3221 = IGNORE ; {@RRG3221} {diagnostic tracer}
RRG3222 = IGNORE ; {@RRG3222} {diagnostic tracer}
RRG3223 = IGNORE ; {@RRG3223} {diagnostic tracer}
RRG3224 = IGNORE ; {@RRG3224} {diagnostic tracer}
RRG4101 = IGNORE ; {@RRG4101} {diagnostic tracer}
RRG4102 = IGNORE ; {@RRG4102} {diagnostic tracer}
RRG4103 = IGNORE ; {@RRG4103} {diagnostic tracer}
RRG4104 = IGNORE ; {@RRG4104} {diagnostic tracer}
RRG4105 = IGNORE ; {@RRG4105} {diagnostic tracer}
RRG4106a = IGNORE ; {@RRG4106a} {diagnostic tracer}
RRG4106b = IGNORE ; {@RRG4106b} {diagnostic tracer}
RRG4107 = IGNORE ; {@RRG4107} {diagnostic tracer}
RRG4108 = IGNORE ; {@RRG4108} {diagnostic tracer}
RRG4109 = IGNORE ; {@RRG4109} {diagnostic tracer}
RRG4110 = IGNORE ; {@RRG4110} {diagnostic tracer}
RRG4111 = IGNORE ; {@RRG4111} {diagnostic tracer}
RRG4200 = IGNORE ; {@RRG4200} {diagnostic tracer}
RRG4201 = IGNORE ; {@RRG4201} {diagnostic tracer}
RRG4202 = IGNORE ; {@RRG4202} {diagnostic tracer}
RRG4203 = IGNORE ; {@RRG4203} {diagnostic tracer}
RRG4204 = IGNORE ; {@RRG4204} {diagnostic tracer}
RRG4205 = IGNORE ; {@RRG4205} {diagnostic tracer}
RRG4206 = IGNORE ; {@RRG4206} {diagnostic tracer}
RRG4207 = IGNORE ; {@RRG4207} {diagnostic tracer}
RRG4208 = IGNORE ; {@RRG4208} {diagnostic tracer}
RRG4209 = IGNORE ; {@RRG4209} {diagnostic tracer}
RRG4210 = IGNORE ; {@RRG4210} {diagnostic tracer}
RRG4211a = IGNORE ; {@RRG4211a} {diagnostic tracer}
RRG4211b = IGNORE ; {@RRG4211b} {diagnostic tracer}
RRG4212 = IGNORE ; {@RRG4212} {diagnostic tracer}
RRG4213 = IGNORE ; {@RRG4213} {diagnostic tracer}
RRG4214 = IGNORE ; {@RRG4214} {diagnostic tracer}
RRG4217 = IGNORE ; {@RRG4217} {diagnostic tracer}
RRG4218 = IGNORE ; {@RRG4218} {diagnostic tracer}
RRG4220 = IGNORE ; {@RRG4220} {diagnostic tracer}
RRG4221 = IGNORE ; {@RRG4221} {diagnostic tracer}
RRG4222 = IGNORE ; {@RRG4222} {diagnostic tracer}
RRG4223 = IGNORE ; {@RRG4223} {diagnostic tracer}
RRG4224 = IGNORE ; {@RRG4224} {diagnostic tracer}
RRG4225 = IGNORE ; {@RRG4225} {diagnostic tracer}
RRG4226 = IGNORE ; {@RRG4226} {diagnostic tracer}
RRG4227 = IGNORE ; {@RRG4227} {diagnostic tracer}
RRG4228 = IGNORE ; {@RRG4228} {diagnostic tracer}
RRG4229 = IGNORE ; {@RRG4229} {diagnostic tracer}
RRG4230 = IGNORE ; {@RRG4230} {diagnostic tracer}
RRG4231 = IGNORE ; {@RRG4231} {diagnostic tracer}
RRG4232 = IGNORE ; {@RRG4232} {diagnostic tracer}
RRG4233 = IGNORE ; {@RRG4233} {diagnostic tracer}
RRG4234 = IGNORE ; {@RRG4234} {diagnostic tracer}
RRG4235 = IGNORE ; {@RRG4235} {diagnostic tracer}
RRG4236 = IGNORE ; {@RRG4236} {diagnostic tracer}
RRG4237 = IGNORE ; {@RRG4237} {diagnostic tracer}
RRG4238 = IGNORE ; {@RRG4238} {diagnostic tracer}
RRG4239 = IGNORE ; {@RRG4239} {diagnostic tracer}
RRG4240 = IGNORE ; {@RRG4240} {diagnostic tracer}
RRG4241 = IGNORE ; {@RRG4241} {diagnostic tracer}
RRG4242 = IGNORE ; {@RRG4242} {diagnostic tracer}
RRG4243 = IGNORE ; {@RRG4243} {diagnostic tracer}
RRG4244 = IGNORE ; {@RRG4244} {diagnostic tracer}
RRG4245 = IGNORE ; {@RRG4245} {diagnostic tracer}
RRG4246a = IGNORE ; {@RRG4246a} {diagnostic tracer}
RRG4246b = IGNORE ; {@RRG4246b} {diagnostic tracer}
RRG4247a = IGNORE ; {@RRG4247a} {diagnostic tracer}
RRG4247b = IGNORE ; {@RRG4247b} {diagnostic tracer}
RRG4248 = IGNORE ; {@RRG4248} {diagnostic tracer}
RRG4300 = IGNORE ; {@RRG4300} {diagnostic tracer}
RRG4301 = IGNORE ; {@RRG4301} {diagnostic tracer}
RRG4302 = IGNORE ; {@RRG4302} {diagnostic tracer}
RRG4303 = IGNORE ; {@RRG4303} {diagnostic tracer}
RRG4304 = IGNORE ; {@RRG4304} {diagnostic tracer}
RRG4305 = IGNORE ; {@RRG4305} {diagnostic tracer}
RRG4306 = IGNORE ; {@RRG4306} {diagnostic tracer}
RRG4307 = IGNORE ; {@RRG4307} {diagnostic tracer}
RRG4311 = IGNORE ; {@RRG4311} {diagnostic tracer}
RRG4312 = IGNORE ; {@RRG4312} {diagnostic tracer}
RRG4313 = IGNORE ; {@RRG4313} {diagnostic tracer}
RRG4314 = IGNORE ; {@RRG4314} {diagnostic tracer}
RRG4315a = IGNORE ; {@RRG4315a} {diagnostic tracer}
RRG4315b = IGNORE ; {@RRG4315b} {diagnostic tracer}
RRG4316 = IGNORE ; {@RRG4316} {diagnostic tracer}
RRG4317 = IGNORE ; {@RRG4317} {diagnostic tracer}
RRG4320 = IGNORE ; {@RRG4320} {diagnostic tracer}
RRG4321 = IGNORE ; {@RRG4321} {diagnostic tracer}
RRG4322 = IGNORE ; {@RRG4322} {diagnostic tracer}
RRG4323 = IGNORE ; {@RRG4323} {diagnostic tracer}
RRG4324 = IGNORE ; {@RRG4324} {diagnostic tracer}
RRG4325 = IGNORE ; {@RRG4325} {diagnostic tracer}
RRG4326a = IGNORE ; {@RRG4326a} {diagnostic tracer}
RRG4326b = IGNORE ; {@RRG4326b} {diagnostic tracer}
RRG4327 = IGNORE ; {@RRG4327} {diagnostic tracer}
RRG4328 = IGNORE ; {@RRG4328} {diagnostic tracer}
RRG4329 = IGNORE ; {@RRG4329} {diagnostic tracer}
RRG4330a = IGNORE ; {@RRG4330a} {diagnostic tracer}
RRG4330b = IGNORE ; {@RRG4330b} {diagnostic tracer}
RRG4331 = IGNORE ; {@RRG4331} {diagnostic tracer}
RRG4332 = IGNORE ; {@RRG4332} {diagnostic tracer}
RRG4333 = IGNORE ; {@RRG4333} {diagnostic tracer}
RRG4334 = IGNORE ; {@RRG4334} {diagnostic tracer}
RRG4335 = IGNORE ; {@RRG4335} {diagnostic tracer}
RRG4400 = IGNORE ; {@RRG4400} {diagnostic tracer}
RRG4401 = IGNORE ; {@RRG4401} {diagnostic tracer}
RRG4402 = IGNORE ; {@RRG4402} {diagnostic tracer}
RRG4403 = IGNORE ; {@RRG4403} {diagnostic tracer}
RRG4404 = IGNORE ; {@RRG4404} {diagnostic tracer}
RRG4405 = IGNORE ; {@RRG4405} {diagnostic tracer}
RRG4406 = IGNORE ; {@RRG4406} {diagnostic tracer}
RRG4413 = IGNORE ; {@RRG4413} {diagnostic tracer}
RRG4414 = IGNORE ; {@RRG4414} {diagnostic tracer}
RRG4415 = IGNORE ; {@RRG4415} {diagnostic tracer}
RRG4416 = IGNORE ; {@RRG4416} {diagnostic tracer}
RRG4417 = IGNORE ; {@RRG4417} {diagnostic tracer}
RRG4418 = IGNORE ; {@RRG4418} {diagnostic tracer}
RRG4419 = IGNORE ; {@RRG4419} {diagnostic tracer}
RRG4420 = IGNORE ; {@RRG4420} {diagnostic tracer}
RRG4421 = IGNORE ; {@RRG4421} {diagnostic tracer}
RRG4422 = IGNORE ; {@RRG4422} {diagnostic tracer}
RRG4423 = IGNORE ; {@RRG4423} {diagnostic tracer}
RRG4424 = IGNORE ; {@RRG4424} {diagnostic tracer}
RRG4425 = IGNORE ; {@RRG4425} {diagnostic tracer}
RRG4426 = IGNORE ; {@RRG4426} {diagnostic tracer}
RRG4427 = IGNORE ; {@RRG4427} {diagnostic tracer}
RRG4428 = IGNORE ; {@RRG4428} {diagnostic tracer}
RRG4429 = IGNORE ; {@RRG4429} {diagnostic tracer}
RRG4430 = IGNORE ; {@RRG4430} {diagnostic tracer}
RRG4431 = IGNORE ; {@RRG4431} {diagnostic tracer}
RRG4432 = IGNORE ; {@RRG4432} {diagnostic tracer}
RRG4433 = IGNORE ; {@RRG4433} {diagnostic tracer}
RRG4434 = IGNORE ; {@RRG4434} {diagnostic tracer}
RRG4435 = IGNORE ; {@RRG4435} {diagnostic tracer}
RRG4436 = IGNORE ; {@RRG4436} {diagnostic tracer}
RRG4437 = IGNORE ; {@RRG4437} {diagnostic tracer}
RRG4438 = IGNORE ; {@RRG4438} {diagnostic tracer}
RRG4439 = IGNORE ; {@RRG4439} {diagnostic tracer}
RRG4440 = IGNORE ; {@RRG4440} {diagnostic tracer}
RRG4441 = IGNORE ; {@RRG4441} {diagnostic tracer}
RRG4442 = IGNORE ; {@RRG4442} {diagnostic tracer}
RRG4443 = IGNORE ; {@RRG4443} {diagnostic tracer}
RRG4444 = IGNORE ; {@RRG4444} {diagnostic tracer}
RRG4445 = IGNORE ; {@RRG4445} {diagnostic tracer}
RRG4446 = IGNORE ; {@RRG4446} {diagnostic tracer}
RRG4447 = IGNORE ; {@RRG4447} {diagnostic tracer}
RRG4448 = IGNORE ; {@RRG4448} {diagnostic tracer}
RRG4449 = IGNORE ; {@RRG4449} {diagnostic tracer}
RRG4450 = IGNORE ; {@RRG4450} {diagnostic tracer}
RRG4451 = IGNORE ; {@RRG4451} {diagnostic tracer}
RRG4452 = IGNORE ; {@RRG4452} {diagnostic tracer}
RRG4453 = IGNORE ; {@RRG4453} {diagnostic tracer}
RRG4454 = IGNORE ; {@RRG4454} {diagnostic tracer}
RRG4455 = IGNORE ; {@RRG4455} {diagnostic tracer}
RRG4456 = IGNORE ; {@RRG4456} {diagnostic tracer}
RRG4500 = IGNORE ; {@RRG4500} {diagnostic tracer}
RRG4501 = IGNORE ; {@RRG4501} {diagnostic tracer}
RRG4509 = IGNORE ; {@RRG4509} {diagnostic tracer}
RRG4510 = IGNORE ; {@RRG4510} {diagnostic tracer}
RRG4511 = IGNORE ; {@RRG4511} {diagnostic tracer}
RRG4512 = IGNORE ; {@RRG4512} {diagnostic tracer}
RRG4513 = IGNORE ; {@RRG4513} {diagnostic tracer}
RRG4514 = IGNORE ; {@RRG4514} {diagnostic tracer}
RRG4515 = IGNORE ; {@RRG4515} {diagnostic tracer}
RRG4516 = IGNORE ; {@RRG4516} {diagnostic tracer}
RRG4517 = IGNORE ; {@RRG4517} {diagnostic tracer}
RRG4518 = IGNORE ; {@RRG4518} {diagnostic tracer}
RRG4519 = IGNORE ; {@RRG4519} {diagnostic tracer}
RRG4520 = IGNORE ; {@RRG4520} {diagnostic tracer}
RRG4521 = IGNORE ; {@RRG4521} {diagnostic tracer}
RRG4522 = IGNORE ; {@RRG4522} {diagnostic tracer}
RRG4523 = IGNORE ; {@RRG4523} {diagnostic tracer}
RRG4524 = IGNORE ; {@RRG4524} {diagnostic tracer}
RRG4525 = IGNORE ; {@RRG4525} {diagnostic tracer}
RRG4526 = IGNORE ; {@RRG4526} {diagnostic tracer}
RRG4527 = IGNORE ; {@RRG4527} {diagnostic tracer}
RRG4528 = IGNORE ; {@RRG4528} {diagnostic tracer}
RRG4529 = IGNORE ; {@RRG4529} {diagnostic tracer}
RRG4530 = IGNORE ; {@RRG4530} {diagnostic tracer}
RRG4531 = IGNORE ; {@RRG4531} {diagnostic tracer}
RRG4532 = IGNORE ; {@RRG4532} {diagnostic tracer}
RRG4533 = IGNORE ; {@RRG4533} {diagnostic tracer}
RRG4534 = IGNORE ; {@RRG4534} {diagnostic tracer}
RRG4535 = IGNORE ; {@RRG4535} {diagnostic tracer}
RRG4536 = IGNORE ; {@RRG4536} {diagnostic tracer}
RRG4537 = IGNORE ; {@RRG4537} {diagnostic tracer}
RRG4538 = IGNORE ; {@RRG4538} {diagnostic tracer}
RRG4539 = IGNORE ; {@RRG4539} {diagnostic tracer}
RRG4540 = IGNORE ; {@RRG4540} {diagnostic tracer}
RRG4541 = IGNORE ; {@RRG4541} {diagnostic tracer}
RRG4542 = IGNORE ; {@RRG4542} {diagnostic tracer}
RRG4543 = IGNORE ; {@RRG4543} {diagnostic tracer}
RRG4544 = IGNORE ; {@RRG4544} {diagnostic tracer}
RRG4545 = IGNORE ; {@RRG4545} {diagnostic tracer}
RRG4546 = IGNORE ; {@RRG4546} {diagnostic tracer}
RRG4547 = IGNORE ; {@RRG4547} {diagnostic tracer}
RRG4548 = IGNORE ; {@RRG4548} {diagnostic tracer}
RRG4549 = IGNORE ; {@RRG4549} {diagnostic tracer}
RRG4550 = IGNORE ; {@RRG4550} {diagnostic tracer}
RRG4551 = IGNORE ; {@RRG4551} {diagnostic tracer}
RRG4552 = IGNORE ; {@RRG4552} {diagnostic tracer}
RRG4553 = IGNORE ; {@RRG4553} {diagnostic tracer}
RRG4554 = IGNORE ; {@RRG4554} {diagnostic tracer}
RRG4555 = IGNORE ; {@RRG4555} {diagnostic tracer}
RRG4556 = IGNORE ; {@RRG4556} {diagnostic tracer}
RRG4557 = IGNORE ; {@RRG4557} {diagnostic tracer}
RRG4558 = IGNORE ; {@RRG4558} {diagnostic tracer}
RRG4559 = IGNORE ; {@RRG4559} {diagnostic tracer}
RRG4560 = IGNORE ; {@RRG4560} {diagnostic tracer}
RRG4561 = IGNORE ; {@RRG4561} {diagnostic tracer}
RRG4562 = IGNORE ; {@RRG4562} {diagnostic tracer}
RRG4563 = IGNORE ; {@RRG4563} {diagnostic tracer}
RRG4564 = IGNORE ; {@RRG4564} {diagnostic tracer}
RRG4565 = IGNORE ; {@RRG4565} {diagnostic tracer}
RRJ1000 = IGNORE ; {@RRJ1000} {diagnostic tracer}
RRJ1001a = IGNORE ; {@RRJ1001a} {diagnostic tracer}
RRJ1001b = IGNORE ; {@RRJ1001b} {diagnostic tracer}
RRJ2101 = IGNORE ; {@RRJ2101} {diagnostic tracer}
RRJ3101 = IGNORE ; {@RRJ3101} {diagnostic tracer}
RRJ3103a = IGNORE ; {@RRJ3103a} {diagnostic tracer}
RRJ3103b = IGNORE ; {@RRJ3103b} {diagnostic tracer}
RRJ3104a = IGNORE ; {@RRJ3104a} {diagnostic tracer}
RRJ3200 = IGNORE ; {@RRJ3200} {diagnostic tracer}
RRJ3201 = IGNORE ; {@RRJ3201} {diagnostic tracer}
RRJ3202 = IGNORE ; {@RRJ3202} {diagnostic tracer}
RRJ4100 = IGNORE ; {@RRJ4100} {diagnostic tracer}
RRJ4101a = IGNORE ; {@RRJ4101a} {diagnostic tracer}
RRJ4101b = IGNORE ; {@RRJ4101b} {diagnostic tracer}
RRJ4200 = IGNORE ; {@RRJ4200} {diagnostic tracer}
RRJ4201 = IGNORE ; {@RRJ4201} {diagnostic tracer}
RRJ4202 = IGNORE ; {@RRJ4202} {diagnostic tracer}
RRJ4204 = IGNORE ; {@RRJ4204} {diagnostic tracer}
RRJ4205 = IGNORE ; {@RRJ4205} {diagnostic tracer}
RRJ4206 = IGNORE ; {@RRJ4206} {diagnostic tracer}
RRJ4207 = IGNORE ; {@RRJ4207} {diagnostic tracer}
RRJ4208 = IGNORE ; {@RRJ4208} {diagnostic tracer}
RRJ4209 = IGNORE ; {@RRJ4209} {diagnostic tracer}
RRJ4210 = IGNORE ; {@RRJ4210} {diagnostic tracer}
RRJ4211 = IGNORE ; {@RRJ4211} {diagnostic tracer}
RRJ4212 = IGNORE ; {@RRJ4212} {diagnostic tracer}
RRJ4300 = IGNORE ; {@RRJ4300} {diagnostic tracer}
RRJ4301 = IGNORE ; {@RRJ4301} {diagnostic tracer}
RRJ4302 = IGNORE ; {@RRJ4302} {diagnostic tracer}
RRJ4303 = IGNORE ; {@RRJ4303} {diagnostic tracer}
RRJ4304 = IGNORE ; {@RRJ4304} {diagnostic tracer}
RRJ4306 = IGNORE ; {@RRJ4306} {diagnostic tracer}
RRJ4307 = IGNORE ; {@RRJ4307} {diagnostic tracer}
RRJ4308 = IGNORE ; {@RRJ4308} {diagnostic tracer}
RRJ4309 = IGNORE ; {@RRJ4309} {diagnostic tracer}
RRJ4310 = IGNORE ; {@RRJ4310} {diagnostic tracer}
RRJ4311 = IGNORE ; {@RRJ4311} {diagnostic tracer}
RRJ4400 = IGNORE ; {@RRJ4400} {diagnostic tracer}
RRJ4401 = IGNORE ; {@RRJ4401} {diagnostic tracer}
RRJ4403 = IGNORE ; {@RRJ4403} {diagnostic tracer}
RRJ4404 = IGNORE ; {@RRJ4404} {diagnostic tracer}
RRJ4405 = IGNORE ; {@RRJ4405} {diagnostic tracer}
RRJ4406 = IGNORE ; {@RRJ4406} {diagnostic tracer}
RRJ4407 = IGNORE ; {@RRJ4407} {diagnostic tracer}
RRJ4408 = IGNORE ; {@RRJ4408} {diagnostic tracer}
RRJ4409 = IGNORE ; {@RRJ4409} {diagnostic tracer}
RRJ4410 = IGNORE ; {@RRJ4410} {diagnostic tracer}
RRJ4411 = IGNORE ; {@RRJ4411} {diagnostic tracer}
RRJ4412 = IGNORE ; {@RRJ4412} {diagnostic tracer}
RRJ4413 = IGNORE ; {@RRJ4413} {diagnostic tracer}
RRJ4414 = IGNORE ; {@RRJ4414} {diagnostic tracer}
RRJ4415 = IGNORE ; {@RRJ4415} {diagnostic tracer}
RRJ4416 = IGNORE ; {@RRJ4416} {diagnostic tracer}
RRJ4417 = IGNORE ; {@RRJ4417} {diagnostic tracer}
RRJ4418 = IGNORE ; {@RRJ4418} {diagnostic tracer}
RRJ4419 = IGNORE ; {@RRJ4419} {diagnostic tracer}
RRJ4420 = IGNORE ; {@RRJ4420} {diagnostic tracer}
RRJ4502 = IGNORE ; {@RRJ4502} {diagnostic tracer}
RRJ4503 = IGNORE ; {@RRJ4503} {diagnostic tracer}
RRJ4504 = IGNORE ; {@RRJ4504} {diagnostic tracer}
RRJ4505 = IGNORE ; {@RRJ4505} {diagnostic tracer}
RRJ4506 = IGNORE ; {@RRJ4506} {diagnostic tracer}
RRJ4507 = IGNORE ; {@RRJ4507} {diagnostic tracer}
RRJ4508 = IGNORE ; {@RRJ4508} {diagnostic tracer}
RRJ4509 = IGNORE ; {@RRJ4509} {diagnostic tracer}
RRJ4510 = IGNORE ; {@RRJ4510} {diagnostic tracer}
RRJ4511 = IGNORE ; {@RRJ4511} {diagnostic tracer}
RRJ4512 = IGNORE ; {@RRJ4512} {diagnostic tracer}
RRJ4513 = IGNORE ; {@RRJ4513} {diagnostic tracer}
RRJ4514 = IGNORE ; {@RRJ4514} {diagnostic tracer}
RRJ4515 = IGNORE ; {@RRJ4515} {diagnostic tracer}
RRJ4516 = IGNORE ; {@RRJ4516} {diagnostic tracer}
{**** END: accumulated reaction rates ****}
