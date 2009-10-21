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

O1D           = O                ; {@O(^1D)}            {O singlet D}
O3P           = O                ; {@O(^3P)}            {O triplet P}
O2            = 2O               ; {@O_2}               {oxygen}
O3            = 3O               ; {@O_3}               {ozone}

{------------------------------------- H -------------------------------------}

H             = H                ; {@H}                 {hydrogen atom}
H2            = 2H               ; {@H_2}               {hydrogen}
OH            = O + H            ; {@OH}                {hydroxyl radical}
HO2           = H + 2O           ; {@HO_2}              {hydroperoxy radical}
H2O           = 2H + O           ; {@H_2O}              {water}
H2O2          = 2H + 2O          ; {@H_2O_2}            {hydrogen peroxide}

{------------------------------------- N -------------------------------------}

N             = N                ; {@N}                 {nitrogen atom}
N2            = 2N               ; {@N_2}               {nitrogen}
NH3           = N + 3H           ; {@NH_3}              {ammonia}
N2O           = 2N + O           ; {@N_2O}              {nitrous oxide}
NO            = N + O            ; {@NO}                {nitric oxide}
NO2           = N + 2O           ; {@NO_2}              {nitrogen dioxide}
NO3           = N + 3O           ; {@NO_3}              {nitrogen trioxide}
N2O5          = 2N + 5O          ; {@N_2O_5}            {dinitrogen pentoxide}
HONO          = H + N + 2O       ; {@HONO}              {nitrous acid}
HNO3          = H + N + 3O       ; {@HNO_3}             {nitric acid}
HNO4          = H + N + 4O       ; {@HNO_4}             {peroxynitric acid}
NH2           = 2H + N           ; {@NH_2}           
HNO           = H + N + O        ; {@HNO}           
NHOH          = 2H + N + O       ; {@NHOH}          
NH2O          = 2H + N + O       ; {@NH_2O}          
NH2OH         = 3H + N + O       ; {@NH_2OH}  
{------------------------------------- C -------------------------------------}

{1C}
CH4           = C + 4H           ; {@CH_4}              {methane}
CH3OH         = C + 4H + O       ; {@CH_3OH}            {methanol}
CH3O2         = C + 3H + 2O      ; {@CH_3O_2}           {methyl peroxy radical}
CH3OOH        = C + 4H + 2O      ; {@CH_3OOH}           {methyl peroxide}
HCHO          = 2H + C + O       ; {@HCHO}              {methanal (formaldehyde)}
CO            = C + O            ; {@CO}                {carbon monoxide}
HCOOH         = C + 2H + 2O      ; {@HCOOH}             {formic acid}
CO2           = C + 2O           ; {@CO_2}              {carbon dioxide}

{2C}
C2H6          = 2C + 6H          ; {@C_2H_6}            {ethane}
C2H4          = 2C + 4H          ; {@C_2H_4}            {ethene}
C2H2          = 2C + 2H          ; {@C_2H_2}            {ethyne}
EtO2          = 2C + 5H + 2O     ; {@C_2H_5O_2}         {ethylperoxy radical}
EtOOH         = 2C + 6H + 2O     ; {@C_2H_5OOH}         {ethyl hydroperoxide}
CH3CHO        = 2C + 4H + O      ; {@CH_3CHO}           {acetaldehyde}
CH3COOH       = 2C + 4H + 2O     ; {@CH_3COOH}          {acetic acid}
PA            = 2C + 3H + 3O     ; {@CH_3C(O)OO}        {peroxy acetyl radical}
PAA           = 2C + 4H + 3O     ; {@CH_3C(O)OOH}       {peroxy acetic acid}
NACA          = 2C + 3H + 4O + N ; {@NACA}              {nitro-oxy acetaldehyde}
PAN           = 2C + 3H + 5O + N ; {@PAN}               {peroxyacetylnitrate}

{3C}
C3H8          = 3C + 8H          ; {@C_3H_8}            {propane}
C3H6          = 3C + 6H          ; {@C_3H_6}            {propene}
PrO2          = 3C + 7H + 2O     ; {@C_3H_7O_2}         {peroxyradical von propane, secondary only}
PrOOH         = 3C + 8H + 2O     ; {@C_3H_7OOH}         {hydroperoxide from PrO2}
C3H6O2        = 3C + 7H + 3O     ; {@CH_3CH(O_2)CH_2OH} {hydroxyperoxyradical from propene+OH}
C3H6OOH       = 3C + 8H + 3O     ; {@CH_3CH(OOH)CH_2OH} {C3H6OHOOH = hydroxyhydroperoxides from C3H6}
CH3COCH3      = 3C + 6H + O      ; {@CH_3COCH_3}        {acetone}
ACETO2        = 3C + 5H + 3O     ; {@CH_3COCH_2O_2}     {peroxyradical from acetone}
ACETP         = 3C + 6H + 3O     ; {@CH_3COCH_2O_2H}    {hydroperoxide from ACETO2}
ACETOL        = 3C + 6H + 2O     ; {@CH_3COCH_2OH}      {HO-CH2-CO-CH3 = hydroxy acetone}
CH3COCHO      = 3C + 4H + 2O     ; {@CH_3COCHO}         {methylglyoxal}
PrONO2        = IGNORE + N       ; {@C_3H_7ONO_2}       {i-propyl nitrate}

{4C}
C4H10         = 4C + 10H         ; {@C_4H_<10>}         {n-butane, representative of higher alkanes}
C4H9O2        = 4C + 9H + 2O     ; {@C_4H_9O_2}         {peroxyradical from butane+OH}
C4H9OOH       = 4C + 10H + 2O    ; {@C_4H_9OOH}         {hydroperoxides from C4H10}
MVK           = 4C + 6H + O      ; {@MVK}               {CH3-CO-CH=CH2 = methyl vinyl ketone (+methacrolein)}
MVKO2         = 4C + 7H + 4O     ; {@MVKO2}             {MVK/MACR peroxy radicals}
MVKOOH        = 4C + 8H + 4O     ; {@MVKOOH}            {MVK hydroperoxides}
MEK           = 4C + 8H + O      ; {@CH_3COC_2H_5}      {methyl ethyl ketone (+all higher ketones)}
MEKO2         = 4C + 7H + 3O     ; {@MEKO2}             {peroxyradical from MEK; multiply oxygenated peroxy radicals}
MEKOOH        = 4C + 8H + 3O     ; {@MEKOOH}            {hydroperoxides from MEK}
MeCOCO        = 4C + 6H + 2O     ; {@MeCOCO}            {CH3-CO-CO-CH3, (+multiply oxigenated C>3 compounds)}
ONIT          = IGNORE + N       ; {@ONIT}              {organic nitrates from higher alkyl nitrates, +C3H6+NO3}
MPAN          = 4C + 5H + 5O + N ; {@MPAN}              {peroxymethacryloyl nitrate; peroxymethacrylic nitric anhydride}

{5C}
ISOP          = 5C + 8H          ; {@ISOP}              {isoprene}
ISO2          = 5C + 9H + 3O     ; {@ISO2}              {isoprene (hydroxy) peroxy radicals}
ISOOH         = 5C + 10H + 3O    ; {@ISOOH}             {isoprene (hydro)peroxides}
ISON          = IGNORE + N       ; {@ISON}              {organic nitrates from ISO2 and ISOP+NO3}

{------------------------------------- F -------------------------------------}

{------------------------------------- Cl ------------------------------------}

Cl            = Cl               ; {@Cl}                {chlorine atom}
Cl2           = 2Cl              ; {@Cl_2}              {chlorine}
ClO           = Cl + O           ; {@ClO}               {chlorine oxide}
HCl           = H + Cl           ; {@HCl}               {hydrochloric acid}
HOCl          = H + O + Cl       ; {@HOCl}              {hypochlorous acid}
Cl2O2         = 2Cl + 2O         ; {@Cl_2O_2}           {dichlorine dioxide}
OClO          = Cl + 2O          ; {@OClO}              {chlorine dioxide}
ClNO2         = Cl + 2O + N      ; {@ClNO_2}            {nitryl chloride}
ClNO3         = Cl + N + 3O      ; {@ClNO_3}            {chlorine nitrate}
CCl4          = C + 4Cl          ; {@CCl_4}             {tetrachloro methane}
CH3Cl         = C + 3H + Cl      ; {@CH_3Cl}            {chloromethane}
CH3CCl3       = 2C + 3H + 3Cl    ; {@CH_3CCl_3}         {1,1,1-trichloroethane = methyl chloroform = MCF}
CF2Cl2        = C + 2F + 2Cl     ; {@CF_2Cl_2}          {dichlorodifluoromethane = F12}
CFCl3         = C + F + 3Cl      ; {@CFCl_3}            {trichlorofluoromethane = F11}

{------------------------------------- Br ------------------------------------}

Br            = Br               ; {@Br}                {bromine atom}
Br2           = 2Br              ; {@Br_2}              {bromine}
BrO           = Br + O           ; {@BrO}               {bromine oxide}
HBr           = H + Br           ; {@HBr}               {hydrobromic acid}
HOBr          = H + O + Br       ; {@HOBr}              {hypobromous acid}
BrNO2         = Br + N + 2O      ; {@BrNO_2}            {nitryl bromide}
BrNO3         = Br + N + 3O      ; {@BrNO_3}            {bromine nitrate}
BrCl          = Br + Cl          ; {@BrCl}              {bromine chloride}
CH3Br         = Br + C +3H       ; {@CH_3Br}            {bromomethane}
CF3Br         = Br + 3F + C      ; {@CF_3Br}            {Halon 1301}
CF2ClBr       = Br + 2F + Cl + C ; {@CF_2ClBr}          {Halon 1211}
CHCl2Br       = C + H + 2Cl + Br ; {@CHCl_2Br}          {}
CHClBr2       = C + H + Cl + 2Br ; {@CHClBr_2}          {}
CH2ClBr       = C + 2H + Cl + Br ; {@CH_2ClBr}          {}
CH2Br2        = C + 2H + 2Br     ; {@CH_2Br_2}          {}
CHBr3         = C + H + 3Br      ; {@CHBr_3}            {}
{------------------------------------- I -------------------------------------}

I             = I                ; {@I}                 {iodine atomic ground state}
I2            = 2I               ; {@I_2}               {molecular iodine}
IO            = I + O            ; {@IO}                {iodine monoxide radical}    
OIO           = I + 2O           ; {@OIO}               {}
I2O2          = 2O + 2I          ; {@I_2O_2}            {}
HI            = H + I            ; {@HI}                {hydrogen iodide}
HOI           = H + O + I        ; {@HOI}               {hypoiodous acid}
HIO3          = H + I + 3O       ; {@HIO_3}             {}
INO2          = I + N + 2O       ; {@INO_2}             {iodine nitrite}
INO3          = I + N + 3O       ; {@INO_3}             {iodine nitrate}
CH3I          = C + 3H + I       ; {@CH_3I}             {iodomethane}
CH2I2         = C + 2H + 2I      ; {@CH_2I_2}           {diiodomethane}
C3H7I         = 3C + 7H + I      ; {@C_3H_7I}           {2-iodopropane} 
ICl           = I + Cl           ; {@ICl}               {iodine chloride}
CH2ClI        = C + 2H + Cl + I  ; {@CH_2ClI}           {chloroiodomethane}
IBr           = I + Br           ; {@IBr}               {iodine bromide}

{------------------------------------- S -------------------------------------}
S             = S                ; {@S}                 {sulfur atomic ground state}
SO            = S + O            ; {@SO}                {sulfur monoxide}
SO2           = S + 2O           ; {@SO_2}              {sulfur dioxide}
SH            = S + H            ; {@SH}                {}
H2SO4         = 2H + S + 4O      ; {@H_2SO_4}           {sulfuric acid}
CH3SO3H       = C + 4H + S + 3O  ; {@CH_3SO_3H}         {MSA: methane sulfonic acid}
DMS           = 2C + 6H + S      ; {@DMS}               {dimethyl sulfide}
DMSO          = 2C + 6H + S + O  ; {@DMSO}              {dimethyl sulfoxide: CH3SOCH3}
CH3SO2        = C + 3H + S + 2O  ; {@CH_3SO_2}          {}
CH3SO3        = C + 3H + S + 3O  ; {@CH_3SO_3}          {}
OCS           = C + S + O        ; {@OCS}               {}
SF6           = S + 6F           ; {@SF_6}              {sulfur hexaflouride}

{--------------------------------- Hg ----------------------------------------}

Hg            = Hg               ; {@Hg}                {}
HgO           = Hg + O           ; {@HgO}               {} 
HgCl          = Hg + Cl          ; {@HgCl}              {}  
HgCl2         = Hg + 2Cl         ; {@HgCl_2}            {}
HgBr          = Hg + Br          ; {@HgBr}              {}  
HgBr2         = Hg + 2Br         ; {@HgBr_2}            {}
ClHgBr        = Hg + Cl + Br     ; {@ClHgBr}            {}
BrHgOBr       = Hg + O + 2Br     ; {@BrHgOBr}           {}
ClHgOBr       = Hg + O + Cl + Br ; {@ClHgOBr}           {}

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
