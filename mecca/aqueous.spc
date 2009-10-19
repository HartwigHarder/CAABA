{-----------------------------------------------------------------------------}
{------------------------------ aerosol mode: ### -----------------------------}
{-----------------------------------------------------------------------------}

{------------------------------- neutral species -----------------------------}

{------------------------------------- O -------------------------------------}

 O2_a##         = IGNORE; {@O_2\aq}          {oxygen}
 O3_a##         = IGNORE; {@O_3\aq}          {ozone}

{------------------------------------- H -------------------------------------}

 OH_a##         = IGNORE; {@OH\aq}           {hydroxyl radical}
 HO2_a##        = IGNORE; {@HO_2\aq}         {perhydroxyl radical}
 H2O_a##        = IGNORE; {@H_2O\aq}         {water}
 H2O2_a##       = IGNORE; {@H_2O_2\aq}       {hydrogen peroxide}

{------------------------------------- N -------------------------------------}

 NH3_a##        = IGNORE; {@NH_3\aq}         {ammonia}
 NO_a##         = IGNORE; {@NO\aq}           {nitric oxide}
 NO2_a##        = IGNORE; {@NO_2\aq}         {nitrogen dioxide}
 NO3_a##        = IGNORE; {@NO_3\aq}         {nitrogen trioxide}
 HONO_a##       = IGNORE; {@HONO\aq}         {nitrous acid}
 HNO3_a##       = IGNORE; {@HNO_3\aq}        {nitric acid}
 HNO4_a##       = IGNORE; {@HNO_4\aq}        {pernitric acid}
 N2O5_a##       = IGNORE; {@N_2O_5\aq}       {dinitrogen pentoxide}

{------------------------------------- C -------------------------------------}

{1C}
 CH3OH_a##      = IGNORE; {@CH_3OH\aq}       {methanol}
 HCOOH_a##      = IGNORE; {@HCOOH\aq}        {formic acid}
 HCHO_a##       = IGNORE; {@HCHO\aq}         {methanal (formaldehyde)}
 CH3O2_a##      = IGNORE; {@CH_3OO\aq}       {methylperoxy radical}
 CH3OOH_a##     = IGNORE; {@CH_3OOH\aq}      {}
 CO2_a##        = IGNORE; {@CO_2\aq}         {carbon dioxide}

{2C}
 CH3COOH_a##    = IGNORE; {@CH_3COOH\aq}     {acetic acid}
 PAN_a##        = IGNORE; {@PAN\aq}          {peroxyacetylnitrate}
 EtO2_a##       = IGNORE; {@C_2H_5O_2\aq}    {ethylperoxy radical}
 CH3CHO_a##     = IGNORE; {@CH_3CHO\aq}      {acetaldehyde}

{3C}
 CH3COCH3_a##   = IGNORE; {@CH_3COCH_3\aq}   {acetone}

{------------------------------------- Cl ------------------------------------}

 Cl_a##         = IGNORE; {@Cl\aq}           {chlorine atom}
 Cl2_a##        = IGNORE; {@Cl_2\aq}         {molecular chlorine}
 HCl_a##        = IGNORE; {@HCl\aq}          {hydrogen chloride}
 HOCl_a##       = IGNORE; {@HOCl\aq}         {hypochlorous acid}

{------------------------------------- Br ------------------------------------}

 Br_a##         = IGNORE; {@Br\aq}           {bromine atom}
 Br2_a##        = IGNORE; {@Br_2\aq}         {molecular bromine}
 HBr_a##        = IGNORE; {@HBr\aq}          {hydrogen bromide}
 HOBr_a##       = IGNORE; {@HOBr\aq}         {hypobromous acid}
 BrCl_a##       = IGNORE; {@BrCl\aq}         {bromine chloride}

{------------------------------------- I -------------------------------------}

 I2_a##         = IGNORE; {@I_2\aq}          {molecular iodine}
 IO_a##         = IGNORE; {@IO\aq}           {iodine monoxide radical}
 HI_a##         = IGNORE; {@HI\aq}           {hydrogen iodide}
 HOI_a##        = IGNORE; {@HOI\aq}          {hypoiodous acid}
 ICl_a##        = IGNORE; {@ICl\aq}          {iodine chloride}
 IBr_a##        = IGNORE; {@IBr\aq}          {iodine bromide}
 HIO3_a##       = IGNORE; {@HIO_3\aq}        {iodic acid}

{------------------------------------- S -------------------------------------}

 SO2_a##        = IGNORE; {@SO_2\aq}         {sulfur dioxide}
 H2SO4_a##      = IGNORE; {@H_2SO_4\aq}      {sulfuric acid}
 DMSO_a##       = IGNORE; {@DMSO\aq}         {dimethyl sulfoxide: CH3SOCH3}

{------------------------------------- Hg ------------------------------------}

 Hg_a##         = IGNORE; {@Hg\aq}           {mercury}
 HgO_a##        = IGNORE; {@HgO\aq}          {} 
 HgOH_a##       = IGNORE; {@HgOH\aq}         {}
 HgOHOH_a##     = IGNORE; {@Hg(OH)_2\aq}     {}
 HgOHCl_a##     = IGNORE; {@Hg(OH)Cl\aq}     {}
 HgCl2_a##      = IGNORE; {@HgCl_2\aq}       {}
 HgBr2_a##      = IGNORE; {@HgBr_2\aq}       {}
 HgSO3_a##      = IGNORE; {@HgSO_3\aq}       {}
 ClHgBr_a##     = IGNORE; {@ClHgBr\aq}       {}
 BrHgOBr_a##    = IGNORE; {@BrHgOBr\aq}      {}
 ClHgOBr_a##    = IGNORE; {@ClHgOBr\aq}      {}

{----------------------------------- ions ------------------------------------}

{------------------------------------- O -------------------------------------}

 O2m_a##        = IGNORE; {@O_2^-\aq}        {}
 OHm_a##        = IGNORE; {@OH^-\aq}         {}

{------------------------------------- H -------------------------------------}

 Hp_a##         = IGNORE; {@H^+\aq}          {}

{------------------------------------- N -------------------------------------}

 NH4p_a##       = IGNORE; {@NH_4^+\aq}       {ammonium}
 NO2m_a##       = IGNORE; {@NO_2^-\aq}       {nitrite}
 NO3m_a##       = IGNORE; {@NO_3^-\aq}       {nitrate}
 NO4m_a##       = IGNORE; {@NO_4^-\aq}       {peroxy nitrate}

{------------------------------------- C -------------------------------------}

{1C}
 CO3m_a##       = IGNORE; {@CO_3^-\aq}       {}
 HCOOm_a##      = IGNORE; {@HCOO^-\aq}       {formate}
 HCO3m_a##      = IGNORE; {@HCO_3^-\aq}      {hydrogen carbonate}

{2C}
 CH3COOm_a##    = IGNORE; {@CH_3COO^-\aq}    {acetate}

{------------------------------------- Cl ------------------------------------}

 Clm_a##        = IGNORE; {@Cl^-\aq}         {chloride}
 Cl2m_a##       = IGNORE; {@Cl_2^-\aq}       {}
 ClOm_a##       = IGNORE; {@ClO^-\aq}        {}
 ClOHm_a##      = IGNORE; {@ClOH^-\aq}       {}

{------------------------------------- Br ------------------------------------}

 Brm_a##        = IGNORE; {@Br^-\aq}         {bromide}
 Br2m_a##       = IGNORE; {@Br_2^-\aq}       {}
 BrOm_a##       = IGNORE; {@BrO^-\aq}        {}
 BrOHm_a##      = IGNORE; {@BrOH^-\aq}       {}
 BrCl2m_a##     = IGNORE; {@BrCl_2^-\aq}     {}
 Br2Clm_a##     = IGNORE; {@Br_2Cl^-\aq}     {}

{------------------------------------- I -------------------------------------}

 Im_a##         = IGNORE; {@I^-\aq}          {iodide}
 IO2m_a##       = IGNORE; {@IO_2^-\aq}       {}
 IO3m_a##       = IGNORE; {@IO_3^-\aq}       {iodate}
 ICl2m_a##      = IGNORE; {@ICl_2^-\aq}      {}
 IClBrm_a##     = IGNORE; {@IClBr^-\aq}      {}
 IBr2m_a##      = IGNORE; {@IBr_2^-\aq}      {}

{------------------------------------- S -------------------------------------}

 SO3m_a##       = IGNORE; {@SO_3^-\aq}       {}
 SO3mm_a##      = IGNORE; {@SO_3^<2->\aq}    {sulfite}
 SO4m_a##       = IGNORE; {@SO_4^-\aq}       {}
 SO4mm_a##      = IGNORE; {@SO_4^<2->\aq}    {sulfate}
 SO5m_a##       = IGNORE; {@SO_5^-\aq}       {}
 HSO3m_a##      = IGNORE; {@HSO_3^-\aq}      {hydrogen sulfite}
 HSO4m_a##      = IGNORE; {@HSO_4^-\aq}      {hydrogen sulfate}
 HSO5m_a##      = IGNORE; {@HSO_5^-\aq}      {}
 CH3SO3m_a##    = IGNORE; {@CH_3SO_3^-\aq}   {MSA anion}
 CH2OHSO3m_a##  = IGNORE; {@CH_2OHSO_3^-\aq} {}

{------------------------------------Hg---------------------------------------}

 Hgp_a##        = IGNORE; {@Hg^+\aq}              {}
 Hgpp_a##       = IGNORE; {@Hg^<2+>\aq}           {}
 HgOHp_a##      = IGNORE; {@HgOH^+\aq}            {}
 HgClp_a##      = IGNORE; {@HgCl^+\aq}            {}
 HgCl3m_a##     = IGNORE; {@HgCl_3^-\aq}          {}
 HgCl4mm_a##    = IGNORE; {@HgCl_4^<2->\aq}       {}
 HgBrp_a##      = IGNORE; {@HgBr^+\aq}            {}
 HgBr3m_a##     = IGNORE; {@HgBr_3^-\aq}          {}
 HgBr4mm_a##    = IGNORE; {@HgBr_4^<2->\aq}       {}
 HgSO32mm_a##   = IGNORE; {@Hg(SO_3)_2^<2->\aq}   {}

{-----------------------------------------------------------------------------}
{------------------------------------ dummies --------------------------------}
{-----------------------------------------------------------------------------}

 D1O_a##        = IGNORE; {@D_1O\aq}         {}
 D2O_a##        = IGNORE; {@D_2O\aq}         {}
 DAHp_a##       = IGNORE; {@DAH^+\aq}        {}
 DA_a##         = IGNORE; {@DA\aq}           {}
 DAm_a##        = IGNORE; {@DA^-\aq}         {}
 DGtAi_a##      = IGNORE; {@DGtAi\aq}        {}
 DGtAs_a##      = IGNORE; {@DGtAs\aq}        {}
 PROD1_a##      = IGNORE; {@PROD1\aq}        {}
 PROD2_a##      = IGNORE; {@PROD2\aq}        {}
 Nap_a##        = IGNORE; {@Na^+\aq}         {dummy cation}
