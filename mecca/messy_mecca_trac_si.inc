! -*- f90 -*- this file was created by xmecca, do not edit!
CALL new_tracer(status, setname, 'N2O',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_N2O)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_N2O, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O, R_molarmass         , MN*2.+MO)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NH3',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NH3)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NH3, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH3, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH3, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH3, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH3, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH3, R_molarmass         , MN+MH*3.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH3, R_henry             , 58._dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH3, R_dryreac_sf        , 0._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'CH4',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_CH4)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_CH4, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH4, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH4, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH4, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH4, R_molarmass         , MC+MH*4.)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'HCOOH',       modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_HCOOH)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_HCOOH, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCOOH, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCOOH, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCOOH, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCOOH, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCOOH, R_molarmass         , MC+MH*2.+MO*2.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCOOH, R_henry             , 4.E6_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCOOH, R_dryreac_sf        , 0._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'O3P',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_O3P)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_O3P, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3P, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3P, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3P, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3P, R_molarmass         , MO)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'N2O5',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_N2O5)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_N2O5, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O5, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O5, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O5, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O5, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O5, R_molarmass         , MN*2.+MO*5.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O5, R_henry             , 1.E4_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2O5, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'H',           modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_H)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_H, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H, I_convect           , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H, I_vdiff             , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H, R_molarmass         , MH)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'H2O2',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_H2O2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_H2O2, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2O2, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2O2, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2O2, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2O2, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2O2, R_molarmass         , MH*2.+MO*2.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2O2, R_henry             , 7.45E4_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2O2, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'H2',          modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_H2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_H2, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_H2, R_molarmass         , MH*2.)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'CH3OH',       modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_CH3OH)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_CH3OH, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OH, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OH, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OH, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OH, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OH, R_molarmass         , MC+MH*4.+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OH, R_henry             , 2.2E2_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OH, R_dryreac_sf        , 0.1_dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'CO',          modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_CO)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_CO, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO, R_molarmass         , MC+MO)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'O1D',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_O1D)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_O1D, I_advect            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O1D, I_convect           , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O1D, I_vdiff             , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O1D, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O1D, R_molarmass         , MO)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NH2OH',       modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NH2OH)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NH2OH, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2OH, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2OH, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2OH, I_drydep            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2OH, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2OH, R_molarmass         , MH*3.+MN+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2OH, R_dryreac_sf        , 0._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NHOH',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NHOH)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NHOH, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NHOH, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NHOH, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NHOH, I_drydep            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NHOH, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NHOH, R_molarmass         , MH*2.+MN+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NHOH, R_dryreac_sf        , 0._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'HNO4',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_HNO4)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_HNO4, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO4, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO4, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO4, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO4, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO4, R_molarmass         , MH+MN+MO*4.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO4, R_henry             , 1.E4_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO4, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'CH3OOH',      modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_CH3OOH)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_CH3OOH, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OOH, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OOH, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OOH, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OOH, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OOH, R_molarmass         , MC+MH*4.+MO*2.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OOH, R_henry             , 3.e2_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3OOH, R_dryreac_sf        , 0.1_dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'HONO',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_HONO)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_HONO, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HONO, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HONO, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HONO, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HONO, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HONO, R_molarmass         , MH+MO+MN+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HONO, R_henry             , 4.9E1_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HONO, R_dryreac_sf        , 0.1_dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'HNO3',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_HNO3)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_HNO3, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO3, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO3, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO3, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO3, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO3, R_molarmass         , MH+MN+MO*3.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO3, R_henry             , 1.E4_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO3, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NH2O',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NH2O)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NH2O, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2O, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2O, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2O, I_drydep            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2O, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2O, R_molarmass         , MH*2.+MN+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2O, R_dryreac_sf        , 0._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'HNO',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_HNO)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_HNO, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO, I_drydep            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO, R_molarmass         , MH+MN+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HNO, R_dryreac_sf        , 0._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NH2',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NH2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NH2, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2, I_drydep            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2, R_molarmass         , MH*2.+MN)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NH2, R_dryreac_sf        , 0._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'HCHO',        modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_HCHO)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_HCHO, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCHO, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCHO, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCHO, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCHO, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCHO, R_molarmass         , MC+MH*2.+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCHO, R_henry             , 3.2e3_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HCHO, R_dryreac_sf        , 0.1_dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'CH3O2',       modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_CH3O2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_CH3O2, I_advect            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3O2, I_convect           , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3O2, I_vdiff             , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3O2, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CH3O2, R_molarmass         , MC+MH*3.+MO*2.)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NO3',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NO3)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NO3, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO3, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO3, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO3, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO3, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO3, R_molarmass         , MN+MO*3.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO3, R_henry             , 1.8_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO3, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'O3',          modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_O3)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_O3, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3, R_molarmass         , MO*3.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3, R_henry             , 0.01_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O3, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NO',          modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NO)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NO, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO, R_molarmass         , MN+MO)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO, R_henry             , 2.E-3_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'OH',          modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_OH)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_OH, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_OH, I_convect           , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_OH, I_vdiff             , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_OH, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_OH, R_molarmass         , MO+MH)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_OH, R_henry             , 25._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'HO2',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_HO2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_HO2, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HO2, I_convect           , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HO2, I_vdiff             , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HO2, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_HO2, R_molarmass         , MH+MO*2.)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'NO2',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_NO2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_NO2, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO2, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO2, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO2, I_drydep            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO2, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO2, R_molarmass         , MN+MO*2.)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO2, R_henry             , 1.0E-2_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_NO2, R_dryreac_sf        , 1._dp)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'O2',          modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_O2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_O2, R_vini              , 0.21_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O2, I_advect            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O2, I_convect           , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O2, I_vdiff             , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O2, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_O2, R_molarmass         , MO*2.)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'N2',          modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_N2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_N2, R_vini              , 0.78_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2, I_advect            , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2, I_convect           , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2, I_vdiff             , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2, I_scav              , OFF)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_N2, R_molarmass         , MN*2.)
CALL tracer_halt(substr, status)
CALL new_tracer(status, setname, 'CO2',         modstr, &
  quantity             = AMOUNTFRACTION, &
  unit                 = 'mol/mol', &
  medium               = AIR, &
  idx                  = idt_CO2)
CALL tracer_halt(substr,status)
CALL set_tracer(status, setname, idt_CO2, R_vini              , 3.5E-04_dp)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO2, I_advect            , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO2, I_convect           , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO2, I_vdiff             , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO2, I_scav              , ON)
CALL tracer_halt(substr, status)
CALL set_tracer(status, setname, idt_CO2, R_molarmass         , MC+MO*2.)
CALL tracer_halt(substr, status)
