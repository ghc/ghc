-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2011
--
-- Generate code to initialise cost centres
--
-- -----------------------------------------------------------------------------

module ProfInit (profilingInitCode) where

import CLabel
import CostCentre
import DynFlags
import Outputable
import FastString
import Module

-- -----------------------------------------------------------------------------
-- Initialising cost centres

-- We must produce declarations for the cost-centres defined in this
-- module;

profilingInitCode :: Module -> CollectedCCs -> SDoc
profilingInitCode this_mod (local_CCs, ___extern_CCs, singleton_CCSs)
 = sdocWithDynFlags $ \dflags ->
   if not (gopt Opt_SccProfilingOn dflags)
   then empty
   else vcat
    [ text "static void prof_init_" <> ppr this_mod
         <> text "(void) __attribute__((constructor));"
    , text "static void prof_init_" <> ppr this_mod <> text "(void)"
    , braces (vcat (
         map emitRegisterCC           local_CCs ++
         map emitRegisterCCS          singleton_CCSs
       ))
    ]
 where
   emitRegisterCC cc   =
      text "extern CostCentre " <> cc_lbl <> ptext (sLit "[];") $$
      text "REGISTER_CC(" <> cc_lbl <> char ')' <> semi
     where cc_lbl = ppr (mkCCLabel cc)
   emitRegisterCCS ccs =
      text "extern CostCentreStack " <> ccs_lbl <> ptext (sLit "[];") $$
      text "REGISTER_CCS(" <> ccs_lbl <> char ')' <> semi
     where ccs_lbl = ppr (mkCCSLabel ccs)
