module GHC.CmmToAsm.ARM.Ppr (pprNatCmmDecl) where

import GHC.Prelude
import GHC.Platform
import GHC.CmmToAsm.Config
import GHC.CmmToAsm.Instr
import GHC.CmmToAsm.ARM.Instr
import GHC.Cmm
import GHC.Utils.Outputable

pprNatCmmDecl
  :: NCGConfig
  -> NatCmmDecl RawCmmStatics Instr
  -> SDoc
pprNatCmmDecl = undefined

pprInstr :: Platform -> Instr -> SDoc
pprInstr _ instr = case instr of
   COMMENT s
      -> asmComment (ftext s)

   LOCATION file line col _name
      -> text "\t.loc " <> ppr file <+> ppr line <+> ppr col

   DELTA d
      -> asmComment $ text ("\tdelta = " ++ show d)

   NEWBLOCK _
      -> panic "pprInstr: NEWBLOCK"

   LDATA _ _
      -> panic "pprInstr: LDATA"

asmComment :: SDoc -> SDoc
asmComment c = whenPprDebug $ text "# " <> c

