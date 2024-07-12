module GHC.CmmToAsm.Types
   ( NatCmm
   , NatCmmDecl
   , NatBasicBlock
   , GenBasicBlock(..)
   , blockId
   , ListGraph(..)
   , RawCmmStatics
   , RawCmmDecl
   )
where

import GHC.Cmm.Dataflow.Label.NonDet
import GHC.Cmm


-- Our flavours of the Cmm types
-- Type synonyms for Cmm populated with native code
type NatCmm instr
        = GenCmmGroup
                RawCmmStatics
                (LabelMap RawCmmStatics)
                (ListGraph instr)

type NatCmmDecl statics instr
        = GenCmmDecl
                statics
                (LabelMap RawCmmStatics)
                (ListGraph instr)

type NatBasicBlock instr
        = GenBasicBlock instr
