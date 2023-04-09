module GHC.CmmToAsm.RISCV64.Ppr where
import GHC.Utils.Outputable
import GHC.CmmToAsm.Types
import GHC.CmmToAsm.RISCV64.Instr
import Prelude

pprNatCmmDeclH :: IsDoc doc => NatCmmDecl RawCmmStatics Instr -> doc
pprNatCmmDeclH _ = error "TODO: pprNatCmmDeclH"

pprNatCmmDeclS :: IsDoc doc => NatCmmDecl RawCmmStatics Instr -> doc
pprNatCmmDeclS _ = error "TODO: pprNatCmmDeclS"
