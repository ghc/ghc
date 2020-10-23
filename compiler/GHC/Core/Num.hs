module GHC.Core.Num
   ( NumOps (..)
   , intOps
   , wordOps
   , mkNumLiteral
   )
where

import GHC.Prelude
import GHC.Platform
import GHC.Types.Literal
import GHC.Builtin.PrimOps

-- | Explicit "type-class"-like dictionary for numeric primops
data NumOps = NumOps
   { numAdd     :: PrimOp     -- ^ Add two numbers
   , numSub     :: PrimOp     -- ^ Sub two numbers
   , numMul     :: PrimOp     -- ^ Multiply two numbers
   , numLitType :: LitNumType -- ^ Literal type
   }

-- | Create a numeric literal
mkNumLiteral :: Platform -> NumOps -> Integer -> Literal
mkNumLiteral platform ops i = mkLitNumberWrap platform (numLitType ops) i

intOps :: NumOps
intOps = NumOps
   { numAdd     = IntAddOp
   , numSub     = IntSubOp
   , numMul     = IntMulOp
   , numLitType = LitNumInt
   }

wordOps :: NumOps
wordOps = NumOps
   { numAdd     = WordAddOp
   , numSub     = WordSubOp
   , numMul     = WordMulOp
   , numLitType = LitNumWord
   }
