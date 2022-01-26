module GHC.Cmm.Unaligned
    ( expandUnalignedLoad
    ) where

import GHC.Prelude
import GHC.Platform
import GHC.Cmm.Expr
import GHC.Cmm.Utils

-- | Expand an unaligned load into multiple byte-sized loads.
expandUnalignedLoad :: Platform -> CmmExpr -> CmmType -> CmmExpr
expandUnalignedLoad platform ptr ty =
    foldr1 (\x y -> CmmMachOp (MO_Or w) [x,y])
    $ zipWith shift [0,8..]
    [ CmmLoad (cmmOffsetB platform ptr n) b8
    | n <- [0..widthInBytes w-1]
    ]
  where
    shift n x = CmmMachOp (MO_Shl w) [x, mkIntExpr platform n]
    w = typeWidth ty
