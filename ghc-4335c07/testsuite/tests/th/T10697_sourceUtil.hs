{-# LANGUAGE StrictData, TemplateHaskell #-}
module T10697_sourceUtil where

import Language.Haskell.TH

makeSimpleDatatype :: Name
                   -> Name
                   -> SourceUnpackednessQ
                   -> SourceStrictnessQ
                   -> Q Dec
makeSimpleDatatype tyName conName srcUpk srcStr =
  dataD (cxt []) tyName [] Nothing [normalC conName
    [bangType (bang srcUpk srcStr) (conT ''Int)]] []

checkBang :: Name
          -> SourceUnpackednessQ
          -> SourceStrictnessQ
          -> ExpQ
checkBang n srcUpk1 srcStr1 = do
  TyConI (DataD _ _ _ _ [NormalC _ [(Bang srcUpk2 srcStr2, _)]] _) <- reify n
  srcUpk1' <- srcUpk1
  srcStr1' <- srcStr1
  if srcUpk1' == srcUpk2 && srcStr1' == srcStr2
    then [| True |]
    else [| False |]

data E1 = E1                   Int -- No unpackedness, no strictness
data E2 = E2                  !Int -- No unpackedness, strict
data E3 = E3                  ~Int -- No unpackedness, lazy
data E4 = E4 {-# NOUNPACK #-}  Int -- NOUNPACK, no strictness
data E5 = E5 {-# NOUNPACK #-} !Int -- NOUNPACK, strict
data E6 = E6 {-# NOUNPACK #-} ~Int -- NOUNPACK, lazy
data E7 = E7 {-#   UNPACK #-}  Int -- UNPACK, no strictness
data E8 = E8 {-#   UNPACK #-} !Int -- UNPACK, strict
data E9 = E9 {-#   UNPACK #-} ~Int -- UNPACK, lazy
