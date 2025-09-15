{-# OPTIONS_GHC -O #-}
module TUnboxer where

-- This checks that the multiplicity of unboxer inside MkId.wrapCo is correct.
-- The test is a minimized version of base/GHC/Event/PSQ.hs and requires -O.
newtype Unique = Unique Int

data IntPSQ = Bin !Unique

deleteView :: Unique -> ()
deleteView k = Bin k `seq` ()
