module T8028a where

import Language.Haskell.TH

x = do n <- newName "F"
       return [ClosedTypeFamilyD (TypeFamilyHead n [] NoSig Nothing) []]
