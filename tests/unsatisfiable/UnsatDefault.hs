{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}

module UnsatDefault where

import GHC.TypeError

class C a where
  method :: a
  default method :: Unsatisfiable (Text "Please define the method manually. You can try...") => a
  method = unsatisfiable


instance C Int
