{-# LANGUAGE DataKinds #-}

module UnsatInstance where

import GHC.TypeError

instance Unsatisfiable (Text "hello")
