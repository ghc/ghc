{-# LANGUAGE DataKinds #-}

module Unsatisfiable2 where

import GHC.TypeError
import Data.Type.Bool ( If )
import Data.Kind
import Data.Proxy


type ExpectTrue x = If x (() :: Constraint) (Unsatisfiable (Text "Input was False!"))

h1 :: ExpectTrue x => proxy x -> ()
h1 _ = ()

h2 :: If x (() :: Constraint) (Unsatisfiable (Text "Input was False!")) => proxy x -> ()
h2 _ = ()

eg11 _ = h1 (Proxy @True)
eg12 p = h1 p
eg21 _ = h2 (Proxy @True)
eg22 p = h2 p
