{-# LANGUAGE DataKinds #-}

module UnsatisfiableFail2 where

import GHC.TypeError
import Data.Type.Bool ( If )
import Data.Kind
import Data.Proxy

type ExpectTrue x = If x (() :: Constraint) (Unsatisfiable (Text "Input was False!"))

h :: ExpectTrue x => proxy x -> ()
h _ = ()

eg3 _ = h (Proxy @False)  -- error
