{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
module Foo where

import Data.Typeable (Proxy(..), typeRep)

data family T a
data instance T Int = MkT

main :: IO ()
main = print $ typeRep (Proxy :: Proxy MkT)
