{-# LANGUAGE TypeFamilies #-}

module OverloadedRecFldsRun12_B ( F(foo, MkFInt, MkFBool) ) where

import OverloadedRecFldsRun12_A ( F(..) )

data instance F Int = MkFInt { foo :: Int }
