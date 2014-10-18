{-# LANGUAGE OverloadedRecordFields, TypeFamilies #-}

module OverloadedRecFldsRun08_B ( F(..) ) where

import OverloadedRecFldsRun08_A ( F(..) )

data instance F Int = MkFInt { foo :: Int }
