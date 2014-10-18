{-# LANGUAGE OverloadedRecordFields, TypeFamilies #-}

module OverloadedRecFldsRun07_B ( F(..) ) where

import OverloadedRecFldsRun07_A ( F(..) )

data instance F Int = MkFInt { foo :: Int }
