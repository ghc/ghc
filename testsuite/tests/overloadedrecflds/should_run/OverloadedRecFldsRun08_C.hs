{-# LANGUAGE OverloadedRecordFields, TypeFamilies #-}

module OverloadedRecFldsRun08_C ( F(..) ) where

import OverloadedRecFldsRun08_A ( F(..) )

data instance F () = MkFUnit { foo :: () }
