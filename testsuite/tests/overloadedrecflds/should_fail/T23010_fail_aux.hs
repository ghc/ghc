{-# LANGUAGE DuplicateRecordFields #-}

module T23010_fail_aux where

import {-# SOURCE #-} T23010_fail ( fld )

data X
bar = fld
