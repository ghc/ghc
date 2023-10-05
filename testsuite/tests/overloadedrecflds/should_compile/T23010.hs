{-# LANGUAGE DuplicateRecordFields #-}

module T23010 ( A(..) ) where

import T23010_aux ( X )

data A = MkA { fld :: A, other :: X }
data B = MkB { fld :: B }
