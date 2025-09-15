{-# LANGUAGE DuplicateRecordFields #-}

module T23010_fail where

import T23010_fail_aux ( X )
data A = MkA { fld :: A, other :: X }
data B = MkB { fld :: B }
