{-# LANGUAGE RecordWildCards #-}

module ImportNullaryRecordWildcard where

import NullaryRecordWildcard as A
import NullaryRecordRecordWildcard as B

g A.X {..} = ()

h B.X {..} = ()
