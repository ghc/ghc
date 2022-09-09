{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module GADTNullaryRecordRecordWildcard where

data X a where
  X :: {} -> X a

f X {..} = ()
