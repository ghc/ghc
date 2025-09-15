{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE RecordWildCards #-}

module GADTNullaryRecordWildcard where

data X a where
  X :: X a

f X {..} = ()
