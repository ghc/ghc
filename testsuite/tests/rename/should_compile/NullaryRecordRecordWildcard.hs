{-# LANGUAGE RecordWildCards #-}

module NullaryRecordRecordWildcard where

data X = X { }

f X {..} = ()
