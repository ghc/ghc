{-# LANGUAGE RecordWildCards #-}

module NullaryRecordWildcard where

data X = X

f X {..} = ()
