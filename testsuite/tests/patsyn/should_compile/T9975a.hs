{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
module T9975a where

data Test = Test { x :: Int }
pattern Test wat = Test { x = wat }

