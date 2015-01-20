{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
module T9975b where

data Test = Test { x :: Int }
pattern PTest wat = Test { x = wat }

