{-# LANGUAGE DuplicateRecordFields #-}

{-# WARNING foo "No warnings for DRFs" #-}
data S = MkS { foo :: Bool }
data T = MkT { foo :: Int }
