{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Bug where

data T a = MkT
deriving instance forall {a}. Show (T a)
