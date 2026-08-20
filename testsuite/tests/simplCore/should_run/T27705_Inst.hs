{-# LANGUAGE UndecidableInstances, UndecidableSuperClasses, FlexibleInstances #-}
module T27705_Inst where

-- The two dictionaries are mutually recursive, and we have to ensure the specialiser
-- doesn't loop when it's peaking through their unfoldings.
class D2 a => D1 a
class D1 a => D2 a
instance D2 Int => D1 Int
instance D1 Int => D2 Int

{-# NOINLINE b #-}
b :: D1 a => a -> Int
b _ = 42
