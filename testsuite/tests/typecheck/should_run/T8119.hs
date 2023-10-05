{-# LANGUAGE TypeFamilies #-}
class Test a where test :: a
instance (a ~ Int, b ~ Int) => Test (a -> b) where test = id
