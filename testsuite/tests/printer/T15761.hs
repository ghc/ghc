{-# LANGUAGE TypeFamilies, TypeOperators #-}

data family (a + b) c d

data instance (Int + Bool) Double = Float
