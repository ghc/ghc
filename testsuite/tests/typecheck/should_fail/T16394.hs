{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds #-}

class C a where
    type T (n :: a)

instance C a => C b => C (a, b) where
    type T '(n, m) = (T n, T m)

-- but this worked fine:
--
-- instance (C a, C b) => C (a, b) where
--   type T '(n, m) = (T n, T m)
