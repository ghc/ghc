{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module T8566 where

data U (s :: *) = forall v. AA v [U s]
-- AA :: forall (s:*) (v:*). v -> [U s] -> U s

data I (u :: U *) (r :: [*]) :: * where
   A :: I (AA t as) r                  -- Existential k

-- A :: forall (u:U *) (r:[*])                  Universal
--             (k:BOX) (t:k) (as:[U *]).        Existential
--             (u ~ AA * k t as) =>
--             I u r

-- fs unused, but needs to be present for the bug
class C (u :: U *) (r :: [*]) (fs :: [*]) where
   c :: I u r -> I u r

-- c :: forall (u :: U *) (r :: [*]) (fs :: [*]). C u r fs => I u r -> I u r

instance (C (AA (t (I a ps)) as) ps fs) => C (AA t (a ': as)) ps fs where
-- instance C (AA t (a ': as)) ps fs where
  c A = c undefined
