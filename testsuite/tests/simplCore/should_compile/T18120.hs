{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Bug where

import Data.Kind

type family
  AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllF _c '[]       = ()
  AllF  c (x ': xs) = (c x, All c xs)

class (AllF c xs, SListI xs) => All (c :: k -> Constraint) (xs :: [k]) where
instance All c '[] where
instance (c x, All c xs) => All c (x ': xs) where

class Top x
instance Top x

type SListI = All Top

class All SListI (Code a) => Generic (a :: Type) where
  type Code a :: [[Type]]

data T = MkT Int
instance Generic T where
  type Code T = '[ '[Int] ]
