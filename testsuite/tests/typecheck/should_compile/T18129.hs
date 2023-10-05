{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE PolyKinds               #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module T18129 where

import Data.Kind (Constraint)
import Data.Proxy (Proxy)
import Data.Typeable (Typeable)

-- First, `generics-sop` code, intact.
--
type family
  AllF (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  AllF _c '[]       = ()
  AllF  c (x ': xs) = (c x, All c xs)

class (AllF c xs, SListI xs) => All (c :: k -> Constraint) (xs :: [k])
instance All c '[]
instance (c x, All c xs) => All c (x ': xs) where

class Top x
instance Top x

type SListI = All Top

-- Next, user code, minimised.
--
data GADT
  = forall (xs :: [*]) (a :: *)
    . (Top a, All Typeable xs)
    => GADT

withSomePipe'
  :: GADT
  -> (forall (xs :: [*])
      . (Proxy xs -> GADT)
      -> GADT)
  -> GADT
withSomePipe' GADT f = f (const GADT)

