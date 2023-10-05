{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ConstraintKinds #-}

module T20033 where

import Data.Typeable

data Some c where
    Some :: c a => a -> Some c

extractSome :: (Typeable a, forall x. c x => Typeable x) => Some c -> Maybe a
extractSome (Some a) = cast a
