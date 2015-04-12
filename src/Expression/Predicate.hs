{-# LANGUAGE TypeFamilies #-}

module Expression.Predicate (
    module Prelude,
    Predicate (..)
    ) where

import qualified Prelude
import Prelude hiding (not, (&&), (||))

class Predicate a where
    type Variable a
    variable    :: Variable a -> a
    true, false :: a
    not         :: a -> a
    (&&), (||)  :: a -> a -> a

instance Predicate Bool where
    type Variable Bool = Bool
    variable = id
    true     = True
    false    = False
    not      = Prelude.not
    (&&)     = (Prelude.&&)
    (||)     = (Prelude.||)
