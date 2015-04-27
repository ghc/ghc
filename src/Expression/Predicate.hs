{-# LANGUAGE TypeFamilies #-}

module Expression.Predicate (
    module Prelude,
    Predicate (..), fromBool
    ) where

import qualified Prelude
import Prelude hiding (not, (&&), (||))

-- Minimal complete definition: 'true' or 'false', 'not', '&&' or '||'.
class Predicate a where
    type Variable a
    variable    :: Variable a -> a
    true, false :: a
    not         :: a -> a
    (&&), (||)  :: a -> a -> a

    -- Default implementations
    true   = not false
    false  = not true
    x && y = not (not x || not y)
    x || y = not (not x && not y)

fromBool :: Predicate a => Bool -> a
fromBool bool = if bool then true else false

infixr 3 &&
infixr 2 ||

instance Predicate Bool where
    type Variable Bool = ()
    variable = const True
    true     = True
    false    = False
    not      = Prelude.not
    (&&)     = (Prelude.&&)
    (||)     = (Prelude.||)

