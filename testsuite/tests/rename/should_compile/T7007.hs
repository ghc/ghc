module T7007 where

import Data.Monoid( Monoid(..) )

infixr 5 ++   -- This unambiguously refers to the local definition

(++) :: Monoid a => a -> a -> a
(++) = mappend


