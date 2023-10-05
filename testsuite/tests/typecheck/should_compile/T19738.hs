module T19738 where

import Data.Kind

type P :: k -> Type
data P a = MkP

type T :: k -> Constraint
class T (a :: j) where
  f :: P a
  f = MkP @j @a
