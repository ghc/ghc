{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module TcShouldTerminate where

import Data.Kind (Constraint)

class C (p :: Constraint)
class D (p :: Constraint)

instance C (D p) => C (D (D p))
