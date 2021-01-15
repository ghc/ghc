{-# LANGUAGE TypeFamilies, UndecidableSuperClasses #-}

module T19186 where

import Data.Kind

type family F a :: Constraint

class F a => C a
