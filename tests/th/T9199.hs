{-# LANGUAGE TemplateHaskell, PolyKinds, TypeFamilies #-}

module T9160 where

import Data.Kind (Type)

$( [d| class C (a :: k) where
          type F (a :: k) :: Type
    |]
 )

