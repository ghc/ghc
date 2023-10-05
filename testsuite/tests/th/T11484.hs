{-# LANGUAGE PolyKinds #-}

module T11484 where

import Data.Kind

type TySyn (k :: Type) (a :: k) = ()

$([d| type TySyn2 (k :: Type) (a :: k) = () |])
