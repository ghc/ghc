{-# LANGUAGE TypeInType #-}

module T11484 where

import Data.Kind

type TySyn (k :: *) (a :: k) = ()

$([d| type TySyn2 (k :: *) (a :: k) = () |])
