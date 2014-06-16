{-# LANGUAGE RankNTypes, MultiParamTypeClasses #-}

module T where

import qualified Prelude as T(length,Monad,Integer)
import qualified Data.ByteString as T(length)
import Prelude(length,(+),(=<<),Monad(..),Maybe(..),Eq)
import Data.Maybe
import Control.Monad(Monad(..),MonadPlus(..))

length :: T.Integer
length = 0

class N a
class S a

class C a b where
  c1 :: N b => a -> b
  c2 :: (N b,S b) => a -> b
  c3 :: forall a. a -> b
  c4 :: a1 -> b

