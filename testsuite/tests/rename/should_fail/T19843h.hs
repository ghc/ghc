{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module T19843h where

import Prelude (Int, map, undefined)
import Data.Traversable hiding (traverse)
import qualified Data.Monoid (Sum(..), getSum)
import Data.Monoid (Alt(..))

data A = A {mop :: ()}
data Mup = Mup

foo = undefined.mup

bar = undefined.traverse

baz = undefined.getSum

quux = undefined.getAlt

quuz = getAlt undefined

quur = undefined.getAll
