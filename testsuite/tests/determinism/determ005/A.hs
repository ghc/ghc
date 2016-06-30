module A (
  ) where

-- This reproduces the issue where type variables would be lifted out in
-- different orders. Compare:
--
-- lvl =
--   \ (@ (c :: * -> *)) (@ (t :: * -> *)) ->
--     undefined
--     @ ((forall d. Data d => c (t d))
--        -> Maybe (c Node))
--     (some Callstack thing)
--
-- $cdataCast1 =
--   \ (@ (c :: * -> *)) (@ (t :: * -> *)) _ [Occ=Dead] ->
--     lvl @ c @ t
--
-- vs
--
-- lvl =
--   \ (@ (t :: * -> *)) (@ (c :: * -> *)) ->
--     undefined
--     @ ((forall d. Data d => c (t d))
--        -> Maybe (c Node))
--     (some Callstack thing)
--
-- $cdataCast1 =
--   \ (@ (c :: * -> *)) (@ (t :: * -> *)) _ [Occ=Dead] ->
--     lvl @ t @ c

import Data.Data

data Node = Node (Maybe Int) [Node]

instance Data Node where

  gfoldl = gfoldl
  gunfold = gunfold
  toConstr = toConstr
  dataTypeOf = dataTypeOf

  dataCast1 _ = undefined
  dataCast2 = dataCast2

  gmapT = gmapT
  gmapQl = gmapQl
  gmapQr = gmapQr
  gmapQ = gmapQ
  gmapQi = gmapQi
  gmapM = gmapM
  gmapMp = gmapMp
  gmapMo = gmapMo
