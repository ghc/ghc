{-# LANGUAGE TypeFamilies #-}

module T2693 where

type family TFn a :: *

f :: Maybe ()
f = do
  let Just x = undefined :: Maybe (TFn a)
  let n = fst x + fst x
  return ()
