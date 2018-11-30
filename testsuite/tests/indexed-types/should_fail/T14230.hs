{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module T14230 where

class C k where
  data CD :: k -> k -> *

instance C (Maybe a) where
  data CD :: (k -> *) -> (k -> *) -> *
