{-# LANGUAGE OverloadedRecordFields, ExistentialQuantification, RankNTypes, TypeFamilies #-}

module OverloadedRecFldsFail08_A where

-- x is existential (naughty)
data T = forall e . MkT { x :: e }

-- y and z are higher-rank
data U = MkU { y :: forall a . a -> a }
       | MkU2 { z :: (forall b . b) -> () }

data family F a
data instance F Int = forall e . MkFInt { foo :: e }
data instance F Bool = MkFBool { foo :: forall a . a -> a }
