{-# LANGUAGE TypeFamilies, DataKinds #-}

module T25991a where

import T25991a_helper as T (C(type (#)))
import T25991a_helper as D (C((#)))

type S a b = a T.# b
f a b = a D.# b