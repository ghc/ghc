{-# LANGUAGE TypeFamilies #-}

module B where

import A

type family FAA a b

type instance FAA a b = b
