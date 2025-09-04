{-# LANGUAGE TypeFamilies #-}

module T26154_B where

import T26154_A

type family FAA a b

type instance FAA a b = b