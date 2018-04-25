{-# LANGUAGE TypeFamilies #-}

module T6018Dfail where

import T6018Bfail

type instance H Bool Int  Char = Int
