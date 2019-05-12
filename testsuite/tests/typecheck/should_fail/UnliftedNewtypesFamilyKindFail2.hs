{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MagicHash #-}

module UnliftedNewtypesFamilyKindFail2 where

import Data.Kind (Type)

data family F k :: k
newtype instance F 5 = MkF (F 5)
