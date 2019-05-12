{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}

module UnliftedNewtypesFamilyKindFail1 where

import Data.Kind (Type)

data family DF (a :: Type) :: 5
