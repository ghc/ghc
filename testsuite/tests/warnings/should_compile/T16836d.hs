{-# OPTIONS_GHC -Wimplicit-field-strictness #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}
module T16836d where

import GHC.Exts

-- unlifted fields can't be usefully annotated; exempt
data P = MkP Int# (# Int, Int #)

type UD :: UnliftedType
data UD = MkUD

-- a field of an unlifted data type is exempt too
data Q = MkQ UD

-- mixed constructor
-- warns only for the lifted field 2
data M = MkM Int# Int
