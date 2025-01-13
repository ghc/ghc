{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module T25611b where

import GHC.Base (Type, TYPE, RuntimeRep (IntRep, BoxedRep), Levity (Unlifted))


-- Enhanced kind inference for data family instance in !13767
-- this is the h98 data instance case

data family V :: (k -> Type) -> k
data instance V f = MkV (f (TYPE (BoxedRep 'Unlifted)))
