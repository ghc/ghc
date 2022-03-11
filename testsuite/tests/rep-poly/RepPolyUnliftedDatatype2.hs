
{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

module RepPolyUnliftedDatatype2 where

import GHC.Exts

type Interpret :: Bool -> Levity
type family Interpret b where
  Interpret True  = Lifted
  Interpret False = Unlifted

type A :: TYPE (BoxedRep (Interpret b))
data A = MkA Int

a :: A @True
a = MkA 42

-- type Interpret :: Bool -> RuntimeRep
-- type family Interpret b where
--   Interpret True  = BoxedRep Lifted
--   Interpret False = BoxedRep Unlifted
--
-- type A :: TYPE (Interpret b)
-- data A = MkA Int
--
-- data B :: TYPE (Interpret b) where
--   MkB :: Int -> B @b
