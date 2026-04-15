{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

-- This program was rejected by GHC 9.12 due to a bug with
-- unification in QuickLook.
module T26030 where

import Data.Kind

type S :: Type -> Type
data S a where
  S1 :: S Bool
  S2 :: S Char

type F :: Type -> Type
type family F a where
  F Bool = Bool
  F Char = Char

foo :: forall a. S a -> IO (F a)
foo sa1 = do
  () <- return ()
  case sa1 of
    S1 -> return $ False
    S2 -> return 'x'
