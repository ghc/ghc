{-# LANGUAGE UndecidableInstances, PatternSynonyms, GADTs #-}

module T11728 where

import Data.Kind
import Data.Int
import GHC.TypeLits
import Text.Show.Functions

data Ty ty where
  I  :: Ty Int
  A  :: Ty a -> Ty [a]

class    GetTy ty  where getTy :: Ty ty
instance GetTy Int where getTy = I
instance GetTy ty => GetTy [ty] where
  getTy = A getTy

data E a where
  UnOp   :: Unary a b -> (E a -> E b)

pattern LEN :: () => (GetTy a) => E [a] -> E Int
pattern LEN xs <- UnOp (Un OpLen _) xs where
        LEN xs = UnOp (Un OpLen length) xs

data Unary a b where
  Un :: (GetTy a, GetTy b) => UnOp a b -> (a -> b) -> Unary a b

data UnOp a b where
  OpLen :: GetTy a => UnOp [a] Int
