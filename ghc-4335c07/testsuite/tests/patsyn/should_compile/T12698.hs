{-# Language ViewPatterns, TypeOperators, KindSignatures, PolyKinds,
             TypeInType, StandaloneDeriving, GADTs, RebindableSyntax,
             RankNTypes, LambdaCase, PatternSynonyms, TypeApplications #-}

module T12698 where

import GHC.Types
import Prelude hiding ( fromInteger )
import Data.Type.Equality hiding ((:~~:)(..))
import Data.Kind
import qualified Prelude

class Ty (a :: k) where ty :: T a
instance Ty Int where ty = TI
instance Ty Bool where ty = TB
instance Ty a => Ty [a] where ty = TA TL ty
instance Ty [] where ty = TL
instance Ty (,) where ty = TP

data AppResult (t :: k) where
  App :: T a -> T b -> AppResult (a b)

data T :: forall k. k -> Type where
  TI :: T Int
  TB :: T Bool
  TL :: T []
  TP :: T (,)
  TA :: T f -> T x -> T (f x)
deriving instance Show (T a)

splitApp :: T a -> Maybe (AppResult a)
splitApp = \case
  TI -> Nothing
  TB -> Nothing
  TL -> Nothing
  TP -> Nothing
  TA f x -> Just (App f x)

data (a :: k1) :~~: (b :: k2) where
  HRefl :: a :~~: a

eqT :: T a -> T b -> Maybe (a :~~: b)
eqT a b =
  case (a, b) of
    (TI, TI) -> Just HRefl
    (TB, TB) -> Just HRefl
    (TL, TL) -> Just HRefl
    (TP, TP) -> Just HRefl

pattern List :: () => [] ~~ b => T b
pattern List <- (eqT (ty @(Type -> Type) @[]) -> Just HRefl)
  where List = ty

pattern Int :: () => Int ~~ b => T b
pattern Int <- (eqT (ty @Type @Int) -> Just HRefl)
  where Int = ty

pattern (:<->:) :: () => fx ~ f x => T f -> T x -> T fx
pattern f :<->: x <- (splitApp -> Just (App f x))
  where f :<->: x = TA f x

pattern Foo <- List :<->: Int
