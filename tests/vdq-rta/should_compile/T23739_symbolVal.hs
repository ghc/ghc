{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module T23739_symbolVal where

import Data.Kind
import Data.Proxy
import GHC.TypeLits

-- Definition:
symbolValVis :: forall s -> KnownSymbol s => String
symbolValVis s = symbolVal (Proxy :: Proxy s)

-- Usage:
strHelloWorld = symbolValVis ("Hello, " `AppendSymbol` "World")

-- Operator taking a term-level argument before a required type argument:
(++.) :: String -> forall (s :: Symbol) -> KnownSymbol s => String
s1 ++. s2 = s1 ++ symbolValVis s2
infixr 5 ++.

strTmPlusTy = "Tm" ++ "+" ++. "Ty"

-------------------------------------------------
--   Required type arguments in class methods  --
--         and in higher-rank positions        --
-------------------------------------------------

-- Continuation-passing encoding of a list spine:
--
-- data Spine xs where
--   Cons :: Spine xs -> Spine (x : xs)
--   Nil :: Spine '[]
--
type WithSpine :: [k] -> Constraint
class WithSpine xs where
  onSpine ::
    forall r.
    forall xs' -> (xs' ~ xs) =>  -- workaround b/c it's not possible to make xs visible
    ((xs ~ '[]) => r) ->
    (forall y ys -> (xs ~ (y : ys)) => WithSpine ys => r) ->
    r

instance WithSpine '[] where
  onSpine xs onNil _ = onNil

instance forall x xs. WithSpine xs => WithSpine (x : xs) where
  onSpine xs' _ onCons = onCons x xs

type All :: (k -> Constraint) -> [k] -> Constraint
type family All c xs where
  All c '[] = ()
  All c (a : as) = (c a, All c as)

type KnownSymbols :: [Symbol] -> Constraint
class All KnownSymbol ss => KnownSymbols ss
instance All KnownSymbol ss => KnownSymbols ss

symbolVals :: forall ss -> (KnownSymbols ss, WithSpine ss) => [String]
symbolVals ss =
  onSpine ss [] $ \s ss' ->
    symbolValVis s : symbolVals ss'

-- Reify a type-level list of strings at the term level.
strsLoremIpsum = symbolVals ["lorem", "ipsum", "dolor", "sit", "amet"]

-- Pass a required type argument to a continuation:
withSymbolVis :: String -> (forall s -> KnownSymbol s => r) -> r
withSymbolVis str cont =
  case someSymbolVal str of
    SomeSymbol (Proxy :: Proxy s) -> cont s

-- Use a required type argument in a continuation:
strLengthViaSymbol :: String -> Int
strLengthViaSymbol str =
  withSymbolVis str $ \s ->
    length (symbolValVis s)