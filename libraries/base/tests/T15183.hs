{-# LANGUAGE DataKinds #-}
module Main (main) where

import Control.Exception (ArithException(..), throw)
import Data.Proxy (Proxy(..))
import GHC.TypeLits ( KnownChar, KnownNat, KnownSymbol
                    , SChar, Nat, SNat, Symbol, SSymbol
                    , charVal, natVal, symbolVal
                    , withKnownChar, withKnownNat, withKnownSymbol
                    , withSomeSChar, withSomeSNat, withSomeSSymbol )

-- As found in the `reflection` library
reifyNat :: Integer -> (forall (n :: Nat). KnownNat n => Proxy n -> r) -> r
reifyNat n k = withSomeSNat n $ \(mbSn :: Maybe (SNat n)) ->
               case mbSn of
                 Just sn -> withKnownNat sn $ k @n Proxy
                 Nothing -> throw Underflow

reifySymbol :: String -> (forall (s :: Symbol). KnownSymbol s => Proxy s -> r) -> r
reifySymbol s k = withSomeSSymbol s $ \(ss :: SSymbol s) ->
                  withKnownSymbol ss $ k @s Proxy

reifyChar :: Char -> (forall (c :: Char). KnownChar c => Proxy c -> r) -> r
reifyChar c k = withSomeSChar c $ \(sc :: SChar c) ->
                withKnownChar sc (k @c Proxy)

main :: IO ()
main = do
  reifyNat    42   $ \(_ :: Proxy n) -> print $ natVal $ Proxy @n
  reifySymbol "hi" $ \(_ :: Proxy s) -> print $ symbolVal $ Proxy @s
  reifyChar   'a'  $ \(_ :: Proxy c) -> print $ charVal $ Proxy @c
