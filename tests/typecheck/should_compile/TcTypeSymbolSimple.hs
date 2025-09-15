{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies #-}
module TcTypeNatSimple where
import GHC.TypeLits
import Data.Proxy

--------------------------------------------------------------------------------
-- Test evaluation

te1 :: Proxy (AppendSymbol "" x) -> Proxy x
te1 = id

te2 :: Proxy (AppendSymbol x "") -> Proxy x
te2 = id

te3 :: Proxy (AppendSymbol "type" "level") -> Proxy "typelevel"
te3 = id

--------------------------------------------------------------------------------
-- Test interactions with inerts

tei1 :: Proxy (AppendSymbol y x) -> Proxy x -> ()
tei1 _ _ = ()

tei2 :: Proxy (AppendSymbol "foo" x) -> ()
tei2 _ = ()

tei3 :: Proxy (AppendSymbol x "foo") -> ()
tei3 _ = ()
