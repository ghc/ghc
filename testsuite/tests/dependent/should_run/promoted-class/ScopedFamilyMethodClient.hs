{-# LANGUAGE DataKinds, Haskell2010, NoExperimentalDependentTypes, MultiParamTypeClasses, TypeFamilies #-}
module Main where

import Data.Proxy (Proxy)
import GHC.TypeLits (natVal)
import Numeric.Natural (Natural)
import ScopedFamilyMethodProvider

instance Collects Int where
    type Element Int = Bool
    compareOrigin x = compare x 0

data AlwaysBelow = AlwaysBelow
instance Collects AlwaysBelow where
    type Element AlwaysBelow = Char
    compareOrigin _ = LT

readEntry :: Collects c => c -> Element c -> (Ordering, Element c)
readEntry x payload = case inspectEntry x payload of
    TaggedEntry order item -> (order, item)

instance Encode Char Bool where
    encode x = x == 'q'

readEncoding :: Collects c => c -> Char -> String
readEncoding x payload = case encodeEntry x payload of
    TaggedEntry _ text -> text

readPayload :: Collects c => c -> a -> (Ordering, a)
readPayload x payload = case inspectPayload x payload of
    TaggedEntry order item -> (order, item)

measureFive :: Collects c => c -> (Ordering, Natural)
measureFive x = case measuredEntry x 5 of
    (TaggedEntry order n, _) -> (order, n)

projectFive :: Collects c => c -> Proxy 5
projectFive x = snd (id (measuredEntry x 5))

main :: IO ()
main = do
    print (readEntry (2 :: Int) True, readEntry (error "unforced" :: AlwaysBelow) 'x')
    print (readPayload (2 :: Int) 'q', fst (readPayload AlwaysBelow (error "unforced payload" :: Char)))
    print (readEncoding (2 :: Int) 'q', readEncoding AlwaysBelow 'x')

    let high = True
        low = False
        unused = error "forced unused local payload" :: Char
    print (case inspectPayload high 'q' of TaggedEntry order value -> (order, value))
    print (case inspectPayload low unused of TaggedEntry order _ -> order)
    print (measureFive True, measureFive False)
    print (natVal (projectFive True))
