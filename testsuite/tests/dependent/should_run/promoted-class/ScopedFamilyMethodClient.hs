{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes, TypeFamilies #-}
module Main where

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

readPayload :: Collects c => c -> a -> (Ordering, a)
readPayload x payload = case inspectPayload x payload of
    TaggedEntry order item -> (order, item)

main :: IO ()
main = do
    print (readEntry (2 :: Int) True, readEntry (error "unforced" :: AlwaysBelow) 'x')
    print (readPayload (2 :: Int) 'q', fst (readPayload AlwaysBelow (error "unforced payload" :: Char)))
