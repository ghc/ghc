{-# LANGUAGE GADTs, TypeApplications #-}
module ForwardMain where

import Control.Exception
import Data.Proxy (Proxy(..))
import Filter
import Nested
import NestedSignature
import ForwardProducer
import ForwardSchemas
import Vector

single :: Hidden a
single = error "single dot"

nested :: PackedPair a
nested = error "nested dot"

nestedOther :: PackedPair a
nestedOther = failAny "nested producer"

nestedKind :: PackedKind
nestedKind = failAny "nested kind"

bar :: HiddenBar a
bar = failAny "bar producer"

missingSlot :: SlotBox Int
missingSlot = failAny "opaque family"

missingRanked :: Ranked
missingRanked = failAny "rank-n payload"

chooseNested :: Bool -> PackedPair a -> PackedPair a -> PackedPair a
chooseNested flag p q = choose flag p q

chooseBar :: Bool -> HiddenBar a -> HiddenBar a -> HiddenBar a
chooseBar flag p q = choose flag p q

keepKnown :: PackedPair a -> PackedPair a
keepKnown p = keepAfter True p

nestedAfter :: PackedPair a
nestedAfter = keepAfter True (failAny "nested producer after an ordinary argument")

strictAfter :: PackedPair a
strictAfter = keepAfter (error "strict ordinary argument" :: Bool) (failAny "unselected argument")

visibleAfter :: PackedPair a
visibleAfter = keepTagged @Int @Char (Proxy @Int) (Proxy @Char) True
    (failAny "visible arguments preserve binder order")

cells :: PackedPair a -> ([a], [a])
cells p = observe p
  where
    observe :: (Vec n a, Vec m a) -> ([a], [a])
    observe (ls, rs) = (list ls, list rs)
    list :: Vec n a -> [a]
    list Nil = []
    list (Cons x xs) = x : list xs

ignored :: PackedPair a -> Bool
ignored p = noDemand p
  where
    noDemand :: (Vec n a, Vec m a) -> Bool
    noDemand _ = True

demand :: String -> a -> IO ()
demand expected payload = do
    result <- try (evaluate payload >> pure ()) :: IO (Either ErrorCall ())
    case result of
        Left e | expected == takeWhile (/= '\n') (displayException e) -> putStrLn expected
        _ -> error "the test missed the original exception"

main :: IO ()
main = do
    let left = packPair (Cons 7 Nil) (Cons 2 (Cons 4 Nil))
        right = packPair Nil (Cons 9 Nil)
    print (ignored (nestedOther :: PackedPair Int) && ignored (nestedAfter :: PackedPair Int),
           cells (chooseNested True left nested),
           cells (chooseNested False nestedOther right),
           cells (keepKnown left))
    print (barIsEmpty (chooseBar True barValue bar))
    print (slotLabel (choose False missingSlot (slotBox (7 :: Int))),
           inspectRanked $ choose True ranked missingRanked)
    demand "opaque family" (slotLabel missingSlot)
    demand "rank-n payload" (inspectRanked missingRanked)
    demand "single dot" (toList single)
    demand "nested dot" (fst (cells nested))
    demand "nested producer" (fst (cells nestedOther))
    demand "nested kind" nestedKind
    demand "bar producer" (barIsEmpty bar)
    demand "nested producer after an ordinary argument" (fst (cells nestedAfter))
    demand "strict ordinary argument" (fst (cells strictAfter))
    demand "visible arguments preserve binder order" (fst (cells visibleAfter))
