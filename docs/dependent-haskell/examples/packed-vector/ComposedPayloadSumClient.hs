{-# LANGUAGE CPP, DataKinds, FlexibleContexts, FlexibleInstances, GADTs, ImpredicativeTypes, NoExperimentalDependentTypes, TypeFamilies, UndecidableInstances #-}
module ComposedPayloadSumClient where

import ComposedPayloadSum
import GHC.Sigma (Sigma(..))

#ifdef FIRST_MISSING
class MissingFirst a
instance {-# OVERLAPPING #-} MissingFirst Int => First [Int] where
    type FirstBody [Int] False = Int
    type FirstBody [Int] True = Char
#endif

#ifdef SECOND_MISSING
class MissingSecond a
instance {-# OVERLAPPING #-} MissingSecond Int => Second [Int] where
    type SecondBody [Int] False = String
    type SecondBody [Int] True = Bool
#endif

#ifdef INFERRED
inferred = payloadSchema distinctValue
#elif defined(REPEATED)
readRepeated :: Repeated -> Bool
readRepeated _ = True
#elif defined(FIRST_MISSING) || defined(SECOND_MISSING)
readDistinct :: Distinct -> Bool
readDistinct _ = True
#else
readNothing :: Optional -> Bool
readNothing (Pair _ Nothing) = True
readNothing _ = False

readScope :: Scoped [Int] -> String
readScope (Pair False (n, msg)) = show (n :: Int) ++ msg
readScope (Pair True (c, flag)) = c : show (flag :: Bool)

readRepeated :: Repeated -> String
readRepeated (Pair False (x, y)) = show (x :: Int) ++ show (y :: Int)
readRepeated (Pair True (x, y)) = [x, y]

-- | Read the optional payload without forcing its tag.
observePayloads :: Bool -> (Bool, String, String)
observePayloads tag =
    (readNothing (Pair tag Nothing), readScope scopeValue, readRepeated repeatedValue)
#endif
