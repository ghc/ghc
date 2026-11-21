{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, TypeFamilies #-}
module NativeLinearRetainedPackageProvider where

type family Flag (b :: Bool) where
    Flag 'True = ()
    Flag 'False = Bool

data Choice a where
    First :: foreach (b :: Bool) -> Flag b -> a %1 -> Choice a
    Other :: foreach (b :: Bool) -> Flag b -> a %1 -> Choice a

takeChoice :: Choice a %1 -> a
takeChoice (First _ _ a) = a
takeChoice (Other _ _ a) = a

data Slot a where
    Slot :: foreach (b :: Bool) -> Flag b -> a %1 -> Slot a

takeRefined :: Slot a %1 -> a
takeRefined (Slot True () a) = a
takeRefined (Slot False _ a) = a

takeOverlap :: Slot a %1 -> (Bool, a)
takeOverlap (Slot False True a) = (True, a)
takeOverlap (Slot False False a) = (False, a)
takeOverlap (Slot True () a) = (True, a)

makeIndex :: Bool -> a -> Slot a
makeIndex b a = Slot b (error "flag payload") a

data Two a where
    Two :: foreach (b :: Bool) -> foreach (c :: Bool) -> Flag b -> Flag c -> a %1 -> Two a

takeTwo :: Two a %1 -> (Bool, a)
takeTwo (Two False c _ flag a) = (readFlag c flag, a)
takeTwo (Two True d () flag a) = (readFlag d flag, a)

readFlag :: foreach (b :: Bool) -> Flag b -> Bool
readFlag True () = True
readFlag False b = b

data Nat = Z | S Nat
data At (n :: Nat) = At Int

type family Payload (n :: Nat) where
    Payload 'Z = ()
    Payload ('S n) = At n

data OwnedStep a where
    Step :: foreach (n :: Nat) -> Bool -> Payload n -> a %1 -> OwnedStep a

readAt :: foreach (n :: Nat) -> At n -> Int
readAt _ (At i) = i

takeStep :: OwnedStep a %1 -> (Int, a)
takeStep (Step Z _ () a) = (0, a)
takeStep (Step (S left) True payload a) = (readAt left payload, a)
takeStep (Step (S right) False payload a) = (1 + readAt right payload, a)

at :: foreach (n :: Nat) -> Int -> At n
at _ = At

next :: Nat -> Bool -> Int -> a -> OwnedStep a
next n b i a = Step (S n) b (at n i) a

data Packet a where
    IndexFirst :: foreach (n :: Nat) -> foreach (b :: Bool) -> Payload n -> Flag b -> a %1 -> Packet a
    FlagFirst :: foreach (n :: Nat) -> foreach (b :: Bool) -> Flag b -> Payload n -> a %1 -> Packet a

inspect :: Packet a %1 -> ((Int, Bool), a)
inspect (IndexFirst Z _ () _ a) = ((0, False), a)
inspect (IndexFirst (S left) True payload () a) = ((readAt left payload, True), a)
inspect (IndexFirst (S right) False payload flag a) = ((1 + readAt right payload, flag), a)
inspect (FlagFirst (S right) False flag payload a) = ((1 + readAt right payload, flag), a)
inspect (FlagFirst Z _ _ () a) = ((0, False), a)
inspect (FlagFirst (S left) True () payload a) = ((readAt left payload, True), a)

packetTrue :: Nat -> Int -> () -> a -> Packet a
packetTrue n i flag a = IndexFirst (S n) True (at n i) flag a

packetFalse :: Nat -> Bool -> Int -> a -> Packet a
packetFalse n flag i a = FlagFirst (S n) False flag (at n i) a

packetZero :: Bool -> a -> Packet a
packetZero b a = FlagFirst Z b (error "unused flag payload") () a

packetUnknown :: Bool -> a -> Packet a
packetUnknown b a = IndexFirst (S Z) b (at Z 0) (error "unused flag payload") a

data Triple a where
    Three :: foreach (n :: Nat) -> Payload n -> Bool
          -> foreach (b :: Bool) -> Flag b -> foreach (c :: Bool) -> Flag c
          -> a %1 -> Triple a

chooseThree :: Triple a %1 -> (Int, a)
chooseThree (Three Z () _ _ _ _ _ a) = (0, a)
chooseThree (Three (S k) payload True True _ True _ a) = (10 + readAt k payload, a)
chooseThree (Three (S k) payload True True _ False _ a) = (20 + readAt k payload, a)
chooseThree (Three (S k) payload True False _ _ _ a) = (30 + readAt k payload, a)
chooseThree (Three (S k) payload False _ _ _ _ a) = (40 + readAt k payload, a)

makeTriple :: Nat -> Int -> Bool -> Bool -> Bool -> a -> Triple a
makeTriple n i ordinary b c a =
    Three (S n) (at n i) ordinary b (error "first flag payload") c (error "second flag payload") a
