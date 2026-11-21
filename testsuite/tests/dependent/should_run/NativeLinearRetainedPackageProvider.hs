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
