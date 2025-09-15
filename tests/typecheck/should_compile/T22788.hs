{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module E where

import Data.Data (Typeable)
import Data.Kind (Type)

infixr 0 ~>
type f ~> g = forall x. f x -> g x

class IFunctor f where
    imap :: (a ~> b) -> f a ~> f b

class IFunctor f => IApplicative f where
    ireturn :: a ~> f a

class IApplicative m => IMonad m where
    ibind :: (a ~> m b) -> m a ~> m b

class IMonad m => IMonadFail m where
    fail :: String -> m a ix

data At :: Type -> k -> k -> Type where
    At :: a -> At a k k
    deriving (Typeable)

(>>=) :: IMonad (m :: (x -> Type) -> x -> Type) => m a ix -> (a ~> m b) -> m b ix
m >>= f = ibind f m

data FHState = FOpen | FClosed

data FHSTATE :: FHState -> Type where
    FOPEN :: FHSTATE FOpen
    FCLOSED :: FHSTATE FClosed

data FH :: (FHState -> Type) -> FHState -> Type where
    FHReturn :: q i -> FH q i
    FHOpen :: FilePath -> (FHSTATE ~> FH q) -> FH q FClosed
    FHClose :: FH q FClosed -> FH q FOpen
    FHRead :: (Maybe Char -> FH q FOpen) -> FH q FOpen
    FHIO :: IO () -> FH q i -> FH q i

instance IFunctor FH where
    imap f (FHReturn q) = FHReturn (f q)
    imap f (FHOpen s k) = FHOpen s (imap f . k)
    imap f (FHClose q) = FHClose (imap f q)
    imap f (FHRead k) = FHRead (imap f . k)
    imap f (FHIO io k) = FHIO io (imap f k)

instance IApplicative FH where
    ireturn = FHReturn

instance IMonad FH where
    ibind f (FHReturn q) = f q
    ibind f (FHOpen fp p) = FHOpen fp (ibind f . p)
    ibind f (FHClose q) = FHClose (ibind f q)
    ibind f (FHRead f') = FHRead (ibind f . f')
    ibind f (FHIO io f') = FHIO io (ibind f f')

fhOpen :: FilePath -> FH FHSTATE 'FClosed
fhOpen f = FHOpen f FHReturn

fhClose :: FH (At () 'FClosed) 'FOpen
fhClose = FHClose . FHReturn $ At ()

fhio :: IO () -> FH (At () i) i
fhio io = FHIO io . FHReturn $ At ()

----------------------right function -------------------------------
rightFun :: FilePath -> FH (At () FClosed) FClosed
rightFun fp =
    fhio (print fp)
        E.>>= ( \(At _) -> E.do
                    fhOpen fp E.>>= \case
                        FCLOSED -> FHReturn (At ())
                        FOPEN -> E.do
                            At _ <- fhClose
                            FHReturn (At ())
              )

----------------------bug function -------------------------------
errorFun :: FilePath -> FH (At () FClosed) FClosed
errorFun fp = E.do
    At _ <- fhio (print fp)
    foState <- fhOpen fp
    case foState of
        FCLOSED -> FHReturn (At ())
        FOPEN -> E.do
            At _ <- fhClose
            FHReturn (At ())
