{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, GADTs, RankNTypes #-}

module T7176 where

type family Sing (a :: b)

data SMaybe (a :: Maybe c) where
   SNothing :: SMaybe Nothing
   SJust :: Sing a -> SMaybe (Just a)
type instance Sing (a :: Maybe d) = SMaybe a

sIsJust :: forall (a :: Maybe e). Sing a -> ()
sIsJust SNothing = ()
sIsJust (SJust _) = ()

