{-# LANGUAGE GADTs, StandaloneKindSignatures, PolyKinds, RankNTypes #-}

import GHC.Types (Type)

type AppTreeT :: forall k. k -> Type
data AppTreeT a where
  Con :: AppTreeT a
  App :: AppTreeT a -> AppTreeT b -> AppTreeT (a b)

tmt :: AppTreeT (Maybe Bool)
tmt = App (Con :: AppTreeT Maybe) Con

f :: AppTreeT a -> Bool
f (App (c@Con) _) = const True c
f _ = False
