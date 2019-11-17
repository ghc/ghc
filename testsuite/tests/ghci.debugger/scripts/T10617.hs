{-# LANGUAGE GADTs, PolyKinds #-}

data AppTreeT (a::k) where
  Con :: AppTreeT a
  App :: AppTreeT a -> AppTreeT b -> AppTreeT (a b)

tmt :: AppTreeT (Maybe Bool)
tmt = App (Con :: AppTreeT Maybe) Con

f :: AppTreeT a -> Bool
f (App (c@Con) _) = const True c
f _ = False
