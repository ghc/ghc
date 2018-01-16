{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}

pattern PATTERN = ()

data Proxy (tag :: k) (a :: *)

wrongLift :: Proxy PATTERN ()
wrongLift = undefined
