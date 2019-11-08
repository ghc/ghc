{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds, KindSignatures, PolyKinds #-}

module Bug where

pattern PATTERN = ()

data Proxy (tag :: k) (a :: *)

wrongLift :: Proxy PATTERN ()
wrongLift = undefined
