{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}

module Bug where

pattern PATTERN = ()

data Proxy (tag :: k) (a :: *)

wrongLift :: Proxy PATTERN ()
wrongLift = undefined
