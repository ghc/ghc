{-# LANGUAGE Haskell2010 #-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnliftedNewtypes #-}

module RepPolyNewtypePat1 where

import GHC.Exts

type X :: TYPE rep -> TYPE rep
newtype X a = MkX a

bar :: forall rep (a :: TYPE rep). X a -> a
bar (MkX bndr_a) = bndr_a
