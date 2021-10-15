{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyDoReturn where

import GHC.Exts
import Prelude ( undefined )

return :: forall rep (ma :: TYPE rep). () -> ma
return = undefined

foo :: forall rep (ma :: TYPE rep). () -> ma
foo _ = do
  return ()
