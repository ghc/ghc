{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyDoBody2 where

import GHC.Exts
import Prelude ( undefined )

(>>)
  :: forall
        rep
        ( mb :: TYPE rep )
        ( mc :: TYPE rep )
  .  () -> mb -> mc
(>>) = undefined

return :: forall rep (ma :: TYPE rep). () -> ma
return = undefined

foo :: forall rep (ma :: TYPE rep). () -> ma
foo _ = do
  undefined :: ()
  return ()
