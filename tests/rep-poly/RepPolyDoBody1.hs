{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyDoBody1 where

import GHC.Exts
import Prelude ( undefined )

(>>)
  :: forall
        rep
        ( ma :: TYPE rep )
        ( mb :: TYPE rep )
        ( mc :: TYPE rep )
  .  ma -> mb -> mc
(>>) = undefined

return :: forall rep (ma :: TYPE rep). () -> ma
return = undefined

foo :: forall rep (ma :: TYPE rep). () -> ma
foo _ = do
  undefined :: ma
  return ()
