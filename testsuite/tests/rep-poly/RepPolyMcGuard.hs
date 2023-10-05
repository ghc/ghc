{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RepPolyMcGuard where

import Data.Kind ( Type )
import GHC.Exts
import Prelude ( Bool(..), undefined )

(>>)
  :: forall
        rep
        ( ma :: TYPE rep )
        ( a  :: Type )
        ( mb :: TYPE rep )
        ( mc :: TYPE rep )
  .  ma -> ( a -> mb ) -> mc
(>>) = undefined

guard :: forall rep (a :: TYPE rep) (b :: TYPE rep). a -> b
guard = undefined

return :: forall rep (ma :: TYPE rep). () -> ma
return = undefined

foo :: forall rep (ma :: TYPE rep). () -> ma
foo _ = [ () | undefined ]
