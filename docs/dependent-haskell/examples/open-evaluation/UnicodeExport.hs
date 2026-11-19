{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}
module UnicodeExport (p3) where

import qualified Data.Char
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownChar, charVal)

-- | Reflect the installed Unicode mapping at the caller's character.
p3 :: forall c uc. (Data.Char.toUpper c ~ uc, KnownChar uc) => Proxy c -> Char
p3 _ = charVal (Proxy @uc)
