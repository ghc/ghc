{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}
module UnicodeExport (p3, Title, Lower) where

import qualified Data.Char
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownChar, charVal)

-- | Reflect the installed Unicode mapping at the caller's character.
p3 :: forall c uc. (Data.Char.toUpper c ~ uc, KnownChar uc) => Proxy c -> Char
p3 _ = charVal (Proxy @uc)

-- | Apply the published title-case equations at the caller's character.
type Title (c :: Char) = Data.Char.toTitle c

-- | Apply the published lower-case equations at the caller's character.
type Lower (c :: Char) = Data.Char.toLower c
