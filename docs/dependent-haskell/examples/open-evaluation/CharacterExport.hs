{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}
module CharacterExport (p3, Flag) where

import CharacterCase (Flag)
import qualified CharacterCase
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownChar, charVal)

-- | Reflect the result of the ordinary worker at the client's character.
p3 :: forall c uc. (CharacterCase.upper c ~ uc, KnownChar uc) => Proxy c -> Char
p3 _ = charVal (Proxy @uc)
