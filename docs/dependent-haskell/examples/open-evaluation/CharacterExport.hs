{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies #-}
module CharacterExport (p3, Flag) where

import CharacterCase (flag)
import qualified CharacterCase
import Data.Proxy (Proxy(..))
import GHC.TypeLits (KnownChar, charVal)

-- | Reflect the result of the ordinary worker at the client's character.
p3 :: forall c uc. (CharacterCase.upper c ~ uc, KnownChar uc) => Proxy c -> Char
p3 _ = charVal (Proxy @uc)

-- | Keep the predicate stuck until the caller supplies a character.
type Flag (c :: Char) = flag c
