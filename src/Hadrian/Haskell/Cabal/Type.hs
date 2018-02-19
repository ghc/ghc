module Hadrian.Haskell.Cabal.Type where

import Development.Shake.Classes
import Hadrian.Package.Type

-- TODO: Use fine-grain tracking instead of tracking the whole Cabal file.
-- | Haskell package metadata extracted from a Cabal file.
data Cabal = Cabal
    { dependencies :: [PackageName]
    , name         :: PackageName
    , synopsis     :: String
    , version      :: String
    } deriving (Eq, Read, Show, Typeable)

instance Binary Cabal where
    put = put . show
    get = fmap read get

instance Hashable Cabal where
    hashWithSalt salt = hashWithSalt salt . show

instance NFData Cabal where
    rnf (Cabal a b c d) = a `seq` b `seq` c `seq` d `seq` ()
