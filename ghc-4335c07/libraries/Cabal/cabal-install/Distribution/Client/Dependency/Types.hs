{-# LANGUAGE DeriveGeneric #-}
module Distribution.Client.Dependency.Types (
    PreSolver(..),
    Solver(..),

    PackagesPreferenceDefault(..),

  ) where

import Data.Char
         ( isAlpha, toLower )

import qualified Distribution.Compat.ReadP as Parse
         ( pfail, munch1 )
import Distribution.Text
         ( Text(..) )

import Text.PrettyPrint
         ( text )
import GHC.Generics (Generic)
import Distribution.Compat.Binary (Binary(..))


-- | All the solvers that can be selected.
data PreSolver = AlwaysModular
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

-- | All the solvers that can be used.
data Solver = Modular
  deriving (Eq, Ord, Show, Bounded, Enum, Generic)

instance Binary PreSolver
instance Binary Solver

instance Text PreSolver where
  disp AlwaysModular = text "modular"
  parse = do
    name <- Parse.munch1 isAlpha
    case map toLower name of
      "modular" -> return AlwaysModular
      _         -> Parse.pfail

-- | Global policy for all packages to say if we prefer package versions that
-- are already installed locally or if we just prefer the latest available.
--
data PackagesPreferenceDefault =

     -- | Always prefer the latest version irrespective of any existing
     -- installed version.
     --
     -- * This is the standard policy for upgrade.
     --
     PreferAllLatest

     -- | Always prefer the installed versions over ones that would need to be
     -- installed. Secondarily, prefer latest versions (eg the latest installed
     -- version or if there are none then the latest source version).
   | PreferAllInstalled

     -- | Prefer the latest version for packages that are explicitly requested
     -- but prefers the installed version for any other packages.
     --
     -- * This is the standard policy for install.
     --
   | PreferLatestForSelected
  deriving Show
