-- | Unit identifier pretty-printing
module GHC.Unit.Ppr
   ( UnitPprInfo (..)
   )
where

import GHC.Prelude
import GHC.Data.FastString
import GHC.Utils.Outputable
import Data.Version

-- | Subset of UnitInfo: just enough to pretty-print a unit-id
--
-- Instead of printing the unit-id which may contain a hash, we print:
--    package-version:componentname
--
data UnitPprInfo = UnitPprInfo
   { unitPprId             :: FastString   -- ^ Identifier
   , unitPprPackageName    :: String       -- ^ Source package name
   , unitPprPackageVersion :: Version      -- ^ Source package version
   , unitPprComponentName  :: Maybe String -- ^ Component name
   }

instance Outputable UnitPprInfo where
  ppr pprinfo = getPprDebug $ \debug ->
    if debug
       then ftext (unitPprId pprinfo)
       else text $ mconcat
         [ unitPprPackageName pprinfo
         , case unitPprPackageVersion pprinfo of
            Version [] [] -> ""
            version       -> "-" ++ showVersion version
         , case unitPprComponentName pprinfo of
            Nothing    -> ""
            Just cname -> ":" ++ cname
         ]
