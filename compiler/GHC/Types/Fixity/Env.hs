module GHC.Types.Fixity.Env
   ( FixityEnv
   , FixItem (..)
   , emptyFixityEnv
   , lookupFixity
   , mkIfaceFixCache
   , emptyIfaceFixCache
   )
where

import GHC.Prelude

import GHC.Types.Fixity
import GHC.Types.Name
import GHC.Types.Name.Env

import GHC.Utils.Outputable

-- | Fixity environment mapping names to their fixities
type FixityEnv = NameEnv FixItem

-- | Fixity information for an 'Name'. We keep the OccName in the range
-- so that we can generate an interface from it
data FixItem = FixItem OccName Fixity

instance Outputable FixItem where
  ppr (FixItem occ fix) = ppr fix <+> ppr occ

emptyFixityEnv :: FixityEnv
emptyFixityEnv = emptyNameEnv

lookupFixity :: FixityEnv -> Name -> Fixity
lookupFixity env n = case lookupNameEnv env n of
                        Just (FixItem _ fix) -> fix
                        Nothing         -> defaultFixity

-- | Creates cached lookup for the 'mi_fix_fn' field of 'ModIface'
mkIfaceFixCache :: [(OccName, Fixity)] -> OccName -> Maybe Fixity
mkIfaceFixCache pairs
  = \n -> lookupOccEnv env n
  where
   env = mkOccEnv pairs

emptyIfaceFixCache :: OccName -> Maybe Fixity
emptyIfaceFixCache _ = Nothing

