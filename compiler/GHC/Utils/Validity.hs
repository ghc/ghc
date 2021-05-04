
module GHC.Utils.Validity where

import GHC.Prelude
import GHC.Utils.Outputable

-------------------------
data Validity
  = IsValid            -- ^ Everything is fine
  | NotValid SDoc    -- ^ A problem, and some indication of why

isValid :: Validity -> Bool
isValid IsValid       = True
isValid (NotValid {}) = False

andValid :: Validity -> Validity -> Validity
andValid IsValid v = v
andValid v _       = v

-- | If they aren't all valid, return the first
allValid :: [Validity] -> Validity
allValid []       = IsValid
allValid (v : vs) = v `andValid` allValid vs

getInvalids :: [Validity] -> [SDoc]
getInvalids vs = [d | NotValid d <- vs]

orValid :: Validity -> Validity -> Validity
orValid IsValid _ = IsValid
orValid _       v = v

