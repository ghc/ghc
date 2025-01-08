module GHC.Types.ProfAuto
  ( ProfAuto (..)
  )
where

import GHC.Prelude

-- | What kind of {-# SCC #-} to add automatically
data ProfAuto
  = NoProfAuto         -- ^ no SCC annotations added
  | ProfAutoAll        -- ^ top-level and nested functions are annotated
  | ProfAutoTop        -- ^ top-level functions annotated only
  | ProfAutoExports    -- ^ exported functions annotated only
  | ProfAutoCalls      -- ^ annotate call-sites
  deriving (Eq,Enum, Show)
