{-# OPTIONS_GHC -fwarn-unused-imports -fno-warn-missing-methods #-}

-- Check that although 'index' is apparently only used
-- unqualified, we nevertheless do not get a redundant-import warning
--   Trac #3776

module T3776 where

import qualified Data.Ix( Ix(index) )

instance Data.Ix.Ix Float where
  index = error "urk"
