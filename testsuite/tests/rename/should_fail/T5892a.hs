{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Werror #-}

module T5892a where

import Data.Version ( Version( Version, versionBranch ))
-- Not importing its field: versionTags

foo :: Version -> Version
foo (Version {..}) -- Pattern match does not bind versionTags 
  = let versionBranch = []
    in Version {..}   -- Hence warning here
