{-# LANGUAGE RecordWildCards #-}
module T5892b where

import Data.Version ( Version( Version, versionBranch ))
-- Not importing its field: versionTags

Version{..} = Version [] []
-- Binds versionBranch only

foo = T5892b.versionBranch
bar = T5892b.versionTags
