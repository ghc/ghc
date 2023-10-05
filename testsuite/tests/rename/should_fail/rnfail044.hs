-- Renamer test
-- The ambiguity is between the Prelude import and the defn
-- of splitAt. The import of Data.List has nothing to do with it.

module A ( splitAt ) where

  import qualified Data.List
  splitAt = undefined
