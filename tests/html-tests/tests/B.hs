module B ( module A, test ) where
import A ( A(..), test2 )

-- | This link shouldn't work: 'other'.
--   These links should work: 'A.other', 'Data.List.sortBy', 'test2', 'A.test2', 'Data.Maybe.fromMaybe'.
test :: Int
test = 1
