module B ( module A, test, reExport, X(..) ) where
import A ( A(..), test2, reExport, X(..) )

-- | This link shouldn't work: 'other'.
--   These links should work: 'A.other', 'Data.List.sortBy', 'test2', 'A.test2', 'Data.Maybe.fromMaybe'.
--   Module link: "Prelude".
test :: Int
test = 1
