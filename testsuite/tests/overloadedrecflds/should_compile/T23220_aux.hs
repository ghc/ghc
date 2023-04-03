module T23220_aux ( makeExtendingDatatype ) where

import Control.Monad ( forM )
import Language.Haskell.TH

-- | @makeExtendingDatatype name extends fields@ generates a record datatype
-- that contains all the fields of @extends@, plus the additional fields in
-- @fields@.
-- e.g.
-- data Foo = { a :: Int }
-- makeExtendingDatatype "bar" [''Foo] [("b", [t| String |])]
-- Will generate
-- data Bar = { a :: Int, b :: String }
makeExtendingDatatype :: String -> [Name] -> [(String, TypeQ)] -> DecsQ
makeExtendingDatatype datatypeNameStr extends fields = do
  extendFields <- fmap concat $ forM extends $ \e -> do
    TyConI (DataD _ _ _ _ [RecC _ eFields] _) <- reify e
    return eFields
  let datatypeName = mkName datatypeNameStr
      constructor = recC datatypeName combinedFields
      userFields = flip map fields $ \(s, typ) -> do
        varBangType (mkName s) (bangType (bang noSourceUnpackedness noSourceStrictness) typ)
      combinedFields = (map pure extendFields) <> userFields
  (\a -> [a]) <$> dataD (cxt []) datatypeName [] Nothing [constructor] []
