module StringInterpolationQualified_SQL where

import qualified Data.String

data SqlQuery = SqlQuery
  { sqlText :: String
  , sqlValues :: [SqlValue]
  }
  deriving (Show)

instance Data.String.IsString SqlQuery where
  fromString s = SqlQuery{sqlText = s, sqlValues = []}
instance Semigroup SqlQuery where
  q1 <> q2 =
    SqlQuery
      { sqlText = sqlText q1 <> sqlText q2
      , sqlValues = sqlValues q1 <> sqlValues q2
      }
instance Monoid SqlQuery where
  mempty =
    SqlQuery
      { sqlText = ""
      , sqlValues = []
      }

data SqlValue
  = SqlString String
  | SqlInt Int
  deriving (Show)

class ToSqlValue a where
  toSqlValue :: a -> SqlValue
instance ToSqlValue String where
  toSqlValue = SqlString
instance ToSqlValue Int where
  toSqlValue = SqlInt

{----- QualifiedLiterals -----}

fromString :: String -> SqlQuery
fromString = Data.String.fromString

interpolate :: ToSqlValue a => a -> SqlQuery
interpolate v = SqlQuery{sqlText = "?", sqlValues = [toSqlValue v]}

fromParts :: [SqlQuery] -> SqlQuery
fromParts = mconcat
