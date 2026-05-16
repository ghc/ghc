module Example.SQL (
  SqlQuery (..),
  SqlValue (..),

  -- * String interpolation
  interpolateRaw,
  interpolateValue,
  interpolateAppend,
  interpolateEmpty,
  interpolateFinalize,
) where

import Data.String (IsString (..))

data SqlQuery = SqlQuery
  { sqlText :: String
  , sqlValues :: [SqlValue]
  }
  deriving (Show)

instance IsString SqlQuery where
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

class ToSqlQuery a where
  toSqlQuery :: a -> SqlQuery
instance ToSqlQuery SqlQuery where
  toSqlQuery = id
instance ToSqlQuery String where
  toSqlQuery s = SqlQuery{sqlText = "?", sqlValues = [SqlString s]}
instance ToSqlQuery Int where
  toSqlQuery x = SqlQuery{sqlText = "?", sqlValues = [SqlInt x]}

interpolateRaw :: String -> SqlQuery
interpolateRaw = fromString

interpolateValue :: ToSqlQuery a => a -> SqlQuery
interpolateValue = toSqlQuery

interpolateAppend :: SqlQuery -> SqlQuery -> SqlQuery
interpolateAppend = mappend

interpolateEmpty :: SqlQuery
interpolateEmpty = mempty

interpolateFinalize :: SqlQuery -> SqlQuery
interpolateFinalize = id
