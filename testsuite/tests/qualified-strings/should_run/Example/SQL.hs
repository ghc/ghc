module Example.SQL (
  SqlQuery (..),
  SqlValue (..),
  interpolateString,
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

interpolateString ::
  ( (forall a. ToSqlQuery a => a -> SqlQuery) ->
    (String -> SqlQuery) ->
    (SqlQuery -> SqlQuery -> SqlQuery) ->
    SqlQuery ->
    SqlQuery
  ) ->
  SqlQuery
interpolateString f = f toSqlQuery fromString (<>) mempty
