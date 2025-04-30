{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StringInterpolation #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Semigroup (Endo (..))
import Data.String (IsString (..))
import Data.String.Interpolate.Experimental

main :: IO ()
main = mapM_ runTest allTests

data TestCase =
  forall a. Show a =>
  TestCase
    { label      :: String
    , expression :: a
    }

runTest :: TestCase -> IO ()
runTest TestCase{..} = do
  putStrLn $ "****************************************"
  putStrLn $ "Input:"
  putStr   $ unlines . map ("    " ++) . lines $ label
  putStrLn $ "====>"
  putStrLn $ "    " ++ show expression

allTests :: [TestCase]
allTests =
  [ TestCase -- custom interpolation implementations
      { label =
          """
          let
            name = "'Robert'; DROP TABLE Students;--" :: String
            age = 10 :: Int
          in
            s"SELECT * FROM tab WHERE name ILIKE ${name} and age = ${age}" :: SqlQuery
          """
      , expression =
          let
            name = "'Robert'; DROP TABLE Students;--" :: String
            age = 10 :: Int
          in
            s"SELECT * FROM tab WHERE name ILIKE ${name} and age = ${age}" :: SqlQuery
      }
  -- TODO(bchinn): overloaded interpolated multiline string
  ]

{----- SQLQuery -----}

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

instance Interpolate SqlQuery SqlQuery where
  interpolate = id
instance Interpolate String SqlQuery where
  interpolate s = SqlQuery{sqlText = "?", sqlValues = [SqlString s]}
instance Interpolate Int SqlQuery where
  interpolate x = SqlQuery{sqlText = "?", sqlValues = [SqlInt x]}
