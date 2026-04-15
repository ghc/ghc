{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE QualifiedStrings #-}
{-# LANGUAGE StringInterpolation #-}

import Data.Typeable (Typeable, typeOf)
import qualified Example.SQL as SQL

main :: IO ()
main = do
  let
    name = "'Robert'; DROP TABLE Students;--" :: String
    age = 10 :: Int

  inspect SQL.s"SELECT * FROM tab WHERE name ILIKE ${name} AND age > ${age}"

  -- FIXME(bchinn)
  -- inspect SQL.s"""
  --   SELECT *
  --   FROM tab
  --   WHERE name ILIKE ${name} AND age > ${age}
  -- """

inspect :: (Typeable a, Show a) => a -> IO ()
inspect a = do
  putStrLn $ ">>> " ++ show a
  putStrLn $ show $ typeOf a
