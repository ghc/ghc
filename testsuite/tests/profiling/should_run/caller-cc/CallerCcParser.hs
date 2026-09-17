-- Unit tests for the CallerCcParser
{-# LANGUAGE StandaloneDeriving #-}
module Main where

import GHC.Core.Opt.CallerCC
import GHC.Unit.Module.Name

deriving instance Eq CallerCcFilter
deriving instance Show CallerCcFilter
deriving instance Eq NamePattern
deriving instance Show NamePattern

runTest :: String -> Maybe CallerCcFilter -> IO ()
runTest filter exp = putStrLn $
  case parseCallerCcFilter filter of
    Left err -> case exp of
                  Nothing -> "Expected failure: " ++ filter ++ " " ++ err
                  Just exp -> "Unexpected failure: " ++ filter ++ " " ++ err
    Right res ->
      case exp of
        Nothing -> "Unexpected parse: " ++ filter ++ " " ++ show res
        Just exp | exp == res -> "Expected parse: " ++ filter ++ " " ++ show res
                 | otherwise  -> "Unexpected parse: " ++ filter ++ show res ++ show exp

mkPattern :: String -> NamePattern
mkPattern s = foldr PChar PEnd s

mkPattern_ :: String -> NamePattern -> NamePattern
mkPattern_ s e = foldr PChar e s

main = do
  runTest "*.map" (Just (CallerCcFilter Nothing (mkPattern "map")))
  runTest "*.parse*" (Just (CallerCcFilter Nothing (mkPattern_ "parse" (PWildcard PEnd))))
  runTest "Data.List.map" (Just (CallerCcFilter (Just (mkModuleName "Data.List")) (mkPattern "map")))
  runTest "*.<\\*>" (Just (CallerCcFilter Nothing (mkPattern "<*>")))
  runTest "as.b" Nothing


