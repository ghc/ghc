
module Utils where

import System.Exit
import System.IO
import Text.Regex.Posix

die :: Errors -> IO a
die errs = do mapM_ (hPutStrLn stderr) errs
              exitFailure

dieOnErrors :: Either Errors a -> IO a
dieOnErrors (Left errs) = die errs
dieOnErrors (Right x) = return x

type Errors = [String]

maybeRead :: Read a => String -> Maybe a
maybeRead str = case reads str of
                [(x, "")] -> Just x
                _ -> Nothing

re :: String -> String -> Maybe [String]
re r str = case matchM r' str :: Maybe (String, String, String, [String]) of
           Just (_, _, _, ms) -> Just ms
           Nothing -> Nothing
    where r' = makeRegex r :: Regex

unSepList :: Eq a => a -> [a] -> [[a]]
unSepList x xs = case break (x ==) xs of
                 (this, _ : xs') ->
                     this : unSepList x xs'
                 (this, []) ->
                     [this]

