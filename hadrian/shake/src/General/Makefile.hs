
module General.Makefile(parseMakefile) where

import qualified Data.ByteString.Char8 as BS
import Data.Char


endsSlash :: BS.ByteString -> Bool
endsSlash = BS.isSuffixOf (BS.singleton '\\')

wordsMakefile :: BS.ByteString -> [BS.ByteString]
wordsMakefile = f . BS.splitWith isSpace
    where
        f (x:xs) | BS.null x = f xs
        f (x:y:xs) | endsSlash x = f $ BS.concat [BS.init x, BS.singleton ' ', y] : xs
        f (x:xs) = x : f xs
        f [] = []

parseMakefile :: BS.ByteString -> [(BS.ByteString, [BS.ByteString])]
parseMakefile = concatMap f . join . linesCR
    where
        join xs = case span endsSlash xs of
            ([], []) -> []
            (xs, []) -> [BS.unwords $ map BS.init xs]
            ([], y:ys) -> y : join ys
            (xs, y:ys) -> BS.unwords (map BS.init xs ++ [y]) : join ys

        f x = [(a, wordsMakefile $ BS.drop 1 b) | a <- wordsMakefile a]
            where (a,b) = BS.break (== ':') $ BS.takeWhile (/= '#') x


-- | This is a hot-spot, so optimised
linesCR :: BS.ByteString -> [BS.ByteString]
linesCR x = case BS.split '\n' x of
    x:xs | Just ('\r',x) <- unsnoc x -> x : map (\x -> case unsnoc x of Just ('\r',x) -> x; _ -> x) xs
    xs -> xs
    where
        -- the ByteString unsnoc was introduced in a newer version
        unsnoc x | BS.null x = Nothing
                 | otherwise = Just (BS.last x, BS.init x)
