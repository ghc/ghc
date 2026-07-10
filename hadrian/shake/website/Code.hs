{-# LANGUAGE ViewPatterns #-}

-- | Generate Haskell-syntax output complete with hyperlinks to Haddock docs
module Code(code) where

import Data.Char
import Control.Monad.Extra
import System.Directory
import Data.List.Extra
import Data.Maybe
import Text.HTML.TagSoup


-- | Given the location of the haddock --hoogle output, generate something that takes code blocks
--   to HTML pieces.
code :: FilePath -> IO (String -> [Tag String])
code file = do
    unlessM (doesFileExist file) $
        fail "You must run cabal haddock --hoogle to generate the necessary info first"
    r <- resolve file
    pure $ format r


resolve :: FilePath -> IO (String -> [Tag String])
resolve file = do
    src <- readFile file
    let info = catMaybes $ snd $ mapAccumL f "" $ map words $ lines src
    pure $ \x -> fromMaybe [TagText x] $ lookup x info
    where
        f _ ("module":modu:_) = (fix modu, Just (modu, link modu "" "" modu))
        f modu (('(':x):rest) = f modu $ init x : rest
        f modu (('[':x):rest) = f modu $ init x : rest
        f modu (x:"::":_) | x /= "*>" = (modu, Just (x, link modu "v" x x))
        f modu (key:x:_)
            | key `elem` ["type","data","newtype","class"]
            , x `notElem` ["Show","Typeable","Binary","Eq","Hashable","NFData"]
            = (modu, Just (x, link modu "t" x x))
        f modu _ = (modu, Nothing)

        fix "Development.Shake.Command" = "Development.Shake"
        fix x = x

        link modu tag name inner = [TagOpen "a" [("href",url)], TagText inner, TagClose "a"]
            where url = "https://hackage.haskell.org/package/shake/docs/" ++
                        intercalate "-" (wordsBy (== '.') modu) ++ ".html" ++
                        (if name == "" then "" else "#" ++ tag ++ ":" ++ concatMap g name)

        g x | x `elem` "%*-<>/&?=|~" = "-" ++ show (ord x) ++ "-"
        g x = [x]


format :: (String -> [Tag String]) -> String -> [Tag String]
format txt x
    | x == "\\" || any (`isPrefixOf` x) ["#!","3m","shake ","cabal "]
    = [TagText x]
format txt xs = concatMap f $ lexer xs
    where
        f x | x `elem` ["import","do","let"] = spn "key" x
        f [x] | x `elem` "(){}[]\\=|" = spn "sym" [x]
        f x | x `elem` ["->","::","<-"] = spn "sym" x
        f (x:xs) | x `elem` "\"\'" = spn "str" $ x:xs
        f x = txt x

spn cls x = [TagOpen "span" [("class",cls)], TagText x, TagClose "span"]


lexer :: String -> [String]
lexer [] = []
lexer (stripPrefix "Development" -> Just x) =
    let (a,b) = span (\x -> isAlpha x || x == '.') x in ("Development" ++ a) : lexer b
lexer x@(c:_) | isSpace c = let (a,b) = span isSpace x in a : lexer b
lexer (lex -> [(a,b)]) = a : lexer b
lexer (x:xs) = [x] : lexer xs
