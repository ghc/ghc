{-# LANGUAGE RecordWildCards #-}

module Main(main) where

import Data.Tuple.Extra
import Control.Monad
import Data.Char
import Data.List.Extra
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity
import qualified Data.Map as Map
import Text.Markdown
import Text.Blaze.Html.Renderer.Text
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Extra
import Code

data Mode = Debug | Release deriving (Eq,Enum,Bounded,Show)

getMode :: IO Mode
getMode = do
    args <- getArgs
    let modes = [Debug, Release]
    case args of
        [] -> pure Release
        [x] | Just x <- lookup (lower x) $ map (lower . show &&& id) modes -> pure x
            | otherwise -> fail $ "Couldn't recognise argument, got " ++ x ++ ", wanted " ++ show modes
        _ -> fail "Only allowed at most one command line argument"


main :: IO ()
main = do
    mode <- getMode
    createDirectoryIfMissing True "output"
    files <- getDirectoryContents "../docs"
    code <- code "../dist/doc/html/shake/shake.txt"
    skeleton <- skeleton mode "parts" "output/index.css"
    forM_ files $ \file -> case takeExtension file of
        ".md" -> do
            putChar '.'
            p <- readPage mode code $ "../docs" </> file
            skeleton ("output" </> lower (takeBaseName file) <.> "html") p
        ".png" -> copyFile ("../docs" </> file) ("output" </> file)
        _ -> pure ()
    copyFile "parts/favicon.ico" "output/favicon.ico"
    putStrLn " done"


data Link = Link
    {linkLevel :: String
    ,linkTitle :: String
    ,linkKey :: String
    } deriving Show

data Page = Page
    {pageTitle :: String
    ,pageTOC :: [Link]
    ,pageBody :: [Tag String]
    } deriving Show

readFileMarkdown :: FilePath -> IO [Tag String]
readFileMarkdown = fmap (parseTags . T.unpack . renderHtml . markdown def{msXssProtect=False}) . T.readFile

readFileTags :: FilePath -> IO [Tag String]
readFileTags = fmap parseTags . readFile'

writeFileTags :: FilePath -> [Tag String] -> IO ()
writeFileTags file = writeFileUTF8 file . renderTagsOptions renderOptions{optEscape=concatMap (\x -> Map.findWithDefault [x] x escapes)}
    where escapes = Map.fromList $ [(b, "&" ++ a ++ ";") | (a,[b]) <- xmlEntities] ++
                                   [(b, "&" ++ takeWhile (/= ';') a ++ ";") | (a,[b]) <- htmlEntities, not $ isAscii b]


---------------------------------------------------------------------
-- READ A PAGE

readPage :: Mode -> (String -> [Tag String]) -> FilePath -> IO Page
readPage mode code file = do
    (pageTOC, pageBody) <- fmap (links . reformat mode code) $ readFileMarkdown $ "../docs" </> file
    let pageTitle = innerText $ inside "h1" pageBody
    pure Page{..}
    where
        links (TagOpen linkLevel@['h',i] at:xs) | i `elem` "234" =
                ([Link{..} | i /= '4'] ++) *** (prefix++) $ links rest
            where linkTitle = innerText $ takeWhile (/= TagClose linkLevel) xs
                  linkKey = intercalate "-" $ map (lower . filter isAlpha) $ words $
                            takeWhile (`notElem` "?.!") $ dropPrefix "Q: " linkTitle
                  (this,rest) = break (== TagClose linkLevel) xs
                  prefix = [TagOpen "span" [("class","target"),("id",linkKey)],TagClose "span"
                           ,TagOpen linkLevel at,TagOpen "a" [("href",'#':linkKey),("class","anchor")]] ++
                           this ++ [TagClose "a"]
        links (x:xs) = second (x:) $ links xs
        links [] = ([], [])


reformat :: Mode -> (String -> [Tag String]) -> [Tag String] -> [Tag String]
reformat mode code (TagOpen "p" []:TagOpen "i" []:TagText s:xs) | "See also" `isPrefixOf` s =
    reformat mode code $ drop1 $ dropWhile (~/= "</p>") xs
reformat mode code (TagOpen "a" at:xs) = TagOpen "a" (map f at) : reformat mode code xs
    where f ("href",x) | ".md" `isPrefixOf` takeExtension x =
                -- watch out for Manual.md#readme
                ("href", noReadme $ dropFileName x ++ lower (takeBaseName x) ++
                         (if mode == Release then "" else ".html") ++
                         drop 3 (takeExtension x))
          f x = x
reformat mode code (TagOpen "pre" []:TagOpen "code" []:xs) = reformat mode code $ TagOpen "pre" [] : xs
reformat mode code (TagClose "code":TagClose "pre":xs) = reformat mode code $ TagClose "pre" : xs
reformat mode code (TagOpen t at:xs) | t `elem` ["pre","code"] = TagOpen t at : concatMap f a ++ reformat mode code b
    where (a,b) = break (== TagClose t) xs
          skip = TagComment " nosyntax " `elem` a || notCode (innerText a)
          f (TagText x) | not skip = code x
          f x = [x]
reformat mode code (TagClose x:xs) | x `elem` ["p","pre","li","ol","ul","h1","h2","h3","h4","h5","h6"] =
    TagClose x : TagText "\n" : reformat mode code xs
reformat mode code (TagText x:xs) = TagText (replace " -- " " \x2013 " x) : reformat mode code xs
    -- properly render the ASCII fallback "--" as Unicode's EN DASH (U+2013)
reformat mode code (x:xs) = x : reformat mode code xs
reformat mode code [] = []

noReadme = dropSuffix "#readme"

notCode :: String -> Bool
notCode x =
    "stack " `isPrefixOf` x ||
    "shake-" `isPrefixOf` x ||
    ("--" `isPrefixOf` x && length (lines x) == 1) ||
    x == "shake" ||
    (let t = takeExtension x in "." `isPrefixOf` t && all isAlpha (drop1 t))


---------------------------------------------------------------------
-- POPULATE A SKELETON

skeleton :: Mode -> FilePath -> FilePath -> IO (FilePath -> Page -> IO ())
skeleton mode dir cssOut = do
    common <- readFile' $ dir </> "index.css"
    header <- readFileTags $ dir </> "header.html"
    content <- readFileTags $ dir </> "content.html"
    footer <- readFileTags $ dir </> "footer.html"
    writeFile cssOut $ common ++ style header ++ style content ++ style footer
    pure $ \file Page{..} -> writeFileTags file $
        inject (takeBaseName file) (takeWhile (~/= "<div id=content>") (remode $ map (activate $ takeFileName file) $ noStyle header)) ++
        parseTags "<div id=content>" ++
        (if length pageTOC <= 1 then [] else
            parseTags "<div id=toc>" ++
            concat [ [TagOpen "a" [("class",linkLevel),("href",'#':linkKey)], TagText linkTitle, TagClose "a"]
                   | Link{..} <- pageTOC] ++
            parseTags "</div>") ++
        pageBody ++
        parseTags "</div>" ++
        dropWhile (~/= "<p id=footer>") footer
    where
        remode xs = if mode == Debug then xs else map f xs
            where f (TagOpen "a" at) = TagOpen "a" $ flip map at $ second $ \v ->
                      if v == "index.html" then "."
                      else if takeExtension v == ".html" then dropExtension v else v
                  f x = x

        style = innerText . inside "style"
        noStyle x = a ++ drop1 (dropWhile (~/= "</style>") b)
            where (a,b) = break (~== "<style>") x

        activate url (TagOpen "a" ats) = TagOpen "a" $ let act = ("class","active") in
            [act | ("href",url) `elem` ats] ++ delete act ats
        activate url x = x

        inject name (TagOpen "body" at:xs) = TagOpen "body" (("class","page-"++name):at) : inject name xs
        inject name (x:xs) = x : inject name xs
        inject name [] = []


inside :: String -> [Tag String] -> [Tag String]
inside tag = takeWhile (~/= TagClose tag) . dropWhile (~/= TagOpen tag [])
