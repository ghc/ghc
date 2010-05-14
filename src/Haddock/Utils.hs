-----------------------------------------------------------------------------
-- |
-- Module      :  Haddock.Utils
-- Copyright   :  (c) The University of Glasgow 2001-2002,
--                    Simon Marlow 2003-2006,
--                    David Waern  2006-2009
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
module Haddock.Utils (

  -- * Misc utilities
  restrictTo, 
  toDescription, toInstalledDescription,

  -- * Filename utilities
  moduleHtmlFile, nameHtmlRef,
  contentsHtmlFile, indexHtmlFile,
  frameIndexHtmlFile,
  moduleIndexFrameName, mainFrameName, synopsisFrameName,
  subIndexHtmlFile,
  anchorNameStr,
  cssFile, iconFile, jsFile, plusFile, minusFile, framesFile,

  -- * Miscellaneous utilities
  getProgramName, bye, die, dieMsg, noDieMsg, mapSnd, mapMaybeM, escapeStr,
 
  -- * HTML cross reference mapping
  html_xrefs_ref,

  -- * Doc markup 
  markup, 
  idMarkup,

  -- * List utilities
  replace,

  -- * MTL stuff
  MonadIO(..),
  
  -- * Logging
  parseVerbosity,
  out
 ) where


import Haddock.Types
import Haddock.GhcUtils

import GHC
import Name

import Control.Monad ( liftM )
import Data.Char ( isAlpha, ord, chr )
import Numeric ( showIntAtBase )
import Data.Map ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( isSuffixOf )
import Data.Maybe ( fromJust )
import System.Environment ( getProgName )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hPutStr, stderr )
import System.IO.Unsafe	 ( unsafePerformIO )
import System.FilePath
import Distribution.Verbosity
import Distribution.ReadE

import MonadUtils ( MonadIO(..) )


-- -----------------------------------------------------------------------------
-- Logging


parseVerbosity :: String -> Either String Verbosity
parseVerbosity = runReadE flagToVerbosity


-- | Print a message to stdout, if it is not too verbose
out :: MonadIO m
    => Verbosity -- ^ program verbosity
    -> Verbosity -- ^ message verbosity
    -> String -> m ()
out progVerbosity msgVerbosity msg
  | msgVerbosity <= progVerbosity = liftIO $ putStrLn msg
  | otherwise = return ()


-- -----------------------------------------------------------------------------
-- Some Utilities


-- | Extract a module's short description.
toDescription :: Interface -> Maybe (Doc Name)
toDescription = hmi_description . ifaceInfo


-- | Extract a module's short description.
toInstalledDescription :: InstalledInterface -> Maybe (Doc Name)
toInstalledDescription = hmi_description . instInfo


-- ---------------------------------------------------------------------------
-- Making abstract declarations


restrictTo :: [Name] -> (LHsDecl Name) -> (LHsDecl Name)
restrictTo names (L loc decl) = L loc $ case decl of
  TyClD d | isDataDecl d && tcdND d == DataType -> 
    TyClD (d { tcdCons = restrictCons names (tcdCons d) }) 
  TyClD d | isDataDecl d && tcdND d == NewType -> 
    case restrictCons names (tcdCons d) of
      []    -> TyClD (d { tcdND = DataType, tcdCons = [] }) 
      [con] -> TyClD (d { tcdCons = [con] })
      _ -> error "Should not happen"
  TyClD d | isClassDecl d -> 
    TyClD (d { tcdSigs = restrictDecls names (tcdSigs d),
               tcdATs = restrictATs names (tcdATs d) })
  _ -> decl

   
restrictCons :: [Name] -> [LConDecl Name] -> [LConDecl Name]
restrictCons names decls = [ L p d | L p (Just d) <- map (fmap keep) decls ]  
  where 
    keep d | unLoc (con_name d) `elem` names = 
      case con_details d of
        PrefixCon _ -> Just d
        RecCon fields  
          | all field_avail fields -> Just d
          | otherwise -> Just (d { con_details = PrefixCon (field_types fields) })
          -- if we have *all* the field names available, then
          -- keep the record declaration.  Otherwise degrade to
          -- a constructor declaration.  This isn't quite right, but
          -- it's the best we can do.
        InfixCon _ _ -> Just d
      where
        field_avail (ConDeclField n _ _) = unLoc n `elem` names
        field_types flds = [ t | ConDeclField _ t _ <- flds ] 
      
    keep _ | otherwise = Nothing


restrictDecls :: [Name] -> [LSig Name] -> [LSig Name]
restrictDecls names decls = filter keep decls
  where keep d = fromJust (sigName d) `elem` names
        -- has to have a name, since it's a class method type signature


restrictATs :: [Name] -> [LTyClDecl Name] -> [LTyClDecl Name]
restrictATs names ats = [ at | at <- ats , tcdName (unL at) `elem` names ]


-- -----------------------------------------------------------------------------
-- Filename mangling functions stolen from s main/DriverUtil.lhs.


moduleHtmlFile :: Module -> FilePath
moduleHtmlFile mdl =
  case Map.lookup mdl html_xrefs of
    Nothing  -> mdl' ++ ".html"
    Just fp0 -> joinPath [fp0, mdl' ++ ".html"]
  where
   mdl' = map (\c -> if c == '.' then '-' else c) 
              (moduleNameString (moduleName mdl))


nameHtmlRef :: Module -> OccName -> String	
nameHtmlRef mdl n = moduleHtmlFile mdl ++ '#':escapeStr (anchorNameStr n)


contentsHtmlFile, indexHtmlFile :: String
contentsHtmlFile = "index.html"
indexHtmlFile = "doc-index.html"


-- | The name of the module index file to be displayed inside a frame.
-- Modules are display in full, but without indentation.  Clicking opens in
-- the main window.
frameIndexHtmlFile :: String
frameIndexHtmlFile = "index-frames.html"


moduleIndexFrameName, mainFrameName, synopsisFrameName :: String
moduleIndexFrameName = "modules"
mainFrameName = "main"
synopsisFrameName = "synopsis"


subIndexHtmlFile :: Char -> String
subIndexHtmlFile a = "doc-index-" ++ b ++ ".html"
   where b | isAlpha a = [a]
           | otherwise = show (ord a)


anchorNameStr :: OccName -> String
anchorNameStr name | isValOcc name = "v:" ++ occNameString name 
                   | otherwise     = "t:" ++ occNameString name


-- -----------------------------------------------------------------------------
-- Files we need to copy from our $libdir


cssFile, iconFile, jsFile, plusFile, minusFile, framesFile :: String
cssFile   = "haddock.css"
iconFile  = "haskell_icon.gif"
jsFile    = "haddock-util.js"
plusFile  = "plus.gif"
minusFile = "minus.gif"
framesFile = "frames.html"


-----------------------------------------------------------------------------
-- misc.


getProgramName :: IO String
getProgramName = liftM (`withoutSuffix` ".bin") getProgName
   where str `withoutSuffix` suff
            | suff `isSuffixOf` str = take (length str - length suff) str
            | otherwise             = str


bye :: String -> IO a
bye s = putStr s >> exitWith ExitSuccess


die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)


dieMsg :: String -> IO a
dieMsg s = getProgramName >>= \prog -> die (prog ++ ": " ++ s)


noDieMsg :: String -> IO ()
noDieMsg s = getProgramName >>= \prog -> hPutStr stderr (prog ++ ": " ++ s)


mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd _ [] = []
mapSnd f ((x,y):xs) = (x,f y) : mapSnd f xs


mapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMaybeM _ Nothing = return Nothing
mapMaybeM f (Just a) = liftM Just (f a)


escapeStr :: String -> String
escapeStr = escapeURIString isUnreserved


-- Following few functions are copy'n'pasted from Network.URI module
-- to avoid depending on the network lib, since doing so gives a
-- circular build dependency between haddock and network
-- (at least if you want to build network with haddock docs)

escapeURIChar :: (Char -> Bool) -> Char -> String
escapeURIChar p c
    | p c       = [c]
    | otherwise = '%' : myShowHex (ord c) ""
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 toChrHex n r of
            []  -> "00"
            [a] -> ['0',a]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))


escapeURIString :: (Char -> Bool) -> String -> String
escapeURIString = concatMap . escapeURIChar


isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `elem` "-_.~")


isAlphaChar, isDigitChar, isAlphaNumChar :: Char -> Bool
isAlphaChar c    = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
isDigitChar c    = c >= '0' && c <= '9'
isAlphaNumChar c = isAlphaChar c || isDigitChar c


-----------------------------------------------------------------------------
-- HTML cross references

-- For each module, we need to know where its HTML documentation lives
-- so that we can point hyperlinks to it.  It is extremely
-- inconvenient to plumb this information to all the places that need
-- it (basically every function in HaddockHtml), and furthermore the
-- mapping is constant for any single run of Haddock.  So for the time
-- being I'm going to use a write-once global variable.


{-# NOINLINE html_xrefs_ref #-}
html_xrefs_ref :: IORef (Map Module FilePath)
html_xrefs_ref = unsafePerformIO (newIORef (error "module_map"))


{-# NOINLINE html_xrefs #-}
html_xrefs :: Map Module FilePath
html_xrefs = unsafePerformIO (readIORef html_xrefs_ref)


-----------------------------------------------------------------------------
-- List utils
-----------------------------------------------------------------------------


replace :: Eq a => a -> a -> [a] -> [a]
replace a b = map (\x -> if x == a then b else x) 


-----------------------------------------------------------------------------
-- put here temporarily


markup :: DocMarkup id a -> Doc id -> a
markup m DocEmpty              = markupEmpty m
markup m (DocAppend d1 d2)     = markupAppend m (markup m d1) (markup m d2)
markup m (DocString s)         = markupString m s
markup m (DocParagraph d)      = markupParagraph m (markup m d)
markup m (DocIdentifier ids)   = markupIdentifier m ids
markup m (DocModule mod0)      = markupModule m mod0
markup m (DocEmphasis d)       = markupEmphasis m (markup m d)
markup m (DocMonospaced d)     = markupMonospaced m (markup m d)
markup m (DocUnorderedList ds) = markupUnorderedList m (map (markup m) ds)
markup m (DocOrderedList ds)   = markupOrderedList m (map (markup m) ds)
markup m (DocDefList ds)       = markupDefList m (map (markupPair m) ds)
markup m (DocCodeBlock d)      = markupCodeBlock m (markup m d)
markup m (DocURL url)          = markupURL m url
markup m (DocAName ref)        = markupAName m ref
markup m (DocPic img)          = markupPic m img
markup m (DocExamples e)       = markupExample m e


markupPair :: DocMarkup id a -> (Doc id, Doc id) -> (a, a)
markupPair m (a,b) = (markup m a, markup m b)


-- | The identity markup
idMarkup :: DocMarkup a (Doc a)
idMarkup = Markup {
  markupEmpty         = DocEmpty,
  markupString        = DocString,
  markupParagraph     = DocParagraph,
  markupAppend        = DocAppend,
  markupIdentifier    = DocIdentifier,
  markupModule        = DocModule,
  markupEmphasis      = DocEmphasis,
  markupMonospaced    = DocMonospaced,
  markupUnorderedList = DocUnorderedList,
  markupOrderedList   = DocOrderedList,
  markupDefList       = DocDefList,
  markupCodeBlock     = DocCodeBlock,
  markupURL           = DocURL,
  markupAName         = DocAName,
  markupPic           = DocPic,
  markupExample       = DocExamples
  }


-----------------------------------------------------------------------------
-- put here temporarily


newtype FormatVersion = FormatVersion Int deriving (Eq,Ord)


nullFormatVersion :: FormatVersion
nullFormatVersion = mkFormatVersion 0


mkFormatVersion :: Int -> FormatVersion
mkFormatVersion = FormatVersion


instance Binary FormatVersion where
   put_ bh (FormatVersion i) =
      case compare i 0 of
         EQ -> return ()
         GT -> put_ bh (-i)
         LT -> error (
            "Binary.hs: negative FormatVersion " ++ show i 
               ++ " is not allowed")
   get bh =
      do
         (w8 :: Word8) <- get bh   
         if testBit w8 7
            then
               do
                  i <- get bh
                  return (FormatVersion (-i))
            else
               return nullFormatVersion
