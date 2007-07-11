--
-- Haddock - A Haskell Documentation Tool
--
-- (c) The University of Glasgow 2001-2002
-- (c) Simon Marlow 2003
--

module Haddock.Utils (

  -- * Misc utilities
  restrictTo, 
  toDescription,

  -- * Filename utilities
  basename, dirname, splitFilename3, 
  moduleHtmlFile, nameHtmlRef,
  contentsHtmlFile, indexHtmlFile, subIndexHtmlFile, pathJoin,
  anchorNameStr,
  cssFile, iconFile, jsFile, plusFile, minusFile,

  -- * Miscellaneous utilities
  getProgramName, bye, die, dieMsg, noDieMsg, mapSnd, mapMaybeM, escapeStr,
  isConSym, isVarSym, nameOccString, moduleString, mkModuleNoPkg,

  -- * HTML cross reference mapping
  html_xrefs_ref,

  -- * HsDoc markup 
  markup, 
  idMarkup,

  -- * Binary extras
--  FormatVersion, mkFormatVersion  
 ) where

import Haddock.Types

import GHC
import SrcLoc
import Name
import OccName
import Binary
import Module
import PackageConfig ( stringToPackageId )

import Control.Monad ( liftM, MonadPlus(..) )
import Data.Map ( Map )
import qualified Data.Map as Map hiding ( Map )
import Data.Char
import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( intersect, isSuffixOf, intersperse )
import Data.Maybe ( maybeToList, fromMaybe, isJust, fromJust )
import Data.Word ( Word8 )
import Data.Bits ( testBit )
import Network.URI
import System.Environment ( getProgName )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hPutStr, stderr )
import System.IO.Unsafe	 ( unsafePerformIO )

-- -----------------------------------------------------------------------------
-- Some Utilities

-- | extract a module's short description.
toDescription :: HaddockModule -> Maybe (HsDoc Name)
toDescription = hmi_description . hmod_info

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
  TyClD d | isClassDecl d -> 
    TyClD (d { tcdSigs = restrictDecls names (tcdSigs d) })
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
        field_avail (ConDeclField n _ _) = (unLoc n) `elem` names
        field_types flds = [ t | ConDeclField _ t _ <- flds ] 
      
    keep d | otherwise = Nothing

restrictDecls :: [Name] -> [LSig Name] -> [LSig Name]
restrictDecls names decls = filter keep decls
  where keep d = fromJust (sigName d) `elem` names
        -- has to have a name, since it's a class method type signature

-- -----------------------------------------------------------------------------
-- Filename mangling functions stolen from s main/DriverUtil.lhs.

type Suffix = String

splitFilename :: String -> (String,Suffix)
splitFilename f = split_longest_prefix f (=='.')

basename :: String -> String
basename f = base where (_dir, base, _suff) = splitFilename3 f

dirname :: String -> String
dirname f = dir where (dir, _base, _suff) = splitFilename3 f

-- "foo/bar/xyzzy.ext" -> ("foo/bar", "xyzzy", ".ext")
splitFilename3 :: String -> (String,String,Suffix)
splitFilename3 str
   = let (dir, rest) = split_longest_prefix str isPathSeparator
	 (name, ext) = splitFilename rest
	 real_dir | null dir  = "."
		  | otherwise = dir
     in  (real_dir, name, ext)

split_longest_prefix :: String -> (Char -> Bool) -> (String,String)
split_longest_prefix s pred0
  = case pre0 of
	[]      -> ([], reverse suf)
	(_:pre) -> (reverse pre, reverse suf)
  where (suf,pre0) = break pred0 (reverse s)

pathSeparator :: Char
#ifdef __WIN32__
pathSeparator = '\\'
#else
pathSeparator = '/'
#endif

isPathSeparator :: Char -> Bool
isPathSeparator ch =
#ifdef mingw32_TARGET_OS
  ch == '/' || ch == '\\'
#else
  ch == '/'
#endif

moduleHtmlFile :: Module -> FilePath
moduleHtmlFile mdl =
  case Map.lookup mdl html_xrefs of
    Nothing  -> mdl' ++ ".html"
    Just fp0 -> pathJoin [fp0, mdl' ++ ".html"]
  where
   mdl' = map (\c -> if c == '.' then '-' else c) 
              (moduleNameString (moduleName mdl))

nameHtmlRef :: Module -> Name -> String	
nameHtmlRef mdl str = moduleHtmlFile mdl ++ '#':escapeStr (anchorNameStr str)

contentsHtmlFile, indexHtmlFile :: String
contentsHtmlFile = "index.html"
indexHtmlFile = "doc-index.html"

subIndexHtmlFile :: Char -> String
subIndexHtmlFile a = "doc-index-" ++ b ++ ".html"
   where b | isAlpha a = [a]
           | otherwise = show (ord a)

anchorNameStr :: Name -> String
anchorNameStr name | isValOcc occName = "v:" ++ getOccString name 
                   | otherwise        = "t:" ++ getOccString name
  where occName = nameOccName name

pathJoin :: [FilePath] -> FilePath
pathJoin = foldr join []
  where join :: FilePath -> FilePath -> FilePath
        join path1 ""    = path1
	join ""    path2 = path2
	join path1 path2
          | isPathSeparator (last path1) = path1++path2
          | otherwise                    = path1++pathSeparator:path2

-- -----------------------------------------------------------------------------
-- Files we need to copy from our $libdir

cssFile, iconFile, jsFile, plusFile,minusFile :: String
cssFile   = "haddock.css"
iconFile  = "haskell_icon.gif"
jsFile    = "haddock.js"
plusFile  = "plus.gif"
minusFile = "minus.gif"

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
mapMaybeM f (Just a) = f a >>= return . Just

escapeStr :: String -> String
#if __GLASGOW_HASKELL__ < 603
escapeStr = flip escapeString unreserved
#else
escapeStr = escapeURIString isUnreserved
#endif

-- there should be a better way to check this using the GHC API
isConSym n = head (nameOccString n) == ':'
isVarSym n = fstChar /= '_' && not (isConSym n) && (not . isLetter) fstChar
  where fstChar = head (nameOccString n)

nameOccString = occNameString . nameOccName 

moduleString :: Module -> String
moduleString = moduleNameString . moduleName 

mkModuleNoPkg :: String -> Module
mkModuleNoPkg str = mkModule (stringToPackageId "") (mkModuleName str)

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
-- put here temporarily

markup :: DocMarkup id a -> HsDoc id -> a
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

markupPair :: DocMarkup id a -> (HsDoc id, HsDoc id) -> (a, a)
markupPair m (a,b) = (markup m a, markup m b)

-- | The identity markup
idMarkup :: DocMarkup a (HsDoc a)
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
  markupURL	      = DocURL,
  markupAName	      = DocAName
  }

-- | Since marking up is just a matter of mapping 'Doc' into some
-- other type, we can \'rename\' documentation by marking up 'Doc' into
-- the same thing, modifying only the identifiers embedded in it.

mapIdent f = idMarkup { markupIdentifier = f }

-----------------------------------------------------------------------------
-- put here temporarily

newtype FormatVersion = FormatVersion Int deriving (Eq,Ord)

nullFormatVersion :: FormatVersion
nullFormatVersion = mkFormatVersion 0

mkFormatVersion :: Int -> FormatVersion
mkFormatVersion i = FormatVersion i

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
