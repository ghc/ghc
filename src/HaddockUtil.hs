--
-- Haddock - A Haskell Documentation Tool
--
-- (c) The University of Glasgow 2001-2002
-- (c) Simon Marlow 2003
--

module HaddockUtil (

  -- * Misc utilities
  nameOfQName, collectNames, declBinders, declMainBinder, declSubBinders, 
  splitTyConApp, restrictTo, declDoc, parseModuleHeader, freeTyCons, unbang,
  addFieldDoc, addFieldDocs, addConDoc, addConDocs,toDescription, unQual,

  -- * Filename utilities
  basename, dirname, splitFilename3, 
  moduleHtmlFile, nameHtmlRef,
  contentsHtmlFile, indexHtmlFile, subIndexHtmlFile, pathJoin,
  cssFile, iconFile, jsFile, plusFile, minusFile,

  -- * Miscellaneous utilities
  getProgramName, bye, die, dieMsg, noDieMsg, mapSnd, mapMaybeM, escapeStr,

  -- * HTML cross reference mapping
  html_xrefs_ref,
 ) where

import Binary
import HaddockLex
import HaddockParse
import HaddockTypes
import HsSyn
import Map ( Map )
import qualified Map hiding ( Map )

import Control.Monad ( liftM, MonadPlus(..) )
import Data.Char ( isAlpha, isSpace, toUpper, ord )
import Data.IORef ( IORef, newIORef, readIORef )
import Data.List ( intersect, isSuffixOf, intersperse )
import Data.Maybe ( maybeToList, fromMaybe )
import Network.URI
import System.Environment ( getProgName )
import System.Exit ( exitWith, ExitCode(..) )
import System.IO ( hPutStr, stderr )
import System.IO.Unsafe	 ( unsafePerformIO )

-- -----------------------------------------------------------------------------
-- Some Utilities

nameOfQName :: HsQName -> HsName
nameOfQName (Qual _ n) = n
nameOfQName (UnQual n) = n

unQual :: HsQName -> HsQName
unQual (Qual _ n) = UnQual n
unQual n = n

collectNames :: [HsDecl] -> [HsName]
collectNames ds = concat (map declBinders ds)

unbang :: HsBangType -> HsType
unbang (HsUnBangedTy ty) = ty
unbang (HsBangedTy   ty) = ty

declBinders :: HsDecl -> [HsName]
declBinders d = maybeToList (declMainBinder d) ++ declSubBinders d

declMainBinder :: HsDecl -> Maybe HsName
declMainBinder d = 
   case d of
     HsTypeDecl _ n _ _ _          -> Just n
     HsDataDecl _ _ n _ _ _ _      -> Just n
     HsNewTypeDecl _ _ n _ _ _  _  -> Just n
     HsClassDecl _ _ n _ _ _ _     -> Just n
     HsTypeSig _ [n] _ _           -> Just n
     HsTypeSig _ _ _ _             -> error "declMainBinder"
     HsForeignImport _ _ _ _ n _ _ -> Just n
     _                             -> Nothing

declSubBinders :: HsDecl -> [HsName]
declSubBinders d =
   case d of
     HsTypeDecl _ _ _ _ _          -> []
     HsDataDecl _ _ _ _ cons _ _   -> concat (map conDeclBinders cons)
     HsNewTypeDecl _ _ _ _ con _ _ -> conDeclBinders con
     HsClassDecl _ _ _ _ _ decls _ -> collectNames decls
     HsTypeSig _ _ _ _             -> []
     HsForeignImport _ _ _ _ _ _ _ -> []
     _                             -> []

conDeclBinders :: HsConDecl -> [HsName]
conDeclBinders (HsConDecl _ n _ _ _ _) = [n]
conDeclBinders (HsRecDecl _ n _ _ fields _) = 
  n : concat (map fieldDeclBinders fields)

fieldDeclBinders :: HsFieldDecl -> [HsName]
fieldDeclBinders (HsFieldDecl ns _ _) = ns

splitTyConApp :: HsType -> (HsQName, [HsType])
splitTyConApp t0 = split t0 []
 where
	split :: HsType -> [HsType] -> (HsQName,[HsType])
	split (HsTyApp t u) ts = split t (u:ts)
	split (HsTyCon t)   ts = (t,ts)
	split _ _ = error "splitTyConApp"

freeTyCons :: HsType -> [HsQName]
freeTyCons ty = go ty []
  where go (HsForAllType _ _ t) r = go t r
	go (HsTyApp t u) r = go t (go u r)
	go (HsTyCon c) r = c : r
	go (HsTyFun f a) r = go f (go a r)
	go (HsTyTuple _ ts) r = foldr go r ts
	go (HsTyVar _) r = r
	go (HsTyDoc t _) r = go t r

-- | extract a module's short description.
toDescription :: Interface -> Maybe Doc
toDescription = description. iface_info

-- -----------------------------------------------------------------------------
-- Adding documentation to record fields (used in parsing).

addFieldDoc :: HsFieldDecl -> Maybe Doc -> HsFieldDecl
addFieldDoc (HsFieldDecl ns ty doc1) doc2 = 
   HsFieldDecl ns ty (doc1 `mplus` doc2)

addFieldDocs :: [HsFieldDecl] -> Maybe Doc -> [HsFieldDecl]
addFieldDocs [] _ = []
addFieldDocs (x:xs) doc = addFieldDoc x doc : xs

addConDoc :: HsConDecl -> Maybe Doc -> HsConDecl
addConDoc (HsConDecl pos nm tvs ctxt typeList doc1) doc2 = 
   HsConDecl pos nm tvs ctxt typeList (doc1 `mplus` doc2)
addConDoc (HsRecDecl pos nm tvs ctxt fields doc1) doc2=
   HsRecDecl pos nm tvs ctxt fields (doc1 `mplus` doc2)

addConDocs :: [HsConDecl] -> Maybe Doc -> [HsConDecl]
addConDocs [] _ = []
addConDocs (x:xs) doc = addConDoc x doc : xs

-- ---------------------------------------------------------------------------
-- Making abstract declarations

restrictTo :: [HsName] -> HsDecl -> HsDecl
restrictTo names decl = case decl of
     HsDataDecl loc ctxt n xs cons drv doc -> 
	HsDataDecl loc ctxt n xs (restrictCons names cons) drv doc
     decl@(HsNewTypeDecl loc ctxt n xs con drv doc) ->
	case restrictCons names [con] of
	   []     -> HsDataDecl loc ctxt n xs [] drv doc
	   [con'] -> HsNewTypeDecl loc ctxt n xs con' drv doc
		-- an abstract newtype decl appears as a data decl.
     HsClassDecl loc ctxt n tys fds decls doc ->
	HsClassDecl loc ctxt n tys fds (restrictDecls names decls) doc
     _ -> decl
   
restrictCons :: [HsName] -> [HsConDecl] -> [HsConDecl]
restrictCons names decls = [ d | Just d <- map keep decls ]
  where keep d@(HsConDecl _ n _ _ _ _)
	  | n `elem` names  = Just d
	keep d@(HsRecDecl loc n tvs ctx fields doc) 
	  | n `elem` names
	  = if all field_avail fields
		then Just d
		else Just (HsConDecl loc n tvs ctx confields doc)
		-- if we have *all* the field names available, then
		-- keep the record declaration.  Otherwise degrade to
		-- a constructor declaration.  This isn't quite right, but
		-- it's the best we can do.
	   where
		field_avail (HsFieldDecl ns _ _) = all (`elem` names) ns
		confields = [ ty | HsFieldDecl ns ty doc <- fields, n <- ns ]
	keep d = Nothing

restrictDecls :: [HsName] -> [HsDecl] -> [HsDecl]
restrictDecls names decls = filter keep decls
  where keep d = not (null (declBinders d `intersect` names))
	-- ToDo: not really correct

-- -----------------------------------------------------------------------------
-- Extract documentation from a declaration

declDoc :: HsDecl -> Maybe Doc
declDoc (HsTypeDecl _ _ _ _ d)          = d
declDoc (HsDataDecl _ _ _ _ _ _ d)      = d
declDoc (HsNewTypeDecl _ _ _ _ _ _ d)   = d
declDoc (HsClassDecl _ _ _ _ _ _ d)     = d
declDoc (HsTypeSig _ _ _ d)             = d
declDoc (HsForeignImport _ _ _ _ _ _ d) = d
declDoc _ = Nothing

-- -----------------------------------------------------------------------------
-- Parsing module headers

-- NB.  The headers must be given in the order Module, Description,
-- Copyright, License, Maintainer, Stability, Portability, except that
-- any or all may be omitted.
parseModuleHeader :: String -> Either String (String,ModuleInfo)
parseModuleHeader str0 = 
   let
      getKey :: String -> String -> (Maybe String,String)
      getKey key str = case parseKey key str of
         Nothing -> (Nothing,str)
         Just (value,rest) -> (Just value,rest)
     
      (moduleOpt,str1) = getKey "Module" str0
      (descriptionOpt,str2) = getKey "Description" str1
      (copyrightOpt,str3) = getKey "Copyright" str2
      (licenseOpt,str4) = getKey "License" str3
      (licenceOpt,str5) = getKey "Licence" str4
      (maintainerOpt,str6) = getKey "Maintainer" str5
      (stabilityOpt,str7) = getKey "Stability" str6
      (portabilityOpt,str8) = getKey "Portability" str7

      description1 :: Either String (Maybe Doc)
      description1 = case descriptionOpt of
         Nothing -> Right Nothing
         Just description -> case parseString . tokenise $ description of
            Left mess -> Left ("Cannot parse Description: " ++ mess)
            Right doc -> Right (Just doc)
   in
      case description1 of
         Left mess -> Left mess
         Right docOpt -> Right (str8,ModuleInfo {
            description = docOpt,
            portability = portabilityOpt,
            stability = stabilityOpt,
            maintainer = maintainerOpt
            })
   

-- | This function is how we read keys.
--
-- all fields in the header are optional and have the form
--
-- [spaces1][field name][spaces] ":" 
--    [text]"\n" ([spaces2][space][text]"\n" | [spaces]"\n")*
-- where each [spaces2] should have [spaces1] as a prefix.
--
-- Thus for the key "Description",
--
-- > Description : this is a
-- >    rather long
-- >
-- >    description
-- >
-- > The module comment starts here
-- 
-- the value will be "this is a .. description" and the rest will begin
-- at "The module comment".
parseKey :: String -> String -> Maybe (String,String)
parseKey key toParse0 =
   do
      let
         (spaces0,toParse1) = extractLeadingSpaces toParse0

         indentation = spaces0
      afterKey0 <- extractPrefix key toParse1
      let
         afterKey1 = extractLeadingSpaces afterKey0
      afterColon0 <- case snd afterKey1 of
         ':':afterColon -> return afterColon
         _ -> Nothing
      let
         (_,afterColon1) = extractLeadingSpaces afterColon0

      return (scanKey True indentation afterColon1)
   where
      scanKey :: Bool -> String -> String -> (String,String)
      scanKey isFirst indentation [] = ([],[])
      scanKey isFirst indentation str =
         let
            (nextLine,rest1) = extractNextLine str

            accept = isFirst || sufficientIndentation || allSpaces

            sufficientIndentation = case extractPrefix indentation nextLine of
               Just (c:_) | isSpace c -> True
               _ -> False

            allSpaces = case extractLeadingSpaces nextLine of
               (_,[]) -> True
               _ -> False
         in
            if accept 
               then
                  let
                     (scanned1,rest2) = scanKey False indentation rest1

                     scanned2 = case scanned1 of 
                        "" -> if allSpaces then "" else nextLine
                        _ -> nextLine ++ "\n" ++ scanned1
                  in
                     (scanned2,rest2)
               else
                  ([],str)

      extractLeadingSpaces :: String -> (String,String)
      extractLeadingSpaces [] = ([],[])
      extractLeadingSpaces (s@(c:cs)) 
         | isSpace c = 
            let
               (spaces1,cs1) = extractLeadingSpaces cs
            in
               (c:spaces1,cs1)
         | True = ([],s)

      extractNextLine :: String -> (String,String)
      extractNextLine [] = ([],[])
      extractNextLine (c:cs) 
         | c == '\n' =
            ([],cs)
         | True =
            let
               (line,rest) = extractNextLine cs
            in
               (c:line,rest)
         

      -- indentation returns characters after last newline.
      indentation :: String -> String
      indentation s = fromMaybe s (indentation0 s)
         where
            indentation0 :: String -> Maybe String
            indentation0 [] = Nothing
            indentation0 (c:cs) =
               case indentation0 cs of
                  Nothing -> if c == '\n' then Just cs else Nothing
                  in0 -> in0
               
      -- comparison is case-insensitive.
      extractPrefix :: String -> String -> Maybe String
      extractPrefix [] s = Just s
      extractPrefix s [] = Nothing
      extractPrefix (c1:cs1) (c2:cs2)
         | toUpper c1 == toUpper c2 = extractPrefix cs1 cs2
         | True = Nothing

-- -----------------------------------------------------------------------------
-- Filename mangling functions stolen from GHC's main/DriverUtil.lhs.

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

moduleHtmlFile :: String -> FilePath
moduleHtmlFile mdl =
  case Map.lookup (Module mdl) html_xrefs of
    Nothing  -> mdl ++ ".html"
    Just fp0 -> pathJoin [fp0, mdl ++ ".html"]

nameHtmlRef :: String -> HsName -> String	
nameHtmlRef mdl str = moduleHtmlFile mdl ++ '#':escapeStr (hsAnchorNameStr str)

contentsHtmlFile, indexHtmlFile :: String
contentsHtmlFile = "index.html"
indexHtmlFile = "doc-index.html"

subIndexHtmlFile :: Char -> String
subIndexHtmlFile a = "doc-index-" ++ b ++ ".html"
   where b | isAlpha a = [a]
           | otherwise = show (ord a)

pathJoin :: [FilePath] -> FilePath
pathJoin = concat . intersperse pathSeparatorStr

pathSeparatorStr :: String
pathSeparatorStr = [pathSeparator]

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
-- Binary instances for stuff

instance Binary Module where
  put_ bh (Module m) = putString bh m
  get bh = do m <- getString bh; return $! (Module m)

instance Binary HsQName where
  put_ bh (Qual m s) = do putByte bh 0; put_ bh m; put_ bh s
  put_ bh (UnQual s) = do putByte bh 1; put_ bh s
  get bh = do b <- getByte bh
	      case b of
		0 -> do m <- get bh; s <- get bh; return (Qual m s)
		_ -> do s <- get bh; return (UnQual s)

instance Binary HsName where
  put_ bh (HsTyClsName s) = do putByte bh 0; put_ bh s
  put_ bh (HsVarName s)   = do putByte bh 1; put_ bh s
  get bh = do b <- getByte bh
	      case b of
		0 -> do s <- get bh; return (HsTyClsName s)
		_ -> do s <- get bh; return (HsVarName s)

instance Binary HsIdentifier where
  put_ bh (HsIdent s)   = do putByte bh 0; putString bh s
  put_ bh (HsSymbol s)  = do putByte bh 1; putString bh s
  put_ bh (HsSpecial s) = do putByte bh 2; putString bh s
  get bh = do b <- getByte bh
	      case b of
		0 -> do s <- getString bh; return (HsIdent s)
		1 -> do s <- getString bh; return (HsSymbol s)
		_ -> do s <- getString bh; return (HsSpecial s)

instance Binary id => Binary (GenDoc id) where
   put_ bh DocEmpty = putByte bh 0
   put_ bh (DocAppend gd1 gd2) = do putByte bh 1;put_ bh gd1;put_ bh gd2
   put_ bh (DocString s) = do putByte bh 2;putString bh s
   put_ bh (DocParagraph gd) = do putByte bh 3;put_ bh gd
   put_ bh (DocIdentifier id) = do putByte bh 4;put_ bh id
   put_ bh (DocModule s) = do putByte bh 5;putString bh s
   put_ bh (DocEmphasis gd) = do putByte bh 6;put_ bh gd
   put_ bh (DocMonospaced gd) = do putByte bh 7;put_ bh gd
   put_ bh (DocUnorderedList gd) = do putByte bh 8;put_ bh gd
   put_ bh (DocOrderedList gd) = do putByte bh 9;put_ bh gd
   put_ bh (DocDefList gd) = do putByte bh 10;put_ bh gd
   put_ bh (DocCodeBlock gd) = do putByte bh 11;put_ bh gd
   put_ bh (DocURL s) = do putByte bh 12;putString bh s
   put_ bh (DocAName s) = do putByte bh 13;putString bh s
   get bh = do b <- getByte bh
               case b of
                  0 -> return DocEmpty
                  1 -> do gd1 <- get bh;gd2 <- get bh;return (DocAppend gd1 gd2)
                  2 -> do s <- getString bh;return (DocString s)
                  3 -> do gd <- get bh;return (DocParagraph gd)
                  4 -> do id <- get bh;return (DocIdentifier id)
                  5 -> do s <- getString bh;return (DocModule s)
                  6 -> do gd <- get bh;return (DocEmphasis gd)
                  7 -> do gd <- get bh;return (DocMonospaced gd)
                  8 -> do gd <- get bh;return (DocUnorderedList gd)
                  9 -> do gd <- get bh;return (DocOrderedList gd)
                  10 -> do gd <- get bh;return (DocDefList gd)
                  11 -> do gd <- get bh;return (DocCodeBlock gd)
                  12 -> do s <- getString bh;return (DocURL s)
                  13 -> do s <- getString bh;return (DocAName s) 
                  _ -> error ("Mysterious byte in document in interface" 
                     ++ show b)


