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
  addFieldDoc, addFieldDocs, addConDoc, addConDocs,

  -- * Filename utilities
  basename, dirname, splitFilename3, 
  isPathSeparator, pathSeparator,
  moduleHtmlFile,

  -- * Miscellaneous utilities
  getProgramName, bye, die, dieMsg, mapSnd, mapMaybeM,

  -- * HTML cross reference mapping
  html_xrefs_ref, html_xrefs,
 ) where

import HsSyn

import List	( intersect, isSuffixOf )
import Maybe
import IO	( hPutStr, stderr )
import System
import Binary
import Monad

#if __GLASGOW_HASKELL__ < 503
import RegexString
import FiniteMap
import IOExts
#else
import Text.Regex
import Data.FiniteMap
import Data.IORef
import System.IO.Unsafe	 ( unsafePerformIO )
#endif

-- -----------------------------------------------------------------------------
-- Some Utilities

nameOfQName :: HsQName -> HsName
nameOfQName (Qual _ n) = n
nameOfQName (UnQual n) = n

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
	   [] -> HsDataDecl loc ctxt n xs [] drv doc
	   _  -> decl
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

parseModuleHeader :: String -> (String, Maybe ModuleInfo)
parseModuleHeader str =
  case matchRegexAll moduleHeaderRE str of
#if __GLASGOW_HASKELL__ < 503
	Just (_, _, after, _, (_:_:_:s1:s2:s3:_)) -> 
#else
	Just (_, _, after, (_:_:_:s1:s2:s3:_)) -> 
#endif
	   (after, Just (ModuleInfo { 
				 portability = s3,
				 stability   = s2,
				 maintainer  = s1 }))
	_other -> (str, Nothing)

moduleHeaderRE :: Regex
moduleHeaderRE = mkRegexWithOpts
			 "^([ \t\n]*Module[ \t]*:.*\n)?\ 
			  \([ \t\n]*Copyright[ \t]*:.*\n)?\ 
			  \([ \t\n]*License[ \t]*:.*\n)?\ 
			  \[ \t\n]*Maintainer[ \t]*:(.*)\n\ 
			  \[ \t\n]*Stability[ \t]*:(.*)\n\ 
			  \[ \t\n]*Portability[ \t]*:([^\n]*)\n"
		True -- match "\n" with "."
		False -- not case sensitive
	-- All fields except the last (Portability) may be multi-line.
	-- This is so that the portability field doesn't swallow up the
	-- rest of the module documentation - we might want to revist
	-- this at some point (perhaps have a separator between the 
	-- portability field and the module documentation?).

#if __GLASGOW_HASKELL__ < 500
mkRegexWithOpts :: String -> Bool -> Bool -> Regex
mkRegexWithOpts s single_line case_sensitive
      = unsafePerformIO (re_compile_pattern (packString s) 
                              single_line case_sensitive)
#endif

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

moduleHtmlFile :: FilePath -> String -> FilePath
moduleHtmlFile "" mod0  = mod0 ++ ".html" -- ToDo: Z-encode filename?
moduleHtmlFile dir mod0 = dir ++ pathSeparator : mod0 ++ ".html"

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

mapSnd :: (b -> c) -> [(a,b)] -> [(a,c)]
mapSnd _ [] = []
mapSnd f ((x,y):xs) = (x,f y) : mapSnd f xs

mapMaybeM :: Monad m => (a -> m b) -> Maybe a -> m (Maybe b)
mapMaybeM _ Nothing = return Nothing
mapMaybeM f (Just a) = f a >>= return . Just

-----------------------------------------------------------------------------
-- HTML cross references

-- For each module, we need to know where its HTML documentation lives
-- so that we can point hyperlinks to it.  It is extremely
-- inconvenient to plumb this information to all the places that need
-- it (basically every function in HaddockHtml), and furthermore the
-- mapping is constant for any single run of Haddock.  So for the time
-- being I'm going to use a write-once global variable.

{-# NOINLINE html_xrefs_ref #-}
html_xrefs_ref :: IORef (FiniteMap Module FilePath)
html_xrefs_ref = unsafePerformIO (newIORef (error "module_map"))

{-# NOINLINE html_xrefs #-}
html_xrefs :: FiniteMap Module FilePath
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

