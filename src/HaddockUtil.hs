--
-- Haddock - A Haskell Documentation Tool
--
-- (c) The University of Glasgow 2001-2002
-- (c) Simon Marlow 2002
--

module HaddockUtil (

  -- * Misc utilities
  nameOfQName, collectNames, declBinders, declMainBinder, splitTyConApp,
  restrictTo,

  -- * Filename utilities
  basename, dirname, splitFilename3, 
  isPathSeparator, pathSeparator

 ) where

import HsSyn
import List (intersect)

-- -----------------------------------------------------------------------------
-- Some Utilities

nameOfQName (Qual _ n) = n
nameOfQName (UnQual n) = n

collectNames :: [HsDecl] -> [HsName]
collectNames ds = concat (map declBinders ds)

declMainBinder :: HsDecl -> Maybe HsName
declMainBinder d = 
   case d of
     HsTypeDecl _ n _ _          -> Just n
     HsDataDecl _ _ n _ cons _   -> Just n
     HsNewTypeDecl _ _ n _ _ _   -> Just n
     HsClassDecl _ qt decls      -> Just (exQtNm qt)
     HsTypeSig _ [n] _           -> Just n
     HsTypeSig _ ns _            -> error "declMainBinder"
     HsForeignImport _ _ _ _ n _ -> Just n
     _                           -> Nothing

declBinders :: HsDecl -> [HsName]
declBinders d =
   case d of
     HsTypeDecl _ n _ _          -> [n]
     HsDataDecl _ _ n _ cons _   -> n : concat (map conDeclBinders cons)
     HsNewTypeDecl _ _ n _ _ _   -> [n]
     HsClassDecl _ qt decls      -> exQtNm qt : collectNames decls
     HsTypeSig _ ns _            -> ns
     HsForeignImport _ _ _ _ n _ -> [n]
     _                           -> []

conDeclBinders (HsConDecl _ n _ _) = [n]
conDeclBinders (HsRecDecl _ n fields _) = n : concat (map fieldDeclBinders fields)

fieldDeclBinders (HsFieldDecl ns _ _) = ns

exQtNm (HsForAllType _ _ t) = nameOfQName (fst (splitTyConApp t))
exQtNm t = nameOfQName (fst (splitTyConApp t))

splitTyConApp :: HsType -> (HsQName,[HsType])
splitTyConApp t = split t []
 where
	split :: HsType -> [HsType] -> (HsQName,[HsType])
	split (HsTyApp t u) ts = split t (u:ts)
	split (HsTyCon t)   ts = (t,ts)
	split _ _ = error "splitTyConApp"

-- ---------------------------------------------------------------------------
-- Making abstract declarations

restrictTo :: [HsName] -> HsDecl -> HsDecl
restrictTo names decl = case decl of
     HsDataDecl loc ctxt n xs cons drv -> 
	HsDataDecl loc ctxt n xs (restrictCons names cons) drv
     HsNewTypeDecl loc ctxt n xs con drv ->
	HsDataDecl loc ctxt n xs (restrictCons names [con]) drv	
     HsClassDecl loc qt decls  ->
	HsClassDecl loc qt (restrictDecls names decls)
     _ -> decl
   
restrictCons :: [HsName] -> [HsConDecl] -> [HsConDecl]
restrictCons names decls = filter keep decls
  where keep (HsConDecl _ n _ _) = n `elem` names
	keep (HsRecDecl _ n _ _) = n `elem` names
	-- ToDo: records not right

restrictDecls :: [HsName] -> [HsDecl] -> [HsDecl]
restrictDecls names decls = filter keep decls
  where keep d = not (null (declBinders d `intersect` names))
	-- ToDo: not really correct

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
split_longest_prefix s pred
  = case pre of
	[]      -> ([], reverse suf)
	(_:pre) -> (reverse pre, reverse suf)
  where (suf,pre) = break pred (reverse s)

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
