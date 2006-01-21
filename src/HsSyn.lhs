% -----------------------------------------------------------------------------
% $Id: HsSyn.lhs,v 1.22 2004/08/09 11:55:07 simonmar Exp $
%
% (c) The GHC Team, 1997-2002
%
% A suite of datatypes describing the abstract syntax of Haskell 98.
%
% -----------------------------------------------------------------------------

\begin{code}
module HsSyn (
    SrcLoc(..), Module(..), HsQName(..), HsName(..), HsIdentifier(..),
    HsModule(..), HsExportSpec(..),  ModuleInfo(..),
    HsImportDecl(..), HsImportSpec(..), HsAssoc(..),
    HsDecl(..), HsMatch(..), HsConDecl(..), HsFieldDecl(..), 
    HsBangType(..), HsRhs(..),
    HsGuardedRhs(..), HsType(..), HsContext, HsAsst, HsIPContext, HsCtxt(..),
    HsLiteral(..), HsExp(..), HsPat(..), HsPatField(..), HsStmt(..),
    HsFieldUpdate(..), HsAlt(..), HsGuardedAlts(..), HsGuardedAlt(..),
    HsCallConv(..), HsFISafety(..), HsFunDep,

    mkHsForAllType,

    prelude_mod, main_mod, 
    unit_con_name, tuple_con_name, nil_con_name,
    as_name, qualified_name, hiding_name, minus_name, pling_name, dot_name,
    forall_name, unsafe_name, safe_name, threadsafe_name, export_name,
    stdcall_name, ccall_name, dotnet_name,
    unit_tycon_name, fun_tycon_name, list_tycon_name, tuple_tycon_name,
    unit_tycon_qname, fun_tycon_qname, list_tycon_qname, tuple_tycon_qname,
    unit_tycon, fun_tycon, list_tycon, tuple_tycon,

    emptyModuleInfo,

    hsIdentifierStr, hsAnchorNameStr, hsNameStr, 
	
    GenDoc(..), Doc, DocMarkup(..),
    markup, mapIdent, idMarkup,
    docAppend, docParagraph,
  ) where

import Char (isSpace)

data SrcLoc = SrcLoc !Int !Int FilePath -- (Line, Indentation, FileName)
  deriving (Eq,Ord,Show)

newtype Module = Module String
  deriving (Eq,Ord)

instance Show Module where
   showsPrec _ (Module m) = showString m

data HsQName
	= Qual Module HsName
	| UnQual HsName
  deriving (Eq,Ord)

instance Show HsQName where
   showsPrec _ (Qual (Module m) s) = 
	showString m . showString "." . shows s
   showsPrec _ (UnQual s) = shows s

data HsName 
	= HsTyClsName HsIdentifier
	| HsVarName HsIdentifier
  deriving (Eq,Ord)

instance Show HsName where
   showsPrec p (HsTyClsName i) = showsPrec p i
   showsPrec p (HsVarName i)   = showsPrec p i

data HsIdentifier
	= HsIdent   String
	| HsSymbol  String
	| HsSpecial String
  deriving (Eq,Ord)

instance Show HsIdentifier where
   showsPrec _ (HsIdent s) = showString s
   showsPrec _ (HsSymbol s) = showString s
   showsPrec _ (HsSpecial s) = showString s

data HsModule = HsModule SrcLoc Module (Maybe [HsExportSpec])
                        [HsImportDecl] [HsDecl] 
			(Maybe String)		-- the doc options
			ModuleInfo      	-- the info (portability etc.)
                        (Maybe Doc)             -- the module doc.
  deriving Show

data ModuleInfo = ModuleInfo
	{ description :: Maybe Doc,
          portability :: Maybe String,
	  stability   :: Maybe String,
	  maintainer  :: Maybe String
          }
  deriving Show

emptyModuleInfo :: ModuleInfo
emptyModuleInfo = ModuleInfo {
   description = Nothing,
   portability = Nothing,
   stability = Nothing,
   maintainer = Nothing
   }

-- Export/Import Specifications

data HsExportSpec
	 = HsEVar HsQName			-- variable
	 | HsEAbs HsQName			-- T
	 | HsEThingAll HsQName			-- T(..)
	 | HsEThingWith HsQName [HsQName]	-- T(C_1,...,C_n)
	 | HsEModuleContents Module		-- module M   (not for imports)
	 | HsEGroup Int Doc			-- a doc section heading
	 | HsEDoc Doc				-- some documentation
	 | HsEDocNamed String			-- a reference to named doc
  deriving (Eq,Show)

data HsImportDecl
	 = HsImportDecl SrcLoc Module Bool (Maybe Module)
	                (Maybe (Bool,[HsImportSpec]))
  deriving (Eq,Show)

data HsImportSpec
	 = HsIVar HsName			-- variable
	 | HsIAbs HsName			-- T
	 | HsIThingAll HsName		-- T(..)
	 | HsIThingWith HsName [HsName]	-- T(C_1,...,C_n)
  deriving (Eq,Show)

data HsAssoc
	 = HsAssocNone
	 | HsAssocLeft
	 | HsAssocRight
  deriving (Eq,Show)

data HsFISafety
 	= HsFIUnsafe
	| HsFISafe
	| HsFIThreadSafe
  deriving (Eq,Show)

data HsCallConv
	= HsCCall
	| HsStdCall
	| HsDotNetCall
  deriving (Eq,Show)

data HsDecl
  = HsTypeDecl SrcLoc HsName [HsName] HsType (Maybe Doc)
 
  | HsDataDecl SrcLoc HsContext HsName [HsName] [HsConDecl] [HsQName]
 		  (Maybe Doc)
 
  | HsInfixDecl SrcLoc HsAssoc Int [HsName]
 
  | HsNewTypeDecl SrcLoc HsContext HsName [HsName] HsConDecl [HsQName]
 		  (Maybe Doc)
 
  | HsClassDecl SrcLoc HsContext HsName [HsName] [HsFunDep] [HsDecl] (Maybe Doc)
 
  | HsInstDecl SrcLoc HsContext HsAsst [HsDecl]
 
  | HsDefaultDecl SrcLoc [HsType]
 
  | HsTypeSig SrcLoc [HsName] HsType (Maybe Doc)
 
  | HsFunBind [HsMatch]
 
  | HsPatBind SrcLoc HsPat HsRhs {-where-} [HsDecl]
 
  | HsForeignImport SrcLoc HsCallConv HsFISafety String HsName HsType
 		    (Maybe Doc)
 
  | HsForeignExport SrcLoc HsCallConv String HsName HsType
 
  | HsDocCommentNext  SrcLoc Doc	-- a documentation annotation
  | HsDocCommentPrev  SrcLoc Doc	-- a documentation annotation
  | HsDocCommentNamed SrcLoc String Doc	-- a documentation annotation
  | HsDocGroup        SrcLoc Int Doc	-- a documentation group
  deriving (Eq,Show)

data HsMatch 
	 = HsMatch SrcLoc HsQName [HsPat] HsRhs {-where-} [HsDecl]
  deriving (Eq,Show)

data HsConDecl
     = HsConDecl SrcLoc HsName [HsName] HsContext [HsBangType] (Maybe Doc)
     | HsRecDecl SrcLoc HsName [HsName] HsContext [HsFieldDecl] (Maybe Doc)
  deriving (Eq,Show)

data HsFieldDecl
	= HsFieldDecl [HsName] HsBangType (Maybe Doc)
  deriving (Eq,Show)

data HsBangType
	 = HsBangedTy   HsType
	 | HsUnBangedTy HsType
  deriving (Eq,Show)

data HsRhs
	 = HsUnGuardedRhs HsExp
	 | HsGuardedRhss  [HsGuardedRhs]
  deriving (Eq,Show)

data HsGuardedRhs
	 = HsGuardedRhs SrcLoc [HsStmt] HsExp
  deriving (Eq,Show)

data HsType
	 = HsForAllType (Maybe [HsName]) HsIPContext HsType
	 | HsTyFun   HsType HsType
	 | HsTyTuple Bool{-boxed-} [HsType]
	 | HsTyApp   HsType HsType
	 | HsTyVar   HsName
	 | HsTyCon   HsQName
	 | HsTyDoc   HsType Doc
	 | HsTyIP    HsName HsType
  deriving (Eq,Show)

type HsFunDep    = ([HsName], [HsName])
type HsContext   = [HsAsst]
type HsIPContext = [HsCtxt]

data HsCtxt
 = HsAssump HsAsst	-- for multi-parameter type classes
 | HsIP     HsName HsType
  deriving (Eq,Show)

type HsAsst = (HsQName,[HsType])

data HsLiteral
	= HsInt		Integer
	| HsChar	Char
	| HsString	String
	| HsFrac	Rational
	-- GHC unboxed literals:
	| HsCharPrim	Char
	| HsStringPrim	String
	| HsIntPrim	Integer
	| HsFloatPrim	Rational
	| HsDoublePrim	Rational
  deriving (Eq, Show)

data HsExp
	= HsIPVar HsQName
	| HsVar HsQName
	| HsCon HsQName
	| HsLit HsLiteral
	| HsInfixApp HsExp HsExp HsExp
	| HsApp HsExp HsExp
	| HsNegApp HsExp
	| HsLambda [HsPat] HsExp
	| HsLet [HsDecl] HsExp
	| HsIf HsExp HsExp HsExp
	| HsCase HsExp [HsAlt]
	| HsDo [HsStmt]
	| HsTuple Bool{-boxed-} [HsExp]
	| HsList [HsExp]
	| HsParen HsExp
	| HsLeftSection HsExp HsExp
	| HsRightSection HsExp HsExp
	| HsRecConstr HsQName [HsFieldUpdate]
	| HsRecUpdate HsExp [HsFieldUpdate]
	| HsEnumFrom HsExp
	| HsEnumFromTo HsExp HsExp
	| HsEnumFromThen HsExp HsExp
	| HsEnumFromThenTo HsExp HsExp HsExp
	| HsListComp HsExp [HsStmt]
	| HsExpTypeSig SrcLoc HsExp HsType
	| HsAsPat HsName HsExp		-- pattern only
	| HsWildCard			-- ditto
	| HsIrrPat HsExp		-- ditto
	-- HsCCall                         (ghc extension)
	-- HsSCC			   (ghc extension)
 deriving (Eq,Show)

data HsPat
	= HsPVar HsName
	| HsPLit HsLiteral
	| HsPNeg HsPat
	| HsPInfixApp HsPat HsQName HsPat
	| HsPApp HsQName [HsPat]
	| HsPTuple Bool{-boxed-} [HsPat]
	| HsPList [HsPat]
	| HsPParen HsPat
	| HsPRec HsQName [HsPatField]
	| HsPAsPat HsName HsPat
	| HsPWildCard
	| HsPIrrPat HsPat
	| HsPTypeSig HsPat HsType
 deriving (Eq,Show)

data HsPatField
	= HsPFieldPat HsQName HsPat
 deriving (Eq,Show)

data HsStmt
	= HsGenerator HsPat HsExp
	| HsParStmt [HsStmt]
	| HsQualifier HsExp
	| HsLetStmt [HsDecl]
 deriving (Eq,Show)

data HsFieldUpdate
	= HsFieldUpdate HsQName HsExp
  deriving (Eq,Show)

data HsAlt
	= HsAlt SrcLoc HsPat HsGuardedAlts [HsDecl]
  deriving (Eq,Show)

data HsGuardedAlts
	= HsUnGuardedAlt HsExp
	| HsGuardedAlts  [HsGuardedAlt]
  deriving (Eq,Show)

data HsGuardedAlt
	= HsGuardedAlt SrcLoc [HsStmt] HsExp
  deriving (Eq,Show)

-----------------------------------------------------------------------------
-- Smart constructors

-- pinched from GHC
mkHsForAllType :: Maybe [HsName] -> HsIPContext -> HsType -> HsType
mkHsForAllType (Just []) [] ty = ty	-- Explicit for-all with no tyvars
mkHsForAllType mtvs1     [] (HsForAllType mtvs2 ctxt ty)
 = mkHsForAllType (mtvs1 `plus` mtvs2) ctxt ty
 where
    mtvs        `plus` Nothing     = mtvs
    Nothing     `plus` mtvs        = mtvs
    (Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)
mkHsForAllType tvs ctxt ty = HsForAllType tvs ctxt ty

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod, main_mod :: Module
prelude_mod	      = Module "Prelude"
main_mod	      = Module "Main"

unit_ident, nil_ident :: HsIdentifier
unit_ident	      = HsSpecial "()"
nil_ident	      = HsSpecial "[]"

tuple_ident :: Int -> HsIdentifier
tuple_ident i	      = HsSpecial ("("++replicate i ','++")")

unit_con_name, nil_con_name :: HsQName
unit_con_name	      = Qual prelude_mod (HsVarName unit_ident)
nil_con_name	      = Qual prelude_mod (HsVarName nil_ident)

tuple_con_name :: Int -> HsQName
tuple_con_name i      = Qual prelude_mod (HsVarName (tuple_ident i))

as_name, qualified_name, hiding_name, unsafe_name, safe_name
       , forall_name, threadsafe_name, export_name, ccall_name, stdcall_name
       , dotnet_name, minus_name, pling_name, dot_name :: HsName

as_name	              = HsVarName (HsIdent "as")
qualified_name        = HsVarName (HsIdent "qualified")
hiding_name	      = HsVarName (HsIdent "hiding")
unsafe_name	      = HsVarName (HsIdent "unsafe")
safe_name	      = HsVarName (HsIdent "safe")
forall_name	      = HsVarName (HsIdent "forall")
threadsafe_name	      = HsVarName (HsIdent "threadsafe")
export_name	      = HsVarName (HsIdent "export")
ccall_name	      = HsVarName (HsIdent "ccall")
stdcall_name	      = HsVarName (HsIdent "stdcall")
dotnet_name	      = HsVarName (HsIdent "dotnet")
minus_name	      = HsVarName (HsSymbol "-")
pling_name	      = HsVarName (HsSymbol "!")
dot_name	      = HsVarName (HsSymbol ".")

unit_tycon_name, fun_tycon_name, list_tycon_name :: HsName

unit_tycon_name       = HsTyClsName unit_ident
fun_tycon_name        = HsTyClsName (HsSpecial "->")
list_tycon_name       = HsTyClsName (HsSpecial "[]")

tuple_tycon_name :: Int -> HsName
tuple_tycon_name i    = HsTyClsName (tuple_ident i)

unit_tycon_qname, fun_tycon_qname, list_tycon_qname :: HsQName

unit_tycon_qname      = Qual prelude_mod unit_tycon_name   
fun_tycon_qname       = Qual prelude_mod fun_tycon_name    
list_tycon_qname      = Qual prelude_mod list_tycon_name   

tuple_tycon_qname :: Int -> HsQName
tuple_tycon_qname i   = Qual prelude_mod (tuple_tycon_name i)

unit_tycon, fun_tycon, list_tycon :: HsType

unit_tycon	      = HsTyCon unit_tycon_qname
fun_tycon	      = HsTyCon fun_tycon_qname
list_tycon	      = HsTyCon list_tycon_qname

tuple_tycon :: Int -> HsType
tuple_tycon i	      = HsTyCon (tuple_tycon_qname i)

hsIdentifierStr :: HsIdentifier -> String
hsIdentifierStr (HsIdent str)   =  str
hsIdentifierStr (HsSymbol str)  =  str
hsIdentifierStr (HsSpecial str) =  str

hsAnchorNameStr :: HsName -> String
hsAnchorNameStr (HsTyClsName id0) = "t:" ++ hsIdentifierStr id0
hsAnchorNameStr (HsVarName id0)   = "v:" ++ hsIdentifierStr id0

hsNameStr :: HsName -> String
hsNameStr (HsTyClsName id0) = hsIdentifierStr id0
hsNameStr (HsVarName id0)   = hsIdentifierStr id0

-- -----------------------------------------------------------------------------
-- Doc strings and formatting

data GenDoc id
  = DocEmpty 
  | DocAppend (GenDoc id) (GenDoc id)
  | DocString String
  | DocParagraph (GenDoc id)
  | DocIdentifier id
  | DocModule String
  | DocEmphasis (GenDoc id)
  | DocMonospaced (GenDoc id)
  | DocUnorderedList [GenDoc id]
  | DocOrderedList [GenDoc id]
  | DocDefList [(GenDoc id, GenDoc id)]
  | DocCodeBlock (GenDoc id)
  | DocURL String
  | DocAName String
  deriving (Eq, Show)

type Doc = GenDoc [HsQName]

-- | DocMarkup is a set of instructions for marking up documentation.
-- In fact, it's really just a mapping from 'GenDoc' to some other
-- type [a], where [a] is usually the type of the output (HTML, say).

data DocMarkup id a = Markup {
  markupEmpty         :: a,
  markupString        :: String -> a,
  markupParagraph     :: a -> a,
  markupAppend        :: a -> a -> a,
  markupIdentifier    :: id -> a,
  markupModule        :: String -> a,
  markupEmphasis      :: a -> a,
  markupMonospaced    :: a -> a,
  markupUnorderedList :: [a] -> a,
  markupOrderedList   :: [a] -> a,
  markupDefList       :: [(a,a)] -> a,
  markupCodeBlock     :: a -> a,
  markupURL	      :: String -> a,
  markupAName	      :: String -> a
  }

markup :: DocMarkup id a -> GenDoc id -> a
markup m DocEmpty		= markupEmpty m
markup m (DocAppend d1 d2)	= markupAppend m (markup m d1) (markup m d2)
markup m (DocString s)		= markupString m s
markup m (DocParagraph d)	= markupParagraph m (markup m d)
markup m (DocIdentifier i)	= markupIdentifier m i
markup m (DocModule mod0)	= markupModule m mod0
markup m (DocEmphasis d)	= markupEmphasis m (markup m d)
markup m (DocMonospaced d)	= markupMonospaced m (markup m d)
markup m (DocUnorderedList ds)	= markupUnorderedList m (map (markup m) ds)
markup m (DocOrderedList ds)	= markupOrderedList m (map (markup m) ds)
markup m (DocDefList ds)        = markupDefList m (map (markupPair m) ds)
markup m (DocCodeBlock d)	= markupCodeBlock m (markup m d)
markup m (DocURL url)		= markupURL m url
markup m (DocAName ref)		= markupAName m ref

markupPair :: DocMarkup id a -> (GenDoc id, GenDoc id) -> (a, a)
markupPair m (a,b) = (markup m a, markup m b)

-- | The identity markup
idMarkup :: DocMarkup a (GenDoc a)
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
mapIdent :: (a -> GenDoc b) -> DocMarkup a (GenDoc b)
mapIdent f = idMarkup{ markupIdentifier = f }

-- -----------------------------------------------------------------------------
-- ** Smart constructors

-- used to make parsing easier; we group the list items later
docAppend :: Doc -> Doc -> Doc
docAppend (DocUnorderedList ds1) (DocUnorderedList ds2) 
  = DocUnorderedList (ds1++ds2)
docAppend (DocUnorderedList ds1) (DocAppend (DocUnorderedList ds2) d)
  = DocAppend (DocUnorderedList (ds1++ds2)) d
docAppend (DocOrderedList ds1) (DocOrderedList ds2) 
  = DocOrderedList (ds1++ds2)
docAppend (DocOrderedList ds1) (DocAppend (DocOrderedList ds2) d)
  = DocAppend (DocOrderedList (ds1++ds2)) d
docAppend (DocDefList ds1) (DocDefList ds2)
  = DocDefList (ds1++ds2)
docAppend (DocDefList ds1) (DocAppend (DocDefList ds2) d)
  = DocAppend (DocDefList (ds1++ds2)) d
docAppend DocEmpty d = d
docAppend d DocEmpty = d
docAppend d1 d2 
  = DocAppend d1 d2

-- again to make parsing easier - we spot a paragraph whose only item
-- is a DocMonospaced and make it into a DocCodeBlock
docParagraph :: Doc -> Doc
docParagraph (DocMonospaced p)
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1) (DocMonospaced p))
  | all isSpace s1
  = DocCodeBlock p
docParagraph (DocAppend (DocString s1)
		(DocAppend (DocMonospaced p) (DocString s2)))
  | all isSpace s1 && all isSpace s2
  = DocCodeBlock p
docParagraph (DocAppend (DocMonospaced p) (DocString s2))
  | all isSpace s2
  = DocCodeBlock p
docParagraph p
  = DocParagraph p
\end{code}
