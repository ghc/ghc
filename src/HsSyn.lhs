% -----------------------------------------------------------------------------
% $Id: HsSyn.lhs,v 1.7 2002/05/08 14:48:41 simonmar Exp $
%
% (c) The GHC Team, 1997-2002
%
% A suite of datatypes describing the abstract syntax of Haskell 98.
%
% -----------------------------------------------------------------------------

\begin{code}
module HsSyn (
    SrcLoc(..), Module(..), HsQName(..), HsName(..), HsIdentifier(..),
    HsModule(..), HsExportSpec(..),
    HsImportDecl(..), HsImportSpec(..), HsAssoc(..),
    HsDecl(..), HsMatch(..), HsConDecl(..), HsFieldDecl(..), 
    HsBangType(..), HsRhs(..),
    HsGuardedRhs(..), HsType(..), HsContext, HsAsst,
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
    unit_tycon, fun_tycon, list_tycon, tuple_tycon,
  ) where


data SrcLoc = SrcLoc Int Int -- (Line, Indentation)
  deriving (Eq,Ord,Show)

newtype Module = Module String
  deriving (Eq,Ord,Show)

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

data HsModule = HsModule Module (Maybe [HsExportSpec])
                        [HsImportDecl] [HsDecl] 
			(Maybe String)	-- the doc options
			(Maybe String)	-- the module doc
  deriving Show

-- Export/Import Specifications

data HsExportSpec
	 = HsEVar HsQName			-- variable
	 | HsEAbs HsQName			-- T
	 | HsEThingAll HsQName			-- T(..)
	 | HsEThingWith HsQName [HsQName]	-- T(C_1,...,C_n)
	 | HsEModuleContents Module		-- module M   (not for imports)
	 | HsEGroup Int String			-- a doc section heading
	 | HsEDoc String			-- some documentation
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
	 = HsTypeDecl	 SrcLoc HsName [HsName] HsType
	 | HsDataDecl	 SrcLoc HsContext HsName [HsName] [HsConDecl] [HsQName]
	 | HsInfixDecl   SrcLoc HsAssoc Int [HsName]
	 | HsNewTypeDecl SrcLoc HsContext HsName [HsName] HsConDecl [HsQName]
	 | HsClassDecl	 SrcLoc HsType [HsFunDep] [HsDecl]
	 | HsInstDecl	 SrcLoc HsType [HsDecl]
	 | HsDefaultDecl SrcLoc [HsType]
	 | HsTypeSig	 SrcLoc [HsName] HsType
	 | HsFunBind     [HsMatch]
	 | HsPatBind	 SrcLoc HsPat HsRhs {-where-} [HsDecl]
	 | HsForeignImport SrcLoc HsCallConv HsFISafety String HsName HsType
	 | HsForeignExport SrcLoc HsCallConv String HsName HsType
	 | HsDocCommentNext String	-- a documentation annotation
	 | HsDocCommentPrev String	-- a documentation annotation
	 | HsDocCommentNamed String	-- a documentation annotation
	 | HsDocGroup    Int String	-- a documentation group
  deriving (Eq,Show)

data HsMatch 
	 = HsMatch SrcLoc HsQName [HsPat] HsRhs {-where-} [HsDecl]
  deriving (Eq,Show)

data HsConDecl
     = HsConDecl SrcLoc HsName [HsName] HsContext [HsBangType] (Maybe String)
     | HsRecDecl SrcLoc HsName [HsName] HsContext [HsFieldDecl] (Maybe String)
  deriving (Eq,Show)

data HsFieldDecl
	= HsFieldDecl [HsName] HsBangType (Maybe String)
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
	 = HsForAllType (Maybe [HsName]) HsContext HsType
	 | HsTyFun   HsType HsType
	 | HsTyTuple Bool{-boxed-} [HsType]
	 | HsTyApp   HsType HsType
	 | HsTyVar   HsName
	 | HsTyCon   HsQName
  deriving (Eq,Show)

type HsFunDep  = ([HsName], [HsName])
type HsContext = [HsAsst]
type HsAsst    = (HsQName,[HsType])	-- for multi-parameter type classes

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
	= HsVar HsQName
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
mkHsForAllType (Just []) [] ty = ty	-- Explicit for-all with no tyvars
mkHsForAllType mtvs1     [] (HsForAllType mtvs2 ctxt ty)
 = mkHsForAllType (mtvs1 `plus` mtvs2) ctxt ty
 where
    mtvs1       `plus` Nothing     = mtvs1
    Nothing     `plus` mtvs2       = mtvs2 
    (Just tvs1) `plus` (Just tvs2) = Just (tvs1 ++ tvs2)
mkHsForAllType tvs ctxt ty = HsForAllType tvs ctxt ty

-----------------------------------------------------------------------------
-- Builtin names.

prelude_mod	      = Module "Prelude"
main_mod	      = Module "Main"

unit_ident	      = HsSpecial "()"
tuple_ident i	      = HsSpecial ("("++replicate i ','++")")
nil_ident	      = HsSpecial "[]"

unit_con_name	      = Qual prelude_mod (HsVarName unit_ident)
tuple_con_name i      = Qual prelude_mod (HsVarName (tuple_ident i))
nil_con_name	      = Qual prelude_mod (HsVarName nil_ident)

as_name	              = HsVarName (HsIdent "as")
qualified_name        = HsVarName (HsIdent "qualified")
hiding_name	      = HsVarName (HsIdent "hiding")
unsafe_name	      = HsVarName (HsIdent "unsafe")
safe_name	      = HsVarName (HsIdent "safe")
forall_name	      = HsVarName (HsIdent "threadsafe")
threadsafe_name	      = HsVarName (HsIdent "threadsafe")
export_name	      = HsVarName (HsIdent "export")
ccall_name	      = HsVarName (HsIdent "ccall")
stdcall_name	      = HsVarName (HsIdent "stdcall")
dotnet_name	      = HsVarName (HsIdent "dotnet")
minus_name	      = HsVarName (HsSymbol "-")
pling_name	      = HsVarName (HsSymbol "!")
dot_name	      = HsVarName (HsSymbol ".")

unit_tycon_name       = Qual prelude_mod (HsTyClsName unit_ident)
fun_tycon_name        = Qual prelude_mod (HsTyClsName (HsSpecial "->"))
list_tycon_name       = Qual prelude_mod (HsTyClsName (HsSpecial "[]"))
tuple_tycon_name i    = Qual prelude_mod (HsTyClsName (tuple_ident i))

unit_tycon	      = HsTyCon unit_tycon_name
fun_tycon	      = HsTyCon fun_tycon_name
list_tycon	      = HsTyCon list_tycon_name
tuple_tycon i	      = HsTyCon (tuple_tycon_name i)
\end{code}
