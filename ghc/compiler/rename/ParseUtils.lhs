%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[ParseUtils]{Help the interface parser}

\begin{code}
#include "HsVersions.h"

module ParseUtils where

import Ubiq{-uitous-}

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsPragmas	( noDataPragmas, noClassPragmas, noClassOpPragmas,
			  noInstancePragmas
			)

import ErrUtils		( Error(..) )
import FiniteMap	( unitFM, listToFM, lookupFM, plusFM, FiniteMap )
import Maybes		( maybeToBool, MaybeErr(..) )
import Name		( isLexConId, isLexVarId, isLexConSym,
			  mkTupNameStr,
			  RdrName(..){-instance Outputable:ToDo:rm-}
			)
import PprStyle		( PprStyle(..) ) -- ToDo: rm debugging
import PrelMods		( fromPrelude )
import Pretty		( ppCat, ppPStr, ppInt, ppShow, ppStr )
import SrcLoc		( mkIfaceSrcLoc )
import Util		( startsWith, isIn, panic, assertPanic )
\end{code}

\begin{code}
type UsagesMap	      = FiniteMap Module (Version, VersionsMap)
			-- module => its version, then to all its entities
			-- and their versions; "instance" is a magic entity
			-- representing all the instances def'd in that module
type VersionsMap      = FiniteMap FAST_STRING Version
			-- Versions for things def'd in this module
type ExportsMap       = FiniteMap FAST_STRING (RdrName, ExportFlag)
type FixitiesMap      = FiniteMap FAST_STRING RdrNameFixityDecl
type LocalTyDefsMap   = FiniteMap FAST_STRING RdrIfaceDecl -- for TyCon/Class
type LocalValDefsMap  = FiniteMap FAST_STRING RdrIfaceDecl -- for values incl DataCon
type LocalPragmasMap  = FiniteMap FAST_STRING PragmaStuff

type PragmaStuff = String

data ParsedIface
  = ParsedIface
      Module		-- Module name
      Version		-- Module version number
      (Maybe Version)	-- Source version number
      UsagesMap		-- Used when compiling this module
      VersionsMap	-- Version numbers of things from this module
      ExportsMap	-- Exported names
      (Bag Module)	-- Special instance modules
      FixitiesMap	-- fixities of local things
      LocalTyDefsMap	-- Local TyCon/Class names defined
      LocalValDefsMap	-- Local value names defined
      (Bag RdrIfaceInst)-- Local instance declarations
      LocalPragmasMap	-- Pragmas for local names

-----------------------------------------------------------------

data RdrIfaceDecl
  = TypeSig    RdrName           	   SrcLoc RdrNameTyDecl
  | NewTypeSig RdrName RdrName	 	   SrcLoc RdrNameTyDecl
  | DataSig    RdrName [RdrName] [RdrName] SrcLoc RdrNameTyDecl
  | ClassSig   RdrName [RdrName] 	   SrcLoc RdrNameClassDecl
  | ValSig     RdrName           	   SrcLoc RdrNamePolyType
				 
data RdrIfaceInst		 
  = InstSig    RdrName RdrName   SrcLoc RdrNameInstDecl
\end{code}

\begin{code}
-----------------------------------------------------------------
data IfaceToken
  = ITinterface		-- keywords
  | ITusages
  | ITversions
  | ITexports
  | ITinstance_modules
  | ITinstances
  | ITfixities
  | ITdeclarations
  | ITpragmas
  | ITdata
  | ITtype
  | ITnewtype
  | ITclass
  | ITwhere
  | ITinstance
  | ITinfixl
  | ITinfixr
  | ITinfix
  | ITbang		-- magic symbols
  | ITvbar
  | ITbquote
  | ITdcolon
  | ITcomma
  | ITdarrow
  | ITdotdot
  | ITequal
  | ITocurly
  | ITobrack
  | IToparen
  | ITrarrow
  | ITccurly
  | ITcbrack
  | ITcparen
  | ITsemi
  | ITinteger Integer	-- numbers and names
  | ITvarid   FAST_STRING
  | ITconid   FAST_STRING
  | ITvarsym  FAST_STRING
  | ITconsym  FAST_STRING
  | ITqvarid  RdrName
  | ITqconid  RdrName
  | ITqvarsym RdrName
  | ITqconsym RdrName
  deriving Text -- debugging

instance Text RdrName where -- debugging
    showsPrec _ rn = showString (ppShow 80 (ppr PprDebug rn))

-----------------------------------------------------------------
de_qual (Unqual n) = n
de_qual (Qual _ n) = n

en_mono :: FAST_STRING -> RdrNameMonoType
en_mono tv = MonoTyVar (Unqual tv)

type2context (MonoTupleTy tys) = map type2class_assertion tys
type2context other_ty	       = [ type2class_assertion other_ty ]

type2class_assertion (MonoTyApp clas [MonoTyVar tyvar]) = (clas, tyvar)
type2class_assertion _ = panic "type2class_assertion: bad format"

-----------------------------------------------------------------
mk_type	:: (RdrName, [FAST_STRING])
	-> RdrNameMonoType
	-> LocalTyDefsMap

mk_type (qtycon, tyvars) ty
  = let
	tycon   = de_qual qtycon
	qtyvars = map Unqual tyvars
    in
    unitFM tycon (TypeSig qtycon mkIfaceSrcLoc (
		  TySynonym qtycon qtyvars ty mkIfaceSrcLoc))

mk_data	:: RdrNameContext
	-> (RdrName, [FAST_STRING])
	-> [(RdrName, RdrNameConDecl)]
	-> (LocalTyDefsMap, LocalValDefsMap)

mk_data ctxt (qtycon, tyvars) names_and_constrs
  = let
	(qconnames, constrs) = unzip names_and_constrs
	qfieldnames = [] -- ToDo ...
	tycon      = de_qual qtycon
	connames   = map de_qual qconnames
	fieldnames = map de_qual qfieldnames
	qtyvars    = map Unqual tyvars
	
	decl = DataSig qtycon qconnames qfieldnames mkIfaceSrcLoc (
		TyData ctxt qtycon qtyvars constrs Nothing noDataPragmas mkIfaceSrcLoc)
    in
    (unitFM tycon decl, listToFM [(c,decl) | c <- connames]
			`plusFM` 
			listToFM [(f,decl) | f <- fieldnames])

mk_new	:: RdrNameContext
	-> (RdrName, [FAST_STRING])
	-> (RdrName, RdrNameMonoType)
	-> (LocalTyDefsMap, LocalValDefsMap)

mk_new ctxt (qtycon, tyvars) (qconname, ty)
  = let
	tycon   = de_qual qtycon
	conname = de_qual qconname
	qtyvars = map Unqual tyvars
	constr  = NewConDecl qconname ty mkIfaceSrcLoc
	
	decl = NewTypeSig qtycon qconname mkIfaceSrcLoc (
		TyNew ctxt qtycon qtyvars [constr] Nothing noDataPragmas mkIfaceSrcLoc)
    in
    (unitFM tycon decl, unitFM conname decl)

mk_class :: RdrNameContext
	 -> (RdrName, RdrName)
	 -> [(FAST_STRING, RdrNameSig)]
	 -> (LocalTyDefsMap, LocalValDefsMap)

mk_class ctxt (qclas, tyvar) ops_and_sigs
  = case (unzip ops_and_sigs) of { (opnames, sigs) ->
    let
	qopnames = map Unqual opnames
	clas	 = de_qual qclas
	op_sigs	 = map opify sigs

	decl = ClassSig qclas qopnames mkIfaceSrcLoc (
		ClassDecl ctxt qclas tyvar op_sigs EmptyMonoBinds noClassPragmas mkIfaceSrcLoc)
    in
    (unitFM clas decl, listToFM [(o,decl) | o <- opnames]) }
  where
    opify (Sig f ty _ loc) = ClassOpSig f ty noClassOpPragmas loc

mk_inst	:: RdrNameContext
	-> RdrName -- class
	-> RdrNameMonoType  -- fish the tycon out yourself...
	-> RdrIfaceInst

mk_inst	ctxt clas mono_ty
  = InstSig clas (tycon_name mono_ty) mkIfaceSrcLoc (
	InstDecl clas (HsPreForAllTy ctxt mono_ty)
	    EmptyMonoBinds False Nothing{-lying-} [{-sigs-}]
	    noInstancePragmas mkIfaceSrcLoc)
  where
    tycon_name (MonoTyApp tc _) = tc
    tycon_name (MonoListTy   _) = Unqual SLIT("[]")
    tycon_name (MonoFunTy  _ _) = Unqual SLIT("->")
    tycon_name (MonoTupleTy ts) = Unqual (mkTupNameStr (length ts))

-----------------------------------------------------------------
lexIface :: String -> [IfaceToken]

lexIface str
  = case str of
      []    -> []

      -- whitespace and comments
      ' '	: cs -> lexIface cs
      '\t'	: cs -> lexIface cs
      '\n'	: cs -> lexIface cs
      '-' : '-' : cs -> lex_comment cs
      '{' : '-' : cs -> lex_nested_comment 1{-one seen-} cs

      '(' : '.' : '.' : ')' : cs -> ITdotdot	: lexIface cs
      '('		    : cs -> IToparen	: lexIface cs
      ')'		    : cs -> ITcparen	: lexIface cs
      '['		    : cs -> ITobrack	: lexIface cs
      ']'		    : cs -> ITcbrack	: lexIface cs
      '{'		    : cs -> ITocurly	: lexIface cs
      '}'		    : cs -> ITccurly	: lexIface cs
      ','		    : cs -> ITcomma	: lexIface cs
      ';'		    : cs -> ITsemi	: lexIface cs
      '`'		    : cs -> ITbquote	: lexIface cs
      
      '_' 		    : cs -> lex_name Nothing is_var_sym str
      c : cs | isUpper c	 -> lex_word str -- don't know if "Module." on front or not
	     | isDigit c 	 -> lex_num  str
	     | isAlpha c	 -> lex_name Nothing is_var_sym str
	     | is_sym_sym c	 -> lex_name Nothing is_sym_sym str
	     
      other -> error ("lexing:"++other)
  where
    lex_comment str
      = case (span ((/=) '\n') str) of { (junk, rest) ->
	lexIface rest }

    ------------------
    lex_nested_comment lvl [] = error "EOF in nested comment in interface"
    lex_nested_comment lvl str
      = case str of
	  '{' : '-' : xs -> lex_nested_comment (lvl+1) xs
	  '-' : '}' : xs -> if lvl == 1
			    then lexIface xs
			    else lex_nested_comment (lvl-1) xs
	  _	    : xs -> lex_nested_comment lvl xs

    -----------
    lex_num str
      = case (span isDigit str) of { (num, rest) ->
	ITinteger (read num) : lexIface rest }

    -----------
    is_var_sym '_' = True
    is_var_sym c   = isAlphanum c

    is_sym_sym c = c `elem` ":!#$%&*+./<=>?@\\^|-~" -- ToDo: add ISOgraphic

    ------------
    lex_word str@(c:cs) -- we know we have a capital letter to start
      = -- we first try for "<module>." on the front...
	case (module_dot str) of
	  Nothing       -> lex_name Nothing  is_var_sym  str
	  Just (m,rest) -> lex_name (Just m) (in_the_club rest) rest
	    where
	      in_the_club []    = panic "lex_word:in_the_club"
	      in_the_club (c:_) | isAlpha    c = is_var_sym
				| is_sym_sym c = is_sym_sym
				| otherwise    = panic ("lex_word:in_the_club="++[c])

    module_dot (c:cs)
      = if not (isUpper c) then
	   Nothing
	else
	   case (span is_var_sym cs) of { (word, rest) ->
	   case rest of
	     []		       -> Nothing
	     (r:rs) | r == '.' -> Just (_PK_ (c:word), rs)
	     _		       -> Nothing
	   }

    lex_name module_dot in_the_club str
      =	case (span in_the_club str)     of { (word, rest) ->
	case (lookupFM keywordsFM word) of
	  Just xx -> ASSERT( not (maybeToBool module_dot) )
		     xx : lexIface rest
	  Nothing -> 
	    (let
		f = head word -- first char
		n = _PK_ word
	     in
	     case module_dot of
	       Nothing ->
		 categ n (ITconid  n) (ITvarid  n) (ITconsym  n) (ITvarsym  n)
	       Just m ->
		 let
		     q = if fromPrelude m then Unqual n else Qual m n
		 in
		 categ n (ITqconid q) (ITqvarid q) (ITqconsym q) (ITqvarsym q)

	     ) : lexIface rest ;
	}
    ------------
    categ n conid varid consym varsym
      = if      isLexConId  n then conid
	else if isLexVarId  n then varid
	else if isLexConSym n then consym
	else			   varsym

    ------------
    keywordsFM :: FiniteMap String IfaceToken
    keywordsFM = listToFM [
	("interface",	 ITinterface)

       ,("__usages__",		ITusages)
       ,("__versions__",	ITversions)
       ,("__exports__",		ITexports)
       ,("__instance_modules__",ITinstance_modules)
       ,("__instances__",	ITinstances)
       ,("__fixities__",	ITfixities)
       ,("__declarations__",	ITdeclarations)
       ,("__pragmas__",		ITpragmas)

       ,("data",		ITdata)
       ,("type",		ITtype)
       ,("newtype",		ITnewtype)
       ,("class",		ITclass)
       ,("where",		ITwhere)
       ,("instance",		ITinstance)
       ,("infixl",		ITinfixl)
       ,("infixr",		ITinfixr)
       ,("infix",		ITinfix)

       ,("->",			ITrarrow)
       ,("|",			ITvbar)
       ,("!",			ITbang)
       ,("::",			ITdcolon)
       ,("=>",			ITdarrow)
       ,("=",			ITequal)
       ]

-----------------------------------------------------------------
type IfM a = MaybeErr a Error

returnIf   :: a -> IfM a
thenIf	   :: IfM a -> (a -> IfM b) -> IfM b
happyError :: Int -> [IfaceToken] -> IfM a

returnIf a = Succeeded a

thenIf (Succeeded a) k = k a
thenIf (Failed  err) _ = Failed err

happyError ln toks = Failed (ifaceParseErr ln toks)
-----------------------------------------------------------------

ifaceParseErr ln toks sty
  = ppCat [ppPStr SLIT("Interface-file parse error: line"), ppInt ln, ppStr "toks=", ppStr (show toks)]
\end{code}
