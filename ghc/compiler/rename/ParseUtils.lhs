%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[ParseUtils]{Help the interface parser}

\begin{code}
#include "HsVersions.h"

module ParseUtils where

IMP_Ubiq(){-uitous-}

IMPORT_1_3(Char(isDigit, isAlpha, isAlphanum, isUpper))
IMPORT_1_3(List(partition))

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsPragmas	( noDataPragmas, noClassPragmas, noClassOpPragmas,
			  noInstancePragmas
			)

import ErrUtils		( SYN_IE(Error) )
import FiniteMap	( unitFM, listToFM, lookupFM, plusFM, FiniteMap )
import Maybes		( maybeToBool, MaybeErr(..) )
import Name		( isLexConId, isLexVarId, isLexConSym,
			  mkTupNameStr, preludeQual, isRdrLexCon,
			  RdrName(..) {-instance Outputable:ToDo:rm-}
			)
import PprStyle		( PprStyle(..) ) -- ToDo: rm debugging
import PrelMods		( pRELUDE )
import Pretty		( ppCat, ppPStr, ppInt, ppShow, ppStr )
import SrcLoc		( mkIfaceSrcLoc )
import Util		( startsWith, isIn, panic, assertPanic{-, pprTrace ToDo:rm-} )
\end{code}

\begin{code}
type UsagesMap	      = FiniteMap Module (Version, VersionsMap)
			-- module => its version, then to all its entities
			-- and their versions; "instance" is a magic entity
			-- representing all the instances def'd in that module
type VersionsMap      = FiniteMap FAST_STRING Version
			-- Versions for things def'd in this module
type ExportsMap       = FiniteMap FAST_STRING (OrigName, ExportFlag)
type FixitiesMap      = FiniteMap FAST_STRING RdrNameFixityDecl
type LocalTyDefsMap   = FiniteMap FAST_STRING RdrIfaceDecl -- for TyCon/Class
type LocalValDefsMap  = FiniteMap FAST_STRING RdrIfaceDecl -- for values incl DataCon
type LocalPragmasMap  = FiniteMap FAST_STRING PragmaStuff

type PragmaStuff = String

data ParsedIface
  = ParsedIface
      Module		 -- Module name
      (Bool, Bag Module) -- From a merging of these modules; True => merging occured
      Version		 -- Module version number
      (Maybe Version)	 -- Source version number
      UsagesMap		 -- Used when compiling this module
      VersionsMap	 -- Version numbers of things from this module
      ExportsMap	 -- Exported names
      (Bag Module)	 -- Special instance modules
      FixitiesMap	 -- fixities of local things
      LocalTyDefsMap	 -- Local TyCon/Class names defined
      LocalValDefsMap	 -- Local value names defined
      (Bag RdrIfaceInst) -- Local instance declarations
      LocalPragmasMap	 -- Pragmas for local names

-----------------------------------------------------------------

data RdrIfaceDecl
  = TypeSig    RdrName           	   SrcLoc RdrNameTyDecl
  | NewTypeSig RdrName RdrName		   SrcLoc RdrNameTyDecl
  | DataSig    RdrName [RdrName] [RdrName] SrcLoc RdrNameTyDecl
  | ClassSig   RdrName [RdrName] 	   SrcLoc RdrNameClassDecl
  | ValSig     RdrName           	   SrcLoc RdrNamePolyType
				 
data RdrIfaceInst		 
  = InstSig    RdrName RdrName  SrcLoc (Module -> RdrNameInstDecl)
	-- InstDecl minus a Module name
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
  | ITforall
  | ITbang		-- magic symbols
  | ITvbar
  | ITdcolon
  | ITcomma
  | ITdarrow
  | ITdotdot
  | ITequal
  | ITocurly
  | ITdccurly
  | ITdocurly
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

{-OLD:
type2context (MonoTupleTy tys) = map type2class_assertion tys
type2context other_ty	       = [ type2class_assertion other_ty ]

type2class_assertion (MonoTyApp clas [MonoTyVar tyvar]) = (clas, tyvar)
type2class_assertion _ = panic "type2class_assertion: bad format"
-}

-----------------------------------------------------------------
mk_type	:: (RdrName, [FAST_STRING])
	-> RdrNameMonoType
	-> LocalTyDefsMap

mk_type (qtycon@(Qual mod tycon), tyvars) ty
  = let
	qtyvars = map Unqual tyvars
    in
    unitFM tycon (TypeSig qtycon mkIfaceSrcLoc $
		  TySynonym qtycon qtyvars ty mkIfaceSrcLoc)

mk_data	:: RdrNameContext
	-> (RdrName, [FAST_STRING])
	-> [(RdrName, RdrNameConDecl)]
	-> (LocalTyDefsMap, LocalValDefsMap)

mk_data ctxt (qtycon@(Qual mod tycon), tyvars) names_and_constrs
  = let
	(qthingnames, constrs) = unzip names_and_constrs
	(qconnames, qfieldnames) = partition isRdrLexCon qthingnames
	thingnames = [ t | (Qual _ t) <- qthingnames]
	qtyvars    = map Unqual tyvars
	
	decl = DataSig qtycon qconnames qfieldnames mkIfaceSrcLoc $
		TyData ctxt qtycon qtyvars constrs Nothing noDataPragmas mkIfaceSrcLoc
    in
    (unitFM tycon decl, listToFM [(t,decl) | t <- thingnames])

mk_new	:: RdrNameContext
	-> (RdrName, [FAST_STRING])
	-> (RdrName, RdrNameMonoType)
	-> (LocalTyDefsMap, LocalValDefsMap)

mk_new ctxt (qtycon@(Qual mod1 tycon), tyvars) (qconname@(Qual mod2 conname), ty)
  = ASSERT(mod1 == mod2)
    let
	qtyvars = map Unqual tyvars
	constr  = NewConDecl qconname ty mkIfaceSrcLoc
	
	decl = NewTypeSig qtycon qconname mkIfaceSrcLoc $
		TyNew ctxt qtycon qtyvars [constr] Nothing noDataPragmas mkIfaceSrcLoc
    in
    (unitFM tycon decl, unitFM conname decl)

mk_class :: RdrNameContext
	 -> (RdrName, RdrName)
	 -> [(FAST_STRING, RdrNameSig)]
	 -> (LocalTyDefsMap, LocalValDefsMap)

mk_class ctxt (qclas@(Qual mod clas), tyvar) ops_and_sigs
  = case (unzip ops_and_sigs) of { (opnames, sigs) ->
    let
	qopnames = map (Qual mod) opnames
	op_sigs	 = map opify sigs

	decl = ClassSig qclas qopnames mkIfaceSrcLoc $
		ClassDecl ctxt qclas tyvar op_sigs EmptyMonoBinds noClassPragmas mkIfaceSrcLoc
    in
    (unitFM clas decl, listToFM [(o,decl) | o <- opnames]) }
  where
    opify (Sig f ty _ loc) = ClassOpSig f ty noClassOpPragmas loc

mk_inst	:: [RdrName]
	-> RdrNameContext
	-> RdrName -- class
	-> RdrNameMonoType  -- fish the tycon out yourself...
	-> RdrIfaceInst

mk_inst	tvs ctxt qclas@(Qual cmod cname) mono_ty
  = let
	ty = HsForAllTy tvs ctxt mono_ty
    in
    -- pprTrace "mk_inst:" (ppr PprDebug ty) $
    InstSig qclas (tycon_name mono_ty) mkIfaceSrcLoc $ \ mod ->
	InstDecl qclas ty
	    EmptyMonoBinds False{-not from_here-} mod [{-sigs-}]
	    noInstancePragmas mkIfaceSrcLoc
  where
    tycon_name (MonoTyApp tc _) = tc
    tycon_name (MonoListTy   _) = preludeQual SLIT("[]")
    tycon_name (MonoFunTy  _ _) = preludeQual SLIT("->")
    tycon_name (MonoTupleTy ts) = preludeQual (mkTupNameStr (length ts))

-----------------------------------------------------------------
lexIface :: String -> [IfaceToken]

lexIface input
  = _scc_ "Lexer"
    case input of
      []    -> []

      -- whitespace and comments
      ' '	: cs -> lexIface cs
      '\t'	: cs -> lexIface cs
      '\n'	: cs -> lexIface cs
      '-' : '-' : cs -> lex_comment cs
      '{' : '-' : cs -> lex_nested_comment 1{-one seen-} cs

      '(' : '.' : '.' : ')' : cs -> ITdotdot	: lexIface cs
      '{' : '{' 	    : cs -> ITdocurly	: lexIface cs
      '}' : '}' 	    : cs -> ITdccurly	: lexIface cs
      '{'		    : cs -> ITocurly	: lexIface cs
      '}'		    : cs -> ITccurly	: lexIface cs
      '('		    : cs -> IToparen	: lexIface cs
      ')'		    : cs -> ITcparen	: lexIface cs
      '['		    : cs -> ITobrack	: lexIface cs
      ']'		    : cs -> ITcbrack	: lexIface cs
      ','		    : cs -> ITcomma	: lexIface cs
      ';'		    : cs -> ITsemi	: lexIface cs
      
      '_' : '_' : cs -> lex_keyword cs

      c : cs | isUpper c	 -> lex_word input -- don't know if "Module." on front or not
	     | isDigit c 	 -> lex_num  input
	     | isAlpha c	 -> lex_name Nothing is_var_sym input
	     | is_sym_sym c	 -> lex_name Nothing is_sym_sym input
	     
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
    is_var_sym c    = isAlphanum c || c `elem` "_'#"
	 -- the last few for for Glasgow-extended names

    is_var_sym1 '\'' = False
    is_var_sym1 '#'  = False
    is_var_sym1 '_'  = False
    is_var_sym1 c    = is_var_sym c

    is_sym_sym c = c `elem` ":!#$%&*+./<=>?@\\^|-~" -- ToDo: add ISOgraphic

    is_list_sym '[' = True
    is_list_sym ']' = True
    is_list_sym _   = False

    is_tuple_sym '(' = True
    is_tuple_sym ')' = True
    is_tuple_sym ',' = True
    is_tuple_sym _   = False

    ------------
    lex_word str@(c:cs) -- we know we have a capital letter to start
      = -- we first try for "<module>." on the front...
	case (module_dot str) of
	  Nothing       -> lex_name Nothing  (in_the_club str)  str
	  Just (m,rest) -> lex_name (Just m) (in_the_club rest) rest
      where
	in_the_club []    = panic "lex_word:in_the_club"
	in_the_club (x:y) | isAlpha    x = is_var_sym
			  | is_sym_sym x = is_sym_sym
			  | x == '['	 = is_list_sym
			  | x == '('	 = is_tuple_sym
			  | otherwise    = panic ("lex_word:in_the_club="++(x:y))

    module_dot (c:cs)
      = if not (isUpper c) || c == '\'' then
	   Nothing
	else
	   case (span is_var_sym cs) of { (word, rest) ->
	   case rest of
	     []		       -> Nothing
	     (r:rs) | r == '.' -> Just (_PK_ (c:word), rs)
	     _		       -> Nothing
	   }

    lex_keyword str
      = case (span is_var_sym str)    of { (kw, rest) ->
	case (lookupFM keywordsFM kw) of
	  Nothing -> panic ("lex_keyword:"++str)
	  Just xx -> xx : lexIface rest
	}

    lex_name module_dot in_the_club str
      =	case (span in_the_club str)     of { (word, rest) ->
	case (lookupFM keywordsFM word) of
	  Just xx -> let
			cont = xx : lexIface rest
		     in
		     case xx of
		       ITbang -> case module_dot of
				   Nothing -> cont
				   Just  m -> ITqvarsym (Qual m SLIT("!"))
					      : lexIface rest
		       _ -> cont
	  Nothing -> 
	    (let
		f = head word -- first char
		n = _PK_ word
	     in
	     case module_dot of
	       Nothing ->
		 categ f n (ITconid  n) (ITvarid  n) (ITconsym  n) (ITvarsym  n)
	       Just m ->
		 let
		     q = Qual m n
		 in
		 categ f n (ITqconid q) (ITqvarid q) (ITqconsym q) (ITqvarsym q)

	     ) : lexIface rest ;
	}
    ------------
    categ f n conid varid consym varsym
      = if f == '[' || f == '(' then
	   conid
	else if isLexConId  n then conid
	else if isLexVarId  n then varid
	else if isLexConSym n then consym
	else			   varsym

    ------------
    keywordsFM :: FiniteMap String IfaceToken
    keywordsFM = listToFM [
	("interface",	 ITinterface)

       ,("usages__",		ITusages)
       ,("versions__",		ITversions)
       ,("exports__",		ITexports)
       ,("instance_modules__",	ITinstance_modules)
       ,("instances__",		ITinstances)
       ,("fixities__",		ITfixities)
       ,("declarations__",	ITdeclarations)
       ,("pragmas__",		ITpragmas)
       ,("forall__",		ITforall)

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
  = ppCat [ppPStr SLIT("Interface-file parse error: line"), ppInt ln, ppStr "toks=", ppStr (show (take 10 toks))]
\end{code}
