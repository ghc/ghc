{
#include "HsVersions.h"

module ParseIface (
	parseIface,

	ParsedIface(..), RdrIfaceDecl(..),

	ExportsMap(..), LocalDefsMap(..), LocalPragmasMap(..),
	LocalVersionsMap(..), PragmaStuff(..)

    ) where

import Ubiq{-uitous-}

import HsSyn		( ClassDecl, InstDecl, TyDecl, PolyType, InPat, Fake )
import RdrHsSyn		( RdrNameTyDecl(..), RdrNameClassDecl(..),
			  RdrNamePolyType(..), RdrNameInstDecl(..)
			)
import FiniteMap	( emptyFM, listToFM, fmToList, lookupFM, keysFM, FiniteMap )
import Name		( ExportFlag(..) )
import Util		( startsWith )
-----------------------------------------------------------------

parseIface = parseIToks . lexIface

type LocalVersionsMap = FiniteMap FAST_STRING Version
type ExportsMap       = FiniteMap FAST_STRING (RdrName, ExportFlag)
type LocalDefsMap     = FiniteMap FAST_STRING RdrIfaceDecl
type LocalPragmasMap  = FiniteMap FAST_STRING PragmaStuff

type PragmaStuff = String

data ParsedIface
  = ParsedIface
      Module		-- Module name
      Version		-- Module version number
      (Maybe Version)	-- Source version number
      LocalVersionsMap  -- Local version numbers
      ExportsMap	-- Exported names
      [Module]		-- Special instance modules
      LocalDefsMap	-- Local names defined
      [RdrIfaceDecl]	-- Local instance declarations
      LocalPragmasMap	-- Pragmas for local names

{-
instance Text ParsedIface where
    showsPrec _ (ParsedIface m v mv lcm exm ims ldm lids ldp)
      = showString "interface "
      . showString (_UNPK_ m)
      . showChar ' '
      . showInt  v
      . showString "\n__versions__\n"
      . showList (fmToList lcm)
      . showString "\n__exports__\n"
      . showList (fmToList exm)
      . showString "\n__instance_modules__\n"
      . showList (map _UNPK_ ims)
      . showString "\n__declarations__\n"
      . showList (map _UNPK_ (keysFM ldm))
      . showString "\n__instances__\n"
      . showList lids
      . showString "\n__pragmas__\n"
      . showList (map _UNPK_ (keysFM ldp))
-}

-----------------------------------------------------------------

data RdrIfaceDecl
  = TypeSig    RdrName           Bool SrcLoc RdrNameTyDecl
  | NewTypeSig RdrName RdrName	 Bool SrcLoc RdrNameTyDecl
  | DataSig    RdrName [RdrName] Bool SrcLoc RdrNameTyDecl
  | ClassSig   RdrName [RdrName] Bool SrcLoc RdrNameClassDecl
  | ValSig     RdrName           Bool SrcLoc RdrNamePolyType
  | InstSig    RdrName RdrName   Bool SrcLoc RdrNameInstDecl
				-- True => Source Iface decl
-----------
type Version = Int

-----------------------------------------------------------------
}

%name	    parseIToks
%tokentype  { IfaceToken }

%token
	interface	    { ITinterface }
	versions_part	    { ITversions }
	exports_part	    { ITexports }
	instance_modules_part { ITinstance_modules }
	instances_part	    { ITinstances }
	declarations_part   { ITdeclarations }
	pragmas_part	    { ITpragmas }
	data		    { ITdata }
	type		    { ITtype }
	newtype		    { ITnewtype }
	class		    { ITclass }
	where		    { ITwhere }
	instance	    { ITinstance }
	bar		    { ITbar }
	colons		    { ITcolons }
	comma		    { ITcomma }
	dblrarrow	    { ITdblrarrow }
	dot		    { ITdot }
	dotdot		    { ITdotdot }
	equal		    { ITequal }
	lbrace		    { ITlbrace }
	lbrack		    { ITlbrack }
	lparen		    { ITlparen }
	rarrow		    { ITrarrow }
	rbrace		    { ITrbrace }
	rbrack		    { ITrbrack }
	rparen		    { ITrparen }
	semicolon	    { ITsemicolon }
	num		    { ITnum  $$ }
	name		    { ITname $$ }
%%

Iface		:: { ParsedIface }
Iface		: interface name num
		  VersionsPart ExportsPart InstanceModulesPart
		  DeclsPart InstancesPart PragmasPart
		  { ParsedIface $2 (fromInteger $3) Nothing{-src version-}
			$4  -- local versions
			$5  -- exports map
			$6  -- instance modules
			$7  -- decls map
			$8  -- local instances
			$9  -- pragmas map
		  }

VersionsPart	:: { LocalVersionsMap }
VersionsPart	:  versions_part NameVersionPairs
		   { listToFM $2 }

NameVersionPairs :: { [(FAST_STRING, Int)] }
NameVersionPairs :  NameVersionPairs name lparen num rparen
		    { ($2, fromInteger $4) : $1 }
	         |  { [] }

ExportsPart	:: { ExportsMap }
ExportsPart	:  exports_part ExportItems
		   { listToFM $2 }

ExportItems	:: { [(FAST_STRING, (RdrName, ExportFlag))] }
ExportItems	:  ExportItems name dot name MaybeDotDot
		   { ($4, (Qual $2 $4, $5)) : $1 }
		|  { [] }

MaybeDotDot	:: { ExportFlag }
MaybeDotDot	:  dotdot { ExportAll }
		|	  { ExportAbs }

InstanceModulesPart :: { [Module] }
InstanceModulesPart :  instance_modules_part ModList
		       { $2 }

ModList		:: { [Module] }
ModList		:  ModList name	{ $2 : $1 }
		|		{ [] }

DeclsPart	:: { LocalDefsMap }
DeclsPart	: declarations_part
		  { emptyFM }

InstancesPart	:: { [RdrIfaceDecl] }
InstancesPart	:  instances_part
		   { [] }

PragmasPart	:: { LocalPragmasMap }
PragmasPart	:  pragmas_part
		   { emptyFM }
{
-----------------------------------------------------------------
happyError :: Int -> [IfaceToken] -> a
happyError i _ = error ("Parse error in line " ++ show i ++ "\n")

-----------------------------------------------------------------
data IfaceToken
  = ITinterface		-- keywords
  | ITversions
  | ITexports
  | ITinstance_modules
  | ITinstances
  | ITdeclarations
  | ITpragmas
  | ITdata
  | ITtype
  | ITnewtype
  | ITclass
  | ITwhere
  | ITinstance
  | ITbar		-- magic symbols
  | ITcolons
  | ITcomma
  | ITdblrarrow
  | ITdot
  | ITdotdot
  | ITequal
  | ITlbrace
  | ITlbrack
  | ITlparen
  | ITrarrow
  | ITrbrace
  | ITrbrack
  | ITrparen
  | ITsemicolon
  | ITnum   Integer	-- numbers and names
  | ITname  FAST_STRING

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
      '('		    : cs -> ITlparen	: lexIface cs
      ')'		    : cs -> ITrparen	: lexIface cs
      '['		    : cs -> ITlbrack	: lexIface cs
      ']'		    : cs -> ITrbrack	: lexIface cs
      '{'		    : cs -> ITlbrace	: lexIface cs
      '}'		    : cs -> ITrbrace	: lexIface cs
      '-' : '>'		    : cs -> ITrarrow	: lexIface cs
      '.'		    : cs -> ITdot	: lexIface cs
      '|'		    : cs -> ITbar	: lexIface cs
      ':' : ':'		    : cs -> ITcolons	: lexIface cs
      '=' : '>'		    : cs -> ITdblrarrow	: lexIface cs
      '='		    : cs -> ITequal	: lexIface cs
      ','		    : cs -> ITcomma	: lexIface cs
      ';'		    : cs -> ITsemicolon	: lexIface cs
      
      '_' 		    : cs -> lex_word str
      c : cs | isDigit c 	 -> lex_num  str
             | isAlpha c	 -> lex_word str

      other -> error ("lexing:"++other)
  where
    lex_comment str
      = case (span ((/=) '\n') str) of { (junk, rest) ->
	lexIface rest }

    lex_nested_comment lvl [] = error "EOF in nested comment in interface"
    lex_nested_comment lvl str
      = case str of
	  '{' : '-' : xs -> lex_nested_comment (lvl+1) xs
	  '-' : '}' : xs -> if lvl == 1
			    then lexIface xs
			    else lex_nested_comment (lvl-1) xs
	  _	    : xs -> lex_nested_comment lvl xs

    lex_num str
      = case (span isDigit str) of { (num, rest) ->
	ITnum (read num) : lexIface rest }

    lex_word str
      = case (span is_word_sym str)     of { (word, rest) ->
	case (lookupFM keywordsFM word) of {
	  Nothing -> ITname (_PK_ word) : lexIface rest ;
	  Just xx -> xx			: lexIface rest
	}}
      where
	is_word_sym '_' = True
	is_word_sym c   = isAlphanum c

	keywordsFM :: FiniteMap String IfaceToken
	keywordsFM = listToFM [
	    ("interface",	 ITinterface)

	   ,("__versions__",	 ITversions)
	   ,("__exports__",	 ITexports)
	   ,("__instance_modules__", ITinstance_modules)
	   ,("__instances__",	 ITinstances)
	   ,("__declarations__", ITdeclarations)
	   ,("__pragmas__",	 ITpragmas)

	   ,("data",		 ITdata)
	   ,("class",		 ITclass)
	   ,("where",		 ITwhere)
	   ,("instance",	 ITinstance)
	   ]
}
