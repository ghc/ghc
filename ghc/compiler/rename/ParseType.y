{
#include "HsVersions.h"
module ParseType ( parseType ) where

IMP_Ubiq(){-uitous-}

import HsSyn		-- quite a bit of stuff
import RdrHsSyn		-- oodles of synonyms
import HsDecls		( HsIdInfo(..), HsStrictnessInfo )
import HsTypes		( mkHsForAllTy )
import HsCore
import Literal
import HsPragmas	( noGenPragmas, noDataPragmas, noClassPragmas, noClassOpPragmas, noInstancePragmas )
import IdInfo		( exactArity, mkStrictnessInfo, mkBottomStrictnessInfo,
			  ArgUsageInfo, FBTypeInfo
			)
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind )
import Lex		

import RnMonad		( SYN_IE(ImportVersion), SYN_IE(LocalVersion), ParsedIface(..),
			  SYN_IE(RdrNamePragma), SYN_IE(ExportItem), GenAvailInfo
			) 
import Bag		( emptyBag, unitBag, snocBag )
import FiniteMap	( emptyFM, unitFM, addToFM, plusFM, bagToFM, FiniteMap )
import Name		( OccName(..), isTCOcc, Provenance )
import SrcLoc		( mkIfaceSrcLoc )
import Util		( panic{-, pprPanic ToDo:rm-} )
import Pretty		( Doc )
import Outputable	( PprStyle(..) )
import Maybes           ( MaybeErr(..) )

------------------------------------------------------------------

parseType :: [IfaceToken] -> MaybeErr RdrNameHsType (PprStyle -> Doc)
parseType ls =
  let
   res =
    case parseT ls of
      v@(Succeeded _) -> v
      Failed err      -> panic (show (err PprDebug))
  in
  res

}

%name parseT
%tokentype { IfaceToken }
%monad	    { IfM }{ thenIf }{ returnIf }

%token
	FORALL		    { ITforall }
	DCOLON		    { ITdcolon }
	COMMA		    { ITcomma }
	DARROW		    { ITdarrow }
	OCURLY		    { ITocurly }
	OBRACK		    { ITobrack }
	OPAREN		    { IToparen }
	RARROW		    { ITrarrow }
	CCURLY		    { ITccurly }
	CBRACK		    { ITcbrack }
	CPAREN		    { ITcparen }

	VARID		    { ITvarid  	 $$ }
	CONID		    { ITconid  	 $$ }
	VARSYM		    { ITvarsym 	 $$ }
	CONSYM		    { ITconsym 	 $$ }
	QCONID		    { ITqconid   $$ }

	UNKNOWN             { ITunknown $$ }
%%

type		:: { RdrNameHsType }
type		: FORALL forall context DARROW type	{ mkHsForAllTy $2 $3 $5 }
		|  btype RARROW type			{ MonoFunTy $1 $3 }
		|  btype				{ $1 }

forall		: OBRACK tv_bndrs CBRACK		{ $2 }

context		:: { RdrNameContext }
context		:  					{ [] }
		| OCURLY context_list1 CCURLY		{ $2 }

context_list1	:: { RdrNameContext }
context_list1	: class					{ [$1] }
		| class COMMA context_list1 		{ $1 : $3 }

class		:: { (RdrName, RdrNameHsType) }
class		:  tc_name atype			{ ($1, $2) }


types2		:: { [RdrNameHsType] 			{- Two or more -}  }	
types2		:  type COMMA type			{ [$1,$3] }
		|  type COMMA types2			{ $1 : $3 }

btype		:: { RdrNameHsType }
btype		:  atype				{ $1 }
		|  btype atype				{ MonoTyApp $1 $2 }

atype		:: { RdrNameHsType }
atype		:  tc_name 			  	{ MonoTyVar $1 }
		|  tv_name			  	{ MonoTyVar $1 }
		|  OPAREN types2 CPAREN	  		{ MonoTupleTy dummyRdrTcName $2 }
		|  OBRACK type CBRACK		  	{ MonoListTy  dummyRdrTcName $2 }
		|  OCURLY tc_name atype CCURLY		{ MonoDictTy $2 $3 }
		|  OPAREN type CPAREN		  	{ $2 }

atypes		:: { [RdrNameHsType] 	{-  Zero or more -} }
atypes		:  					{ [] }
		|  atype atypes				{ $1 : $2
---------------------------------------------------------------------
					 		}

tv_bndr		:: { HsTyVar RdrName }
tv_bndr		:  tv_name DCOLON akind	{ IfaceTyVar $1 $3 }
		|  tv_name		{ UserTyVar $1 }

tv_bndrs	:: { [HsTyVar RdrName] }
		:  			{ [] }
		| tv_bndr tv_bndrs	{ $1 : $2 }

kind		:: { Kind }
		: akind			{ $1 }
		| akind RARROW kind	{ mkArrowKind $1 $3 }

akind		:: { Kind }
		: VARSYM		{ mkBoxedTypeKind {- ToDo: check that it's "*" -} }
		| OPAREN kind CPAREN	{ $2 }

tv_name		:: { RdrName }
tv_name		:  VARID 		{ Unqual (TvOcc $1) }
		|  VARSYM		{ Unqual (TvOcc $1) {- Allow $t2 as a tyvar -} }

tv_names	:: { [RdrName] }
		:  			{ [] }
		| tv_name tv_names	{ $1 : $2 }

tc_name		:: { RdrName }
tc_name		:  QCONID		{ lexTcQual $1 }
		|  CONID		{ Unqual (TCOcc $1) }
		|  CONSYM		{ Unqual (TCOcc $1) }
		|  OPAREN RARROW CPAREN	{ Unqual (TCOcc SLIT("->")) }

