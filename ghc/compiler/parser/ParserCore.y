{
module ParserCore ( parseCore ) where

import HsCore
import RdrHsSyn
import HsSyn
import TyCon
import TcType
import RdrName
import OccName
import Module
import ParserCoreUtils
import LexCore
import Literal
import BasicTypes
import Type
import SrcLoc

#include "../HsVersions.h"

}

%name parseCore
%tokentype { Token }

%token
 '%module'	{ TKmodule }
 '%data'	{ TKdata }
 '%newtype'	{ TKnewtype }
 '%forall'	{ TKforall }
 '%rec'		{ TKrec }
 '%let'		{ TKlet }
 '%in'		{ TKin }
 '%case'	{ TKcase }
 '%of'		{ TKof }
 '%coerce'	{ TKcoerce }
 '%note'	{ TKnote }
 '%external'	{ TKexternal }
 '%_'		{ TKwild }
 '('		{ TKoparen }
 ')'		{ TKcparen }
 '{'		{ TKobrace }
 '}'		{ TKcbrace }
 '#' 		{ TKhash}
 '='		{ TKeq }
 '::'		{ TKcoloncolon }
 '*'		{ TKstar }
 '->'		{ TKrarrow }
 '\\'		{ TKlambda}
 '@'		{ TKat }
 '.'		{ TKdot }
 '?'		{ TKquestion}
 ';'            { TKsemicolon }
 NAME		{ TKname $$ }
 CNAME 		{ TKcname $$ }
 INTEGER	{ TKinteger $$ }
 RATIONAL	{ TKrational $$ }
 STRING		{ TKstring $$ }
 CHAR		{ TKchar $$ }

%monad { P } { thenP } { returnP }
%lexer { lexer } { TKEOF }

%%

module	:: { RdrNameHsModule }
	: '%module' modid tdefs vdefgs
		{ HsModule $2 Nothing Nothing [] ($3 ++ concat $4) Nothing noSrcLoc}

tdefs	:: { [RdrNameHsDecl] }
	: {- empty -}	{[]}
	| tdef ';' tdefs	{$1:$3}

tdef	:: { RdrNameHsDecl }
	: '%data' qcname tbinds '=' '{' cons1 '}'
                { TyClD (TyData DataType [] $2 $3 (DataCons $6) Nothing [] noSrcLoc) }
	| '%newtype' qcname tbinds trep 
		{ TyClD (TyData NewType []  $2 $3 ($4 $2 $3) Nothing [] noSrcLoc) }

trep    :: { (RdrName -> [HsTyVarBndr RdrName] -> DataConDetails (ConDecl RdrName)) }
        : {- empty -}   { (\ x ts -> Unknown) }
        | '=' ty        { (\ x ts -> DataCons [ConDecl x x ts [] (VanillaCon [unbangedType $2]) noSrcLoc]) }

tbind	:: { HsTyVarBndr RdrName }
	:  name                    { IfaceTyVar $1 liftedTypeKind }
	|  '(' name '::' akind ')' { IfaceTyVar $2 $4 }

tbinds 	:: { [HsTyVarBndr RdrName] }
	: {- empty -}	{ [] }
	| tbind tbinds	{ $1:$2 }

vdefgs	:: { [[RdrNameHsDecl]] }
	: {- empty -}	        { [] }
	| vdefg ';' vdefgs	{ ($1:$3) }

vdefg	:: { [RdrNameHsDecl] }
	: '%rec' '{' vdefs1 '}' { $3   }
	|  vdef                 { [$1] }

vdefs1	:: { [RdrNameHsDecl] }
	: vdef		        { [$1] }
	| vdef ';' vdefs1       { $1:$3 }

vdef	:: { RdrNameHsDecl }
	: qname '::' ty '=' exp { TyClD (CoreDecl  $1 $3 $5 noSrcLoc) }


vbind	:: { (RdrName, RdrNameHsType) }
	: '(' name '::' ty ')'	{ ($2,$4) }

vbinds	:: { [(RdrName, RdrNameHsType)] }
	: {-empty -} 	{ [] }
	| vbind vbinds	{ $1:$2 }

bind	:: { UfBinder RdrName }
        : '@' tbind 	{ let (IfaceTyVar v k) = $2  in UfTyBinder  v k  }
	| vbind		{ let (v,ty) = $1 in UfValBinder v ty }

binds1 	:: { [UfBinder RdrName] }
	: bind		{ [$1] }
	| bind binds1	{ $1:$2 }

attbinds :: { [RdrNameHsTyVar] }
	: {- empty -} 	     { [] }
	| '@' tbind attbinds { $2:$3 }

akind	:: { Kind }
	: '*' 		   { liftedTypeKind   }	
	| '#'		   { unliftedTypeKind }
	| '?'		   { openTypeKind     }
        | '(' kind ')'	   { $2 }

kind 	:: { Kind }
	: akind 	   { $1 }
	| akind '->' kind  { mkArrowKind $1 $3 }

cons1	:: { [ConDecl RdrName] }
	: con		{ [$1] }
	| con ';' cons1	{ $1:$3 }

con	:: { ConDecl RdrName }
	: qcname attbinds atys 
		{ ConDecl $1 $1 $2 [] (VanillaCon (map unbangedType $3)) noSrcLoc}

atys	:: { [ RdrNameHsType] }
	: {- empty -}   { [] }
	| aty atys      { $1:$2 }

aty	:: { RdrNameHsType }
	: name	     { HsTyVar $1 }
	| qcname     { HsTyVar $1 }
	| '(' ty ')' { $2 }


bty	:: { RdrNameHsType }
	: aty	     { $1 }
        | bty aty    { HsAppTy $1 $2 }

ty	:: { RdrNameHsType }
	: bty	                   { $1 }
	| bty '->' ty              { HsFunTy $1 $3 }
	| '%forall' tbinds '.' ty  { HsForAllTy (Just $2) [] $4 }

aexp    :: { UfExpr RdrName }
	: qname 	{ UfVar $1 }
        | qcname 	{ UfVar $1 } 
	| lit		{ UfLit $1 }
	| '(' exp ')' 	{ $2 }

fexp	:: { UfExpr RdrName }
	: fexp aexp	{ UfApp $1 $2 }
	| fexp '@' aty	{ UfApp $1 (UfType $3) }
	| aexp		{ $1 }

exp	:: { UfExpr RdrName }
	: fexp		           { $1 }
	| '\\' binds1 '->' exp 	   { foldr UfLam $4 $2 }
	| '%let' vdefg '%in' exp   { UfLet (toUfBinder $2) $4 }
	| '%case' aexp '%of' vbind
	  '{' alts1 '}'		   { UfCase $2 (fst $4) $6 }
	| '%coerce' aty exp   	   { UfNote (UfCoerce $2) $3 } -- what about the 'from' type?
	| '%note' STRING exp 	   
	    { case $2 of
	       --"SCC"        -> UfNote (UfSCC "scc") $3
	       "InlineCall" -> UfNote UfInlineCall $3
	       "InlineMe"   -> UfNote UfInlineMe $3
            }
--        | '%external' STRING aty   { External $2 $3 }

alts1	:: { [UfAlt RdrName] }
	: alt		{ [$1] }
	| alt ';' alts1	{ $1:$3 }

alt	:: { UfAlt RdrName }
	: qcname attbinds vbinds '->' exp 
		{ {- ToDo: sort out-} (UfDataAlt $1, (map hsTyVarName $2 ++ map fst $3), $5) } 
	| lit '->' exp
		{ (UfLitAlt $1, [], $3) }
	| '%_' '->' exp
		{ (UfDefault, [], $3) }

lit	:: { Literal }
	: '(' INTEGER '::' aty ')'	{ MachInt $2 }
	| '(' RATIONAL '::' aty ')'	{ MachDouble $2 }
	| '(' CHAR '::' aty ')'		{ MachChar (fromEnum $2) }
	| '(' STRING '::' aty ')'	{ MachStr (_PK_ $2) }

name	:: { RdrName }
	: NAME	{ mkUnqual varName (_PK_ $1) }

cname	:: { String }
	: CNAME	{ $1 }
         
mname	:: { String }
	: CNAME	{ $1 }

modid	:: { ModuleName }
	: CNAME	{ mkSysModuleNameFS (_PK_ $1) }

qname	:: { RdrName }
	: name	{ $1 }
	| mname '.' NAME
	  { mkIfaceOrig varName (_PK_ $1,_PK_ $3) }

qcname	:: { RdrName }
        : mname '.' cname 
		{ mkIfaceOrig dataName (_PK_ $1,_PK_ $3) }


{

toUfBinder :: [RdrNameHsDecl] -> UfBinding RdrName
toUfBinder xs  = 
 case xs of 
   [x] -> uncurry UfNonRec (conv x)
   _   -> UfRec (map conv xs)
 where
  conv (TyClD (CoreDecl n ty rhs _)) = (UfValBinder n ty, rhs)

happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l

}
