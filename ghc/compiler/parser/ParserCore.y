{
module ParserCore ( parseCore ) where

import ForeignCall

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
import PrelNames
import FastString
import Outputable

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
		{ HsModule (Just (mkHomeModule $2)) Nothing 
		           [] ($3 ++ concat $4) Nothing noSrcLoc}

tdefs	:: { [RdrNameHsDecl] }
	: {- empty -}	{[]}
	| tdef ';' tdefs	{$1:$3}

tdef	:: { RdrNameHsDecl }
	: '%data' q_tc_name tbinds '=' '{' cons1 '}'
                { TyClD (mkTyData DataType ([], $2, $3) (DataCons $6) Nothing noSrcLoc) }
	| '%newtype' q_tc_name tbinds trep 
		{ TyClD (mkTyData NewType ([], $2, $3) ($4 $2) Nothing noSrcLoc) }

-- For a newtype we have to invent a fake data constructor name
-- It doesn't matter what it is, because it won't be used
trep    :: { (RdrName -> DataConDetails (ConDecl RdrName)) }
        : {- empty -}   { (\ tc_name -> Unknown) }
        | '=' ty        { (\ tc_name -> let { dc_name  = setRdrNameSpace tc_name dataName ;
			                      con_info = PrefixCon [unbangedType $2] }
			                in DataCons [ConDecl dc_name [] [] con_info noSrcLoc]) }

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
	: '%rec' '{' vdefs1 '}' { map CoreD $3   }
	|  vdef                 { [CoreD $1] }

let_bind :: { UfBinding RdrName }
	: '%rec' '{' vdefs1 '}' { UfRec (map convBind $3)   }
	|  vdef                 { let (b,r) = convBind $1
				  in UfNonRec b r }

vdefs1	:: { [RdrNameCoreDecl] }
	: vdef		        { [$1] }
	| vdef ';' vdefs1       { $1:$3 }

vdef	:: { RdrNameCoreDecl }
	: qname '::' ty '=' exp { CoreDecl $1 $3 $5 noSrcLoc }
  -- NB: qname includes data constructors, because
  --     we allow data-constructor wrappers at top level


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
	: q_d_patt attbinds atys 
		{ ConDecl $1 $2 [] (PrefixCon (map unbangedType $3)) noSrcLoc}

atys	:: { [ RdrNameHsType] }
	: {- empty -}   { [] }
	| aty atys      { $1:$2 }

aty	:: { RdrNameHsType }
	: name	     { HsTyVar $1 }
	| q_tc_name     { HsTyVar $1 }
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
	| lit		{ UfLit $1 }
	| '(' exp ')' 	{ $2 }

fexp	:: { UfExpr RdrName }
	: fexp aexp	{ UfApp $1 $2 }
	| fexp '@' aty	{ UfApp $1 (UfType $3) }
	| aexp		{ $1 }

exp	:: { UfExpr RdrName }
	: fexp		           { $1 }
	| '\\' binds1 '->' exp 	   { foldr UfLam $4 $2 }
	| '%let' let_bind '%in' exp   { UfLet $2 $4 }
	| '%case' aexp '%of' vbind
	  '{' alts1 '}'		   { UfCase $2 (fst $4) $6 }
	| '%coerce' aty exp   	   { UfNote (UfCoerce $2) $3 } -- what about the 'from' type?
	| '%note' STRING exp 	   
	    { case $2 of
	       --"SCC"        -> UfNote (UfSCC "scc") $3
	       "InlineCall" -> UfNote UfInlineCall $3
	       "InlineMe"   -> UfNote UfInlineMe $3
            }
        | '%external' STRING aty   { UfFCall (ForeignCall.CCall 
                                               (CCallSpec (StaticTarget 
                                                            (mkFastString $2)) 
                                                          CCallConv (PlaySafe False))) $3 }
alts1	:: { [UfAlt RdrName] }
	: alt		{ [$1] }
	| alt ';' alts1	{ $1:$3 }

alt	:: { UfAlt RdrName }
	: q_d_patt attbinds vbinds '->' exp 
		{ (UfDataAlt $1, (map hsTyVarName $2 ++ map fst $3), $5) } 
	| lit '->' exp
		{ (UfLitAlt $1, [], $3) }
	| '%_' '->' exp
		{ (UfDefault, [], $3) }

lit	:: { Literal }
	: '(' INTEGER '::' aty ')'	{ convIntLit $2 $4 }
	| '(' RATIONAL '::' aty ')'	{ convRatLit $2 $4 }
	| '(' CHAR '::' aty ')'		{ MachChar (fromEnum $2) }
	| '(' STRING '::' aty ')'	{ MachStr (mkFastString $2) }

name	:: { RdrName }
	: NAME	{ mkRdrUnqual (mkVarOccEncoded (mkFastString $1)) }

cname	:: { String }
	: CNAME	{ $1 }
         
mname	:: { String }
	: CNAME	{ $1 }

modid	:: { ModuleName }
	: CNAME	{ mkSysModuleNameFS (mkFastString $1) }

qname	:: { RdrName }           -- Includes data constructors
	: name	                 { $1 }
	| mname '.' NAME	 { mkIfaceOrig varName (mkFastString $1) (mkFastString $3) }
        | q_d_occ                { $1 }


-- Type constructor
q_tc_name	:: { RdrName }
        : mname '.' cname 
		{ mkIfaceOrig tcName (mkFastString $1) (mkFastString $3) }

-- Data constructor in a pattern or data type declaration; use the dataName, 
-- because that's what we expect in Core case patterns
q_d_patt :: { RdrName }
        : mname '.' cname 
		{ mkIfaceOrig dataName (mkFastString $1) (mkFastString $3) }

-- Data constructor occurrence in an expression;
-- use the varName because that's the worker Id
q_d_occ :: { RdrName }
        : mname '.' cname 
		{ mkIfaceOrig varName (mkFastString $1) (mkFastString $3) }


{
convBind :: RdrNameCoreDecl -> (UfBinder RdrName, UfExpr RdrName)
convBind (CoreDecl n ty rhs _) = (UfValBinder n ty, rhs)

convIntLit :: Integer -> RdrNameHsType -> Literal
convIntLit i (HsTyVar n)
  | n == intPrimRdrName  = MachInt  i  
  | n == wordPrimRdrName = MachWord i
  | n == charPrimRdrName = MachChar (fromInteger i)
  | n == addrPrimRdrName && i == 0 = MachNullAddr
convIntLit i aty
  = pprPanic "Unknown integer literal type" (ppr aty $$ ppr intPrimRdrName) 

convRatLit :: Rational -> RdrNameHsType -> Literal
convRatLit r (HsTyVar n)
  | n == floatPrimRdrName  = MachFloat  r
  | n == doublePrimRdrName = MachDouble r
convRatLit i aty
  = pprPanic "Unknown rational literal type" (ppr aty $$ ppr intPrimRdrName) 


wordPrimRdrName, intPrimRdrName, floatPrimRdrName, doublePrimRdrName, addrPrimRdrName :: RdrName
wordPrimRdrName   = nameRdrName wordPrimTyConName
intPrimRdrName    = nameRdrName intPrimTyConName
charPrimRdrName   = nameRdrName charPrimTyConName
floatPrimRdrName  = nameRdrName floatPrimTyConName
doublePrimRdrName = nameRdrName doublePrimTyConName
addrPrimRdrName   = nameRdrName addrPrimTyConName

happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l
}

