{
module ParserCore ( parseCore ) where

import IfaceSyn
import ForeignCall
import RdrHsSyn
import HsSyn
import RdrName
import OccName
import Kind( Kind(..) )
import Name( nameOccName, nameModule )
import Module
import PackageConfig	( mainPackageId )
import ParserCoreUtils
import LexCore
import Literal
import SrcLoc
import TysPrim( wordPrimTyCon, intPrimTyCon, charPrimTyCon, 
		floatPrimTyCon, doublePrimTyCon, addrPrimTyCon )
import TyCon ( TyCon, tyConName )
import FastString
import Outputable
import Char

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

module	:: { HsExtCore RdrName }
         : '%module' modid tdefs vdefgs { HsExtCore $2 $3 $4 }

modid	:: { Module }
        : CNAME	                 { mkModule mainPackageId  -- ToDo: wrong
		  			(mkModuleNameFS (mkFastString $1)) }

-------------------------------------------------------------
--     Type and newtype declarations are in HsSyn syntax

tdefs	:: { [TyClDecl RdrName] }
	: {- empty -}	{[]}
	| tdef ';' tdefs	{$1:$3}

tdef	:: { TyClDecl RdrName }
	: '%data' q_tc_name tv_bndrs '=' '{' cons '}'
                { mkTyData DataType (noLoc [], noLoc (ifaceExtRdrName $2), map toHsTvBndr $3) Nothing $6 Nothing }
	| '%newtype' q_tc_name tv_bndrs trep 
		{ let tc_rdr = ifaceExtRdrName $2 in
                  mkTyData NewType (noLoc [], noLoc tc_rdr, map toHsTvBndr $3) Nothing ($4 (rdrNameOcc tc_rdr)) Nothing }

-- For a newtype we have to invent a fake data constructor name
-- It doesn't matter what it is, because it won't be used
trep    :: { OccName -> [LConDecl RdrName] }
        : {- empty -}   { (\ tc_occ -> []) }
        | '=' ty        { (\ tc_occ -> let { dc_name  = mkRdrUnqual (setOccNameSpace dataName tc_occ) ;
			                     con_info = PrefixCon [toHsType $2] }
			                in [noLoc $ ConDecl (noLoc dc_name) Explicit []
					   (noLoc []) con_info ResTyH98]) }

cons	:: { [LConDecl RdrName] }
	: {- empty -}	{ [] } -- 20060420 Empty data types allowed. jds
	| con ';' cons	{ $1:$3 }

con	:: { LConDecl RdrName }
	: d_pat_occ attv_bndrs hs_atys 
		{ noLoc $ ConDecl (noLoc (mkRdrUnqual $1)) Explicit $2 (noLoc []) (PrefixCon $3) ResTyH98}
        | d_pat_occ '::' ty
                -- XXX - autrijus - $3 needs to be split into argument and return types!
                -- also not sure whether the [] below (quantified vars) appears.
                -- also the "PrefixCon []" is wrong.
                -- also we want to munge $3 somehow.
                -- extractWhatEver to unpack ty into the parts to ConDecl
                -- XXX - define it somewhere in RdrHsSyn
		{ noLoc $ ConDecl (noLoc (mkRdrUnqual $1)) Explicit [] (noLoc []) (PrefixCon []) (undefined $3) }

attv_bndrs :: { [LHsTyVarBndr RdrName] }
	: {- empty -} 	         { [] }
	| '@' tv_bndr attv_bndrs {  toHsTvBndr $2 : $3 }

hs_atys :: { [LHsType RdrName] }
         : atys               { map toHsType $1 }


---------------------------------------
--                 Types
---------------------------------------

atys	:: { [IfaceType] }
	: {- empty -}   { [] }
	| aty atys      { $1:$2 }

aty	:: { IfaceType }
	: tv_occ    { IfaceTyVar $1 }
	| q_tc_name  { IfaceTyConApp (IfaceTc $1) [] }
	| '(' ty ')' { $2 }

bty	:: { IfaceType }
	: tv_occ atys    { foldl IfaceAppTy (IfaceTyVar $1) $2 }
        | q_tc_name atys  { IfaceTyConApp (IfaceTc $1) $2 }
        | '(' ty ')' { $2 }

ty	:: { IfaceType }
	: bty	                     { $1 }
	| bty '->' ty                { IfaceFunTy $1 $3 }
	| '%forall' tv_bndrs '.' ty  { foldr IfaceForAllTy $4 $2 }

----------------------------------------------
--        Bindings are in Iface syntax

vdefgs	:: { [IfaceBinding] }
	: {- empty -}	        { [] }
	| let_bind ';' vdefgs	{ $1 : $3 }

let_bind :: { IfaceBinding }
	: '%rec' '{' vdefs1 '}' { IfaceRec $3 }
	|  vdef                 { let (b,r) = $1
				  in IfaceNonRec b r }

vdefs1	:: { [(IfaceIdBndr, IfaceExpr)] }
	: vdef		        { [$1] }
	| vdef ';' vdefs1       { $1:$3 }

vdef	:: { (IfaceIdBndr, IfaceExpr) }
	: qd_occ '::' ty '=' exp { (($1, $3), $5) }
  -- NB: qd_occ includes data constructors, because
  --     we allow data-constructor wrappers at top level
  -- But we discard the module name, because it must be the
  -- same as the module being compiled, and Iface syntax only
  -- has OccNames in binding positions

qd_occ :: { FastString }
        : var_occ { $1 }
        | d_occ   { $1 }

---------------------------------------
--  Binders
bndr	:: { IfaceBndr }
        : '@' tv_bndr 	{ IfaceTvBndr $2 }
	| id_bndr	{ IfaceIdBndr $1 }

bndrs 	:: { [IfaceBndr] }
	: bndr		{ [$1] }
	| bndr bndrs	{ $1:$2 }

id_bndr	:: { IfaceIdBndr }
	: '(' var_occ '::' ty ')'	{ ($2,$4) }

id_bndrs :: { [IfaceIdBndr] }
	: {-empty -}    	{ [] }
	| id_bndr id_bndrs	{ $1:$2 }

tv_bndr	:: { IfaceTvBndr }
	:  tv_occ                    { ($1, LiftedTypeKind) }
	|  '(' tv_occ '::' akind ')' { ($2, $4) }

tv_bndrs 	:: { [IfaceTvBndr] }
	: {- empty -}	{ [] }
	| tv_bndr tv_bndrs	{ $1:$2 }

akind	:: { IfaceKind }
	: '*' 		   { LiftedTypeKind   }	
	| '#'		   { UnliftedTypeKind }
	| '?'		   { OpenTypeKind     }
        | '(' kind ')'	   { $2 }

kind 	:: { IfaceKind }
	: akind 	   { $1 }
	| akind '->' kind  { FunKind $1 $3 }

-----------------------------------------
--             Expressions

aexp    :: { IfaceExpr }
	: var_occ	         { IfaceLcl $1 }
        | modid '.' qd_occ	 { IfaceExt (ExtPkg $1 (mkVarOccFS $3)) }
	| lit		{ IfaceLit $1 }
	| '(' exp ')' 	{ $2 }

fexp	:: { IfaceExpr }
	: fexp aexp	{ IfaceApp $1 $2 }
	| fexp '@' aty	{ IfaceApp $1 (IfaceType $3) }
	| aexp		{ $1 }

exp	:: { IfaceExpr }
	: fexp		              { $1 }
	| '\\' bndrs '->' exp 	      { foldr IfaceLam $4 $2 }
	| '%let' let_bind '%in' exp   { IfaceLet $2 $4 }
-- gaw 2004
	| '%case' '(' ty ')' aexp '%of' id_bndr
	  '{' alts1 '}'		      { IfaceCase $5 (fst $7) $3 $9 }
	| '%coerce' aty exp   	      { IfaceNote (IfaceCoerce $2) $3 }
	| '%note' STRING exp 	   
	    { case $2 of
	       --"SCC"      -> IfaceNote (IfaceSCC "scc") $3
	       "InlineMe"   -> IfaceNote IfaceInlineMe $3
            }
        | '%external' STRING aty   { IfaceFCall (ForeignCall.CCall 
                                                    (CCallSpec (StaticTarget (mkFastString $2)) 
                                                               CCallConv (PlaySafe False))) 
                                                 $3 }

alts1	:: { [IfaceAlt] }
	: alt		{ [$1] }
	| alt ';' alts1	{ $1:$3 }

alt	:: { IfaceAlt }
	: modid '.' d_pat_occ bndrs '->' exp 
		{ (IfaceDataAlt $3, map ifaceBndrName $4, $6) } 
                       -- The external syntax currently includes the types of the
		       -- the args, but they aren't needed internally
                       -- Nor is the module qualifier
	| lit '->' exp
		{ (IfaceLitAlt $1, [], $3) }
	| '%_' '->' exp
		{ (IfaceDefault, [], $3) }

lit	:: { Literal }
	: '(' INTEGER '::' aty ')'	{ convIntLit $2 $4 }
	| '(' RATIONAL '::' aty ')'	{ convRatLit $2 $4 }
	| '(' CHAR '::' aty ')'		{ MachChar $2 }
	| '(' STRING '::' aty ')'	{ MachStr (mkFastString $2) }

tv_occ	:: { FastString }
	: NAME	{ mkFastString $1 }

var_occ	:: { FastString }
	: NAME	{ mkFastString $1 }


-- Type constructor
q_tc_name	:: { IfaceExtName }
        : modid '.' CNAME	{ ExtPkg $1 (mkOccName tcName $3) }

-- Data constructor in a pattern or data type declaration; use the dataName, 
-- because that's what we expect in Core case patterns
d_pat_occ :: { OccName }
        : CNAME      { mkOccName dataName $1 }

-- Data constructor occurrence in an expression;
-- use the varName because that's the worker Id
d_occ :: { FastString }
       : CNAME { mkFastString $1 }

{

ifaceBndrName (IfaceIdBndr (n,_)) = n
ifaceBndrName (IfaceTvBndr (n,_)) = n

convIntLit :: Integer -> IfaceType -> Literal
convIntLit i (IfaceTyConApp tc [])
  | tc `eqTc` intPrimTyCon  = MachInt  i  
  | tc `eqTc` wordPrimTyCon = MachWord i
  | tc `eqTc` charPrimTyCon = MachChar (chr (fromInteger i))
  | tc `eqTc` addrPrimTyCon && i == 0 = MachNullAddr
convIntLit i aty
  = pprPanic "Unknown integer literal type" (ppr aty)

convRatLit :: Rational -> IfaceType -> Literal
convRatLit r (IfaceTyConApp tc [])
  | tc `eqTc` floatPrimTyCon  = MachFloat  r
  | tc `eqTc` doublePrimTyCon = MachDouble r
convRatLit i aty
  = pprPanic "Unknown rational literal type" (ppr aty)

eqTc :: IfaceTyCon -> TyCon -> Bool   -- Ugh!
eqTc (IfaceTc (ExtPkg mod occ)) tycon
  = mod == nameModule nm && occ == nameOccName nm
  where
    nm = tyConName tycon

-- Tiresomely, we have to generate both HsTypes (in type/class decls) 
-- and IfaceTypes (in Core expressions).  So we parse them as IfaceTypes,
-- and convert to HsTypes here.  But the IfaceTypes we can see here
-- are very limited (see the productions for 'ty', so the translation
-- isn't hard
toHsType :: IfaceType -> LHsType RdrName
toHsType (IfaceTyVar v)        		 = noLoc $ HsTyVar (mkRdrUnqual (mkTyVarOcc v))
toHsType (IfaceAppTy t1 t2)    		 = noLoc $ HsAppTy (toHsType t1) (toHsType t2)
toHsType (IfaceFunTy t1 t2)    		 = noLoc $ HsFunTy (toHsType t1) (toHsType t2)
toHsType (IfaceTyConApp (IfaceTc tc) ts) = foldl mkHsAppTy (noLoc $ HsTyVar (ifaceExtRdrName tc)) (map toHsType ts) 
toHsType (IfaceForAllTy tv t)            = add_forall (toHsTvBndr tv) (toHsType t)

toHsTvBndr :: IfaceTvBndr -> LHsTyVarBndr RdrName
toHsTvBndr (tv,k) = noLoc $ KindedTyVar (mkRdrUnqual (mkTyVarOcc tv)) k

ifaceExtRdrName :: IfaceExtName -> RdrName
ifaceExtRdrName (ExtPkg mod occ) = mkOrig mod occ
ifaceExtRdrName other = pprPanic "ParserCore.ifaceExtRdrName" (ppr other)

add_forall tv (L _ (HsForAllTy exp tvs cxt t))
  = noLoc $ HsForAllTy exp (tv:tvs) cxt t
add_forall tv t
  = noLoc $ HsForAllTy Explicit [tv] (noLoc []) t
  
happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l
}

