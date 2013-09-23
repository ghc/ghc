{
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -Wwarn -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module ParserCore ( parseCore ) where

import IfaceSyn
import ForeignCall
import RdrHsSyn
import HsSyn
import RdrName
import OccName
import TypeRep ( TyThing(..) )
import Type ( Kind,
              liftedTypeKindTyCon, openTypeKindTyCon, unliftedTypeKindTyCon,
              mkTyConApp
            )
import Kind( mkArrowKind )
import Name( Name, nameOccName, nameModule, mkExternalName, wiredInNameTyThing_maybe )
import Module
import ParserCoreUtils
import LexCore
import Literal
import SrcLoc
import PrelNames
import TysPrim
import TyCon ( TyCon, tyConName )
import FastString
import Outputable
import Data.Char
import Unique

#include "../HsVersions.h"

}

%name parseCore
%expect 0
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
 '%cast'	{ TKcast }
 '%note'	{ TKnote }
 '%external'	{ TKexternal }
 '%local'	{ TKlocal }
 '%_'		{ TKwild }
 '('		{ TKoparen }
 ')'		{ TKcparen }
 '{'		{ TKobrace }
 '}'		{ TKcbrace }
 '#' 		{ TKhash}
 '='		{ TKeq }
 ':'		{ TKcolon }
 '::'		{ TKcoloncolon }
 ':=:'		{ TKcoloneqcolon }
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
	-- : '%module' modid tdefs vdefgs	{ HsExtCore $2 $3 $4 }
	: '%module' modid tdefs vdefgs	{ HsExtCore $2 [] [] }


-------------------------------------------------------------
--     Names: the trickiest bit in here

-- A name of the form A.B.C could be:
--   module A.B.C
--   dcon C in module A.B
--   tcon C in module A.B
modid	:: { Module }
	: NAME ':' mparts		{ undefined }

q_dc_name :: { Name }
	  : NAME ':' mparts		{ undefined }

q_tc_name :: { Name }
 	  : NAME ':' mparts		{ undefined }

q_var_occ :: { Name }
          : NAME ':' vparts             { undefined }

mparts	:: { [String] }
	: CNAME				{ [$1] }
	| CNAME '.' mparts		{ $1:$3 }

vparts  :: { [String] }
        : var_occ                       { [$1] }
        | CNAME '.' vparts              { $1:$3 }

-------------------------------------------------------------
--     Type and newtype declarations are in HsSyn syntax

tdefs	:: { [TyClDecl RdrName] }
	: {- empty -}	{[]}
	| tdef tdefs	{$1:$2}

tdef	:: { TyClDecl RdrName }
	: '%data' q_tc_name tv_bndrs '=' '{' cons '}' ';'
	{ DataDecl { tcdLName = noLoc (ifaceExtRdrName $2)
                   , tcdTyVars = mkHsQTvs (map toHsTvBndr $3)
                   , tcdDataDefn = HsDataDefn { dd_ND = DataType, dd_ctxt = noLoc [] 
     	                                      , dd_kindSig = Nothing
                                              , dd_cons = $6, dd_derivs = Nothing } } }
	| '%newtype' q_tc_name tv_bndrs trep ';'
	{ let tc_rdr = ifaceExtRdrName $2 in
          DataDecl { tcdLName = noLoc tc_rdr
	           , tcdTyVars = mkHsQTvs (map toHsTvBndr $3)
                   , tcdDataDefn = HsDataDefn { dd_ND = NewType, dd_ctxt = noLoc []
		                              , dd_kindSig = Nothing
                                              , dd_cons = $4 (rdrNameOcc tc_rdr), dd_derivs = Nothing } } }

-- For a newtype we have to invent a fake data constructor name
-- It doesn't matter what it is, because it won't be used
trep    :: { OccName -> [LConDecl RdrName] }
        : {- empty -}   { (\ tc_occ -> []) }
        | '=' ty        { (\ tc_occ -> let { dc_name  = mkRdrUnqual (setOccNameSpace dataName tc_occ) ;
			                     con_info = PrefixCon [toHsType $2] }
			                in [noLoc $ mkSimpleConDecl (noLoc dc_name) []
					               (noLoc []) con_info]) }

cons	:: { [LConDecl RdrName] }
	: {- empty -}	{ [] } -- 20060420 Empty data types allowed. jds
        | con           { [$1] }
	| con ';' cons	{ $1:$3 }

con	:: { LConDecl RdrName }
	: d_pat_occ attv_bndrs hs_atys 
		{ noLoc $ mkSimpleConDecl (noLoc (mkRdrUnqual $1)) $2 (noLoc []) (PrefixCon $3) }
-- ToDo: parse record-style declarations

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
	: fs_var_occ { IfaceTyVar $1 }
	| q_tc_name  { IfaceTyConApp (IfaceTc $1) [] }
	| '(' ty ')' { $2 }

bty	:: { IfaceType }
	: fs_var_occ atys { foldl IfaceAppTy (IfaceTyVar $1) $2 }
        | q_var_occ atys  { undefined }
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
	: '%rec' '{' vdefs1 '}' { IfaceRec $3 } -- Can be empty. Do we care?
	|  vdef                 { let (b,r) = $1
				  in IfaceNonRec b r }

vdefs1	:: { [(IfaceLetBndr, IfaceExpr)] }
	: vdef  	        { [$1] }
	| vdef ';' vdefs1       { $1:$3 }

vdef	:: { (IfaceLetBndr, IfaceExpr) }
	: fs_var_occ '::' ty '=' exp { (IfLetBndr $1 $3 NoInfo, $5) }
        | '%local' vdef              { $2 }

  -- NB: qd_occ includes data constructors, because
  --     we allow data-constructor wrappers at top level
  -- But we discard the module name, because it must be the
  -- same as the module being compiled, and Iface syntax only
  -- has OccNames in binding positions. Ah, but it has Names now!

---------------------------------------
--  Binders
bndr	:: { IfaceBndr }
        : '@' tv_bndr 	{ IfaceTvBndr $2 }
	| id_bndr	{ IfaceIdBndr $1 }

bndrs 	:: { [IfaceBndr] }
	: bndr		{ [$1] }
	| bndr bndrs	{ $1:$2 }

id_bndr	:: { IfaceIdBndr }
	: '(' fs_var_occ '::' ty ')'	{ ($2,$4) }

tv_bndr	:: { IfaceTvBndr }
	:  fs_var_occ                    { ($1, ifaceLiftedTypeKind) }
	|  '(' fs_var_occ '::' akind ')' { ($2, $4) }

tv_bndrs 	:: { [IfaceTvBndr] }
	: {- empty -}	{ [] }
	| tv_bndr tv_bndrs	{ $1:$2 }

akind	:: { IfaceKind }
	: '*' 		   { ifaceLiftedTypeKind }	
	| '#'		   { ifaceUnliftedTypeKind }
	| '?'		   { ifaceOpenTypeKind }
        | '(' kind ')'	   { $2 }

kind 	:: { IfaceKind }
	: akind 	   { $1 }
	| akind '->' kind  { ifaceArrow $1 $3 }

-----------------------------------------
--             Expressions

aexp    :: { IfaceExpr }
	: fs_var_occ    { IfaceLcl $1 }
        | q_var_occ    	{ IfaceExt $1 }
	| q_dc_name	{ IfaceExt $1 }
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
	  '{' alts1 '}'		      { IfaceCase $5 (fst $7) $9 }
-- The following line is broken and is hard to fix. Not fixing now
-- because this whole parser is bitrotten anyway.
-- Richard Eisenberg, July 2013
--        | '%cast' aexp aty { IfaceCast $2 $3 }
-- No InlineMe any more
-- 	| '%note' STRING exp 	   
--	    { case $2 of
--	       --"SCC"      -> IfaceNote (IfaceSCC "scc") $3
--	       "InlineMe"   -> IfaceNote IfaceInlineMe $3
--            }
        | '%external' STRING aty   { IfaceFCall (ForeignCall.CCall 
                                                    (CCallSpec (StaticTarget (mkFastString $2) Nothing True) 
                                                               CCallConv PlaySafe)) 
                                                 $3 }

alts1	:: { [IfaceAlt] }
	: alt		{ [$1] }
	| alt ';' alts1	{ $1:$3 }

alt	:: { IfaceAlt }
	: q_dc_name bndrs '->' exp 
		{ (IfaceDataAlt $1, map ifaceBndrName $2, $4) } 
                       -- The external syntax currently includes the types of the
		       -- the args, but they aren't needed internally
                       -- Nor is the module qualifier
	| q_dc_name '->' exp 
		{ (IfaceDataAlt $1, [], $3) } 
	| lit '->' exp
		{ (IfaceLitAlt $1, [], $3) }
	| '%_' '->' exp
		{ (IfaceDefault, [], $3) }

lit	:: { Literal }
	: '(' INTEGER '::' aty ')'	{ convIntLit $2 $4 }
	| '(' RATIONAL '::' aty ')'	{ convRatLit $2 $4 }
	| '(' CHAR '::' aty ')'		{ MachChar $2 }
	| '(' STRING '::' aty ')'	{ MachStr (fastStringToByteString (mkFastString $2)) }

fs_var_occ	:: { FastString }
		: NAME	{ mkFastString $1 }

var_occ	:: { String }
	: NAME	{ $1 }


-- Data constructor in a pattern or data type declaration; use the dataName, 
-- because that's what we expect in Core case patterns
d_pat_occ :: { OccName }
        : CNAME      { mkOccName dataName $1 }

{

ifaceKind kc = IfaceTyConApp kc []

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
eqTc (IfaceTc name) tycon = name == tyConName tycon

-- Tiresomely, we have to generate both HsTypes (in type/class decls) 
-- and IfaceTypes (in Core expressions).  So we parse them as IfaceTypes,
-- and convert to HsTypes here.  But the IfaceTypes we can see here
-- are very limited (see the productions for 'ty'), so the translation
-- isn't hard
toHsType :: IfaceType -> LHsType RdrName
toHsType (IfaceTyVar v)        		 = noLoc $ HsTyVar (mkRdrUnqual (mkTyVarOccFS v))
toHsType (IfaceAppTy t1 t2)    		 = noLoc $ HsAppTy (toHsType t1) (toHsType t2)
toHsType (IfaceFunTy t1 t2)    		 = noLoc $ HsFunTy (toHsType t1) (toHsType t2)
toHsType (IfaceTyConApp (IfaceTc tc) ts) = foldl mkHsAppTy (noLoc $ HsTyVar (ifaceExtRdrName tc)) (map toHsType ts) 
toHsType (IfaceForAllTy tv t)            = add_forall (toHsTvBndr tv) (toHsType t)

-- Only a limited form of kind will be encountered... hopefully
toHsKind :: IfaceKind -> LHsKind RdrName
-- IA0_NOTE: Shouldn't we add kind variables?
toHsKind (IfaceFunTy ifK1 ifK2)  = noLoc $ HsFunTy (toHsKind ifK1) (toHsKind ifK2)
toHsKind (IfaceTyConApp ifKc []) = noLoc $ HsTyVar (nameRdrName (tyConName (toKindTc ifKc)))
toHsKind other                   = pprPanic "toHsKind" (ppr other)

toKindTc :: IfaceTyCon -> TyCon
toKindTc (IfaceTc n) | Just (ATyCon tc) <- wiredInNameTyThing_maybe n = tc
toKindTc other = pprPanic "toKindTc" (ppr other)

ifaceTcType ifTc = IfaceTyConApp ifTc []

ifaceLiftedTypeKind   = ifaceTcType (IfaceTc liftedTypeKindTyConName)
ifaceOpenTypeKind     = ifaceTcType (IfaceTc openTypeKindTyConName)
ifaceUnliftedTypeKind = ifaceTcType (IfaceTc unliftedTypeKindTyConName)

ifaceArrow ifT1 ifT2 = IfaceFunTy ifT1 ifT2

toHsTvBndr :: IfaceTvBndr -> LHsTyVarBndr RdrName
toHsTvBndr (tv,k) = noLoc $ KindedTyVar (mkRdrUnqual (mkTyVarOccFS tv)) bsig
                  where
                    bsig = toHsKind k

ifaceExtRdrName :: Name -> RdrName
ifaceExtRdrName name = mkOrig (nameModule name) (nameOccName name)
ifaceExtRdrName other = pprPanic "ParserCore.ifaceExtRdrName" (ppr other)

add_forall tv (L _ (HsForAllTy exp tvs cxt t))
  = noLoc $ HsForAllTy exp (mkHsQTvs (tv : hsQTvBndrs tvs)) cxt t
add_forall tv t
  = noLoc $ HsForAllTy Explicit (mkHsQTvs [tv]) (noLoc []) t
  
happyError :: P a 
happyError s l = failP (show l ++ ": Parse error\n") (take 100 s) l
}

