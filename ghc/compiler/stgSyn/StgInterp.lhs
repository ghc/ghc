%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-2000
%
\section[StgInterp]{Translates STG syntax to interpretable form, and run it}

\begin{code}

module StgInterp ( runStgI ) where

#include "HsVersions.h"

import StgSyn
import Id 		( Id, idPrimRep )
import Panic		( panic )
import Outputable
import Var
import PrimOp		( PrimOp(..) )
import PrimRep		( PrimRep(..) )
import Literal		( Literal(..) )
import Type		( Type, typePrimRep, deNoteType, repType, funResultTy )
import DataCon		( DataCon, dataConTag, dataConRepArgTys )
import TyCon		( TyCon, isDataTyCon, tyConFamilySize, tyConDataCons )
import ClosureInfo	( mkVirtHeapOffsets )
import Class		( Class, classTyCon )

-- giga-hack
import {-# SOURCE #-} MCI_make_constr

import PrelGHC		--( unsafeCoerce#, dataToTag#,
			--  indexPtrOffClosure#, indexWordOffClosure# )
import IO		( hPutStr, stderr )
import PrelAddr 	( Addr(..) )
import Addr		( intToAddr, addrToInt )
import Storable
import Addr		-- again ...
import Word
import Bits


runStgI :: [TyCon] -> [Class] -> [StgBinding] -> IO Int

#ifndef GHCI
runStgI tycons classes stgbinds
   = panic "runStgI called in non-GHCI build"

#else

-- the bindings need to have a binding for stgMain, and the
-- body of it had better represent something of type Int# -> Int#
runStgI tycons classes stgbinds
   = do itbl_env <- mkITbls (tycons ++ map classTyCon classes)
        let binds = concatMap (stg2bind itbl_env) stgbinds
        let dbg_txt 
               = "-------------------- Binds --------------------\n" 
                 ++ showSDoc (vcat (map (\bind -> pprBind bind $$ char ' ') binds))

        hPutStr stderr dbg_txt

        let stgMain
               = case [rhs | Bind v rhs <- binds, showSDoc (ppr v) == "stgMain"] of
                    (b:_) -> b
                    []    -> error "\n\nCan't find `stgMain'.  Giving up.\n\n"        
        let result 
               = I# (evalI (AppII stgMain (LitI 0#))
                           (mkInitialSEnv binds){-initial se (never changes)-}
                           []{-initial de-}
                    )
        return result

type ItblEnv = [(DataCon,Addr)]

-- Make info tables for the data decls in this module
mkITbls :: [TyCon] -> IO ItblEnv
mkITbls [] = return []
mkITbls (tc:tcs) = do itbls  <- mkITbl tc
                      itbls2 <- mkITbls tcs
                      return (itbls ++ itbls2)

mkITbl :: TyCon -> IO ItblEnv
mkITbl tc
--   | trace ("TYCON: " ++ showSDoc (ppr tc)) False
--   = error "?!?!"
   | not (isDataTyCon tc) 
   = return []
   | n == length dcs  -- paranoia; this is an assertion.
   = make_constr_itbls dcs
     where
        dcs = tyConDataCons tc
        n   = tyConFamilySize tc


stg2bind :: ItblEnv -> StgBinding -> [Bind]
stg2bind ie (StgNonRec v e) = [Bind v (rhs2expr ie e)]
stg2bind ie (StgRec vs_n_es) = [Bind v (rhs2expr ie e) | (v,e) <- vs_n_es]

isRec (StgNonRec _ _) = False
isRec (StgRec _)      = True

rhs2expr :: ItblEnv -> StgRhs -> Expr
rhs2expr ie (StgRhsClosure ccs binfo srt fvs uflag args rhs)
   = mkLambdas args
     where
        rhsExpr = stg2expr ie rhs
        rhsRep  = repOfStgExpr rhs
        mkLambdas [] = rhsExpr
        mkLambdas (v:vs) = mkLam (repOfId v) rhsRep v (mkLambdas vs)
rhs2expr ie (StgRhsCon ccs dcon args)
   = conapp2expr ie dcon args

conapp2expr :: ItblEnv -> DataCon -> [StgArg] -> Expr
conapp2expr ie dcon args
   = mkAppCon itbl reps exprs
     where
        itbl        = findItbl ie dcon
        exprs       = map arg2expr inHeapOrder
        reps        = map repOfArg inHeapOrder
        inHeapOrder = toHeapOrder args

        toHeapOrder :: [StgArg] -> [StgArg]
        toHeapOrder args
           = let (_, _, rearranged_w_offsets) = mkVirtHeapOffsets getArgPrimRep args
                 (rearranged, offsets) = unzip rearranged_w_offsets
             in
                 rearranged

        findItbl [] dcon
           -- Not in the list?  A bit of kludgery for testing purposes.
           | dconIs dcon "std.PrelBase.Izh"
           = prelbase_Izh_con_info
           | otherwise
           = pprPanic "StgInterp.findItbl for " (ppr dcon)
        findItbl ((dc,itbl):rest) dcon
           = if dc == dcon then itbl else findItbl rest dcon

        dconIs dcon str 
           = let cleaned = takeWhile (/= '{') (showSDocDebug (ppr dcon))
             in --trace ("Cleaned = `" ++ cleaned ++ "'") (
                str == cleaned
                --)

foreign label "PrelBase_Izh_con_info" prelbase_Izh_con_info :: Addr

-- Handle most common cases specially; do the rest with a generic
-- mechanism (deferred till later :)
mkAppCon :: Addr -> [Rep] -> [Expr] -> Expr
mkAppCon itbl []               []         = AppCon itbl
mkAppCon itbl [RepI]           [a1]       = AppConI itbl a1
mkAppCon itbl [RepP]           [a1]       = AppConP itbl a1
mkAppCon itbl [RepP,RepP]      [a1,a2]    = AppConPP itbl a1 a2
mkAppCon itbl [RepP,RepP,RepP] [a1,a2,a3] = AppConPPP itbl a1 a2 a3
mkAppCon itbl reps args
   = pprPanic "StgInterp.mkAppCon: unhandled reps" (hsep (map pprRep reps))


mkLam RepP RepP = LamPP
mkLam RepI RepP = LamIP
mkLam RepP RepI = LamPI
mkLam RepI RepI = LamII
mkLam repa repr = pprPanic "StgInterp.mkLam" (pprRep repa <+> pprRep repr)

mkApp RepP RepP = AppPP
mkApp RepI RepP = AppIP
mkApp RepP RepI = AppPI
mkApp RepI RepI = AppII
mkApp repa repr = pprPanic "StgInterp.mkApp" (pprRep repa <+> pprRep repr)

repOfId :: Id -> Rep
repOfId = primRep2Rep . idPrimRep

primRep2Rep primRep
   = case primRep of
        PtrRep -> RepP
        IntRep -> RepI
        other -> pprPanic "primRep2Rep" (ppr other)

repOfStgExpr :: StgExpr -> Rep
repOfStgExpr stgexpr
   = case stgexpr of
        StgLit lit 
           -> repOfLit lit
        StgCase scrut live liveR bndr srt alts
           -> case altRhss alts of
                 (a:_) -> repOfStgExpr a
                 []    -> panic "repOfStgExpr: no alts"
        StgApp var []
           -> repOfId var
        StgApp var args
           -> repOfApp ((deNoteType.repType.idType) var) (length args)

        StgPrimApp op args res_ty
           -> (primRep2Rep.typePrimRep) res_ty

        StgLet binds body -> repOfStgExpr body
        StgLetNoEscape live liveR binds body -> repOfStgExpr body

        StgConApp con args -> RepP -- by definition

        other 
           -> pprPanic "repOfStgExpr" (ppr other)
     where
        altRhss (StgAlgAlts ty alts def)
           = [rhs | (dcon,bndrs,uses,rhs) <- alts] ++ defRhs def
        altRhss (StgPrimAlts ty alts def)
           = [rhs | (lit,rhs) <- alts] ++ defRhs def
        defRhs StgNoDefault 
           = []
        defRhs (StgBindDefault rhs)
           = [rhs]

        -- returns the Rep of the result of applying ty to n args.
        repOfApp :: Type -> Int -> Rep
        repOfApp ty 0 = (primRep2Rep.typePrimRep) ty
        repOfApp ty n = repOfApp (funResultTy ty) (n-1)



repOfLit lit
   = case lit of
        MachInt _ -> RepI
        MachStr _ -> RepI   -- because it's a ptr outside the heap
        other -> pprPanic "repOfLit" (ppr lit)

lit2expr :: Literal -> Expr
lit2expr lit
   = case lit of
        MachInt i -> case fromIntegral i of I# i# -> LitI i#
        MachStr s -> LitS s
        other -> pprPanic "lit2expr" (ppr lit)

stg2expr :: ItblEnv -> StgExpr -> Expr
stg2expr ie stgexpr
   = case stgexpr of
        StgApp var []
           -> mkVar (repOfId var) var
        StgApp var args
           -> mkAppChain (repOfStgExpr stgexpr) (mkVar (repOfId var) var) args
        StgLit lit
           -> lit2expr lit

        StgCase scrut live liveR bndr srt (StgPrimAlts ty alts def)
           |  repOfStgExpr scrut /= RepP
           -> mkCasePrim (repOfStgExpr stgexpr) 
                         bndr (stg2expr ie scrut) 
                              (map doPrimAlt alts) 
                              (def2expr def)

        StgCase scrut live liveR bndr srt (StgAlgAlts ty alts def)
           |  repOfStgExpr scrut == RepP
           -> mkCaseAlg (repOfStgExpr stgexpr) 
                        bndr (stg2expr ie scrut) 
                             (map doAlgAlt alts) 
                             (def2expr def)

        StgPrimApp op args res_ty
           -> mkPrimOp (repOfStgExpr stgexpr)
                       op (map arg2expr args)

        StgConApp dcon args
           -> conapp2expr ie dcon args

        StgLet binds body
           |  isRec binds 
           -> mkRec (repOfStgExpr stgexpr) (stg2bind ie binds) (stg2expr ie body)
           |  otherwise
           -> mkNonRec (repOfStgExpr stgexpr) (head (stg2bind ie binds)) (stg2expr ie body)

        other 
           -> pprPanic "stg2expr" (ppr stgexpr)
     where
        doPrimAlt (lit,rhs) 
           = AltPrim (lit2expr lit) (stg2expr ie rhs)
        doAlgAlt (dcon,vars,uses,rhs) 
           = AltAlg (dataConTag dcon - 1) 
                    (map id2VaaRep (toHeapOrder vars)) (stg2expr ie rhs)

        toHeapOrder vars
           = let (_,_,rearranged_w_offsets) = mkVirtHeapOffsets idPrimRep vars
                 (rearranged,offsets)       = unzip rearranged_w_offsets
             in
                 rearranged

        def2expr StgNoDefault         = Nothing
        def2expr (StgBindDefault rhs) = Just (stg2expr ie rhs)

        mkAppChain result_rep so_far []
           = panic "mkAppChain"
        mkAppChain result_rep so_far [a]
           = mkApp (repOfArg a) result_rep so_far (arg2expr a)
        mkAppChain result_rep so_far (a:as)
           = mkAppChain result_rep (mkApp (repOfArg a) RepP so_far (arg2expr a)) as

mkCasePrim RepI = CasePrimI
mkCasePrim RepP = CasePrimP

mkCaseAlg RepI = CaseAlgI
mkCaseAlg RepP = CaseAlgP

mkVar RepI = VarI
mkVar RepP = VarP

mkRec RepI = RecI
mkRec RepP = RecP
mkNonRec RepI = NonRecI
mkNonRec RepP = NonRecP

mkPrimOp RepI = PrimOpI
mkPrimOp RepP = PrimOpP        

arg2expr :: StgArg -> Expr
arg2expr (StgVarArg v)   = mkVar (repOfId v) v
arg2expr (StgLitArg lit) = lit2expr lit
arg2expr (StgTypeArg ty) = pprPanic "arg2expr" (ppr ty)



repOfArg :: StgArg -> Rep
repOfArg (StgVarArg v)   = repOfId v
repOfArg (StgLitArg lit) = repOfLit lit
repOfArg (StgTypeArg ty) = pprPanic "repOfArg" (ppr ty)

id2VaaRep var = VaaRep var (repOfId var)

--------------------------------------------------------------------
--------------------------------------------------------------------


data Bind = Bind Vaa Expr

pprBind :: Bind -> SDoc
pprBind (Bind v e) = ppr v <+> char '=' <+> pprExpr e

binder (Bind v e) = v
bindee (Bind v e) = e


data AltAlg = AltAlg Int{-tagNo-} [VaaRep] Expr

pprAltAlg (AltAlg tag vars rhs)
   = text "Tag_" <> int tag <+> hsep (map pprVaaRep vars)
     <+> text "->" <+> pprExpr rhs


data AltPrim = AltPrim Lit Expr

pprAltPrim (AltPrim tag rhs)
   = pprExpr tag <+> text "->" <+> pprExpr rhs


-- HACK ALERT!  A Lit may *only* be one of LitI, LitL, LitF, LitD
type Lit = Expr


-- var, no rep info (inferrable from context)
-- Vaa because Var conflicts with Var.Var
--type Vaa = String
type Vaa = Id

data VaaRep = VaaRep Vaa Rep

pprVaaRep (VaaRep v r) = ppr v <> text ":" <> pprRep r


repOfVaa (VaaRep v r) = r
varOfVaa (VaaRep v r) = v

data Rep = RepI | RepP deriving Eq

pprRep RepI = text "I"
pprRep RepP = text "P"



-- LambdaXY indicates a function of reps X -> Y
-- ie var rep = X, result rep = Y
-- NOTE: repOf (LambdaXY _ _) = RepI regardless of X and Y
--
-- AppXY means apply a fn (always of Ptr rep) to 
-- an arg of rep X giving result of Rep Y
-- therefore: repOf (AppXY _ _) = RepY

-- index???OffClosure needs to traverse indirection nodes.

-- You can always tell the representation of an Expr by examining
-- its root node.
data Expr
   = CaseAlgP   Vaa Expr [AltAlg]  (Maybe Expr)
   | CasePrimP  Vaa Expr [AltPrim] (Maybe Expr)

   | CaseAlgI   Vaa Expr [AltAlg]  (Maybe Expr)
   | CasePrimI  Vaa Expr [AltPrim] (Maybe Expr)

   -- saturated constructor apps; args are in heap order.
   -- The Addrs are the info table pointers.  Descriptors refer to the
   -- arg reps; all constructor applications return pointer rep.
   | AppCon    Addr
   | AppConI   Addr Expr
   | AppConP   Addr Expr
   | AppConPP  Addr Expr Expr
   | AppConPPP Addr Expr Expr Expr

   | PrimOpI PrimOp [Expr]
   | PrimOpP PrimOp [Expr]

   | Native VoidStar

   | NonRecP Bind Expr
   | RecP    [Bind] Expr

   | NonRecI Bind Expr
   | RecI    [Bind] Expr

   | LitI   Int#  -- and LitF Float# | LitD Double# | LitL Int64#
   | LitS   FAST_STRING

   | VarP   Vaa
   | VarI   Vaa

   | LamPP  Vaa Expr
   | LamPI  Vaa Expr
   | LamIP  Vaa Expr
   | LamII  Vaa Expr

   | AppPP  Expr Expr
   | AppPI  Expr Expr
   | AppIP  Expr Expr
   | AppII  Expr Expr


pprDefault Nothing = text "NO_DEFAULT"
pprDefault (Just e) = text "DEFAULT ->" $$ nest 2 (pprExpr e)

pprExpr expr
   = case expr of
        PrimOpI op args -> doPrimOp 'I' op args
        PrimOpP op args -> doPrimOp 'P' op args

        VarI v    -> ppr v
        VarP v    -> ppr v
        LitI i#   -> int (I# i#) <> char '#'
        LitS s    -> char '"' <> ptext s <> char '"'

        LamPP v e -> doLam "PP" v e
        LamPI v e -> doLam "PI" v e
        LamIP v e -> doLam "IP" v e
        LamII v e -> doLam "II" v e

        AppPP f a -> doApp "PP" f a
        AppPI f a -> doApp "PI" f a
        AppIP f a -> doApp "IP" f a
        AppII f a -> doApp "II" f a

        CasePrimI b sc alts def -> doCasePrim 'I' b sc alts def
        CasePrimP b sc alts def -> doCasePrim 'P' b sc alts def

        CaseAlgI b sc alts def -> doCaseAlg 'I' b sc alts def
        CaseAlgP b sc alts def -> doCaseAlg 'P' b sc alts def

        NonRecP bind body -> doNonRec 'P' bind body

        AppCon    i          -> doAppCon "" i []
        AppConI   i a1       -> doAppCon "" i [a1]
        AppConP   i a1       -> doAppCon "" i [a1]
        AppConPP  i a1 a2    -> doAppCon "" i [a1,a2]
        AppConPPP i a1 a2 a3 -> doAppCon "" i [a1,a2,a3]

        other     -> text "pprExpr: unimplemented tag:" 
                     <+> text (showExprTag other)
     where
        doAppCon repstr itbl args
           = text "Con" <> text repstr <> char '_' <> (int (addrToInt itbl)) 
             <+> char '[' <> hsep (map pprExpr args) <> char ']'
        doPrimOp repchar op args
           = char repchar <> ppr op <+> char '[' <> hsep (map pprExpr args) <> char ']'
        doNonRec repchr bind body
           = vcat [text "let" <> char repchr <+> pprBind bind, text "in", pprExpr body]
        doCasePrim repchr b sc alts def
           = sep [text "CasePrim" <> char repchr 
                     <+> pprExpr sc <+> text "of" <+> ppr b <+> char '{',
                  nest 2 (vcat (map pprAltPrim alts) $$ pprDefault def),
                  char '}'
                 ]

        doCaseAlg repchr b sc alts def
           = sep [text "CaseAlg" <> char repchr 
                     <+> pprExpr sc <+> text "of" <+> ppr b <+> char '{',
                  nest 2 (vcat (map pprAltAlg alts) $$ pprDefault def),
                  char '}'
                 ]

        doApp repstr f a
           = text "(@" <> text repstr <+> pprExpr f <+> pprExpr a <> char ')'
        doLam repstr v e 
           = (char '\\' <> text repstr <+> ppr v <+> text "->") $$ pprExpr e

data VoidStar 
   = VoidStar



showExprTag :: Expr -> String
showExprTag expr
   = case expr of
        CaseAlgP  _ _ _ _ -> "CaseAlgP"
        CasePrimP _ _ _ _ -> "CasePrimP"
        CaseAlgI  _ _ _ _ -> "CaseAlgI"
        CasePrimI _ _ _ _ -> "CasePrimI"
        AppCon _          -> "AppCon"
        AppConI _ _       -> "AppConI"
        AppConP _ _       -> "AppConP"
        AppConPP _ _ _    -> "AppConPP"
        AppConPPP _ _ _ _ -> "AppConPPP"
        PrimOpI _ _       -> "PrimOpI"
        Native _          -> "Native"
        NonRecP _ _       -> "NonRecP"
        RecP _ _          -> "RecP"
        NonRecI _ _       -> "NonRecI"
        RecI _ _          -> "RecI"
        LitI _            -> "LitI"
        LitS _            -> "LitS"
        VarP _            -> "VarP"
        VarI _            -> "VarI"
        LamPP _ _         -> "LamPP"
        LamPI _ _         -> "LamPI"
        LamIP _ _         -> "LamIP"
        LamII _ _         -> "LamII"
        AppPP _ _         -> "AppPP"
        AppPI _ _         -> "AppPI"
        AppIP _ _         -> "AppIP"
        AppII _ _         -> "AppII"
        other             -> "(showExprTag:unhandled case)"

-- The dynamic environment contains everything boxed.
-- eval* functions which look up values in it will know the
-- representation of the thing they are looking up, so they
-- can cast/unbox it as necessary.
type DEnv a = [(Vaa, a)]

-- whereas the static env contains trees for top-level binds.
type SEnv = [(Vaa, Expr)]

------------------------------------------------------------------------
--- The interpreter proper                                           ---
------------------------------------------------------------------------

mkInitialSEnv :: [Bind] -> SEnv
mkInitialSEnv binds
   = unsafeCoerce# [(var,rhs) | Bind var rhs <- binds]


--------------------------------------------------------
--- Evaluator for things of boxed (pointer) representation
--------------------------------------------------------

evalP :: Expr -> SEnv -> DEnv boxed -> boxed

evalP expr se de
--   | trace ("evalP: " ++ showExprTag expr) False
   | trace ("evalP:\n" ++ showSDoc (pprExpr expr) ++ "\n") False
   = error "evalP: ?!?!"

evalP (Native p) se de
   = unsafeCoerce# p

-- First try the dynamic env.  If that fails, assume it's a top-level
-- binding and look in the static env.  That gives an Expr, which we
-- must convert to a boxed thingy by applying evalP to it.  Because
-- top-level bindings are always ptr-rep'd (either lambdas or boxed
-- CAFs), it's always safe to use evalP.
evalP (VarP v) se de 
   = case lookupDeP de v of
        Just xx -> xx
        Nothing -> evalP (lookupSe se v) se de 


-- Deal with application of a function returning a pointer rep
-- to arguments of any persuasion.  Note that the function itself
-- always has pointer rep.
evalP (AppIP e1 e2) se de 
   = unsafeCoerce# (evalP e1 se de) (evalI e2 se de)
evalP (AppPP e1 e2) se de 
   = unsafeCoerce# (evalP e1 se de) (evalP e2 se de)


-- Lambdas always return P-rep, but we need to do different things
-- depending on both the argument and result representations.
evalP (LamPP x b) se de
   = unsafeCoerce# 
        (\ xP -> evalP b se (augment de x xP))
evalP (LamPI x b) se de
   = unsafeCoerce# 
        (\ xP -> evalI b se (augment de x xP))
evalP (LamIP x b) se de
   = unsafeCoerce# 
        (\ xI -> evalP b se (augment de x (unsafeCoerce# (I# xI))))
evalP (LamII x b) se de
   = unsafeCoerce#
        (\ xI -> evalI b se (augment de x (unsafeCoerce# (I# xI))))


-- NonRec, Rec, CaseAlg and CasePrim are the same for all result reps, 
-- except in the sense that we go on and evaluate the body with whichever
-- evaluator was used for the expression as a whole.
evalP (NonRecP bind b) se de
   = evalP b se (augment_nonrec bind se de)
evalP (RecP binds b) se de
   = evalP b se (augment_rec binds se de)
evalP (CaseAlgP bndr expr alts def) se de
   = case helper_caseAlg bndr expr alts def se de of
        (rhs, de') -> evalP rhs se de'
evalP (CasePrimP bndr expr alts def) se de
   = case helper_casePrim bndr expr alts def se de of
        (rhs, de') -> evalP rhs se de'

{-
-- AppCon can only be handled by evalP
evalP (AppCon itbl args) se de
   = loop args
     where
        -- This appalling hack suggested (gleefully) by SDM
        -- It is not well typed (needless to say?)
        loop :: [Expr] -> boxed
        loop [] 
           = trace "loop-empty" (
             case itbl of A# addr# -> unsafeCoerce# (mci_make_constr addr#)
             )
        loop (a:as) 
           = trace "loop-not-empty" (
             case repOf a of
                RepI -> case evalI a se de of i# -> loop as i#
                RepP -> let p = evalP a se de in loop as p                
             )
-}

evalP (AppConI (A# itbl) a1) se de
   = case evalI a1 se de of i1 -> mci_make_constrI itbl i1

evalP (AppCon (A# itbl)) se de
   = mci_make_constr itbl

evalP (AppConP (A# itbl) a1) se de
   = let p1 = evalP a1 se de
     in  mci_make_constrP itbl p1

evalP (AppConPP (A# itbl) a1 a2) se de
   = let p1 = evalP a1 se de
         p2 = evalP a2 se de
     in  mci_make_constrPP itbl p1 p2

evalP (AppConPPP (A# itbl) a1 a2 a3) se de
   = let p1 = evalP a1 se de
         p2 = evalP a2 se de
         p3 = evalP a3 se de
     in  mci_make_constrPPP itbl p1 p2 p3



evalP other se de
   = error ("evalP: unhandled case: " ++ showExprTag other)

--------------------------------------------------------
--- Evaluator for things of Int# representation
--------------------------------------------------------


-- Evaluate something which has an unboxed Int rep
evalI :: Expr -> SEnv -> DEnv boxed -> Int#

evalI expr se de
--   | trace ("evalI: " ++ showExprTag expr) False
   | trace ("evalI:\n" ++ showSDoc (pprExpr expr) ++ "\n") False
   = error "evalP: ?!?!"

evalI (LitI i#) se de = i#

evalI (VarI v) se de = lookupDeI de v

-- Deal with application of a function returning an Int# rep
-- to arguments of any persuasion.  Note that the function itself
-- always has pointer rep.
evalI (AppII e1 e2) se de 
   = unsafeCoerce# (evalP e1 se de) (evalI e2 se de)
evalI (AppPI e1 e2) se de
   = unsafeCoerce# (evalP e1 se de) (evalP e2 se de)

-- NonRec, Rec, CaseAlg and CasePrim are the same for all result reps, 
-- except in the sense that we go on and evaluate the body with whichever
-- evaluator was used for the expression as a whole.
evalI (NonRecI bind b) se de
   = evalI b se (augment_nonrec bind se de)
evalI (RecI binds b) se de
   = evalI b se (augment_rec binds se de)
evalI (CaseAlgI bndr expr alts def) se de
   = case helper_caseAlg bndr expr alts def se de of
        (rhs, de') -> evalI rhs se de'
evalI (CasePrimI bndr expr alts def) se de
   = case helper_casePrim bndr expr alts def se de of
        (rhs, de') -> evalI rhs se de'

-- evalI can't be applied to a lambda term, by defn, since those
-- are ptr-rep'd.

evalI (PrimOpI IntAddOp [e1,e2]) se de  = evalI e1 se de +# evalI e2 se de
evalI (PrimOpI IntSubOp [e1,e2]) se de  = evalI e1 se de -# evalI e2 se de

--evalI (NonRec (Bind v e) b) se de
--   = evalI b (augment se de v (eval e se de))

evalI other se de
   = error ("evalI: unhandled case: " ++ showExprTag other)


--------------------------------------------------------
--- Helper bits and pieces
--------------------------------------------------------

-- Find something in the dynamic environment.  The values are
-- always boxed, but the caller of lookupDe* knows what representation
-- the thing really is, so we unbox it accordingly here.

lookupDeI :: DEnv boxed -> Var -> Int#
lookupDeI []          v' = error ("lookupDeI: " ++ show v')
lookupDeI ((v,u):vus) v' 
   | v == v'   = case unsafeCoerce# u of I# i -> i 
   | otherwise = lookupDeI vus v' 

-- Here, we want to allow the lookup to fail, since in that
-- case the caller (evalP VarP) will then need to search the
-- static environment instead.
lookupDeP :: DEnv boxed -> Var -> Maybe boxed
lookupDeP []          v' = Nothing
lookupDeP ((v,u):vus) v' 
   | v == v'   = Just u
   | otherwise = lookupDeP vus v' 

-- Find something in the static (top-level-binds) environment.
lookupSe :: SEnv -> Var -> Expr
lookupSe []          v' = error ("lookupSe: " ++ show v')
lookupSe ((v,u):vus) v' 
   | v == v'   = u
   | otherwise = lookupSe vus v' 


-- Find the Rep of any Expr
repOf :: Expr -> Rep

repOf (LamII _ _)      = RepP    -- careful!  Lambdas are always P-rep
repOf (LamPP _ _)      = RepP

repOf (NonRecI _ _)    = RepI
repOf (LitI _)         = RepI
repOf (VarI _)         = RepI
repOf (PrimOpI _ _)    = RepI

repOf (AppII _ _)      = RepI
repOf (AppPI _ _)      = RepI
repOf (AppPP _ _)      = RepP

repOf (AppConPP _ _ _) = RepP -- as are all AppCon's
repOf other         
   = error ("repOf: unhandled case: " ++ showExprTag other)

-- how big (in words) is one of these
repSizeW :: Rep -> Int
repSizeW RepI = 1
repSizeW RepP = 1


-- Evaluate an expression, using the appropriate evaluator,
-- then box up the result.  Note that it's only safe to use this 
-- to create values to put in the environment.  You can't use it 
-- to create a value which might get passed to native code since that
-- code will have no idea that unboxed things have been boxed.
eval :: Expr -> SEnv -> DEnv boxed -> boxed
eval expr se de
   = case repOf expr of
        RepI -> unsafeCoerce# (I# (evalI expr se de))
        RepP -> evalP expr se de


-- Evaluate the scrutinee of a case, select an alternative,
-- augment the environment appropriately, and return the alt
-- and the augmented environment.
helper_caseAlg :: Var -> Expr -> [AltAlg] -> Maybe Expr 
                  -> SEnv -> DEnv boxed
                  -> (Expr, DEnv boxed)
helper_caseAlg bndr expr alts def se de
   = let exprEv = evalP expr se de
     in  
     exprEv `seq` -- vitally important; otherwise exprEv is never eval'd
     case select_altAlg (tagOf exprEv) alts def of
        (vars,rhs) -> (rhs, augment_from_constr (augment de bndr exprEv) 
                                                exprEv (vars,1))

helper_casePrim :: Var -> Expr -> [AltPrim] -> Maybe Expr 
                   -> SEnv -> DEnv boxed
                   -> (Expr, DEnv boxed)
helper_casePrim bndr expr alts def se de
   = case repOf expr of
        -- Umm, can expr have any other rep?  Yes ...
        -- CharRep, DoubleRep, FloatRep.  What about string reps?
        RepI -> case evalI expr se de of 
                   i# -> (select_altPrim alts def (LitI i#), 
                          augment de bndr (unsafeCoerce# (I# i#)))


augment_from_constr :: DEnv boxed -> a -> ([VaaRep],Int) -> DEnv boxed
augment_from_constr de con ([],offset) 
   = de
augment_from_constr de con (v:vs,offset)
   = let v_binding
            = case repOfVaa v of
                 RepP -> indexPtrOffClosure con offset
                 RepI -> unsafeCoerce# (I# (indexIntOffClosure con offset))
     in
         augment_from_constr ((varOfVaa v,v_binding):de) con 
                             (vs,offset + repSizeW (repOfVaa v))

-- Augment the environment for a non-recursive let.
augment_nonrec :: Bind -> SEnv -> DEnv boxed -> DEnv boxed
augment_nonrec (Bind v e) se de
   = (v, eval e se de) : de

-- Augment the environment for a recursive let.
augment_rec :: [Bind] -> SEnv -> DEnv boxed -> DEnv boxed
augment_rec binds se de
   = let vars   = map binder binds
         rhss   = map bindee binds
         rhs_vs = map (\rhs -> eval rhs se de') rhss
         de'    = zip vars rhs_vs ++ de
     in
         de'

augment :: DEnv boxed -> Var -> boxed -> DEnv boxed
augment de v e = ((v,e):de)


-- a must be a constructor?
tagOf :: a -> Int
tagOf x = I# (dataToTag# x)

select_altAlg :: Int -> [AltAlg] -> Maybe Expr -> ([VaaRep],Expr)
select_altAlg tag [] Nothing = error "select_altAlg: no match and no default?!"
select_altAlg tag [] (Just def) = ([],def)
select_altAlg tag ((AltAlg tagNo vars rhs):alts) def
   = if   tag == tagNo 
     then (vars,rhs) 
     else select_altAlg tag alts def

-- literal may only be a literal, not an arbitrary expression
select_altPrim :: [AltPrim] -> Maybe Expr -> Expr -> Expr
select_altPrim [] Nothing    literal = error "select_altPrim: no match and no default?!"
select_altPrim [] (Just def) literal = def
select_altPrim ((AltPrim lit rhs):alts) def literal
   = if eqLits lit literal
     then rhs
     else select_altPrim alts def literal

eqLits (LitI i1#) (LitI i2#) = i1# ==# i2#


-- a is a constructor
indexPtrOffClosure :: a -> Int -> b
indexPtrOffClosure con (I# offset)
   = case indexPtrOffClosure# con offset of (# x #) -> x

indexIntOffClosure :: a -> Int -> Int#
indexIntOffClosure con (I# offset)
   = case wordToInt (W# (indexWordOffClosure# con offset)) of I# i# -> i#


------------------------------------------------------------------------
--- Manufacturing of info tables for DataCons defined in this module ---
------------------------------------------------------------------------

cONSTR :: Int
cONSTR = 1  -- as defined in ghc/includes/ClosureTypes.h

-- Assumes constructors are numbered from zero, not one
make_constr_itbls :: [DataCon] -> IO ItblEnv
make_constr_itbls cons
   | length cons <= 8
   = mapM mk_vecret_itbl (zip cons [0..])
   | otherwise
   = mapM mk_dirret_itbl (zip cons [0..])
     where
        mk_vecret_itbl (dcon, conNo)
           = mk_itbl dcon conNo (vecret_entry conNo)
        mk_dirret_itbl (dcon, conNo)
           = mk_itbl dcon conNo mci_constr_entry

        mk_itbl :: DataCon -> Int -> Addr -> IO (DataCon,Addr)
        mk_itbl dcon conNo entry_addr
           = let (tot_wds, ptr_wds, _) 
                    = mkVirtHeapOffsets typePrimRep (dataConRepArgTys dcon)
                 ptrs = ptr_wds
                 nptrs  = tot_wds - ptr_wds
                 itbl  = StgInfoTable {
                           ptrs = fromIntegral ptrs, nptrs = fromIntegral nptrs,
                           tipe = fromIntegral cONSTR,
                           srtlen = fromIntegral conNo,
                           code0 = fromIntegral code0, code1 = fromIntegral code1,
                           code2 = fromIntegral code2, code3 = fromIntegral code3,
                           code4 = fromIntegral code4, code5 = fromIntegral code5,
                           code6 = fromIntegral code6, code7 = fromIntegral code7 
                        }
                 -- Make a piece of code to jump to "entry_label".
                 -- This is the only arch-dependent bit.
                 -- On x86, if entry_label has an address 0xWWXXYYZZ,
                 -- emit   movl $0xWWXXYYZZ,%eax  ;  jmp *%eax
                 -- which is
                 -- B8 ZZ YY XX WW FF E0
                 (code0,code1,code2,code3,code4,code5,code6,code7)
                    = (0xB8, byte 0 entry_addr_w, byte 1 entry_addr_w, 
                             byte 2 entry_addr_w, byte 3 entry_addr_w, 
                       0xFF, 0xE0, 
                       0x90 {-nop-})

                 entry_addr_w :: Word32
                 entry_addr_w = fromIntegral (addrToInt entry_addr)
             in
                 do addr <- mallocElem itbl
                    putStrLn ("SIZE of itbl is " ++ show (sizeOf itbl))
                    putStrLn ("# ptrs  of itbl is " ++ show ptrs)
                    putStrLn ("# nptrs of itbl is " ++ show nptrs)
                    poke addr itbl
                    return (dcon, intToAddr (addrToInt addr + 8))


byte :: Int -> Word32 -> Word32
byte 0 w = w .&. 0xFF
byte 1 w = (w `shiftR` 8) .&. 0xFF
byte 2 w = (w `shiftR` 16) .&. 0xFF
byte 3 w = (w `shiftR` 24) .&. 0xFF


vecret_entry 0 = mci_constr1_entry
vecret_entry 1 = mci_constr2_entry
vecret_entry 2 = mci_constr3_entry
vecret_entry 3 = mci_constr4_entry
vecret_entry 4 = mci_constr5_entry
vecret_entry 5 = mci_constr6_entry
vecret_entry 6 = mci_constr7_entry
vecret_entry 7 = mci_constr8_entry

-- entry point for direct returns for created constr itbls
foreign label "mci_constr_entry" mci_constr_entry :: Addr
-- and the 8 vectored ones
foreign label "mci_constr1_entry" mci_constr1_entry :: Addr
foreign label "mci_constr2_entry" mci_constr2_entry :: Addr
foreign label "mci_constr3_entry" mci_constr3_entry :: Addr
foreign label "mci_constr4_entry" mci_constr4_entry :: Addr
foreign label "mci_constr5_entry" mci_constr5_entry :: Addr
foreign label "mci_constr6_entry" mci_constr6_entry :: Addr
foreign label "mci_constr7_entry" mci_constr7_entry :: Addr
foreign label "mci_constr8_entry" mci_constr8_entry :: Addr



data Constructor = Constructor Int{-ptrs-} Int{-nptrs-}


-- Ultra-minimalist version specially for constructors
data StgInfoTable = StgInfoTable {
   ptrs :: Word16,
   nptrs :: Word16,
   srtlen :: Word16,
   tipe :: Word16,
   code0, code1, code2, code3, code4, code5, code6, code7 :: Word8
}


instance Storable StgInfoTable where

   sizeOf itbl 
      = (sum . map (\f -> f itbl))
        [fieldSz ptrs, fieldSz nptrs, fieldSz srtlen, fieldSz tipe,
         fieldSz code0, fieldSz code1, fieldSz code2, fieldSz code3, 
         fieldSz code4, fieldSz code5, fieldSz code6, fieldSz code7]

   alignment itbl 
      = (sum . map (\f -> f itbl))
        [fieldAl ptrs, fieldAl nptrs, fieldAl srtlen, fieldAl tipe,
         fieldAl code0, fieldAl code1, fieldAl code2, fieldAl code3, 
         fieldAl code4, fieldAl code5, fieldAl code6, fieldAl code7]

   poke a0 itbl
      = do a1 <- store (ptrs   itbl) a0
           a2 <- store (nptrs  itbl) a1
           a3 <- store (tipe   itbl) a2
           a4 <- store (srtlen itbl) a3
           a5 <- store (code0  itbl) a4
           a6 <- store (code1  itbl) a5
           a7 <- store (code2  itbl) a6
           a8 <- store (code3  itbl) a7
           a9 <- store (code4  itbl) a8
           aA <- store (code5  itbl) a9
           aB <- store (code6  itbl) aA
           aC <- store (code7  itbl) aB
           return ()

   peek a0
      = do (a1,ptrs)   <- load a0
           (a2,nptrs)  <- load a1
           (a3,tipe)   <- load a2
           (a4,srtlen) <- load a3
           (a5,code0)  <- load a4
           (a6,code1)  <- load a5
           (a7,code2)  <- load a6
           (a8,code3)  <- load a7
           (a9,code4)  <- load a8
           (aA,code5)  <- load a9
           (aB,code6)  <- load aA
           (aC,code7)  <- load aB
           return StgInfoTable { ptrs = ptrs, nptrs = nptrs, 
                                 srtlen = srtlen, tipe = tipe,
                                 code0 = code0, code1 = code1, code2 = code2,
                                 code3 = code3, code4 = code4, code5 = code5,
                                 code6 = code6, code7 = code7 }

fieldSz :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldSz sel x = sizeOf (sel x)

fieldAl :: (Storable a, Storable b) => (a -> b) -> a -> Int
fieldAl sel x = alignment (sel x)

store :: Storable a => a -> Addr -> IO Addr
store x addr = do poke addr x
                  return (addr `plusAddr` fromIntegral (sizeOf x))

load :: Storable a => Addr -> IO (Addr, a)
load addr = do x <- peek addr
               return (addr `plusAddr` fromIntegral (sizeOf x), x)

#endif /* ndef GHCI */

\end{code}

