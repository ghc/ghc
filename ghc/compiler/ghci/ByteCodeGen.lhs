%
% (c) The University of Glasgow 2000
%
\section[ByteCodeGen]{Generate bytecode from Core}

\begin{code}
module ByteCodeGen ( byteCodeGen ) where

#include "HsVersions.h"

--import Id
--import Name
--import PrimOp

import Outputable
import Name		( Name, getName )
import Id		( Id, idType, isDataConId_maybe )
import OrdList		( OrdList, consOL, snocOL, appOL, unitOL, 
			  nilOL, toOL, concatOL )
import FiniteMap	( FiniteMap, addListToFM, listToFM, 
			  addToFM, lookupFM, fmToList )
import CoreSyn
import Literal		( Literal(..) )
import PrimRep		( PrimRep(..) )
import CoreFVs		( freeVars )
import Type		( typePrimRep )
import DataCon		( DataCon, dataConTag, fIRST_TAG )
import Util		( zipEqual, zipWith4Equal )
import VarSet		( VarSet, varSetElems )
--import FastTypes
\end{code}

Entry point.

\begin{code}
byteCodeGen :: [CoreBind] -> [BCO Name]
byteCodeGen binds
   = let flatBinds = concatMap getBind binds
         getBind (NonRec bndr rhs) = [(bndr, freeVars rhs)]
         getBind (Rec binds)       = [(bndr, freeVars rhs) | (bndr,rhs) <- binds]
     in  
         snd (initBc [] (mapBc schemeR flatBinds))
\end{code}

The real machinery.

\begin{code}
data BCInstr
   -- Messing with the stack
   = ARGCHECK Int
   | PUSH_L Int{-size-} Int{-offset-}
   | PUSH_G Name
   | PUSH_ALTS Name{-labels the alt BCO; derived from case binder-}
   | PUSH_I Integer
   | SLIDE Int{-this many-} Int{-down by this much-}
   -- To do with the heap
   | ALLOC Int
   | MKAP Int{-place ptr to heap this far down stack-} Int{-# words-}
   | UNPACK Int
   | PACK DataCon Int
   -- Casery (in French: caseage)
   | CASE_PTR    [(Discr, BCInstrList)]
   | CASE_INT    [(Discr, BCInstrList)]
   | CASE_FLOAT  [(Discr, BCInstrList)]
   | CASE_DOUBLE [(Discr, BCInstrList)]
   -- To Infinity And Beyond
   | ENTER


type BCInstrList = OrdList BCInstr

data BCO a = BCO a BCInstrList


type Sequel = Int	-- back off to this depth before ENTER

-- Maps Ids to the offset from the stack _base_ so we don't have
-- to mess with it after each push/pop.
type BCEnv = FiniteMap Id Int	-- To find vars on the stack

lookupBCEnv :: BCEnv -> Id -> Int
lookupBCEnv env nm
   = case lookupFM env nm of
        Nothing -> pprPanic "lookupBCEnv" 
                            (ppr nm $$ char ' ' $$ vcat (map ppr (fmToList env)))
        Just xx -> xx

lookupBCEnv_maybe :: BCEnv -> Id -> Maybe Int
lookupBCEnv_maybe = lookupFM


-- Describes case alts
data Discr 
   = DiscrI Int
   | DiscrF Rational
   | DiscrD Rational
   | DiscrP Int
   | NoDiscr

-- Hmm.  This isn't really right (ie on Alpha, idSizeW Double -> 2)
-- There must be an Officially Approved way to do this somewhere.
idSizeW :: Id -> Int
idSizeW nm 
   = let pr = typePrimRep (idType nm) 
     in  case pr of IntRep -> 2
                    FloatRep -> 2
                    DoubleRep -> 3
                    PtrRep -> 1
                    other -> pprPanic "ByteCodeGen.idSizeW" (ppr pr)



-- Compile code for the right hand side of a let binding.
-- Park the resulting BCO in the monad.  Also requires the
-- variable to which this value was bound, so as to give the
-- resulting BCO a name.
schemeR :: (Id, AnnExpr Id VarSet) -> BcM ()
schemeR (nm, rhs) = schemeR_wrk nm (collect [] rhs)

collect xs (_, AnnLam x e) = collect (x:xs) e
collect xs not_lambda      = (reverse xs, not_lambda)

schemeR_wrk nm (args, body)
   = let fvs       = fst body
         all_args  = varSetElems fvs ++ args
         szsw_args = map idSizeW all_args
         szw_args  = sum szsw_args
         p_init    = listToFM (zip all_args (scanl (+) 0 szsw_args))
         argcheck  = if null args then nilOL else unitOL (ARGCHECK szw_args)
     in
     schemeE szw_args 0 p_init body 		`thenBc` \ body_code ->
     emitBc (BCO (getName nm) (appOL argcheck body_code))


-- Compile code to apply the given expression to the remaining args
-- on the stack, returning a HNF.
schemeE :: Int -> Sequel -> BCEnv -> AnnExpr Id VarSet -> BcM BCInstrList

-- Delegate tail-calls to schemeT.
schemeE d s p (fvs, AnnApp f a) = returnBc (schemeT d s 0 p (fvs, AnnApp f a))
schemeE d s p (fvs, AnnVar v)   = returnBc (schemeT d s 0 p (fvs, AnnVar v))

schemeE d s p (fvs, AnnLet binds b)
   = let (xs,rhss) = case binds of AnnNonRec x rhs  -> ([x],[rhs])
                                   AnnRec xs_n_rhss -> unzip xs_n_rhss
     in
     mapBc schemeR (zip xs rhss)			`thenBc_`
     let n     = length xs
         fvss  = map (varSetElems.fst) rhss
         sizes = map (\rhs_fvs -> 1 + sum (map idSizeW rhs_fvs)) fvss
         p'    = addListToFM p (zipE xs [d .. d+n-1])
         d'    = d + n
         infos = zipE4 fvss sizes xs [n, n-1 .. 1]
         zipE  = zipEqual "schemeE"
         zipE4 = zipWith4Equal "schemeE" (\a b c d -> (a,b,c,d))

         -- ToDo: don't build thunks for things with no free variables
         buildThunk (fvs, size, id, off)
            = case unzip (map (pushAtom d' p . AnnVar) (reverse fvs)) of
                (push_codes, pushed_szsw) 
                   -> ASSERT(sum pushed_szsw == size - 1)
                            (toOL push_codes `snocOL` PUSH_G (getName id) 
                                             `appOL` unitOL (MKAP off size))

         thunkCode = concatOL (map buildThunk infos)
         allocCode = toOL (map ALLOC sizes)
     in
     schemeE d' s p' b   				`thenBc` \ bodyCode ->
     mapBc schemeR (zip xs rhss) 			`thenBc` \_ ->
     returnBc (allocCode `appOL` thunkCode `appOL` bodyCode)


schemeE d s p (fvs, AnnCase scrut bndr alts)
   = let
        -- Top of stack is the return itbl, as usual.
        -- underneath it is the pointer to the alt_code BCO.
        -- When an alt is entered, it assumes the returned value is
        -- on top of the itbl.
        ret_frame_sizeW = 2

        -- Env and depth in which to compile the alts, not including
        -- any vars bound by the alts themselves
        d' = d + ret_frame_sizeW + idSizeW bndr
        p' = addToFM p bndr d'

        (case_instr, isAlgCase)
           = case typePrimRep (idType bndr) of
                IntRep -> (CASE_INT, False)
                FloatRep -> (CASE_FLOAT, False)
                DoubleRep -> (CASE_DOUBLE, False)
                PtrRep -> (CASE_PTR, True)
                other -> pprPanic "ByteCodeGen.schemeE" (ppr other)

        -- make the code for an alt
        codeAlt (discr, binds, rhs)
           | isAlgCase 
           = let binds_szsw = map idSizeW binds
                 binds_szw  = sum binds_szsw
                 p'' = addListToFM p' (zip binds (scanl (+) d' binds_szsw))
                 d'' = d' + binds_szw
             in schemeE d'' s p'' rhs	`thenBc` \ rhs_code -> 
                returnBc (UNPACK binds_szw `consOL` rhs_code)
           | otherwise 
           = ASSERT(null binds) schemeE d' s p' rhs

        discr (DEFAULT, binds, rhs)  = NoDiscr
        discr (DataAlt dc, binds, rhs) = DiscrP (dataConTag dc - fIRST_TAG)
        discr (LitAlt l, binds, rhs)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachFloat r   -> DiscrF r
                       MachDouble r  -> DiscrD r

        discrs = map discr alts
     in 
     mapBc codeAlt alts 				`thenBc` \ alt_codes ->
     let 
         alt_code     = case_instr (zip discrs alt_codes)
         alt_bco_name = getName bndr
         alt_bco      = BCO alt_bco_name (unitOL alt_code)
     in
     schemeE (d + ret_frame_sizeW) 
             (d + ret_frame_sizeW) p scrut		`thenBc` \ scrut_code ->

     emitBc alt_bco 					`thenBc_`
     returnBc (PUSH_ALTS alt_bco_name `consOL` scrut_code)


-- Compile code to do a tail call.  Doesn't need to be monadic.
schemeT :: Int -> Sequel -> Int -> BCEnv -> AnnExpr Id VarSet -> BCInstrList

schemeT d s narg_words p (_, AnnApp f a) 
   = let (push, arg_words) = pushAtom d p (snd a)
     in push 
        `consOL` schemeT (d+arg_words) s (narg_words+arg_words) p f

schemeT d s narg_words p (_, AnnVar f)
   | Just con <- isDataConId_maybe f
   = PACK con narg_words `consOL` SLIDE 1 (d-s-1) `consOL` unitOL ENTER
   | otherwise
   = let (push, arg_words) = pushAtom d p (AnnVar f)
     in push 
        `consOL` SLIDE (narg_words+arg_words) (d - s - narg_words)
        `consOL` unitOL ENTER


-- Push an atom onto the stack, returning suitable code & number of
-- stack words used.
pushAtom d p (AnnVar v) 
   = case lookupBCEnv_maybe p v of
        Just offset -> (PUSH_L sz offset, sz)
        Nothing     -> ASSERT(sz == 1) (PUSH_G nm, 1)
     where
        nm = getName v
        sz = idSizeW v

pushAtom d p (AnnLit lit)
   = case lit of
        MachInt i -> (PUSH_I i, 2)
\end{code}

The bytecode generator's monad.

\begin{code}
type BcM_State = [BCO Name]		-- accumulates completed BCOs

type BcM result = BcM_State -> (result, BcM_State)

mkBcM_State :: [BCO Name] -> BcM_State
mkBcM_State = id

initBc :: BcM_State -> BcM a -> (a, BcM_State)
initBc init_st m = case m init_st of { (r,st) -> (r,st) }

thenBc :: BcM a -> (a -> BcM b) -> BcM b
thenBc expr cont st
  = case expr st of { (result, st') -> cont result st' }

thenBc_ :: BcM a -> BcM b -> BcM b
thenBc_ expr cont st
  = case expr st of { (result, st') -> cont st' }

returnBc :: a -> BcM a
returnBc result st = (result, st)

mapBc :: (a -> BcM b) -> [a] -> BcM [b]
mapBc f []     = returnBc []
mapBc f (x:xs)
  = f x          `thenBc` \ r  ->
    mapBc f xs   `thenBc` \ rs ->
    returnBc (r:rs)

emitBc :: BCO Name -> BcM ()
emitBc bco bcos
   = ((), bcos)
\end{code}
