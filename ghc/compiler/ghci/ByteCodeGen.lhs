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
			  nilOL, toOL, concatOL, fromOL )
import FiniteMap	( FiniteMap, addListToFM, listToFM, 
			  addToFM, lookupFM, fmToList )
import CoreSyn
import Literal		( Literal(..) )
import PrimRep		( PrimRep(..) )
import CoreFVs		( freeVars )
import Type		( typePrimRep )
import DataCon		( DataCon, dataConTag, fIRST_TAG )
import Util		( zipEqual, zipWith4Equal, naturalMergeSortLe )
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
         final_state = runBc (BcM_State [] 0) 
                             (mapBc schemeR flatBinds `thenBc_` returnBc ())
     in  
         case final_state of
            BcM_State bcos final_ctr -> bcos

\end{code}

The real machinery.

\begin{code}
type LocalLabel = Int

data BCInstr
   -- Messing with the stack
   = ARGCHECK Int
   | PUSH_L Int{-size-} Int{-offset-}
   | PUSH_G Name
   | PUSH_I Integer
   | SLIDE Int{-this many-} Int{-down by this much-}
   -- To do with the heap
   | ALLOC Int
   | MKAP Int{-place ptr to heap this far down stack-} Int{-# words-}
   | UNPACK Int
   | PACK DataCon Int
   -- For doing case trees
   | LABEL       LocalLabel
   | TESTLT_I    Int LocalLabel
   | TESTEQ_I    Int LocalLabel
   | TESTLT_F    Float LocalLabel
   | TESTEQ_F    Float LocalLabel
   | TESTLT_D    Double LocalLabel
   | TESTEQ_D    Double LocalLabel
   | TESTLT_P    Int LocalLabel
   | TESTEQ_P    Int LocalLabel
   | CASEFAIL
   -- To Infinity And Beyond
   | ENTER

instance Outputable BCInstr where
   ppr (ARGCHECK n)          = text "ARGCHECK" <+> int n
   ppr (PUSH_L sz offset)    = text "PUSH_L  " <+> int sz <+> int offset
   ppr (PUSH_G nm)           = text "PUSH_G  " <+> ppr nm
   ppr (PUSH_I i)            = text "PUSH_I  " <+> integer i
   ppr (SLIDE n d)           = text "SLIDE   " <+> int n <+> int d
   ppr (ALLOC sz)            = text "ALLOC   " <+> int sz
   ppr (MKAP offset sz)      = text "MKAP    " <+> int offset <+> int sz
   ppr (UNPACK sz)           = text "UNPACK  " <+> int sz
   ppr (PACK dcon sz)        = text "PACK    " <+> ppr dcon <+> ppr sz
   ppr ENTER                 = text "ENTER"

pprAltCode discrs_n_codes
   = vcat (map f discrs_n_codes)
     where f (discr, code) = ppr discr <> colon <+> vcat (map ppr (fromOL code))


type BCInstrList = OrdList BCInstr

data BCO a = BCO a BCInstrList

instance Outputable a => Outputable (BCO a) where
   ppr (BCO name instrs)
      = (text "BCO" <+> ppr name <> colon)
        $$ nest 6 (vcat (map ppr (fromOL instrs)))




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
   | DiscrF Float
   | DiscrD Double
   | DiscrP Int
   | NoDiscr

instance Outputable Discr where
   ppr (DiscrI i) = int i
   ppr (DiscrF f) = text (show f)
   ppr (DiscrD d) = text (show d)
   ppr (DiscrP i) = int i
   ppr NoDiscr    = text "DEF"


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

        isAlgCase
           = case typePrimRep (idType bndr) of
                IntRep -> False ; FloatRep -> False ; DoubleRep -> False
                PtrRep -> True
                other  -> pprPanic "ByteCodeGen.schemeE" (ppr other)

        -- given an alt, return a discr and code for it.
        codeAlt alt@(discr, binds, rhs)
           | isAlgCase 
           = let binds_szsw = map idSizeW binds
                 binds_szw  = sum binds_szsw
                 p'' = addListToFM p' (zip binds (scanl (+) d' binds_szsw))
                 d'' = d' + binds_szw
             in schemeE d'' s p'' rhs	`thenBc` \ rhs_code -> 
                returnBc (my_discr alt, UNPACK binds_szw `consOL` rhs_code)
           | otherwise 
           = ASSERT(null binds) 
             schemeE d' s p' rhs	`thenBc` \ rhs_code ->
             returnBc (my_discr alt, rhs_code)

        my_discr (DEFAULT, binds, rhs)  = NoDiscr
        my_discr (DataAlt dc, binds, rhs) = DiscrP (dataConTag dc - fIRST_TAG)
        my_discr (LitAlt l, binds, rhs)
           = case l of MachInt i     -> DiscrI (fromInteger i)
                       MachFloat r   -> DiscrF (fromRational r)
                       MachDouble r  -> DiscrD (fromRational r)

     in 
     mapBc codeAlt alts 				`thenBc` \ alt_stuff ->
     mkMultiBranch alt_stuff				`thenBc` \ alt_final ->
     let 
         alt_bco_name = getName bndr
         alt_bco      = BCO alt_bco_name alt_final
     in
     schemeE (d + ret_frame_sizeW) 
             (d + ret_frame_sizeW) p scrut		`thenBc` \ scrut_code ->

     emitBc alt_bco 					`thenBc_`
     returnBc (PUSH_G alt_bco_name `consOL` scrut_code)


-- Given a bunch of alts code and their discrs, do the donkey work
-- of making a multiway branch using a switch tree.
-- What a load of hassle!
mkMultiBranch :: [(Discr, BCInstrList)] -> BcM BCInstrList
mkMultiBranch raw_ways
   = let d_way     = filter (isNoDiscr.fst) raw_ways
         notd_ways = naturalMergeSortLe 
                        (\w1 w2 -> leAlt (fst w1) (fst w2))
                        (filter (not.isNoDiscr.fst) raw_ways)

         mkTree :: [(Discr, BCInstrList)] -> Discr -> Discr -> BcM BCInstrList
         mkTree [] range_lo range_hi = returnBc the_default

         mkTree [val] range_lo range_hi
            | range_lo `eqAlt` range_hi 
            = returnBc (snd val)
            | otherwise
            = getLabelBc 				`thenBc` \ label_neq ->
              returnBc (mkTestEQ (fst val) label_neq 
			`consOL` (snd val
			`appOL`   unitOL (LABEL label_neq)
			`appOL`   the_default))

         mkTree vals range_lo range_hi
            = let n = length vals `div` 2
                  vals_lo = take n vals
                  vals_hi = drop n vals
                  v_mid = fst (head vals_hi)
              in
              getLabelBc 				`thenBc` \ label_geq ->
              mkTree vals_lo range_lo (dec v_mid) 	`thenBc` \ code_lo ->
              mkTree vals_hi v_mid range_hi 		`thenBc` \ code_hi ->
              returnBc (mkTestLT v_mid label_geq
                        `consOL` (code_lo
			`appOL`   unitOL (LABEL label_geq)
			`appOL`   code_hi))
 
         the_default 
            = case d_way of [] -> unitOL CASEFAIL
                            [(_, def)] -> def

         -- None of these will be needed if there are no non-default alts
         (mkTestLT, mkTestEQ, init_lo, init_hi)
            | null notd_ways
            = panic "mkMultiBranch: awesome foursome"
            | otherwise
            = case fst (head notd_ways) of {
              DiscrI _ -> ( \(DiscrI i) fail_label -> TESTLT_I i fail_label,
                            \(DiscrI i) fail_label -> TESTEQ_I i fail_label,
                            DiscrI minBound,
                            DiscrI maxBound );
              DiscrF _ -> ( \(DiscrF f) fail_label -> TESTLT_F f fail_label,
                            \(DiscrF f) fail_label -> TESTEQ_F f fail_label,
                            DiscrF minF,
                            DiscrF maxF );
              DiscrD _ -> ( \(DiscrD d) fail_label -> TESTLT_D d fail_label,
                            \(DiscrD d) fail_label -> TESTEQ_D d fail_label,
                            DiscrD minD,
                            DiscrD maxD );
              DiscrP _ -> ( \(DiscrP i) fail_label -> TESTLT_P i fail_label,
                            \(DiscrP i) fail_label -> TESTEQ_P i fail_label,
                            DiscrP minBound,
                            DiscrP maxBound )
              }

         (DiscrI i1) `eqAlt` (DiscrI i2) = i1 == i2
         (DiscrF f1) `eqAlt` (DiscrF f2) = f1 == f2
         (DiscrD d1) `eqAlt` (DiscrD d2) = d1 == d2
         (DiscrP i1) `eqAlt` (DiscrP i2) = i1 == i2
         NoDiscr     `eqAlt` NoDiscr     = True
         _           `eqAlt` _           = False

         (DiscrI i1) `leAlt` (DiscrI i2) = i1 <= i2
         (DiscrF f1) `leAlt` (DiscrF f2) = f1 <= f2
         (DiscrD d1) `leAlt` (DiscrD d2) = d1 <= d2
         (DiscrP i1) `leAlt` (DiscrP i2) = i1 <= i2
         NoDiscr     `leAlt` NoDiscr     = True
         _           `leAlt` _           = False

         isNoDiscr NoDiscr = True
         isNoDiscr _       = False

         dec (DiscrI i) = DiscrI (i-1)
         dec (DiscrP i) = DiscrP (i-1)
         dec other      = other		-- not really right, but if you
		-- do cases on floating values, you'll get what you deserve

         -- same snotty comment applies to the following
         minF, maxF :: Float
         minD, maxD :: Double
         minF = -1.0e37
         maxF =  1.0e37
         minD = -1.0e308
         maxD =  1.0e308
     in
         mkTree notd_ways init_lo init_hi


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
data BcM_State 
   = BcM_State { bcos      :: [BCO Name],	-- accumulates completed BCOs
                 nextlabel :: Int }		-- for generating local labels

type BcM result = BcM_State -> (result, BcM_State)

mkBcM_State :: [BCO Name] -> Int -> BcM_State
mkBcM_State = BcM_State

runBc :: BcM_State -> BcM () -> BcM_State
runBc init_st m = case m init_st of { (r,st) -> st }

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
emitBc bco st
   = ((), st{bcos = bco : bcos st})

getLabelBc :: BcM Int
getLabelBc st
   = (nextlabel st, st{nextlabel = 1 + nextlabel st})
\end{code}
