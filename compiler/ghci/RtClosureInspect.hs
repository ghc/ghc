-----------------------------------------------------------------------------
--
-- GHC Interactive support for inspecting arbitrary closures at runtime
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module RtClosureInspect(
  
     cvObtainTerm,       -- :: HscEnv -> Bool -> Maybe Type -> HValue -> IO Term

     Term(..),
     pprTerm, 
     cPprTerm, 
     cPprTermBase,
     termType,
     foldTerm, 
     TermFold(..), 
     idTermFold, 
     idTermFoldM,
     isFullyEvaluated, 
     isPointed,
     isFullyEvaluatedTerm,
     mapTermType,
     termTyVars,
--     unsafeDeepSeq, 
     cvReconstructType
 ) where 

#include "HsVersions.h"

import ByteCodeItbls    ( StgInfoTable )
import qualified ByteCodeItbls as BCI( StgInfoTable(..) )
import ByteCodeLink     ( HValue )
import HscTypes         ( HscEnv )

import DataCon          
import Type             
import TcRnMonad        ( TcM, initTcPrintErrors, ioToTcRn, recoverM
                        , writeMutVar )
import TcType
import TcMType
import TcUnify
import TcGadt
import TyCon		
import Var
import Name 
import VarEnv
import OccName
import VarSet
import {-#SOURCE#-} TcRnDriver ( tcRnRecoverDataCon )

import TysPrim		
import PrelNames
import TysWiredIn

import Constants
import Outputable
import Maybes
import Panic
import FiniteMap

import GHC.Arr          ( Array(..) )
import GHC.Ptr          ( Ptr(..), castPtr )
import GHC.Exts

import Control.Monad
import Data.Maybe
import Data.Array.Base
import Data.List        ( partition, nub )
import Foreign
import System.IO.Unsafe

---------------------------------------------
-- * A representation of semi evaluated Terms
---------------------------------------------
{-
  A few examples in this representation:

  > Just 10 = Term Data.Maybe Data.Maybe.Just (Just 10) [Term Int I# (10) "10"]

  > (('a',_,_),_,('b',_,_)) = 
      Term ((Char,b,c),d,(Char,e,f)) (,,) (('a',_,_),_,('b',_,_))
          [ Term (Char, b, c) (,,) ('a',_,_) [Term Char C# "a", Suspension, Suspension]
          , Suspension
          , Term (Char, e, f) (,,) ('b',_,_) [Term Char C# "b", Suspension, Suspension]]
-}

data Term = Term { ty        :: Type 
                 , dc        :: DataCon 
                 , val       :: HValue 
                 , subTerms  :: [Term] }

          | Prim { ty        :: Type
                 , value     :: [Word] }

          | Suspension { ctype    :: ClosureType
                       , mb_ty    :: Maybe Type
                       , val      :: HValue
                       , bound_to :: Maybe Name   -- Useful for printing
                       }

isTerm Term{} = True
isTerm   _    = False
isSuspension Suspension{} = True
isSuspension      _       = False
isPrim Prim{} = True
isPrim   _    = False

termType t@(Suspension {}) = mb_ty t
termType t = Just$ ty t

isFullyEvaluatedTerm :: Term -> Bool
isFullyEvaluatedTerm Term {subTerms=tt} = all isFullyEvaluatedTerm tt
isFullyEvaluatedTerm Suspension {}      = False
isFullyEvaluatedTerm Prim {}            = True

instance Outputable (Term) where
 ppr = head . cPprTerm cPprTermBase

-------------------------------------------------------------------------
-- Runtime Closure Datatype and functions for retrieving closure related stuff
-------------------------------------------------------------------------
data ClosureType = Constr 
                 | Fun 
                 | Thunk Int 
                 | ThunkSelector
                 | Blackhole 
                 | AP 
                 | PAP 
                 | Indirection Int 
                 | Other Int
 deriving (Show, Eq)

data Closure = Closure { tipe         :: ClosureType 
                       , infoPtr      :: Ptr ()
                       , infoTable    :: StgInfoTable
                       , ptrs         :: Array Int HValue
                       , nonPtrs      :: [Word]
                       }

instance Outputable ClosureType where
  ppr = text . show 

#include "../includes/ClosureTypes.h"

aP_CODE = AP
pAP_CODE = PAP
#undef AP
#undef PAP

getClosureData :: a -> IO Closure
getClosureData a =
   case unpackClosure# a of 
     (# iptr, ptrs, nptrs #) -> do
           itbl <- peek (Ptr iptr)
           let tipe = readCType (BCI.tipe itbl)
               elems = BCI.ptrs itbl 
               ptrsList = Array 0 (fromIntegral$ elems) ptrs
               nptrs_data = [W# (indexWordArray# nptrs i)
                              | I# i <- [0.. fromIntegral (BCI.nptrs itbl)] ]
           ptrsList `seq` 
            return (Closure tipe (Ptr iptr) itbl ptrsList nptrs_data)

readCType :: Integral a => a -> ClosureType
readCType i
 | i >= CONSTR && i <= CONSTR_NOCAF_STATIC = Constr
 | i >= FUN    && i <= FUN_STATIC          = Fun
 | i >= THUNK  && i < THUNK_SELECTOR       = Thunk (fromIntegral i)
 | i == THUNK_SELECTOR                     = ThunkSelector
 | i == BLACKHOLE                          = Blackhole
 | i >= IND    && i <= IND_STATIC          = Indirection (fromIntegral i)
 | fromIntegral i == aP_CODE               = AP
 | i == AP_STACK                           = AP
 | fromIntegral i == pAP_CODE              = PAP
 | otherwise                               = Other (fromIntegral i)

isConstr, isIndirection :: ClosureType -> Bool
isConstr Constr = True
isConstr    _   = False

isIndirection (Indirection _) = True
--isIndirection ThunkSelector = True
isIndirection _ = False

isThunk (Thunk _)     = True
isThunk ThunkSelector = True
isThunk AP            = True
isThunk _             = False

isFullyEvaluated :: a -> IO Bool
isFullyEvaluated a = do 
  closure <- getClosureData a 
  case tipe closure of
    Constr -> do are_subs_evaluated <- amapM isFullyEvaluated (ptrs closure)
                 return$ and are_subs_evaluated
    otherwise -> return False
  where amapM f = sequence . amap' f

amap' f (Array i0 i arr#) = map (\(I# i#) -> case indexArray# arr# i# of
                                   (# e #) -> f e)
                                [0 .. i - i0]

-- TODO: Fix it. Probably the otherwise case is failing, trace/debug it
{-
unsafeDeepSeq :: a -> b -> b
unsafeDeepSeq = unsafeDeepSeq1 2
 where unsafeDeepSeq1 0 a b = seq a $! b
       unsafeDeepSeq1 i a b   -- 1st case avoids infinite loops for non reducible thunks
        | not (isConstr tipe) = seq a $! unsafeDeepSeq1 (i-1) a b     
     -- | unsafePerformIO (isFullyEvaluated a) = b
        | otherwise = case unsafePerformIO (getClosureData a) of
                        closure -> foldl' (flip unsafeDeepSeq) b (ptrs closure)
        where tipe = unsafePerformIO (getClosureType a)
-}
isPointed :: Type -> Bool
isPointed t | Just (t, _) <- splitTyConApp_maybe t 
            = not$ isUnliftedTypeKind (tyConKind t)
isPointed _ = True

extractUnboxed  :: [Type] -> Closure -> [[Word]]
extractUnboxed tt clos = go tt (nonPtrs clos)
   where sizeofType t
           | Just (tycon,_) <- splitTyConApp_maybe t
           = ASSERT (isPrimTyCon tycon) sizeofTyCon tycon
           | otherwise = pprPanic "Expected a TcTyCon" (ppr t)
         go [] _ = []
         go (t:tt) xx 
           | (x, rest) <- splitAt (sizeofType t `div` wORD_SIZE) xx 
           = x : go tt rest

sizeofTyCon = sizeofPrimRep . tyConPrimRep

-----------------------------------
-- * Traversals for Terms
-----------------------------------

data TermFold a = TermFold { fTerm :: Type -> DataCon -> HValue -> [a] -> a
                           , fPrim :: Type -> [Word] -> a
                           , fSuspension :: ClosureType -> Maybe Type -> HValue
                                           -> Maybe Name -> a
                           }

foldTerm :: TermFold a -> Term -> a
foldTerm tf (Term ty dc v tt) = fTerm tf ty dc v (map (foldTerm tf) tt)
foldTerm tf (Prim ty    v   ) = fPrim tf ty v
foldTerm tf (Suspension ct ty v b) = fSuspension tf ct ty v b

idTermFold :: TermFold Term
idTermFold = TermFold {
              fTerm = Term,
              fPrim = Prim,
              fSuspension = Suspension
                      }
idTermFoldM :: Monad m => TermFold (m Term)
idTermFoldM = TermFold {
              fTerm       = \ty dc v tt -> sequence tt >>= return . Term ty dc v,
              fPrim       = (return.). Prim,
              fSuspension = (((return.).).). Suspension
                       }

mapTermType f = foldTerm idTermFold {
          fTerm       = \ty dc hval tt -> Term (f ty) dc hval tt,
          fSuspension = \ct mb_ty hval n ->
                          Suspension ct (fmap f mb_ty) hval n }

termTyVars = foldTerm TermFold {
            fTerm       = \ty _ _ tt   -> 
                          tyVarsOfType ty `plusVarEnv` concatVarEnv tt,
            fSuspension = \_ mb_ty _ _ -> 
                          maybe emptyVarEnv tyVarsOfType mb_ty,
            fPrim       = \ _ _ -> emptyVarEnv }
    where concatVarEnv = foldr plusVarEnv emptyVarEnv
----------------------------------
-- Pretty printing of terms
----------------------------------

app_prec::Int
app_prec = 10

pprTerm :: Int -> Term -> SDoc
pprTerm p Term{dc=dc, subTerms=tt} 
{-  | dataConIsInfix dc, (t1:t2:tt') <- tt 
  = parens (pprTerm1 True t1 <+> ppr dc <+> pprTerm1 True ppr t2) 
    <+> hsep (map (pprTerm1 True) tt) 
-}
  | null tt   = ppr dc
  | otherwise = cparen (p >= app_prec) 
                       (ppr dc <+> sep (map (pprTerm app_prec) tt))

  where fixity   = undefined 

pprTerm _ t = pprTerm1 t

pprTerm1 Prim{value=words, ty=ty} = text$ repPrim (tyConAppTyCon ty) words
pprTerm1 t@Term{} = pprTerm 0 t 
pprTerm1 Suspension{bound_to=Nothing} =  char '_' -- <> ppr ct <> char '_'
pprTerm1 Suspension{mb_ty=Just ty, bound_to=Just n}
  | Just _ <- splitFunTy_maybe ty = ptext SLIT("<function>")
  | otherwise = parens$ ppr n <> text "::" <> ppr ty 


cPprTerm :: forall m. Monad m => 
           ((Int->Term->m SDoc)->[Int->Term->m (Maybe SDoc)]) -> Term -> m SDoc
cPprTerm custom = go 0 where
  go prec t@Term{subTerms=tt, dc=dc} = do
    let mb_customDocs = map (($t) . ($prec)) (custom go) :: [m (Maybe SDoc)]    
    first_success <- firstJustM mb_customDocs
    case first_success of
      Just doc -> return$ cparen (prec>app_prec+1) doc
--    | dataConIsInfix dc, (t1:t2:tt') <- tt =
      Nothing  -> do pprSubterms <- mapM (go (app_prec+1)) tt
                     return$ cparen (prec >= app_prec) 
                                    (ppr dc <+> sep pprSubterms)
  go _ t = return$ pprTerm1 t
  firstJustM (mb:mbs) = mb >>= maybe (firstJustM mbs) (return . Just)
  firstJustM [] = return Nothing

cPprTermBase :: Monad m => (Int->Term-> m SDoc)->[Int->Term->m (Maybe SDoc)]
cPprTermBase pprP =
  [ 
    ifTerm isTupleDC            (\_ -> liftM (parens . hcat . punctuate comma) 
                                 . mapM (pprP (-1)) . subTerms)
  , ifTerm (isDC consDataCon)   (\ p Term{subTerms=[h,t]} -> doList p h t)
  , ifTerm (isDC intDataCon)    (coerceShow$ \(a::Int)->a)
  , ifTerm (isDC charDataCon)   (coerceShow$ \(a::Char)->a)
--  , ifTerm (isDC wordDataCon) (coerceShow$ \(a::Word)->a)
  , ifTerm (isDC floatDataCon)  (coerceShow$ \(a::Float)->a)
  , ifTerm (isDC doubleDataCon) (coerceShow$ \(a::Double)->a)
  , ifTerm isIntegerDC          (coerceShow$ \(a::Integer)->a)
  ] 
     where ifTerm pred f p t = if pred t then liftM Just (f p t) 
                                         else return Nothing
           isIntegerDC Term{dc=dc} = 
              dataConName dc `elem` [ smallIntegerDataConName
                                    , largeIntegerDataConName] 
           isTupleDC Term{dc=dc} = dc `elem` snd (unzip (elems boxedTupleArr))
           isDC a_dc Term{dc=dc} = a_dc == dc
           coerceShow f _ = return . text . show . f . unsafeCoerce# . val
           --TODO pprinting of list terms is not lazy
           doList p h t = do
               let elems = h : getListTerms t
                   isConsLast = termType(last elems) /= termType h
               print_elems <- mapM (pprP 5) elems
               return$ if isConsLast
                     then cparen (p >= 5) . hsep . punctuate (space<>colon) 
                           $ print_elems
                     else brackets (hcat$ punctuate comma print_elems)

                where Just a /= Just b = not (a `coreEqType` b)
                      _      /=   _    = True
                      getListTerms Term{subTerms=[h,t]} = h : getListTerms t
                      getListTerms t@Term{subTerms=[]}  = []
                      getListTerms t@Suspension{}       = [t]
                      getListTerms t = pprPanic "getListTerms" (ppr t)

repPrim :: TyCon -> [Word] -> String
repPrim t = rep where 
   rep x
    | t == charPrimTyCon   = show (build x :: Char)
    | t == intPrimTyCon    = show (build x :: Int)
    | t == wordPrimTyCon   = show (build x :: Word)
    | t == floatPrimTyCon  = show (build x :: Float)
    | t == doublePrimTyCon = show (build x :: Double)
    | t == int32PrimTyCon  = show (build x :: Int32)
    | t == word32PrimTyCon = show (build x :: Word32)
    | t == int64PrimTyCon  = show (build x :: Int64)
    | t == word64PrimTyCon = show (build x :: Word64)
    | t == addrPrimTyCon   = show (nullPtr `plusPtr` build x)
    | t == stablePtrPrimTyCon  = "<stablePtr>"
    | t == stableNamePrimTyCon = "<stableName>"
    | t == statePrimTyCon      = "<statethread>"
    | t == realWorldTyCon      = "<realworld>"
    | t == threadIdPrimTyCon   = "<ThreadId>"
    | t == weakPrimTyCon       = "<Weak>"
    | t == arrayPrimTyCon      = "<array>"
    | t == byteArrayPrimTyCon  = "<bytearray>"
    | t == mutableArrayPrimTyCon = "<mutableArray>"
    | t == mutableByteArrayPrimTyCon = "<mutableByteArray>"
    | t == mutVarPrimTyCon= "<mutVar>"
    | t == mVarPrimTyCon  = "<mVar>"
    | t == tVarPrimTyCon  = "<tVar>"
    | otherwise = showSDoc (char '<' <> ppr t <> char '>')
    where build ww = unsafePerformIO $ withArray ww (peek . castPtr) 
--   This ^^^ relies on the representation of Haskell heap values being 
--   the same as in a C array. 

-----------------------------------
-- Type Reconstruction
-----------------------------------
{-
Type Reconstruction is type inference done on heap closures.
The algorithm walks the heap generating a set of equations, which
are solved with syntactic unification.
A type reconstruction equation looks like:

  <datacon reptype>  =  <actual heap contents> 

The full equation set is generated by traversing all the subterms, starting
from a given term.

The only difficult part is that newtypes are only found in the lhs of equations.
Right hand sides are missing them. We can either (a) drop them from the lhs, or 
(b) reconstruct them in the rhs when possible. 

The function congruenceNewtypes takes a shot at (b)
-}

-- The Type Reconstruction monad
type TR a = TcM a

runTR :: HscEnv -> TR a -> IO a
runTR hsc_env c = do 
  mb_term <- initTcPrintErrors hsc_env iNTERACTIVE c
  case mb_term of 
    Nothing -> panic "Can't unify"
    Just x -> return x

trIO :: IO a -> TR a 
trIO = liftTcM . ioToTcRn

liftTcM = id

newVar :: Kind -> TR TcTyVar
newVar = liftTcM . newFlexiTyVar

-- | Returns the instantiated type scheme ty', and the substitution sigma 
--   such that sigma(ty') = ty 
instScheme :: Type -> TR (TcType, TvSubst)
instScheme ty | (tvs, rho) <- tcSplitForAllTys ty = liftTcM$ do
   (tvs',theta,ty') <- tcInstType (mapM tcInstTyVar) ty
   return (ty', zipTopTvSubst tvs' (mkTyVarTys tvs))

-- Adds a constraint of the form t1 == t2
-- t1 is expected to come from walking the heap
-- t2 is expected to come from a datacon signature
-- Before unification, congruenceNewtypes needs to
-- do its magic.
addConstraint :: TcType -> TcType -> TR ()
addConstraint t1 t2  = congruenceNewtypes t1 t2 >>= uncurry unifyType 



-- Type & Term reconstruction 
cvObtainTerm :: HscEnv -> Bool -> Maybe Type -> HValue -> IO Term
cvObtainTerm hsc_env force mb_ty hval = runTR hsc_env $ do
   tv <- liftM mkTyVarTy (newVar argTypeKind)
   case mb_ty of
     Nothing -> go tv tv hval >>= zonkTerm
     Just ty | isMonomorphic ty -> go ty ty hval >>= zonkTerm
     Just ty -> do 
              (ty',rev_subst) <- instScheme (sigmaType ty)
              addConstraint tv ty'
              term <- go tv tv hval >>= zonkTerm
              --restore original Tyvars
              return$ mapTermType (substTy rev_subst) term
    where 
  go tv ty a = do 
    let monomorphic = not(isTyVarTy tv)   
    -- This ^^^ is a convention. The ancestor tests for
    -- monomorphism and passes a type instead of a tv
    clos <- trIO $ getClosureData a
    case tipe clos of
-- Thunks we may want to force
-- NB. this won't attempt to force a BLACKHOLE.  Even with :force, we never
-- force blackholes, because it would almost certainly result in deadlock,
-- and showing the '_' is more useful.
      t | isThunk t && force -> seq a $ go tv ty a
-- We always follow indirections 
      Indirection _ -> go tv ty $! (ptrs clos ! 0)
 -- The interesting case
      Constr -> do
        m_dc <- trIO$ tcRnRecoverDataCon hsc_env (infoPtr clos)
        case m_dc of
          Nothing -> panic "Can't find the DataCon for a term"
          Just dc -> do 
            let extra_args = length(dataConRepArgTys dc) - 
                             length(dataConOrigArgTys dc)
                subTtypes  = matchSubTypes dc ty
                (subTtypesP, subTtypesNP) = partition isPointed subTtypes
            subTermTvs <- sequence
                 [ if isMonomorphic t then return t 
                                      else (mkTyVarTy `fmap` newVar k)
                   | (t,k) <- zip subTtypesP (map typeKind subTtypesP)]
            -- It is vital for newtype reconstruction that the unification step
            --  is done right here, _before_ the subterms are RTTI reconstructed
            when (not monomorphic) $ do
                  let myType = mkFunTys (reOrderTerms subTermTvs 
                                                      subTtypesNP 
                                                      subTtypes) 
                                        tv
                  (signatureType,_) <- instScheme(dataConRepType dc) 
                  addConstraint myType signatureType
            subTermsP <- sequence $ drop extra_args 
                                 -- ^^^  all extra arguments are pointed
                  [ appArr (go tv t) (ptrs clos) i
                   | (i,tv,t) <- zip3 [0..] subTermTvs subTtypesP]
            let unboxeds   = extractUnboxed subTtypesNP clos
                subTermsNP = map (uncurry Prim) (zip subTtypesNP unboxeds)      
                subTerms   = reOrderTerms subTermsP subTermsNP 
                                (drop extra_args subTtypes)
            return (Term tv dc a subTerms)
-- The otherwise case: can be a Thunk,AP,PAP,etc.
      otherwise -> 
         return (Suspension (tipe clos) (Just tv) a Nothing)

  matchSubTypes dc ty
    | Just (_,ty_args) <- splitTyConApp_maybe (repType ty) 
    , null (dataConExTyVars dc)  --TODO case of extra existential tyvars
    = dataConInstArgTys dc ty_args

    | otherwise = dataConRepArgTys dc

-- This is used to put together pointed and nonpointed subterms in the 
--  correct order.
  reOrderTerms _ _ [] = []
  reOrderTerms pointed unpointed (ty:tys) 
   | isPointed ty = ASSERT2(not(null pointed)
                            , ptext SLIT("reOrderTerms") $$ 
                                        (ppr pointed $$ ppr unpointed))
                    head pointed : reOrderTerms (tail pointed) unpointed tys
   | otherwise    = ASSERT2(not(null unpointed)
                           , ptext SLIT("reOrderTerms") $$ 
                                       (ppr pointed $$ ppr unpointed))
                    head unpointed : reOrderTerms pointed (tail unpointed) tys



-- Fast, breadth-first Type reconstruction

cvReconstructType :: HscEnv -> Bool -> Maybe Type -> HValue -> IO Type
cvReconstructType hsc_env force mb_ty hval = runTR hsc_env $ do
   tv <- liftM mkTyVarTy (newVar argTypeKind)
   case mb_ty of
     Nothing -> do search (isMonomorphic `fmap` zonkTcType tv) 
                       (uncurry go) 
                       [(tv, hval)]  
                   zonkTcType tv  -- TODO untested!
     Just ty | isMonomorphic ty -> return ty
     Just ty -> do 
              (ty',rev_subst) <- instScheme (sigmaType ty)
              addConstraint tv ty'
              search (isMonomorphic `fmap` zonkTcType tv) 
                     (uncurry go) 
                     [(tv, hval)]
              substTy rev_subst `fmap` zonkTcType tv
    where 
--  search :: m Bool -> ([a] -> [a] -> [a]) -> [a] -> m ()
  search stop expand []     = return ()
  search stop expand  (x:xx) = do new <- expand x 
                                  unlessM stop $ search stop expand (xx ++ new)

   -- returns unification tasks,since we are going to want a breadth-first search
  go :: Type -> HValue -> TR [(Type, HValue)]
  go tv a = do 
    clos <- trIO $ getClosureData a
    case tipe clos of
      Indirection _ -> go tv $! (ptrs clos ! 0)
      Constr -> do
        m_dc <- trIO$ tcRnRecoverDataCon hsc_env (infoPtr clos)
        case m_dc of
          Nothing -> panic "Can't find the DataCon for a term"
          Just dc -> do 
            let extra_args = length(dataConRepArgTys dc) - 
                             length(dataConOrigArgTys dc)
            subTtypes <- mapMif (not . isMonomorphic)
                                (\t -> mkTyVarTy `fmap` newVar (typeKind t))
                                (dataConRepArgTys dc)
            -- It is vital for newtype reconstruction that the unification step
            -- is done right here, _before_ the subterms are RTTI reconstructed
            let myType         = mkFunTys subTtypes tv
            (signatureType,_) <- instScheme(dataConRepType dc) 
            addConstraint myType signatureType
            return $ map (\(I# i#,t) -> case ptrs clos of 
                             (Array _ _ ptrs#) -> case indexArray# ptrs# i# of 
                                                       (# e #) -> (t,e))
                        (drop extra_args $ zip [0..] subTtypes)
      otherwise -> return []


-- Dealing with newtypes
{-
   A parallel fold over two Type values, 
 compensating for missing newtypes on both sides. 
 This is necessary because newtypes are not present 
 in runtime, but since sometimes there is evidence 
 available we do our best to reconstruct them. 
   Evidence can come from DataCon signatures or 
 from compile-time type inference.
   I am using the words congruence and rewriting 
 because what we are doing here is an approximation 
 of unification modulo a set of equations, which would 
 come from newtype definitions. These should be the 
 equality coercions seen in System Fc. Rewriting 
 is performed, taking those equations as rules, 
 before launching unification.

   It doesn't make sense to rewrite everywhere, 
 or we would end up with all newtypes. So we rewrite 
 only in presence of evidence.
   The lhs comes from the heap structure of ptrs,nptrs. 
   The rhs comes from a DataCon type signature. 
 Rewriting in the rhs is restricted to the result type.

   Note that it is very tricky to make this 'rewriting'
 work with the unification implemented by TcM, where
 substitutions are 'inlined'. The order in which 
 constraints are unified is vital for this (or I am 
 using TcM wrongly).
-}
congruenceNewtypes ::  TcType -> TcType -> TcM (TcType,TcType)
congruenceNewtypes = go True
  where 
   go rewriteRHS lhs rhs  
 -- TyVar lhs inductive case
    | Just tv <- getTyVar_maybe lhs 
    = recoverM (return (lhs,rhs)) $ do  
         Indirect ty_v <- readMetaTyVar tv
         (lhs', rhs') <- go rewriteRHS ty_v rhs
         writeMutVar (metaTvRef tv) (Indirect lhs')
         return (lhs, rhs')
 -- TyVar rhs inductive case
    | Just tv <- getTyVar_maybe rhs 
    = recoverM (return (lhs,rhs)) $ do  
         Indirect ty_v <- readMetaTyVar tv
         (lhs', rhs') <- go rewriteRHS lhs ty_v
         writeMutVar (metaTvRef tv) (Indirect rhs')
         return (lhs', rhs)
-- FunTy inductive case
    | Just (l1,l2) <- splitFunTy_maybe lhs
    , Just (r1,r2) <- splitFunTy_maybe rhs
    = do (l2',r2') <- go True l2 r2
         (l1',r1') <- go False l1 r1
         return (mkFunTy l1' l2', mkFunTy r1' r2')
-- TyconApp Inductive case; this is the interesting bit.
    | Just (tycon_l, args_l) <- splitNewTyConApp_maybe lhs
    , Just (tycon_r, args_r) <- splitNewTyConApp_maybe rhs = do

      let (tycon_l',args_l') = if isNewTyCon tycon_r && not(isNewTyCon tycon_l)
                                then (tycon_r, rewrite tycon_r lhs)
                                else (tycon_l, args_l)
          (tycon_r',args_r') = if rewriteRHS && isNewTyCon tycon_l && 
                                    not(isNewTyCon tycon_r)
                                then (tycon_l, rewrite tycon_l rhs)
                                else (tycon_r, args_r)
      (args_l'', args_r'') <- unzip `liftM` zipWithM (go rewriteRHS) 
                                                     args_l' 
                                                     args_r'
      return (mkTyConApp tycon_l' args_l'', mkTyConApp tycon_r' args_r'') 

    | otherwise = return (lhs,rhs)

    where rewrite newtyped_tc lame_tipe
           | (tvs, tipe) <- newTyConRep newtyped_tc 
           = case tcUnifyTys (const BindMe) [tipe] [lame_tipe] of
               Just subst -> substTys subst (map mkTyVarTy tvs)
               otherwise  -> panic "congruenceNewtypes: Can't unify a newtype"


--------------------------------------------------------------------------------

isMonomorphic ty | (tvs, ty') <- splitForAllTys ty
                 = null tvs && (isEmptyVarSet . tyVarsOfType) ty'

mapMif :: Monad m => (a -> Bool) -> (a -> m a) -> [a] -> m [a]
mapMif pred f xx = sequence $ mapMif_ pred f xx
mapMif_ pred f []     = []
mapMif_ pred f (x:xx) = (if pred x then f x else return x) : mapMif_ pred f xx

unlessM condM acc = condM >>= \c -> unless c acc

-- Strict application of f at index i
appArr f (Array _ _ ptrs#) (I# i#) = case indexArray# ptrs# i# of 
                                       (# e #) -> f e

zonkTerm :: Term -> TcM Term
zonkTerm = foldTerm idTermFoldM {
              fTerm = \ty dc v tt -> sequence tt      >>= \tt ->
                                     zonkTcType ty    >>= \ty' ->
                                     return (Term ty' dc v tt)
             ,fSuspension = \ct ty v b -> fmapMMaybe zonkTcType ty >>= \ty ->
                                          return (Suspension ct ty v b)}  


-- Is this defined elsewhere?
-- Generalize the type: find all free tyvars and wrap in the appropiate ForAll.
sigmaType ty = mkForAllTys (varSetElems$ tyVarsOfType (dropForAlls ty)) ty


