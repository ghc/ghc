-----------------------------------------------------------------------------
--
-- GHC Interactive support for inspecting arbitrary closures at runtime
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module RtClosureInspect(
  
     cvObtainTerm,      -- :: HscEnv -> Int -> Bool -> Maybe Type -> HValue -> IO Term

     Term(..),
     isTerm,
     isSuspension,
     isPrim,
     pprTerm, 
     cPprTerm, 
     cPprTermBase,
     CustomTermPrinter,
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
     cvReconstructType,
     computeRTTIsubst, 
     sigmaType,
     Closure(..),
     getClosureData,
     ClosureType(..),
     isConstr,
     isIndirection
 ) where 

#include "HsVersions.h"

import ByteCodeItbls    ( StgInfoTable )
import qualified ByteCodeItbls as BCI( StgInfoTable(..) )
import HscTypes         ( HscEnv )
import Linker

import DataCon
import Type
import Var
import TcRnMonad        ( TcM, initTc, ioToTcRn,
                          tryTcErrs, traceTc)
import TcType
import TcMType
import TcUnify
import TcGadt
import TcEnv
import DriverPhases
import TyCon
import Name
import VarEnv
import Util
import VarSet

import TysPrim
import PrelNames
import TysWiredIn

import Constants
import Outputable
import Maybes
import Panic

import GHC.Arr          ( Array(..) )
import GHC.Exts

import Control.Monad
import Data.Maybe
import Data.Array.Base
import Data.Ix
import Data.List        ( partition )
import qualified Data.Sequence as Seq
import Data.Monoid
import Data.Sequence hiding (null, length, index, take, drop, splitAt, reverse)
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
                 , dc        :: Either String DataCon
                               -- Empty if the datacon aint exported by the .hi
                               -- (private constructors in -O0 libraries)
                 , val       :: HValue 
                 , subTerms  :: [Term] }

          | Prim { ty        :: Type
                 , value     :: [Word] }

          | Suspension { ctype    :: ClosureType
                       , mb_ty    :: Maybe Type
                       , val      :: HValue
                       , bound_to :: Maybe Name   -- Useful for printing
                       }
          | NewtypeWrap{ ty           :: Type
                       , dc           :: Either String DataCon
                       , wrapped_term :: Term }

isTerm, isSuspension, isPrim, isNewtypeWrap :: Term -> Bool
isTerm Term{} = True
isTerm   _    = False
isSuspension Suspension{} = True
isSuspension      _       = False
isPrim Prim{} = True
isPrim   _    = False
isNewtypeWrap NewtypeWrap{} = True
isNewtypeWrap _             = False

termType :: Term -> Maybe Type
termType t@(Suspension {}) = mb_ty t
termType t = Just$ ty t

isFullyEvaluatedTerm :: Term -> Bool
isFullyEvaluatedTerm Term {subTerms=tt} = all isFullyEvaluatedTerm tt
isFullyEvaluatedTerm Prim {}            = True
isFullyEvaluatedTerm NewtypeWrap{wrapped_term=t} = isFullyEvaluatedTerm t
isFullyEvaluatedTerm _                  = False

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

aP_CODE, pAP_CODE :: Int
aP_CODE = AP
pAP_CODE = PAP
#undef AP
#undef PAP

getClosureData :: a -> IO Closure
getClosureData a =
   case unpackClosure# a of 
     (# iptr, ptrs, nptrs #) -> do
#ifndef GHCI_TABLES_NEXT_TO_CODE
           -- the info pointer we get back from unpackClosure# is to the
           -- beginning of the standard info table, but the Storable instance
           -- for info tables takes into account the extra entry pointer
           -- when !tablesNextToCode, so we must adjust here:
           itbl <- peek (Ptr iptr `plusPtr` negate wORD_SIZE)
#else
           itbl <- peek (Ptr iptr)
#endif
           let tipe = readCType (BCI.tipe itbl)
               elems = fromIntegral (BCI.ptrs itbl)
               ptrsList = Array 0 (elems - 1) elems ptrs
               nptrs_data = [W# (indexWordArray# nptrs i)
                              | I# i <- [0.. fromIntegral (BCI.nptrs itbl)] ]
           ASSERT(elems >= 0) return ()
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

isConstr, isIndirection, isThunk :: ClosureType -> Bool
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
    _      -> return False
  where amapM f = sequence . amap' f

amap' :: (t -> b) -> Array Int t -> [b]
amap' f (Array i0 i _ arr#) = map g [0 .. i - i0]
    where g (I# i#) = case indexArray# arr# i# of
                          (# e #) -> f e

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
           | (x, rest) <- splitAt ((sizeofType t + wORD_SIZE - 1) `div` wORD_SIZE) xx 
           = x : go tt rest

sizeofTyCon :: TyCon -> Int
sizeofTyCon = sizeofPrimRep . tyConPrimRep

-----------------------------------
-- * Traversals for Terms
-----------------------------------
type TermProcessor a b = Type -> Either String DataCon -> HValue -> [a] -> b

data TermFold a = TermFold { fTerm :: TermProcessor a a
                           , fPrim :: Type -> [Word] -> a
                           , fSuspension :: ClosureType -> Maybe Type -> HValue
                                           -> Maybe Name -> a
                           , fNewtypeWrap :: Type -> Either String DataCon
                                            -> a -> a
                           }

foldTerm :: TermFold a -> Term -> a
foldTerm tf (Term ty dc v tt) = fTerm tf ty dc v (map (foldTerm tf) tt)
foldTerm tf (Prim ty    v   ) = fPrim tf ty v
foldTerm tf (Suspension ct ty v b) = fSuspension tf ct ty v b
foldTerm tf (NewtypeWrap ty dc t)  = fNewtypeWrap tf ty dc (foldTerm tf t)

idTermFold :: TermFold Term
idTermFold = TermFold {
              fTerm = Term,
              fPrim = Prim,
              fSuspension  = Suspension,
              fNewtypeWrap = NewtypeWrap
                      }
idTermFoldM :: Monad m => TermFold (m Term)
idTermFoldM = TermFold {
              fTerm       = \ty dc v tt -> sequence tt >>= return . Term ty dc v,
              fPrim       = (return.). Prim,
              fSuspension = (((return.).).). Suspension,
              fNewtypeWrap= \ty dc t -> NewtypeWrap ty dc `liftM` t
                       }

mapTermType :: (Type -> Type) -> Term -> Term
mapTermType f = foldTerm idTermFold {
          fTerm       = \ty dc hval tt -> Term (f ty) dc hval tt,
          fSuspension = \ct mb_ty hval n ->
                          Suspension ct (fmap f mb_ty) hval n,
          fNewtypeWrap= \ty dc t -> NewtypeWrap (f ty) dc t}

termTyVars :: Term -> TyVarSet
termTyVars = foldTerm TermFold {
            fTerm       = \ty _ _ tt   -> 
                          tyVarsOfType ty `plusVarEnv` concatVarEnv tt,
            fSuspension = \_ mb_ty _ _ -> 
                          maybe emptyVarEnv tyVarsOfType mb_ty,
            fPrim       = \ _ _ -> emptyVarEnv,
            fNewtypeWrap= \ty _ t -> tyVarsOfType ty `plusVarEnv` t}
    where concatVarEnv = foldr plusVarEnv emptyVarEnv

----------------------------------
-- Pretty printing of terms
----------------------------------

app_prec,cons_prec ::Int
app_prec = 10
cons_prec = 5 -- TODO Extract this info from GHC itself

pprTerm :: (Int -> Term -> Maybe SDoc) -> Int -> Term -> SDoc
pprTerm y p t | Just doc <- pprTermM y p t = doc
pprTerm _ _ _ = panic "pprTerm"

pprTermM, pprNewtypeWrap :: Monad m => 
                           (Int -> Term -> m SDoc) -> Int -> Term -> m SDoc
pprTermM y p Term{dc=Left dc_tag, subTerms=tt} = do
  tt_docs <- mapM (y app_prec) tt
  return$ cparen (not(null tt) && p >= app_prec) (text dc_tag <+> sep tt_docs)
  
pprTermM y p Term{dc=Right dc, subTerms=tt} 
{-  | dataConIsInfix dc, (t1:t2:tt') <- tt  --TODO fixity
  = parens (pprTerm1 True t1 <+> ppr dc <+> pprTerm1 True ppr t2) 
    <+> hsep (map (pprTerm1 True) tt) 
-} -- TODO Printing infix constructors properly
  | null tt   = return$ ppr dc
  | otherwise = do
         tt_docs <- mapM (y app_prec) tt
         return$ cparen (p >= app_prec) (ppr dc <+> sep tt_docs)

pprTermM y p t@NewtypeWrap{} = pprNewtypeWrap y p t

pprTermM _ _ t = pprTermM1 t

pprTermM1 :: Monad m => Term -> m SDoc
pprTermM1 Prim{value=words, ty=ty} = 
    return$ text$ repPrim (tyConAppTyCon ty) words
pprTermM1 Term{} = panic "pprTermM1 - unreachable"
pprTermM1 Suspension{bound_to=Nothing} = return$ char '_'
pprTermM1 Suspension{mb_ty=Just ty, bound_to=Just n}
  | Just _ <- splitFunTy_maybe ty = return$ ptext SLIT("<function>")
  | otherwise = return$ parens$ ppr n <> text "::" <> ppr ty 
pprTermM1 _ = panic "pprTermM1"

pprNewtypeWrap y p NewtypeWrap{ty=ty, wrapped_term=t} 
  | Just (tc,_) <- splitNewTyConApp_maybe ty
  , ASSERT(isNewTyCon tc) True
  , Just new_dc <- maybeTyConSingleCon tc = do 
         real_term <- y 10 t
         return$ cparen (p >= app_prec) (ppr new_dc <+> real_term)
pprNewtypeWrap _ _ _ = panic "pprNewtypeWrap"

-------------------------------------------------------
-- Custom Term Pretty Printers
-------------------------------------------------------

-- We can want to customize the representation of a 
--  term depending on its type. 
-- However, note that custom printers have to work with
--  type representations, instead of directly with types.
-- We cannot use type classes here, unless we employ some 
--  typerep trickery (e.g. Weirich's RepLib tricks),
--  which I didn't. Therefore, this code replicates a lot
--  of what type classes provide for free.

-- Concretely a custom term printer takes an explicit
--  recursion knot, and produces a list of Term Processors,
--  which additionally need a precedence value to
--  either produce a SDoc or fail (and they do this in some monad m).

type Precedence          = Int
type RecursionKnot m     = Precedence -> Term -> m SDoc
type CustomTermPrinter m = RecursionKnot m
                         -> [Precedence -> Term -> (m (Maybe SDoc))]

-- Takes a list of custom printers with a explicit recursion knot and a term, 
-- and returns the output of the first succesful printer, or the default printer
cPprTerm :: Monad m => CustomTermPrinter m -> Term -> m SDoc
cPprTerm printers_ = go 0 where
  printers = printers_ go
  go prec t | isTerm t || isNewtypeWrap t = do
    let default_ = Just `liftM` pprTermM go prec t
        mb_customDocs = [pp prec t | pp <- printers] ++ [default_]
    Just doc <- firstJustM mb_customDocs
    return$ cparen (prec>app_prec+1) doc
  go _ t = pprTermM1 t

  firstJustM (mb:mbs) = mb >>= maybe (firstJustM mbs) (return . Just)
  firstJustM [] = return Nothing

-- Default set of custom printers. Note that the recursion knot is explicit
cPprTermBase :: Monad m => CustomTermPrinter m
cPprTermBase y =
  [ ifTerm (isTupleTy.ty) (\_p -> liftM (parens . hcat . punctuate comma) 
                                      . mapM (y (-1))
                                      . subTerms)
  , ifTerm (\t -> isTyCon listTyCon (ty t) && subTerms t `lengthIs` 2)
           (\ p Term{subTerms=[h,t]} -> doList p h t)
  , ifTerm (isTyCon intTyCon    . ty) (coerceShow$ \(a::Int)->a)
  , ifTerm (isTyCon charTyCon   . ty) (coerceShow$ \(a::Char)->a)
  , ifTerm (isTyCon floatTyCon  . ty) (coerceShow$ \(a::Float)->a)
  , ifTerm (isTyCon doubleTyCon . ty) (coerceShow$ \(a::Double)->a)
  , ifTerm (isIntegerTy         . ty) (coerceShow$ \(a::Integer)->a)
  ]
     where ifTerm pred f prec t@Term{}
               | pred t    = Just `liftM` f prec t
           ifTerm _ _ _ _  = return Nothing

           isIntegerTy ty  = fromMaybe False $ do
             (tc,_) <- splitTyConApp_maybe ty 
             return (tyConName tc == integerTyConName)

           isTupleTy ty    = fromMaybe False $ do 
             (tc,_) <- splitTyConApp_maybe ty 
             return (tc `elem` (fst.unzip.elems) boxedTupleArr)

           isTyCon a_tc ty = fromMaybe False $ do 
             (tc,_) <- splitTyConApp_maybe ty
             return (a_tc == tc)

           coerceShow f _p = return . text . show . f . unsafeCoerce# . val

           --TODO pprinting of list terms is not lazy
           doList p h t = do
               let elems      = h : getListTerms t
                   isConsLast = termType(last elems) /= termType h
               print_elems <- mapM (y cons_prec) elems
               return$ if isConsLast
                     then cparen (p >= cons_prec) 
                        . hsep 
                        . punctuate (space<>colon)
                        $ print_elems
                     else brackets (hcat$ punctuate comma print_elems)

                where Just a /= Just b = not (a `coreEqType` b)
                      _      /=   _    = True
                      getListTerms Term{subTerms=[h,t]} = h : getListTerms t
                      getListTerms Term{subTerms=[]}  = []
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
  mb_term <- runTR_maybe hsc_env c
  case mb_term of 
    Nothing -> panic "Can't unify"
    Just x  -> return x

runTR_maybe :: HscEnv -> TR a -> IO (Maybe a)
runTR_maybe hsc_env = fmap snd . initTc hsc_env HsSrcFile False iNTERACTIVE

traceTR :: SDoc -> TR ()
traceTR = liftTcM . traceTc

trIO :: IO a -> TR a 
trIO = liftTcM . ioToTcRn

liftTcM :: TcM a -> TR a
liftTcM = id

newVar :: Kind -> TR TcType
newVar = liftTcM . fmap mkTyVarTy . newFlexiTyVar

-- | Returns the instantiated type scheme ty', and the substitution sigma 
--   such that sigma(ty') = ty 
instScheme :: Type -> TR (TcType, TvSubst)
instScheme ty | (tvs, _rho) <- tcSplitForAllTys ty = liftTcM$ do
   (tvs',_theta,ty') <- tcInstType (mapM tcInstTyVar) ty
   return (ty', zipTopTvSubst tvs' (mkTyVarTys tvs))

-- Adds a constraint of the form t1 == t2
-- t1 is expected to come from walking the heap
-- t2 is expected to come from a datacon signature
-- Before unification, congruenceNewtypes needs to
-- do its magic.
addConstraint :: TcType -> TcType -> TR ()
addConstraint t1 t2  = congruenceNewtypes t1 t2 >>= uncurry unifyType 
		       >> return () -- TOMDO: what about the coercion?
				    -- we should consider family instances 

-- Type & Term reconstruction 
cvObtainTerm :: HscEnv -> Int -> Bool -> Maybe Type -> HValue -> IO Term
cvObtainTerm hsc_env bound force mb_ty hval = runTR hsc_env $ do
   tv <- newVar argTypeKind
   case mb_ty of
     Nothing ->      go bound tv tv hval 
                >>= zonkTerm 
                >>= return . expandNewtypes
     Just ty | isMonomorphic ty ->     go bound ty ty hval 
                                   >>= zonkTerm
                                   >>= return . expandNewtypes
     Just ty -> do 
              (ty',rev_subst) <- instScheme (sigmaType ty)
              addConstraint tv ty'
              term <- go bound tv tv hval >>= zonkTerm
              --restore original Tyvars
              return$ expandNewtypes $ mapTermType (substTy rev_subst) term
    where 
  go bound _ _ _ | seq bound False = undefined
  go 0 tv _ty a = do
    clos <- trIO $ getClosureData a
    return (Suspension (tipe clos) (Just tv) a Nothing)
  go bound tv ty a = do 
    let monomorphic = not(isTyVarTy tv)   
    -- This ^^^ is a convention. The ancestor tests for
    -- monomorphism and passes a type instead of a tv
    clos <- trIO $ getClosureData a
    case tipe clos of
-- Thunks we may want to force
-- NB. this won't attempt to force a BLACKHOLE.  Even with :force, we never
-- force blackholes, because it would almost certainly result in deadlock,
-- and showing the '_' is more useful.
      t | isThunk t && force -> seq a $ go (pred bound) tv ty a
-- We always follow indirections 
      Indirection _ -> go (pred bound) tv ty $! (ptrs clos ! 0)
 -- The interesting case
      Constr -> do
        Right dcname <- dataConInfoPtrToName (infoPtr clos)
        (_,mb_dc)    <- tryTcErrs (tcLookupDataCon dcname)
        case mb_dc of
          Nothing -> do -- This can happen for private constructors compiled -O0
                        -- where the .hi descriptor does not export them
                        -- In such case, we return a best approximation:
                        --  ignore the unpointed args, and recover the pointeds
                        -- This preserves laziness, and should be safe.
                       let tag = showSDoc (ppr dcname)
                       vars     <- replicateM (length$ elems$ ptrs clos) 
                                              (newVar (liftedTypeKind))
                       subTerms <- sequence [appArr (go (pred bound) tv tv) (ptrs clos) i 
                                              | (i, tv) <- zip [0..] vars]
                       return (Term tv (Left ('<' : tag ++ ">")) a subTerms)
          Just dc -> do 
            let extra_args = length(dataConRepArgTys dc) - 
                             length(dataConOrigArgTys dc)
                subTtypes  = matchSubTypes dc ty
                (subTtypesP, subTtypesNP) = partition isPointed subTtypes
            subTermTvs <- sequence
                 [ if isMonomorphic t then return t 
                                      else (newVar k)
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
                  [ appArr (go (pred bound) tv t) (ptrs clos) i
                   | (i,tv,t) <- zip3 [0..] subTermTvs subTtypesP]
            let unboxeds   = extractUnboxed subTtypesNP clos
                subTermsNP = map (uncurry Prim) (zip subTtypesNP unboxeds)      
                subTerms   = reOrderTerms subTermsP subTermsNP 
                                (drop extra_args subTtypes)
            return (Term tv (Right dc) a subTerms)
-- The otherwise case: can be a Thunk,AP,PAP,etc.
      tipe_clos -> 
         return (Suspension tipe_clos (Just tv) a Nothing)

  matchSubTypes dc ty
    | Just (_,ty_args) <- splitTyConApp_maybe (repType ty) 
--     assumption:             ^^^ looks through newtypes 
    , isVanillaDataCon dc  --TODO non-vanilla case
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
  
  expandNewtypes t@Term{ ty=ty, subTerms=tt }
   | Just (tc, args) <- splitNewTyConApp_maybe ty
   , isNewTyCon tc
   , wrapped_type    <- newTyConInstRhs tc args
   , Just dc         <- maybeTyConSingleCon tc
   , t'              <- expandNewtypes t{ ty = wrapped_type
                                        , subTerms = map expandNewtypes tt }
   = NewtypeWrap ty (Right dc) t'

   | otherwise = t{ subTerms = map expandNewtypes tt }

  expandNewtypes t = t


-- Fast, breadth-first Type reconstruction
cvReconstructType :: HscEnv -> Int -> Maybe Type -> HValue -> IO (Maybe Type)
cvReconstructType hsc_env max_depth mb_ty hval = runTR_maybe hsc_env $ do
   tv <- newVar argTypeKind
   case mb_ty of
     Nothing -> do search (isMonomorphic `fmap` zonkTcType tv)
                          (uncurry go)
                          (Seq.singleton (tv, hval))
                          max_depth
                   zonkTcType tv  -- TODO untested!
     Just ty | isMonomorphic ty -> return ty
     Just ty -> do
              (ty',rev_subst) <- instScheme (sigmaType ty)
              addConstraint tv ty'
              search (isMonomorphic `fmap` zonkTcType tv)
                     (\(ty,a) -> go ty a)
                     (Seq.singleton (tv, hval))
                     max_depth
              substTy rev_subst `fmap` zonkTcType tv
    where 
--  search :: m Bool -> ([a] -> [a] -> [a]) -> [a] -> m ()
  search _ _ _ 0 = traceTR (text "Failed to reconstruct a type after " <>
                                int max_depth <> text " steps")
  search stop expand l d =
    case viewl l of 
      EmptyL  -> return ()
      x :< xx -> unlessM stop $ do
                  new <- expand x
                  search stop expand (xx `mappend` Seq.fromList new) $! (pred d)

   -- returns unification tasks,since we are going to want a breadth-first search
  go :: Type -> HValue -> TR [(Type, HValue)]
  go tv a = do
    clos <- trIO $ getClosureData a
    case tipe clos of
      Indirection _ -> go tv $! (ptrs clos ! 0)
      Constr -> do
        Right dcname <- dataConInfoPtrToName (infoPtr clos)
        (_,mb_dc)    <- tryTcErrs (tcLookupDataCon dcname)
        case mb_dc of
          Nothing-> do
                     --  TODO: Check this case
            forM [0..length (elems $ ptrs clos)] $ \i -> do
                        tv <- newVar liftedTypeKind
                        return$ appArr (\e->(tv,e)) (ptrs clos) i

          Just dc -> do
            let extra_args = length(dataConRepArgTys dc) -
                             length(dataConOrigArgTys dc)
            subTtypes <- mapMif (not . isMonomorphic)
                                (\t -> newVar (typeKind t))
                                (dataConRepArgTys dc)

            -- It is vital for newtype reconstruction that the unification step
            -- is done right here, _before_ the subterms are RTTI reconstructed
            let myType         = mkFunTys subTtypes tv
            (signatureType,_) <- instScheme(dataConRepType dc) 
            addConstraint myType signatureType
            return $ [ appArr (\e->(t,e)) (ptrs clos) i
                       | (i,t) <- drop extra_args $ 
                                     zip [0..] (filter isPointed subTtypes)]
      _ -> return []

     -- This helper computes the difference between a base type t and the 
     -- improved rtti_t computed by RTTI
     -- The main difference between RTTI types and their normal counterparts
     --  is that the former are _not_ polymorphic, thus polymorphism must
     --  be stripped. Syntactically, forall's must be stripped
computeRTTIsubst :: Type -> Type -> Maybe TvSubst
computeRTTIsubst ty rtti_ty = 
     -- In addition, we strip newtypes too, since the reconstructed type might
     --   not have recovered them all
           tcUnifyTys (const BindMe) 
                      [repType' $ dropForAlls$ ty]
                      [repType' $ rtti_ty]  
-- TODO stripping newtypes shouldn't be necessary, test


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
 constraints are unified is vital for this.
   This is a simple form of residuation, the technique of 
 delaying unification steps until enough information
 is available.
-}
congruenceNewtypes ::  TcType -> TcType -> TR (TcType,TcType)
congruenceNewtypes lhs rhs 
 -- TyVar lhs inductive case
    | Just tv <- getTyVar_maybe lhs 
    = recoverTc (return (lhs,rhs)) $ do  
         Indirect ty_v <- readMetaTyVar tv
         (_lhs1, rhs1) <- congruenceNewtypes ty_v rhs
         return (lhs, rhs1)
-- FunTy inductive case
    | Just (l1,l2) <- splitFunTy_maybe lhs
    , Just (r1,r2) <- splitFunTy_maybe rhs
    = do (l2',r2') <- congruenceNewtypes l2 r2
         (l1',r1') <- congruenceNewtypes l1 r1
         return (mkFunTy l1' l2', mkFunTy r1' r2')
-- TyconApp Inductive case; this is the interesting bit.
    | Just (tycon_l, _) <- splitNewTyConApp_maybe lhs
    , Just (tycon_r, _) <- splitNewTyConApp_maybe rhs 
    , tycon_l /= tycon_r 
    = do rhs' <- upgrade tycon_l rhs
         return (lhs, rhs')

    | otherwise = return (lhs,rhs)

    where upgrade :: TyCon -> Type -> TR Type
          upgrade new_tycon ty
            | not (isNewTyCon new_tycon) = return ty 
            | otherwise = do 
               vars <- mapM (newVar . tyVarKind) (tyConTyVars new_tycon)
               let ty' = mkTyConApp new_tycon vars
               liftTcM (unifyType ty (repType ty'))
        -- assumes that reptype doesn't ^^^^ touch tyconApp args 
               return ty'


--------------------------------------------------------------------------------
-- Semantically different to recoverM in TcRnMonad 
-- recoverM retains the errors in the first action,
--  whereas recoverTc here does not
recoverTc :: TcM a -> TcM a -> TcM a
recoverTc recover thing = do 
  (_,mb_res) <- tryTcErrs thing
  case mb_res of 
    Nothing  -> recover
    Just res -> return res

isMonomorphic :: Type -> Bool
isMonomorphic ty | (tvs, ty') <- splitForAllTys ty
                 = null tvs && (isEmptyVarSet . tyVarsOfType) ty'

mapMif :: Monad m => (a -> Bool) -> (a -> m a) -> [a] -> m [a]
mapMif pred f xx = sequence $ mapMif_ pred f xx
  where
   mapMif_ _ _ []     = []
   mapMif_ pred f (x:xx) = (if pred x then f x else return x) : mapMif_ pred f xx

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM acc = condM >>= \c -> unless c acc

-- Strict application of f at index i
appArr :: Ix i => (e -> a) -> Array i e -> Int -> a
appArr f a@(Array _ _ _ ptrs#) i@(I# i#)
 = ASSERT (i < length(elems a))
   case indexArray# ptrs# i# of
       (# e #) -> f e

zonkTerm :: Term -> TcM Term
zonkTerm = foldTerm idTermFoldM {
              fTerm = \ty dc v tt -> sequence tt      >>= \tt ->
                                     zonkTcType ty    >>= \ty' ->
                                     return (Term ty' dc v tt)
             ,fSuspension = \ct ty v b -> fmapMMaybe zonkTcType ty >>= \ty ->
                                          return (Suspension ct ty v b)
             ,fNewtypeWrap= \ty dc t -> 
                   return NewtypeWrap `ap` zonkTcType ty `ap` return dc `ap` t}


-- Is this defined elsewhere?
-- Generalize the type: find all free tyvars and wrap in the appropiate ForAll.
sigmaType :: Type -> Type
sigmaType ty = mkForAllTys (varSetElems$ tyVarsOfType (dropForAlls ty)) ty


