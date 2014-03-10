-----------------------------------------------------------------------------
--
-- GHC Interactive support for inspecting arbitrary closures at runtime
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module RtClosureInspect(
     cvObtainTerm,      -- :: HscEnv -> Int -> Bool -> Maybe Type -> HValue -> IO Term
     cvReconstructType,
     improveRTTIType,

     Term(..),
     isTerm, isSuspension, isPrim, isFun, isFunLike, isNewtypeWrap,
     isFullyEvaluated, isFullyEvaluatedTerm,
     termType, mapTermType, termTyVars,
     foldTerm, TermFold(..), foldTermM, TermFoldM(..), idTermFold,
     pprTerm, cPprTerm, cPprTermBase, CustomTermPrinter,

--     unsafeDeepSeq,

     Closure(..), getClosureData, ClosureType(..), isConstr, isIndirection
 ) where

#include "HsVersions.h"

import DebuggerUtils
import ByteCodeItbls    ( StgInfoTable, peekItbl )
import qualified ByteCodeItbls as BCI( StgInfoTable(..) )
import BasicTypes       ( HValue )
import HscTypes

import DataCon
import Type
import qualified Unify as U
import Var
import TcRnMonad
import TcType
import TcMType
import TcHsSyn ( zonkTcTypeToType, mkEmptyZonkEnv )
import TcUnify
import TcEnv

import TyCon
import Name
import VarEnv
import Util
import VarSet
import BasicTypes       ( TupleSort(UnboxedTuple) )
import TysPrim
import PrelNames
import TysWiredIn
import DynFlags
import Outputable as Ppr
import GHC.Arr          ( Array(..) )
import GHC.Exts
import GHC.IO ( IO(..) )

import StaticFlags( opt_PprStyle_Debug )
import Control.Monad
import Data.Maybe
import Data.Array.Base
import Data.Ix
import Data.List
import qualified Data.Sequence as Seq
import Data.Monoid (mappend)
import Data.Sequence (viewl, ViewL(..))
import Foreign.Safe
import System.IO.Unsafe

---------------------------------------------
-- * A representation of semi evaluated Terms
---------------------------------------------

data Term = Term { ty        :: RttiType
                 , dc        :: Either String DataCon
                               -- Carries a text representation if the datacon is
                               -- not exported by the .hi file, which is the case 
                               -- for private constructors in -O0 compiled libraries
                 , val       :: HValue 
                 , subTerms  :: [Term] }

          | Prim { ty        :: RttiType
                 , value     :: [Word] }

          | Suspension { ctype    :: ClosureType
                       , ty       :: RttiType
                       , val      :: HValue
                       , bound_to :: Maybe Name   -- Useful for printing
                       }
          | NewtypeWrap{       -- At runtime there are no newtypes, and hence no
                               -- newtype constructors. A NewtypeWrap is just a
                               -- made-up tag saying "heads up, there used to be
                               -- a newtype constructor here".
                         ty           :: RttiType
                       , dc           :: Either String DataCon
                       , wrapped_term :: Term }
          | RefWrap    {       -- The contents of a reference
                         ty           :: RttiType
                       , wrapped_term :: Term }

isTerm, isSuspension, isPrim, isFun, isFunLike, isNewtypeWrap :: Term -> Bool
isTerm Term{} = True
isTerm   _    = False
isSuspension Suspension{} = True
isSuspension      _       = False
isPrim Prim{} = True
isPrim   _    = False
isNewtypeWrap NewtypeWrap{} = True
isNewtypeWrap _             = False

isFun Suspension{ctype=Fun} = True
isFun _ = False

isFunLike s@Suspension{ty=ty} = isFun s || isFunTy ty
isFunLike _ = False

termType :: Term -> RttiType
termType t = ty t

isFullyEvaluatedTerm :: Term -> Bool
isFullyEvaluatedTerm Term {subTerms=tt} = all isFullyEvaluatedTerm tt
isFullyEvaluatedTerm Prim {}            = True
isFullyEvaluatedTerm NewtypeWrap{wrapped_term=t} = isFullyEvaluatedTerm t
isFullyEvaluatedTerm RefWrap{wrapped_term=t}     = isFullyEvaluatedTerm t
isFullyEvaluatedTerm _                  = False

instance Outputable (Term) where
 ppr t | Just doc <- cPprTerm cPprTermBase t = doc
       | otherwise = panic "Outputable Term instance"

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
                 | MutVar Int
                 | MVar   Int
                 | Other  Int
 deriving (Show, Eq)

data Closure = Closure { tipe         :: ClosureType 
                       , infoPtr      :: Ptr ()
                       , infoTable    :: StgInfoTable
                       , ptrs         :: Array Int HValue
                       , nonPtrs      :: [Word]
                       }

instance Outputable ClosureType where
  ppr = text . show 

#include "../includes/rts/storage/ClosureTypes.h"

aP_CODE, pAP_CODE :: Int
aP_CODE = AP
pAP_CODE = PAP
#undef AP
#undef PAP

getClosureData :: DynFlags -> a -> IO Closure
getClosureData dflags a =
   case unpackClosure# a of 
     (# iptr, ptrs, nptrs #) -> do
           let iptr'
                | ghciTablesNextToCode =
                   Ptr iptr
                | otherwise =
                   -- the info pointer we get back from unpackClosure#
                   -- is to the beginning of the standard info table,
                   -- but the Storable instance for info tables takes
                   -- into account the extra entry pointer when
                   -- !ghciTablesNextToCode, so we must adjust here:
                   Ptr iptr `plusPtr` negate (wORD_SIZE dflags)
           itbl <- peekItbl dflags iptr'
           let tipe = readCType (BCI.tipe itbl)
               elems = fromIntegral (BCI.ptrs itbl)
               ptrsList = Array 0 (elems - 1) elems ptrs
               nptrs_data = [W# (indexWordArray# nptrs i)
                              | I# i <- [0.. fromIntegral (BCI.nptrs itbl)-1] ]
           ASSERT(elems >= 0) return ()
           ptrsList `seq` 
            return (Closure tipe (Ptr iptr) itbl ptrsList nptrs_data)

readCType :: Integral a => a -> ClosureType
readCType i 
 | i >= CONSTR && i <= CONSTR_NOCAF_STATIC = Constr
 | i >= FUN    && i <= FUN_STATIC          = Fun
 | i >= THUNK  && i < THUNK_SELECTOR       = Thunk i'
 | i == THUNK_SELECTOR                     = ThunkSelector
 | i == BLACKHOLE                          = Blackhole
 | i >= IND    && i <= IND_STATIC          = Indirection i'
 | i' == aP_CODE                           = AP
 | i == AP_STACK                           = AP
 | i' == pAP_CODE                          = PAP
 | i == MUT_VAR_CLEAN || i == MUT_VAR_DIRTY= MutVar i'
 | i == MVAR_CLEAN    || i == MVAR_DIRTY   = MVar i'
 | otherwise                               = Other  i'
  where i' = fromIntegral i
 
isConstr, isIndirection, isThunk :: ClosureType -> Bool
isConstr Constr = True
isConstr    _   = False

isIndirection (Indirection _) = True
isIndirection _ = False

isThunk (Thunk _)     = True
isThunk ThunkSelector = True
isThunk AP            = True
isThunk _             = False

isFullyEvaluated :: DynFlags -> a -> IO Bool
isFullyEvaluated dflags a = do
  closure <- getClosureData dflags a
  case tipe closure of
    Constr -> do are_subs_evaluated <- amapM (isFullyEvaluated dflags) (ptrs closure)
                 return$ and are_subs_evaluated
    _      -> return False
  where amapM f = sequence . amap' f

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

-----------------------------------
-- * Traversals for Terms
-----------------------------------
type TermProcessor a b = RttiType -> Either String DataCon -> HValue -> [a] -> b

data TermFold a = TermFold { fTerm        :: TermProcessor a a
                           , fPrim        :: RttiType -> [Word] -> a
                           , fSuspension  :: ClosureType -> RttiType -> HValue
                                            -> Maybe Name -> a
                           , fNewtypeWrap :: RttiType -> Either String DataCon
                                            -> a -> a
                           , fRefWrap     :: RttiType -> a -> a
                           }


data TermFoldM m a =
                   TermFoldM {fTermM        :: TermProcessor a (m a)
                            , fPrimM        :: RttiType -> [Word] -> m a
                            , fSuspensionM  :: ClosureType -> RttiType -> HValue
                                             -> Maybe Name -> m a
                            , fNewtypeWrapM :: RttiType -> Either String DataCon
                                            -> a -> m a
                            , fRefWrapM     :: RttiType -> a -> m a
                           }

foldTerm :: TermFold a -> Term -> a
foldTerm tf (Term ty dc v tt) = fTerm tf ty dc v (map (foldTerm tf) tt)
foldTerm tf (Prim ty    v   ) = fPrim tf ty v
foldTerm tf (Suspension ct ty v b) = fSuspension tf ct ty v b
foldTerm tf (NewtypeWrap ty dc t)  = fNewtypeWrap tf ty dc (foldTerm tf t)
foldTerm tf (RefWrap ty t)         = fRefWrap tf ty (foldTerm tf t)


foldTermM :: Monad m => TermFoldM m a -> Term -> m a
foldTermM tf (Term ty dc v tt) = mapM (foldTermM tf) tt >>= fTermM tf ty dc v
foldTermM tf (Prim ty    v   ) = fPrimM tf ty v
foldTermM tf (Suspension ct ty v b) = fSuspensionM tf ct ty v b
foldTermM tf (NewtypeWrap ty dc t)  = foldTermM tf t >>=  fNewtypeWrapM tf ty dc
foldTermM tf (RefWrap ty t)         = foldTermM tf t >>= fRefWrapM tf ty

idTermFold :: TermFold Term
idTermFold = TermFold {
              fTerm = Term,
              fPrim = Prim,
              fSuspension  = Suspension,
              fNewtypeWrap = NewtypeWrap,
              fRefWrap = RefWrap
                      }

mapTermType :: (RttiType -> Type) -> Term -> Term
mapTermType f = foldTerm idTermFold {
          fTerm       = \ty dc hval tt -> Term (f ty) dc hval tt,
          fSuspension = \ct ty hval n ->
                          Suspension ct (f ty) hval n,
          fNewtypeWrap= \ty dc t -> NewtypeWrap (f ty) dc t,
          fRefWrap    = \ty t -> RefWrap (f ty) t}

mapTermTypeM :: Monad m =>  (RttiType -> m Type) -> Term -> m Term
mapTermTypeM f = foldTermM TermFoldM {
          fTermM       = \ty dc hval tt -> f ty >>= \ty' -> return $ Term ty'  dc hval tt,
          fPrimM       = (return.) . Prim,
          fSuspensionM = \ct ty hval n ->
                          f ty >>= \ty' -> return $ Suspension ct ty' hval n,
          fNewtypeWrapM= \ty dc t -> f ty >>= \ty' -> return $ NewtypeWrap ty' dc t,
          fRefWrapM    = \ty t -> f ty >>= \ty' -> return $ RefWrap ty' t}

termTyVars :: Term -> TyVarSet
termTyVars = foldTerm TermFold {
            fTerm       = \ty _ _ tt   -> 
                          tyVarsOfType ty `plusVarEnv` concatVarEnv tt,
            fSuspension = \_ ty _ _ -> tyVarsOfType ty,
            fPrim       = \ _ _ -> emptyVarEnv,
            fNewtypeWrap= \ty _ t -> tyVarsOfType ty `plusVarEnv` t,
            fRefWrap    = \ty t -> tyVarsOfType ty `plusVarEnv` t}
    where concatVarEnv = foldr plusVarEnv emptyVarEnv

----------------------------------
-- Pretty printing of terms
----------------------------------

type Precedence        = Int
type TermPrinter       = Precedence -> Term ->   SDoc
type TermPrinterM m    = Precedence -> Term -> m SDoc

app_prec,cons_prec, max_prec ::Int
max_prec  = 10
app_prec  = max_prec
cons_prec = 5 -- TODO Extract this info from GHC itself

pprTerm :: TermPrinter -> TermPrinter
pprTerm y p t | Just doc <- pprTermM (\p -> Just . y p) p t = doc
pprTerm _ _ _ = panic "pprTerm"

pprTermM, ppr_termM, pprNewtypeWrap :: Monad m => TermPrinterM m -> TermPrinterM m
pprTermM y p t = pprDeeper `liftM` ppr_termM y p t

ppr_termM y p Term{dc=Left dc_tag, subTerms=tt} = do
  tt_docs <- mapM (y app_prec) tt
  return $ cparen (not (null tt) && p >= app_prec)
                  (text dc_tag <+> pprDeeperList fsep tt_docs)
  
ppr_termM y p Term{dc=Right dc, subTerms=tt} 
{-  | dataConIsInfix dc, (t1:t2:tt') <- tt  --TODO fixity
  = parens (ppr_term1 True t1 <+> ppr dc <+> ppr_term1 True ppr t2) 
    <+> hsep (map (ppr_term1 True) tt) 
-} -- TODO Printing infix constructors properly
  | null sub_terms_to_show
  = return (ppr dc)
  | otherwise 
  = do { tt_docs <- mapM (y app_prec) sub_terms_to_show
       ; return $ cparen (p >= app_prec) $
         sep [ppr dc, nest 2 (pprDeeperList fsep tt_docs)] }
  where
    sub_terms_to_show	-- Don't show the dictionary arguments to 
    			-- constructors unless -dppr-debug is on
      | opt_PprStyle_Debug = tt
      | otherwise = dropList (dataConTheta dc) tt

ppr_termM y p t@NewtypeWrap{} = pprNewtypeWrap y p t
ppr_termM y p RefWrap{wrapped_term=t}  = do
  contents <- y app_prec t
  return$ cparen (p >= app_prec) (text "GHC.Prim.MutVar#" <+> contents)
  -- The constructor name is wired in here ^^^ for the sake of simplicity.
  -- I don't think mutvars are going to change in a near future.
  -- In any case this is solely a presentation matter: MutVar# is
  -- a datatype with no constructors, implemented by the RTS
  -- (hence there is no way to obtain a datacon and print it).
ppr_termM _ _ t = ppr_termM1 t


ppr_termM1 :: Monad m => Term -> m SDoc
ppr_termM1 Prim{value=words, ty=ty} = 
    return $ repPrim (tyConAppTyCon ty) words
ppr_termM1 Suspension{ty=ty, bound_to=Nothing} = 
    return (char '_' <+> ifPprDebug (text "::" <> ppr ty))
ppr_termM1 Suspension{ty=ty, bound_to=Just n}
--  | Just _ <- splitFunTy_maybe ty = return$ ptext (sLit("<function>")
  | otherwise = return$ parens$ ppr n <> text "::" <> ppr ty
ppr_termM1 Term{}        = panic "ppr_termM1 - Term"
ppr_termM1 RefWrap{}     = panic "ppr_termM1 - RefWrap"
ppr_termM1 NewtypeWrap{} = panic "ppr_termM1 - NewtypeWrap"

pprNewtypeWrap y p NewtypeWrap{ty=ty, wrapped_term=t}
  | Just (tc,_) <- tcSplitTyConApp_maybe ty
  , ASSERT(isNewTyCon tc) True
  , Just new_dc <- tyConSingleDataCon_maybe tc = do 
             real_term <- y max_prec t
             return $ cparen (p >= app_prec) (ppr new_dc <+> real_term)
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

type CustomTermPrinter m = TermPrinterM m
                         -> [Precedence -> Term -> (m (Maybe SDoc))]

-- | Takes a list of custom printers with a explicit recursion knot and a term, 
-- and returns the output of the first successful printer, or the default printer
cPprTerm :: Monad m => CustomTermPrinter m -> Term -> m SDoc
cPprTerm printers_ = go 0 where
  printers = printers_ go
  go prec t = do
    let default_ = Just `liftM` pprTermM go prec t
        mb_customDocs = [pp prec t | pp <- printers] ++ [default_]
    Just doc <- firstJustM mb_customDocs
    return$ cparen (prec>app_prec+1) doc

  firstJustM (mb:mbs) = mb >>= maybe (firstJustM mbs) (return . Just)
  firstJustM [] = return Nothing

-- Default set of custom printers. Note that the recursion knot is explicit
cPprTermBase :: forall m. Monad m => CustomTermPrinter m
cPprTermBase y =
  [ ifTerm (isTupleTy.ty) (\_p -> liftM (parens . hcat . punctuate comma) 
                                      . mapM (y (-1))
                                      . subTerms)
  , ifTerm (\t -> isTyCon listTyCon (ty t) && subTerms t `lengthIs` 2)
           ppr_list
  , ifTerm (isTyCon intTyCon    . ty) ppr_int
  , ifTerm (isTyCon charTyCon   . ty) ppr_char
  , ifTerm (isTyCon floatTyCon  . ty) ppr_float
  , ifTerm (isTyCon doubleTyCon . ty) ppr_double
  , ifTerm (isIntegerTy         . ty) ppr_integer
  ]
 where 
   ifTerm :: (Term -> Bool)
          -> (Precedence -> Term -> m SDoc)
          -> Precedence -> Term -> m (Maybe SDoc)
   ifTerm pred f prec t@Term{}
       | pred t    = Just `liftM` f prec t
   ifTerm _ _ _ _  = return Nothing

   isTupleTy ty    = fromMaybe False $ do 
     (tc,_) <- tcSplitTyConApp_maybe ty 
     return (isBoxedTupleTyCon tc)

   isTyCon a_tc ty = fromMaybe False $ do 
     (tc,_) <- tcSplitTyConApp_maybe ty
     return (a_tc == tc)

   isIntegerTy ty = fromMaybe False $ do
     (tc,_) <- tcSplitTyConApp_maybe ty
     return (tyConName tc == integerTyConName)

   ppr_int, ppr_char, ppr_float, ppr_double, ppr_integer 
      :: Precedence -> Term -> m SDoc
   ppr_int     _ v = return (Ppr.int     (unsafeCoerce# (val v)))
   ppr_char    _ v = return (Ppr.char '\'' <> Ppr.char (unsafeCoerce# (val v)) <> Ppr.char '\'')
   ppr_float   _ v = return (Ppr.float   (unsafeCoerce# (val v)))
   ppr_double  _ v = return (Ppr.double  (unsafeCoerce# (val v)))
   ppr_integer _ v = return (Ppr.integer (unsafeCoerce# (val v)))

   --Note pprinting of list terms is not lazy
   ppr_list :: Precedence -> Term -> m SDoc
   ppr_list p (Term{subTerms=[h,t]}) = do
       let elems      = h : getListTerms t
           isConsLast = not(termType(last elems) `eqType` termType h)
   	   is_string  = all (isCharTy . ty) elems

       print_elems <- mapM (y cons_prec) elems
       if is_string
        then return (Ppr.doubleQuotes (Ppr.text (unsafeCoerce# (map val elems))))
        else if isConsLast
        then return $ cparen (p >= cons_prec) 
                    $ pprDeeperList fsep 
                    $ punctuate (space<>colon) print_elems
        else return $ brackets 
                    $ pprDeeperList fcat
                    $ punctuate comma print_elems

        where getListTerms Term{subTerms=[h,t]} = h : getListTerms t
              getListTerms Term{subTerms=[]}    = []
              getListTerms t@Suspension{}       = [t]
              getListTerms t = pprPanic "getListTerms" (ppr t)
   ppr_list _ _ = panic "doList"


repPrim :: TyCon -> [Word] -> SDoc
repPrim t = rep where
   rep x
    | t == charPrimTyCon             = text $ show (build x :: Char)
    | t == intPrimTyCon              = text $ show (build x :: Int)
    | t == wordPrimTyCon             = text $ show (build x :: Word)
    | t == floatPrimTyCon            = text $ show (build x :: Float)
    | t == doublePrimTyCon           = text $ show (build x :: Double)
    | t == int32PrimTyCon            = text $ show (build x :: Int32)
    | t == word32PrimTyCon           = text $ show (build x :: Word32)
    | t == int64PrimTyCon            = text $ show (build x :: Int64)
    | t == word64PrimTyCon           = text $ show (build x :: Word64)
    | t == addrPrimTyCon             = text $ show (nullPtr `plusPtr` build x)
    | t == stablePtrPrimTyCon        = text "<stablePtr>"
    | t == stableNamePrimTyCon       = text "<stableName>"
    | t == statePrimTyCon            = text "<statethread>"
    | t == proxyPrimTyCon            = text "<proxy>"
    | t == realWorldTyCon            = text "<realworld>"
    | t == threadIdPrimTyCon         = text "<ThreadId>"
    | t == weakPrimTyCon             = text "<Weak>"
    | t == arrayPrimTyCon            = text "<array>"
    | t == byteArrayPrimTyCon        = text "<bytearray>"
    | t == mutableArrayPrimTyCon     = text "<mutableArray>"
    | t == mutableByteArrayPrimTyCon = text "<mutableByteArray>"
    | t == mutVarPrimTyCon           = text "<mutVar>"
    | t == mVarPrimTyCon             = text "<mVar>"
    | t == tVarPrimTyCon             = text "<tVar>"
    | otherwise                      = char '<' <> ppr t <> char '>'
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


-- A (non-mutable) tau type containing
-- existentially quantified tyvars.
--    (since GHC type language currently does not support
--     existentials, we leave these variables unquantified)
type RttiType = Type

-- An incomplete type as stored in GHCi:
--  no polymorphism: no quantifiers & all tyvars are skolem.
type GhciType = Type


-- The Type Reconstruction monad
--------------------------------
type TR a = TcM a

runTR :: HscEnv -> TR a -> IO a
runTR hsc_env thing = do
  mb_val <- runTR_maybe hsc_env thing
  case mb_val of
    Nothing -> error "unable to :print the term"
    Just x  -> return x

runTR_maybe :: HscEnv -> TR a -> IO (Maybe a)
runTR_maybe hsc_env thing_inside
  = do { (_errs, res) <- initTc hsc_env HsSrcFile False 
                                (icInteractiveModule (hsc_IC hsc_env))
                                thing_inside
       ; return res }

traceTR :: SDoc -> TR ()
traceTR = liftTcM . traceOptTcRn Opt_D_dump_rtti


-- Semantically different to recoverM in TcRnMonad 
-- recoverM retains the errors in the first action,
--  whereas recoverTc here does not
recoverTR :: TR a -> TR a -> TR a
recoverTR recover thing = do 
  (_,mb_res) <- tryTcErrs thing
  case mb_res of 
    Nothing  -> recover
    Just res -> return res

trIO :: IO a -> TR a 
trIO = liftTcM . liftIO

liftTcM :: TcM a -> TR a
liftTcM = id

newVar :: Kind -> TR TcType
newVar = liftTcM . newFlexiTyVarTy

instTyVars :: [TyVar] -> TR ([TcTyVar], [TcType], TvSubst)
-- Instantiate fresh mutable type variables from some TyVars
-- This function preserves the print-name, which helps error messages
instTyVars = liftTcM . tcInstTyVars

type RttiInstantiation = [(TcTyVar, TyVar)]
   -- Associates the typechecker-world meta type variables 
   -- (which are mutable and may be refined), to their 
   -- debugger-world RuntimeUnk counterparts.
   -- If the TcTyVar has not been refined by the runtime type
   -- elaboration, then we want to turn it back into the
   -- original RuntimeUnk

-- | Returns the instantiated type scheme ty', and the 
--   mapping from new (instantiated) -to- old (skolem) type variables
instScheme :: QuantifiedType -> TR (TcType, RttiInstantiation)
instScheme (tvs, ty) 
  = liftTcM $ do { (tvs', _, subst) <- tcInstTyVars tvs
                 ; let rtti_inst = [(tv',tv) | (tv',tv) <- tvs' `zip` tvs]
                 ; return (substTy subst ty, rtti_inst) }

applyRevSubst :: RttiInstantiation -> TR ()
-- Apply the *reverse* substitution in-place to any un-filled-in
-- meta tyvars.  This recovers the original debugger-world variable
-- unless it has been refined by new information from the heap
applyRevSubst pairs = liftTcM (mapM_ do_pair pairs)
  where
    do_pair (tc_tv, rtti_tv)
      = do { tc_ty <- zonkTcTyVar tc_tv
           ; case tcGetTyVar_maybe tc_ty of
               Just tv | isMetaTyVar tv -> writeMetaTyVar tv (mkTyVarTy rtti_tv)
               _                        -> return () }

-- Adds a constraint of the form t1 == t2
-- t1 is expected to come from walking the heap
-- t2 is expected to come from a datacon signature
-- Before unification, congruenceNewtypes needs to
-- do its magic.
addConstraint :: TcType -> TcType -> TR ()
addConstraint actual expected = do
    traceTR (text "add constraint:" <+> fsep [ppr actual, equals, ppr expected])
    recoverTR (traceTR $ fsep [text "Failed to unify", ppr actual,
                                    text "with", ppr expected]) $
      do { (ty1, ty2) <- congruenceNewtypes actual expected
         ; _  <- captureConstraints $ unifyType ty1 ty2
         ; return () }
     -- TOMDO: what about the coercion?
     -- we should consider family instances


-- Type & Term reconstruction
------------------------------
cvObtainTerm :: HscEnv -> Int -> Bool -> RttiType -> HValue -> IO Term
cvObtainTerm hsc_env max_depth force old_ty hval = runTR hsc_env $ do
  -- we quantify existential tyvars as universal,
  -- as this is needed to be able to manipulate
  -- them properly
   let quant_old_ty@(old_tvs, old_tau) = quantifyType old_ty
       sigma_old_ty = mkForAllTys old_tvs old_tau
   traceTR (text "Term reconstruction started with initial type " <> ppr old_ty)
   term <-
     if null old_tvs
      then do
        term  <- go max_depth sigma_old_ty sigma_old_ty hval
        term' <- zonkTerm term
        return $ fixFunDictionaries $ expandNewtypes term'
      else do
              (old_ty', rev_subst) <- instScheme quant_old_ty
              my_ty <- newVar openTypeKind
              when (check1 quant_old_ty) (traceTR (text "check1 passed") >>
                                          addConstraint my_ty old_ty')
              term  <- go max_depth my_ty sigma_old_ty hval
              new_ty <- zonkTcType (termType term)
              if isMonomorphic new_ty || check2 (quantifyType new_ty) quant_old_ty
                 then do
                      traceTR (text "check2 passed")
                      addConstraint new_ty old_ty'
                      applyRevSubst rev_subst
                      zterm' <- zonkTerm term
                      return ((fixFunDictionaries . expandNewtypes) zterm')
                 else do
                      traceTR (text "check2 failed" <+> parens
                                       (ppr term <+> text "::" <+> ppr new_ty))
                      -- we have unsound types. Replace constructor types in
                      -- subterms with tyvars
                      zterm' <- mapTermTypeM
                                 (\ty -> case tcSplitTyConApp_maybe ty of
                                           Just (tc, _:_) | tc /= funTyCon
                                               -> newVar openTypeKind
                                           _   -> return ty)
                                 term
                      zonkTerm zterm'
   traceTR (text "Term reconstruction completed." $$
            text "Term obtained: " <> ppr term $$
            text "Type obtained: " <> ppr (termType term))
   return term
    where 
  dflags = hsc_dflags hsc_env

  go :: Int -> Type -> Type -> HValue -> TcM Term
   -- I believe that my_ty should not have any enclosing
   -- foralls, nor any free RuntimeUnk skolems;
   -- that is partly what the quantifyType stuff achieved
   --
   -- [SPJ May 11] I don't understand the difference between my_ty and old_ty

  go max_depth _ _ _ | seq max_depth False = undefined
  go 0 my_ty _old_ty a = do
    traceTR (text "Gave up reconstructing a term after" <>
                  int max_depth <> text " steps")
    clos <- trIO $ getClosureData dflags a
    return (Suspension (tipe clos) my_ty a Nothing)
  go max_depth my_ty old_ty a = do
    let monomorphic = not(isTyVarTy my_ty)   
    -- This ^^^ is a convention. The ancestor tests for
    -- monomorphism and passes a type instead of a tv
    clos <- trIO $ getClosureData dflags a
    case tipe clos of
-- Thunks we may want to force
      t | isThunk t && force -> traceTR (text "Forcing a " <> text (show t)) >>
                                seq a (go (pred max_depth) my_ty old_ty a)
-- Blackholes are indirections iff the payload is not TSO or BLOCKING_QUEUE.  So we
-- treat them like indirections; if the payload is TSO or BLOCKING_QUEUE, we'll end up
-- showing '_' which is what we want.
      Blackhole -> do traceTR (text "Following a BLACKHOLE")
                      appArr (go max_depth my_ty old_ty) (ptrs clos) 0
-- We always follow indirections
      Indirection i -> do traceTR (text "Following an indirection" <> parens (int i) )
                          go max_depth my_ty old_ty $! (ptrs clos ! 0)
-- We also follow references
      MutVar _ | Just (tycon,[world,contents_ty]) <- tcSplitTyConApp_maybe old_ty
             -> do
                  -- Deal with the MutVar# primitive
                  -- It does not have a constructor at all, 
                  -- so we simulate the following one
                  -- MutVar# :: contents_ty -> MutVar# s contents_ty
         traceTR (text "Following a MutVar")
         contents_tv <- newVar liftedTypeKind
         contents <- trIO$ IO$ \w -> readMutVar# (unsafeCoerce# a) w
         ASSERT(isUnliftedTypeKind $ typeKind my_ty) return ()
         (mutvar_ty,_) <- instScheme $ quantifyType $ mkFunTy 
                            contents_ty (mkTyConApp tycon [world,contents_ty])
         addConstraint (mkFunTy contents_tv my_ty) mutvar_ty
         x <- go (pred max_depth) contents_tv contents_ty contents
         return (RefWrap my_ty x)

 -- The interesting case
      Constr -> do
        traceTR (text "entering a constructor " <>
                      if monomorphic
                        then parens (text "already monomorphic: " <> ppr my_ty)
                        else Ppr.empty)
        Right dcname <- dataConInfoPtrToName (infoPtr clos)
        (_,mb_dc)    <- tryTcErrs (tcLookupDataCon dcname)
        case mb_dc of
          Nothing -> do -- This can happen for private constructors compiled -O0
                        -- where the .hi descriptor does not export them
                        -- In such case, we return a best approximation:
                        --  ignore the unpointed args, and recover the pointeds
                        -- This preserves laziness, and should be safe.
		       traceTR (text "Not constructor" <+> ppr dcname)
                       let dflags = hsc_dflags hsc_env
                           tag = showPpr dflags dcname
                       vars     <- replicateM (length$ elems$ ptrs clos) 
                                              (newVar liftedTypeKind)
                       subTerms <- sequence [appArr (go (pred max_depth) tv tv) (ptrs clos) i 
                                              | (i, tv) <- zip [0..] vars]
                       return (Term my_ty (Left ('<' : tag ++ ">")) a subTerms)
          Just dc -> do
            traceTR (text "Is constructor" <+> (ppr dc $$ ppr my_ty))
            subTtypes <- getDataConArgTys dc my_ty
            subTerms <- extractSubTerms (\ty -> go (pred max_depth) ty ty) clos subTtypes
            return (Term my_ty (Right dc) a subTerms)

-- The otherwise case: can be a Thunk,AP,PAP,etc.
      tipe_clos ->
         return (Suspension tipe_clos my_ty a Nothing)

  -- insert NewtypeWraps around newtypes
  expandNewtypes = foldTerm idTermFold { fTerm = worker } where
   worker ty dc hval tt
     | Just (tc, args) <- tcSplitTyConApp_maybe ty
     , isNewTyCon tc
     , wrapped_type    <- newTyConInstRhs tc args
     , Just dc'        <- tyConSingleDataCon_maybe tc
     , t'              <- worker wrapped_type dc hval tt
     = NewtypeWrap ty (Right dc') t'
     | otherwise = Term ty dc hval tt


   -- Avoid returning types where predicates have been expanded to dictionaries.
  fixFunDictionaries = foldTerm idTermFold {fSuspension = worker} where
      worker ct ty hval n | isFunTy ty = Suspension ct (dictsView ty) hval n
                          | otherwise  = Suspension ct ty hval n

extractSubTerms :: (Type -> HValue -> TcM Term)
                -> Closure -> [Type] -> TcM [Term]
extractSubTerms recurse clos = liftM thirdOf3 . go 0 (nonPtrs clos)
  where
    go ptr_i ws [] = return (ptr_i, ws, [])
    go ptr_i ws (ty:tys)
      | Just (tc, elem_tys) <- tcSplitTyConApp_maybe ty
      , isUnboxedTupleTyCon tc
      = do (ptr_i, ws, terms0) <- go ptr_i ws elem_tys
           (ptr_i, ws, terms1) <- go ptr_i ws tys
           return (ptr_i, ws, unboxedTupleTerm ty terms0 : terms1)
      | otherwise
      = case repType ty of
          UnaryRep rep_ty -> do
            (ptr_i, ws, term0)  <- go_rep ptr_i ws ty (typePrimRep rep_ty)
            (ptr_i, ws, terms1) <- go ptr_i ws tys
            return (ptr_i, ws, term0 : terms1)
          UbxTupleRep rep_tys -> do
            (ptr_i, ws, terms0) <- go_unary_types ptr_i ws rep_tys
            (ptr_i, ws, terms1) <- go ptr_i ws tys
            return (ptr_i, ws, unboxedTupleTerm ty terms0 : terms1)

    go_unary_types ptr_i ws [] = return (ptr_i, ws, [])
    go_unary_types ptr_i ws (rep_ty:rep_tys) = do
      tv <- newVar liftedTypeKind
      (ptr_i, ws, term0)  <- go_rep ptr_i ws tv (typePrimRep rep_ty)
      (ptr_i, ws, terms1) <- go_unary_types ptr_i ws rep_tys
      return (ptr_i, ws, term0 : terms1)

    go_rep ptr_i ws ty rep = case rep of
      PtrRep -> do
        t <- appArr (recurse ty) (ptrs clos) ptr_i
        return (ptr_i + 1, ws, t)
      _ -> do
        dflags <- getDynFlags
        let (ws0, ws1) = splitAt (primRepSizeW dflags rep) ws
        return (ptr_i, ws1, Prim ty ws0)

    unboxedTupleTerm ty terms = Term ty (Right (tupleCon UnboxedTuple (length terms)))
                                        (error "unboxedTupleTerm: no HValue for unboxed tuple") terms


-- Fast, breadth-first Type reconstruction
------------------------------------------
cvReconstructType :: HscEnv -> Int -> GhciType -> HValue -> IO (Maybe Type)
cvReconstructType hsc_env max_depth old_ty hval = runTR_maybe hsc_env $ do
   traceTR (text "RTTI started with initial type " <> ppr old_ty)
   let sigma_old_ty@(old_tvs, _) = quantifyType old_ty
   new_ty <-
       if null old_tvs
        then return old_ty
        else do
          (old_ty', rev_subst) <- instScheme sigma_old_ty
          my_ty <- newVar openTypeKind
          when (check1 sigma_old_ty) (traceTR (text "check1 passed") >>
                                      addConstraint my_ty old_ty')
          search (isMonomorphic `fmap` zonkTcType my_ty)
                 (\(ty,a) -> go ty a)
                 (Seq.singleton (my_ty, hval))
                 max_depth
          new_ty <- zonkTcType my_ty
          if isMonomorphic new_ty || check2 (quantifyType new_ty) sigma_old_ty
            then do
                 traceTR (text "check2 passed" <+> ppr old_ty $$ ppr new_ty)
                 addConstraint my_ty old_ty'
                 applyRevSubst rev_subst
                 zonkRttiType new_ty
            else traceTR (text "check2 failed" <+> parens (ppr new_ty)) >>
                 return old_ty
   traceTR (text "RTTI completed. Type obtained:" <+> ppr new_ty)
   return new_ty
    where
  dflags = hsc_dflags hsc_env

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
  go my_ty a = do
    traceTR (text "go" <+> ppr my_ty)
    clos <- trIO $ getClosureData dflags a
    case tipe clos of
      Blackhole -> appArr (go my_ty) (ptrs clos) 0 -- carefully, don't eval the TSO
      Indirection _ -> go my_ty $! (ptrs clos ! 0)
      MutVar _ -> do
         contents <- trIO$ IO$ \w -> readMutVar# (unsafeCoerce# a) w
         tv'   <- newVar liftedTypeKind
         world <- newVar liftedTypeKind
         addConstraint my_ty (mkTyConApp mutVarPrimTyCon [world,tv'])
         return [(tv', contents)]
      Constr -> do
        Right dcname <- dataConInfoPtrToName (infoPtr clos)
        traceTR (text "Constr1" <+> ppr dcname)
        (_,mb_dc)    <- tryTcErrs (tcLookupDataCon dcname)
        case mb_dc of
          Nothing-> do
                     --  TODO: Check this case
            forM [0..length (elems $ ptrs clos)] $ \i -> do
                        tv <- newVar liftedTypeKind
                        return$ appArr (\e->(tv,e)) (ptrs clos) i

          Just dc -> do
            arg_tys <- getDataConArgTys dc my_ty
            (_, itys) <- findPtrTyss 0 arg_tys
            traceTR (text "Constr2" <+> ppr dcname <+> ppr arg_tys)
            return $ [ appArr (\e-> (ty,e)) (ptrs clos) i
                     | (i,ty) <- itys]
      _ -> return []

findPtrTys :: Int  -- Current pointer index
           -> Type -- Type
           -> TR (Int, [(Int, Type)])
findPtrTys i ty
  | Just (tc, elem_tys) <- tcSplitTyConApp_maybe ty
  , isUnboxedTupleTyCon tc
  = findPtrTyss i elem_tys
  
  | otherwise
  = case repType ty of
      UnaryRep rep_ty | typePrimRep rep_ty == PtrRep -> return (i + 1, [(i, ty)])
                      | otherwise                    -> return (i,     [])
      UbxTupleRep rep_tys  -> foldM (\(i, extras) rep_ty -> if typePrimRep rep_ty == PtrRep
                                                             then newVar liftedTypeKind >>= \tv -> return (i + 1, extras ++ [(i, tv)])
                                                             else return (i, extras))
                                    (i, []) rep_tys

findPtrTyss :: Int
            -> [Type]
            -> TR (Int, [(Int, Type)])
findPtrTyss i tys = foldM step (i, []) tys
  where step (i, discovered) elem_ty = findPtrTys i elem_ty >>= \(i, extras) -> return (i, discovered ++ extras)


-- Compute the difference between a base type and the type found by RTTI
-- improveType <base_type> <rtti_type>
-- The types can contain skolem type variables, which need to be treated as normal vars.
-- In particular, we want them to unify with things.
improveRTTIType :: HscEnv -> RttiType -> RttiType -> Maybe TvSubst
improveRTTIType _ base_ty new_ty = U.tcUnifyTy base_ty new_ty

getDataConArgTys :: DataCon -> Type -> TR [Type]
-- Given the result type ty of a constructor application (D a b c :: ty)
-- return the types of the arguments.  This is RTTI-land, so 'ty' might
-- not be fully known.  Moreover, the arg types might involve existentials;
-- if so, make up fresh RTTI type variables for them
--
-- I believe that con_app_ty should not have any enclosing foralls
getDataConArgTys dc con_app_ty
  = do { let UnaryRep rep_con_app_ty = repType con_app_ty
       ; traceTR (text "getDataConArgTys 1" <+> (ppr con_app_ty $$ ppr rep_con_app_ty 
                   $$ ppr (tcSplitTyConApp_maybe rep_con_app_ty)))
       ; (_, _, subst) <- instTyVars (univ_tvs ++ ex_tvs)
       ; addConstraint rep_con_app_ty (substTy subst (dataConOrigResTy dc))
              -- See Note [Constructor arg types]
       ; let con_arg_tys = substTys subst (dataConRepArgTys dc)
       ; traceTR (text "getDataConArgTys 2" <+> (ppr rep_con_app_ty $$ ppr con_arg_tys $$ ppr subst))
       ; return con_arg_tys }
  where
    univ_tvs = dataConUnivTyVars dc
    ex_tvs   = dataConExTyVars dc

{- Note [Constructor arg types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a GADT (cf Trac #7386)
   data family D a b
   data instance D [a] a where
     MkT :: a -> D [a] (Maybe a)
     ...

In getDataConArgTys
* con_app_ty is the known type (from outside) of the constructor application, 
  say D [Int] Int

* The data constructor MkT has a (representation) dataConTyCon = DList,
  say where
    data DList a where
      MkT :: a -> DList a (Maybe a)
      ...

So the dataConTyCon of the data constructor, DList, differs from 
the "outside" type, D. So we can't straightforwardly decompose the
"outside" type, and we end up in the "_" branch of the case.

Then we match the dataConOrigResTy of the data constructor against the
outside type, hoping to get a substitution that tells how to instantiate
the *representation* type constructor.   This looks a bit delicate to
me, but it seems to work.
-}

-- Soundness checks
--------------------
{-
This is not formalized anywhere, so hold to your seats!
RTTI in the presence of newtypes can be a tricky and unsound business.

Example:
~~~~~~~~~
Suppose we are doing RTTI for a partially evaluated
closure t, the real type of which is t :: MkT Int, for

   newtype MkT a = MkT [Maybe a]

The table below shows the results of RTTI and the improvement
calculated for different combinations of evaluatedness and :type t.
Regard the two first columns as input and the next two as output.

  # |     t     |  :type t  | rtti(t)  | improv.    | result
    ------------------------------------------------------------
  1 |     _     |    t b    |    a     | none       | OK
  2 |     _     |   MkT b   |    a     | none       | OK
  3 |     _     |   t Int   |    a     | none       | OK

  If t is not evaluated at *all*, we are safe.

  4 |  (_ : _)  |    t b    |   [a]    | t = []     | UNSOUND
  5 |  (_ : _)  |   MkT b   |  MkT a   | none       | OK (compensating for the missing newtype)
  6 |  (_ : _)  |   t Int   |  [Int]   | t = []     | UNSOUND

  If a is a minimal whnf, we run into trouble. Note that
  row 5 above does newtype enrichment on the ty_rtty parameter.

  7 | (Just _:_)|    t b    |[Maybe a] | t = [],    | UNSOUND
    |                       |          | b = Maybe a|

  8 | (Just _:_)|   MkT b   |  MkT a   |  none      | OK
  9 | (Just _:_)|   t Int   |   FAIL   |  none      | OK

  And if t is any more evaluated than whnf, we are still in trouble.
  Because constraints are solved in top-down order, when we reach the
  Maybe subterm what we got is already unsound. This explains why the
  row 9 fails to complete.

  10 | (Just _:_)|  t Int  | [Maybe a]   |  FAIL    | OK
  11 | (Just 1:_)|  t Int  | [Maybe Int] |  FAIL    | OK

  We can undo the failure in row 9 by leaving out the constraint
  coming from the type signature of t (i.e., the 2nd column).
  Note that this type information is still used
  to calculate the improvement. But we fail
  when trying to calculate the improvement, as there is no unifier for
  t Int = [Maybe a] or t Int = [Maybe Int].


  Another set of examples with t :: [MkT (Maybe Int)]  \equiv  [[Maybe (Maybe Int)]]

  # |     t     |    :type t    |  rtti(t)    | improvement | result
    ---------------------------------------------------------------------
  1 |(Just _:_) | [t (Maybe a)] | [[Maybe b]] | t = []      |
    |           |               |             | b = Maybe a |

The checks:
~~~~~~~~~~~
Consider a function obtainType that takes a value and a type and produces
the Term representation and a substitution (the improvement).
Assume an auxiliar rtti' function which does the actual job if recovering
the type, but which may produce a false type.

In pseudocode:

  rtti' :: a -> IO Type  -- Does not use the static type information

  obtainType :: a -> Type -> IO (Maybe (Term, Improvement))
  obtainType v old_ty = do
       rtti_ty <- rtti' v
       if monomorphic rtti_ty || (check rtti_ty old_ty)
        then ...
         else return Nothing
  where check rtti_ty old_ty = check1 rtti_ty &&
                              check2 rtti_ty old_ty

  check1 :: Type -> Bool
  check2 :: Type -> Type -> Bool

Now, if rtti' returns a monomorphic type, we are safe.
If that is not the case, then we consider two conditions.


1. To prevent the class of unsoundness displayed by
   rows 4 and 7 in the example: no higher kind tyvars
   accepted.

  check1 (t a)   = NO
  check1 (t Int) = NO
  check1 ([] a)  = YES

2. To prevent the class of unsoundness shown by row 6,
   the rtti type should be structurally more
   defined than the old type we are comparing it to.
  check2 :: NewType -> OldType -> Bool
  check2 a  _        = True
  check2 [a] a       = True
  check2 [a] (t Int) = False
  check2 [a] (t a)   = False  -- By check1 we never reach this equation
  check2 [Int] a     = True
  check2 [Int] (t Int) = True
  check2 [Maybe a]   (t Int) = False
  check2 [Maybe Int] (t Int) = True
  check2 (Maybe [a])   (m [Int]) = False
  check2 (Maybe [Int]) (m [Int]) = True

-}

check1 :: QuantifiedType -> Bool
check1 (tvs, _) = not $ any isHigherKind (map tyVarKind tvs)
 where
   isHigherKind = not . null . fst . splitKindFunTys

check2 :: QuantifiedType -> QuantifiedType -> Bool
check2 (_, rtti_ty) (_, old_ty)
  | Just (_, rttis) <- tcSplitTyConApp_maybe rtti_ty
  = case () of
      _ | Just (_,olds) <- tcSplitTyConApp_maybe old_ty
        -> and$ zipWith check2 (map quantifyType rttis) (map quantifyType olds)
      _ | Just _ <- splitAppTy_maybe old_ty
        -> isMonomorphicOnNonPhantomArgs rtti_ty
      _ -> True
  | otherwise = True

-- Dealing with newtypes
--------------------------
{-
 congruenceNewtypes does a parallel fold over two Type values, 
 compensating for missing newtypes on both sides. 
 This is necessary because newtypes are not present 
 in runtime, but sometimes there is evidence available.
   Evidence can come from DataCon signatures or
 from compile-time type inference.
 What we are doing here is an approximation
 of unification modulo a set of equations derived
 from newtype definitions. These equations should be the
 same as the equality coercions generated for newtypes
 in System Fc. The idea is to perform a sort of rewriting,
 taking those equations as rules, before launching unification.

 The caller must ensure the following.
 The 1st type (lhs) comes from the heap structure of ptrs,nptrs.
 The 2nd type (rhs) comes from a DataCon type signature.
 Rewriting (i.e. adding/removing a newtype wrapper) can happen
 in both types, but in the rhs it is restricted to the result type.

   Note that it is very tricky to make this 'rewriting'
 work with the unification implemented by TcM, where
 substitutions are operationally inlined. The order in which
 constraints are unified is vital as we cannot modify
 anything that has been touched by a previous unification step.
Therefore, congruenceNewtypes is sound only if the types
recovered by the RTTI mechanism are unified Top-Down.
-}
congruenceNewtypes ::  TcType -> TcType -> TR (TcType,TcType)
congruenceNewtypes lhs rhs = go lhs rhs >>= \rhs' -> return (lhs,rhs')
 where
   go l r
 -- TyVar lhs inductive case
    | Just tv <- getTyVar_maybe l
    , isTcTyVar tv
    , isMetaTyVar tv
    = recoverTR (return r) $ do
         Indirect ty_v <- readMetaTyVar tv
         traceTR $ fsep [text "(congruence) Following indirect tyvar:",
                          ppr tv, equals, ppr ty_v]
         go ty_v r
-- FunTy inductive case
    | Just (l1,l2) <- splitFunTy_maybe l
    , Just (r1,r2) <- splitFunTy_maybe r
    = do r2' <- go l2 r2
         r1' <- go l1 r1
         return (mkFunTy r1' r2')
-- TyconApp Inductive case; this is the interesting bit.
    | Just (tycon_l, _) <- tcSplitTyConApp_maybe lhs
    , Just (tycon_r, _) <- tcSplitTyConApp_maybe rhs 
    , tycon_l /= tycon_r 
    = upgrade tycon_l r

    | otherwise = return r

    where upgrade :: TyCon -> Type -> TR Type
          upgrade new_tycon ty
            | not (isNewTyCon new_tycon) = do
              traceTR (text "(Upgrade) Not matching newtype evidence: " <>
                       ppr new_tycon <> text " for " <> ppr ty)
              return ty 
            | otherwise = do
               traceTR (text "(Upgrade) upgraded " <> ppr ty <>
                        text " in presence of newtype evidence " <> ppr new_tycon)
               (_, vars, _) <- instTyVars (tyConTyVars new_tycon)
               let ty' = mkTyConApp new_tycon vars
                   UnaryRep rep_ty = repType ty'
               _ <- liftTcM (unifyType ty rep_ty)
        -- assumes that reptype doesn't ^^^^ touch tyconApp args 
               return ty'


zonkTerm :: Term -> TcM Term
zonkTerm = foldTermM (TermFoldM
             { fTermM = \ty dc v tt -> zonkRttiType ty    >>= \ty' ->
                                       return (Term ty' dc v tt)
             , fSuspensionM  = \ct ty v b -> zonkRttiType ty >>= \ty ->
                                             return (Suspension ct ty v b)
             , fNewtypeWrapM = \ty dc t -> zonkRttiType ty >>= \ty' ->
                                           return$ NewtypeWrap ty' dc t
             , fRefWrapM     = \ty t -> return RefWrap  `ap` 
                                        zonkRttiType ty `ap` return t
             , fPrimM        = (return.) . Prim })

zonkRttiType :: TcType -> TcM Type
-- Zonk the type, replacing any unbound Meta tyvars
-- by skolems, safely out of Meta-tyvar-land
zonkRttiType = zonkTcTypeToType (mkEmptyZonkEnv zonk_unbound_meta)
  where
    zonk_unbound_meta tv 
      = ASSERT( isTcTyVar tv )
        do { tv' <- skolemiseUnboundMetaTyVar tv RuntimeUnk
	     -- This is where RuntimeUnks are born: 
	     -- otherwise-unconstrained unification variables are
	     -- turned into RuntimeUnks as they leave the
	     -- typechecker's monad
           ; return (mkTyVarTy tv') }

--------------------------------------------------------------------------------
-- Restore Class predicates out of a representation type
dictsView :: Type -> Type
dictsView ty = ty


-- Use only for RTTI types
isMonomorphic :: RttiType -> Bool
isMonomorphic ty = noExistentials && noUniversals
 where (tvs, _, ty')  = tcSplitSigmaTy ty
       noExistentials = isEmptyVarSet (tyVarsOfType ty')
       noUniversals   = null tvs

-- Use only for RTTI types
isMonomorphicOnNonPhantomArgs :: RttiType -> Bool
isMonomorphicOnNonPhantomArgs ty
  | UnaryRep rep_ty <- repType ty
  , Just (tc, all_args) <- tcSplitTyConApp_maybe rep_ty
  , phantom_vars  <- tyConPhantomTyVars tc
  , concrete_args <- [ arg | (tyv,arg) <- tyConTyVars tc `zip` all_args
                           , tyv `notElem` phantom_vars]
  = all isMonomorphicOnNonPhantomArgs concrete_args
  | Just (ty1, ty2) <- splitFunTy_maybe ty
  = all isMonomorphicOnNonPhantomArgs [ty1,ty2]
  | otherwise = isMonomorphic ty

tyConPhantomTyVars :: TyCon -> [TyVar]
tyConPhantomTyVars tc
  | isAlgTyCon tc
  , Just dcs <- tyConDataCons_maybe tc
  , dc_vars  <- concatMap dataConUnivTyVars dcs
  = tyConTyVars tc \\ dc_vars
tyConPhantomTyVars _ = []

type QuantifiedType = ([TyVar], Type)
   -- Make the free type variables explicit
   -- The returned Type should have no top-level foralls (I believe)

quantifyType :: Type -> QuantifiedType
-- Generalize the type: find all free and forall'd tyvars
-- and return them, together with the type inside, which
-- should not be a forall type.
--
-- Thus (quantifyType (forall a. a->[b]))
-- returns ([a,b], a -> [b])

quantifyType ty = (varSetElems (tyVarsOfType rho), rho)
  where
    (_tvs, rho) = tcSplitForAllTys ty

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condM acc = condM >>= \c -> unless c acc


-- Strict application of f at index i
appArr :: Ix i => (e -> a) -> Array i e -> Int -> a
appArr f a@(Array _ _ _ ptrs#) i@(I# i#)
 = ASSERT2(i < length(elems a), ppr(length$ elems a, i))
   case indexArray# ptrs# i# of
       (# e #) -> f e

amap' :: (t -> b) -> Array Int t -> [b]
amap' f (Array i0 i _ arr#) = map g [0 .. i - i0]
    where g (I# i#) = case indexArray# arr# i# of
                          (# e #) -> f e
