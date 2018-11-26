{-# LANGUAGE BangPatterns, CPP, ScopedTypeVariables, MagicHash, UnboxedTuples #-}

-----------------------------------------------------------------------------
--
-- GHC Interactive support for inspecting arbitrary closures at runtime
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------
module RtClosureInspect(
     -- * Entry points and types
     cvObtainTerm,
     cvReconstructType,
     improveRTTIType,
     Term(..),

     -- * Utils
     isFullyEvaluatedTerm,
     termType, mapTermType, termTyCoVars,
     foldTerm, TermFold(..),
     cPprTerm, cPprTermBase,

     constrClosToName -- exported to use in test T4891
 ) where

#include "HsVersions.h"

import GhcPrelude

import GHCi
import GHCi.RemoteTypes
import HscTypes

import DataCon
import Type
import RepType
import Multiplicity
import qualified Unify as U
import Var
import TcRnMonad
import TcType
import TcMType
import TcHsSyn ( zonkTcTypeToTypeX, mkEmptyZonkEnv, ZonkFlexi( RuntimeUnkFlexi ) )
import TcUnify
import TcEnv

import TyCon
import Name
import OccName
import Module
import IfaceEnv
import Util
import VarSet
import BasicTypes       ( Boxity(..) )
import TysPrim
import PrelNames
import TysWiredIn
import DynFlags
import Outputable as Ppr
import GHC.Char
import GHC.Exts.Heap
import SMRep ( roundUpTo )

import Control.Monad
import Data.Maybe
import Data.List
#if defined(INTEGER_GMP)
import GHC.Exts
import Data.Array.Base
import GHC.Integer.GMP.Internals
#endif
import qualified Data.Sequence as Seq
import Data.Sequence (viewl, ViewL(..))
import Foreign
import System.IO.Unsafe


---------------------------------------------
-- * A representation of semi evaluated Terms
---------------------------------------------

data Term = Term { ty        :: RttiType
                 , dc        :: Either String DataCon
                               -- Carries a text representation if the datacon is
                               -- not exported by the .hi file, which is the case
                               -- for private constructors in -O0 compiled libraries
                 , val       :: ForeignHValue
                 , subTerms  :: [Term] }

          | Prim { ty        :: RttiType
                 , valRaw    :: [Word] }

          | Suspension { ctype    :: ClosureType
                       , ty       :: RttiType
                       , val      :: ForeignHValue
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

----------------------------------------
-- Runtime Closure information functions
----------------------------------------

isThunk :: GenClosure a -> Bool
isThunk ThunkClosure{} = True
isThunk APClosure{} = True
isThunk APStackClosure{} = True
isThunk _             = False

-- Lookup the name in a constructor closure
constrClosToName :: HscEnv -> GenClosure a -> IO (Either String Name)
constrClosToName hsc_env ConstrClosure{pkg=pkg,modl=mod,name=occ} = do
   let occName = mkOccName OccName.dataName occ
       modName = mkModule (stringToUnitId pkg) (mkModuleName mod)
   Right `fmap` lookupOrigIO hsc_env modName occName
constrClosToName _hsc_env clos =
   return (Left ("conClosToName: Expected ConstrClosure, got " ++ show (fmap (const ()) clos)))

-----------------------------------
-- * Traversals for Terms
-----------------------------------
type TermProcessor a b = RttiType -> Either String DataCon -> ForeignHValue -> [a] -> b

data TermFold a = TermFold { fTerm        :: TermProcessor a a
                           , fPrim        :: RttiType -> [Word] -> a
                           , fSuspension  :: ClosureType -> RttiType -> ForeignHValue
                                            -> Maybe Name -> a
                           , fNewtypeWrap :: RttiType -> Either String DataCon
                                            -> a -> a
                           , fRefWrap     :: RttiType -> a -> a
                           }


data TermFoldM m a =
                   TermFoldM {fTermM        :: TermProcessor a (m a)
                            , fPrimM        :: RttiType -> [Word] -> m a
                            , fSuspensionM  :: ClosureType -> RttiType -> ForeignHValue
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

termTyCoVars :: Term -> TyCoVarSet
termTyCoVars = foldTerm TermFold {
            fTerm       = \ty _ _ tt   ->
                          tyCoVarsOfType ty `unionVarSet` concatVarEnv tt,
            fSuspension = \_ ty _ _ -> tyCoVarsOfType ty,
            fPrim       = \ _ _ -> emptyVarSet,
            fNewtypeWrap= \ty _ t -> tyCoVarsOfType ty `unionVarSet` t,
            fRefWrap    = \ty t -> tyCoVarsOfType ty `unionVarSet` t}
    where concatVarEnv = foldr unionVarSet emptyVarSet

----------------------------------
-- Pretty printing of terms
----------------------------------

type Precedence        = Int
type TermPrinterM m    = Precedence -> Term -> m SDoc

app_prec,cons_prec, max_prec ::Int
max_prec  = 10
app_prec  = max_prec
cons_prec = 5 -- TODO Extract this info from GHC itself

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
  = do { tt_docs' <- mapM (y app_prec) tt
       ; return $ ifPprDebug (show_tm tt_docs')
                             (show_tm (dropList (dataConTheta dc) tt_docs'))
                  -- Don't show the dictionary arguments to
                  -- constructors unless -dppr-debug is on
       }
  where
    show_tm tt_docs
      | null tt_docs = ppr dc
      | otherwise    = cparen (p >= app_prec) $
                       sep [ppr dc, nest 2 (pprDeeperList fsep tt_docs)]

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
ppr_termM1 Prim{valRaw=words, ty=ty} =
    return $ repPrim (tyConAppTyCon ty) words
ppr_termM1 Suspension{ty=ty, bound_to=Nothing} =
    return (char '_' <+> whenPprDebug (text "::" <> ppr ty))
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
    mdoc <- firstJustM mb_customDocs
    case mdoc of
      Nothing -> panic "cPprTerm"
      Just doc -> return $ cparen (prec>app_prec+1) doc

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
  , ifTerm' (isTyCon intTyCon    . ty) ppr_int
  , ifTerm' (isTyCon charTyCon   . ty) ppr_char
  , ifTerm' (isTyCon floatTyCon  . ty) ppr_float
  , ifTerm' (isTyCon doubleTyCon . ty) ppr_double
#if defined(INTEGER_GMP)
  , ifTerm' (isIntegerTy         . ty) ppr_integer
#endif
  ]
 where
   ifTerm :: (Term -> Bool)
          -> (Precedence -> Term -> m SDoc)
          -> Precedence -> Term -> m (Maybe SDoc)
   ifTerm pred f = ifTerm' pred (\prec t -> Just <$> f prec t)

   ifTerm' :: (Term -> Bool)
          -> (Precedence -> Term -> m (Maybe SDoc))
          -> Precedence -> Term -> m (Maybe SDoc)
   ifTerm' pred f prec t@Term{}
       | pred t    = f prec t
   ifTerm' _ _ _ _  = return Nothing

   isTupleTy ty    = fromMaybe False $ do
     (tc,_) <- tcSplitTyConApp_maybe ty
     return (isBoxedTupleTyCon tc)

   isTyCon a_tc ty = fromMaybe False $ do
     (tc,_) <- tcSplitTyConApp_maybe ty
     return (a_tc == tc)

   isIntegerTy ty = fromMaybe False $ do
     (tc,_) <- tcSplitTyConApp_maybe ty
     return (tyConName tc == integerTyConName)

   ppr_int, ppr_char, ppr_float, ppr_double
      :: Precedence -> Term -> m (Maybe SDoc)
   ppr_int _ Term{subTerms=[Prim{valRaw=[w]}]} =
      return (Just (Ppr.int (fromIntegral w)))
   ppr_int _ _ = return Nothing

   ppr_char _ Term{subTerms=[Prim{valRaw=[w]}]} =
      return (Just (Ppr.pprHsChar (chr (fromIntegral w))))
   ppr_char _ _ = return Nothing

   ppr_float   _ Term{subTerms=[Prim{valRaw=[w]}]} = do
      let f = unsafeDupablePerformIO $
                alloca $ \p -> poke p w >> peek (castPtr p)
      return (Just (Ppr.float f))
   ppr_float _ _ = return Nothing

   ppr_double  _ Term{subTerms=[Prim{valRaw=[w]}]} = do
      let f = unsafeDupablePerformIO $
                alloca $ \p -> poke p w >> peek (castPtr p)
      return (Just (Ppr.double f))
   -- let's assume that if we get two words, we're on a 32-bit
   -- machine. There's no good way to get a DynFlags to check the word
   -- size here.
   ppr_double  _ Term{subTerms=[Prim{valRaw=[w1,w2]}]} = do
      let f = unsafeDupablePerformIO $
                alloca $ \p -> do
                  poke p (fromIntegral w1 :: Word32)
                  poke (p `plusPtr` 4) (fromIntegral w2 :: Word32)
                  peek (castPtr p)
      return (Just (Ppr.double f))
   ppr_double _ _ = return Nothing

   ppr_integer :: Precedence -> Term -> m (Maybe SDoc)
#if defined(INTEGER_GMP)
   -- Reconstructing Integers is a bit of a pain. This depends deeply
   -- on the integer-gmp representation, so it'll break if that
   -- changes (but there are several tests in
   -- tests/ghci.debugger/scripts that will tell us if this is wrong).
   --
   --   data Integer
   --     = S# Int#
   --     | Jp# {-# UNPACK #-} !BigNat
   --     | Jn# {-# UNPACK #-} !BigNat
   --
   --   data BigNat = BN# ByteArray#
   --
   ppr_integer _ Term{subTerms=[Prim{valRaw=[W# w]}]} =
      return (Just (Ppr.integer (S# (word2Int# w))))
   ppr_integer _ Term{dc=Right con,
                      subTerms=[Term{subTerms=[Prim{valRaw=ws}]}]} = do
      -- We don't need to worry about sizes that are not an integral
      -- number of words, because luckily GMP uses arrays of words
      -- (see GMP_LIMB_SHIFT).
      let
        !(UArray _ _ _ arr#) = listArray (0,length ws-1) ws
        constr
          | "Jp#" <- occNameString (nameOccName (dataConName con)) = Jp#
          | otherwise = Jn#
      return (Just (Ppr.integer (constr (BN# arr#))))
#endif
   ppr_integer _ _ = return Nothing

   --Note pprinting of list terms is not lazy
   ppr_list :: Precedence -> Term -> m SDoc
   ppr_list p (Term{subTerms=[h,t]}) = do
       let elems      = h : getListTerms t
           isConsLast = not (termType (last elems) `eqType` termType h)
           is_string  = all (isCharTy . ty) elems
           chars = [ chr (fromIntegral w)
                   | Term{subTerms=[Prim{valRaw=[w]}]} <- elems ]

       print_elems <- mapM (y cons_prec) elems
       if is_string
        then return (Ppr.doubleQuotes (Ppr.text chars))
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
    -- Char# uses native machine words, whereas Char's Storable instance uses
    -- Int32, so we have to read it as an Int.
    | t == charPrimTyCon             = text $ show (chr (build x :: Int))
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
    | t == smallArrayPrimTyCon       = text "<smallArray>"
    | t == byteArrayPrimTyCon        = text "<bytearray>"
    | t == mutableArrayPrimTyCon     = text "<mutableArray>"
    | t == smallMutableArrayPrimTyCon = text "<smallMutableArray>"
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
  = do { (_errs, res) <- initTcInteractive hsc_env thing_inside
       ; return res }

-- | Term Reconstruction trace
traceTR :: SDoc -> TR ()
traceTR = liftTcM . traceOptTcRn Opt_D_dump_rtti


-- Semantically different to recoverM in TcRnMonad
-- recoverM retains the errors in the first action,
--  whereas recoverTc here does not
recoverTR :: TR a -> TR a -> TR a
recoverTR = tryTcDiscardingErrs

trIO :: IO a -> TR a
trIO = liftTcM . liftIO

liftTcM :: TcM a -> TR a
liftTcM = id

newVar :: Kind -> TR TcType
newVar = liftTcM . newFlexiTyVarTy

newOpenVar :: TR TcType
newOpenVar = liftTcM newOpenFlexiTyVarTy

instTyVars :: [TyVar] -> TR (TCvSubst, [TcTyVar])
-- Instantiate fresh mutable type variables from some TyVars
-- This function preserves the print-name, which helps error messages
instTyVars tvs
  = liftTcM $ fst <$> captureConstraints (newMetaTyVars tvs)

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
  = do { (subst, tvs') <- instTyVars tvs
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
      discardResult $
      captureConstraints $
      do { (ty1, ty2) <- congruenceNewtypes actual expected
         ; unifyType Nothing ty1 ty2 }
     -- TOMDO: what about the coercion?
     -- we should consider family instances


-- | Term reconstruction
--
-- Given a pointer to a heap object (`HValue`) and its type, build a `Term`
-- representation of the object. Subterms (objects in the payload) are also
-- built up to the given `max_depth`. After `max_depth` any subterms will appear
-- as `Suspension`s. Any thunks found while traversing the object will be forced
-- based on `force` parameter.
--
-- Types of terms will be refined based on constructors we find during term
-- reconstruction. See `cvReconstructType` for an overview of how type
-- reconstruction works.
--
cvObtainTerm
    :: HscEnv
    -> Int      -- ^ How many times to recurse for subterms
    -> Bool     -- ^ Force thunks
    -> RttiType -- ^ Type of the object to reconstruct
    -> ForeignHValue   -- ^ Object to reconstruct
    -> IO Term
cvObtainTerm hsc_env max_depth force old_ty hval = runTR hsc_env $ do
  -- we quantify existential tyvars as universal,
  -- as this is needed to be able to manipulate
  -- them properly
   let quant_old_ty@(old_tvs, old_tau) = quantifyType old_ty
       sigma_old_ty = mkInvForAllTys old_tvs old_tau
   traceTR (text "Term reconstruction started with initial type " <> ppr old_ty)
   term <-
     if null old_tvs
      then do
        term  <- go max_depth sigma_old_ty sigma_old_ty hval
        term' <- zonkTerm term
        return $ fixFunDictionaries $ expandNewtypes term'
      else do
              (old_ty', rev_subst) <- instScheme quant_old_ty
              my_ty <- newOpenVar
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
                                               -> newOpenVar
                                           _   -> return ty)
                                 term
                      zonkTerm zterm'
   traceTR (text "Term reconstruction completed." $$
            text "Term obtained: " <> ppr term $$
            text "Type obtained: " <> ppr (termType term))
   return term
    where
  go :: Int -> Type -> Type -> ForeignHValue -> TcM Term
   -- I believe that my_ty should not have any enclosing
   -- foralls, nor any free RuntimeUnk skolems;
   -- that is partly what the quantifyType stuff achieved
   --
   -- [SPJ May 11] I don't understand the difference between my_ty and old_ty

  go 0 my_ty _old_ty a = do
    traceTR (text "Gave up reconstructing a term after" <>
                  int max_depth <> text " steps")
    clos <- trIO $ GHCi.getClosure hsc_env a
    return (Suspension (tipe (info clos)) my_ty a Nothing)
  go !max_depth my_ty old_ty a = do
    let monomorphic = not(isTyVarTy my_ty)
    -- This ^^^ is a convention. The ancestor tests for
    -- monomorphism and passes a type instead of a tv
    clos <- trIO $ GHCi.getClosure hsc_env a
    case clos of
-- Thunks we may want to force
      t | isThunk t && force -> do
         traceTR (text "Forcing a " <> text (show (fmap (const ()) t)))
         liftIO $ GHCi.seqHValue hsc_env a
         go (pred max_depth) my_ty old_ty a
-- Blackholes are indirections iff the payload is not TSO or BLOCKING_QUEUE. If
-- the indirection is a TSO or BLOCKING_QUEUE, we return the BLACKHOLE itself as
-- the suspension so that entering it in GHCi will enter the BLACKHOLE instead
-- of entering the TSO or BLOCKING_QUEUE (which leads to runtime panic).
      BlackholeClosure{indirectee=ind} -> do
         traceTR (text "Following a BLACKHOLE")
         ind_clos <- trIO (GHCi.getClosure hsc_env ind)
         let return_bh_value = return (Suspension BLACKHOLE my_ty a Nothing)
         case ind_clos of
           -- TSO and BLOCKING_QUEUE cases
           BlockingQueueClosure{} -> return_bh_value
           OtherClosure info _ _
             | tipe info == TSO -> return_bh_value
           UnsupportedClosure info
             | tipe info == TSO -> return_bh_value
           -- Otherwise follow the indirectee
           -- (NOTE: This code will break if we support TSO in ghc-heap one day)
           _ -> go max_depth my_ty old_ty ind
-- We always follow indirections
      IndClosure{indirectee=ind} -> do
         traceTR (text "Following an indirection" )
         go max_depth my_ty old_ty ind
-- We also follow references
      MutVarClosure{var=contents}
         | Just (tycon,[world,contents_ty]) <- tcSplitTyConApp_maybe old_ty
             -> do
                  -- Deal with the MutVar# primitive
                  -- It does not have a constructor at all,
                  -- so we simulate the following one
                  -- MutVar# :: contents_ty -> MutVar# s contents_ty
         traceTR (text "Following a MutVar")
         contents_tv <- newVar liftedTypeKind
         MASSERT(isUnliftedType my_ty)
         (mutvar_ty,_) <- instScheme $ quantifyType $ mkFunTyOm
                            contents_ty (mkTyConApp tycon [world,contents_ty])
         addConstraint (mkFunTyOm contents_tv my_ty) mutvar_ty
         x <- go (pred max_depth) contents_tv contents_ty contents
         return (RefWrap my_ty x)

 -- The interesting case
      ConstrClosure{ptrArgs=pArgs,dataArgs=dArgs} -> do
        traceTR (text "entering a constructor " <> ppr dArgs <+>
                      if monomorphic
                        then parens (text "already monomorphic: " <> ppr my_ty)
                        else Ppr.empty)
        Right dcname <- liftIO $ constrClosToName hsc_env clos
        (_,mb_dc)    <- tryTc (tcLookupDataCon dcname)
        case mb_dc of
          Nothing -> do -- This can happen for private constructors compiled -O0
                        -- where the .hi descriptor does not export them
                        -- In such case, we return a best approximation:
                        --  ignore the unpointed args, and recover the pointeds
                        -- This preserves laziness, and should be safe.
                       traceTR (text "Not constructor" <+> ppr dcname)
                       let dflags = hsc_dflags hsc_env
                           tag = showPpr dflags dcname
                       vars     <- replicateM (length pArgs)
                                              (newVar liftedTypeKind)
                       subTerms <- sequence $ zipWith (\x tv ->
                           go (pred max_depth) tv tv x) pArgs vars
                       return (Term my_ty (Left ('<' : tag ++ ">")) a subTerms)
          Just dc -> do
            traceTR (text "Is constructor" <+> (ppr dc $$ ppr my_ty))
            subTtypes <- getDataConArgTys dc my_ty
            subTerms <- extractSubTerms (\ty -> go (pred max_depth) ty ty) clos subTtypes
            return (Term my_ty (Right dc) a subTerms)

      -- This is to support printing of Integers. It's not a general
      -- mechanism by any means; in particular we lose the size in
      -- bytes of the array.
      ArrWordsClosure{bytes=b, arrWords=ws} -> do
         traceTR (text "ByteArray# closure, size " <> ppr b)
         return (Term my_ty (Left "ByteArray#") a [Prim my_ty ws])

-- The otherwise case: can be a Thunk,AP,PAP,etc.
      _ -> do
         traceTR (text "Unknown closure:" <+>
                  text (show (fmap (const ()) clos)))
         return (Suspension (tipe (info clos)) my_ty a Nothing)

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

extractSubTerms :: (Type -> ForeignHValue -> TcM Term)
                -> GenClosure ForeignHValue -> [Type] -> TcM [Term]
extractSubTerms recurse clos = liftM thdOf3 . go 0 0
  where
    array = dataArgs clos

    go ptr_i arr_i [] = return (ptr_i, arr_i, [])
    go ptr_i arr_i (ty:tys)
      | Just (tc, elem_tys) <- tcSplitTyConApp_maybe ty
      , isUnboxedTupleTyCon tc
                -- See Note [Unboxed tuple RuntimeRep vars] in TyCon
      = do (ptr_i, arr_i, terms0) <-
               go ptr_i arr_i (dropRuntimeRepArgs elem_tys)
           (ptr_i, arr_i, terms1) <- go ptr_i arr_i tys
           return (ptr_i, arr_i, unboxedTupleTerm ty terms0 : terms1)
      | otherwise
      = case typePrimRepArgs ty of
          [rep_ty] ->  do
            (ptr_i, arr_i, term0)  <- go_rep ptr_i arr_i ty rep_ty
            (ptr_i, arr_i, terms1) <- go ptr_i arr_i tys
            return (ptr_i, arr_i, term0 : terms1)
          rep_tys -> do
           (ptr_i, arr_i, terms0) <- go_unary_types ptr_i arr_i rep_tys
           (ptr_i, arr_i, terms1) <- go ptr_i arr_i tys
           return (ptr_i, arr_i, unboxedTupleTerm ty terms0 : terms1)

    go_unary_types ptr_i arr_i [] = return (ptr_i, arr_i, [])
    go_unary_types ptr_i arr_i (rep_ty:rep_tys) = do
      tv <- newVar liftedTypeKind
      (ptr_i, arr_i, term0)  <- go_rep ptr_i arr_i tv rep_ty
      (ptr_i, arr_i, terms1) <- go_unary_types ptr_i arr_i rep_tys
      return (ptr_i, arr_i, term0 : terms1)

    go_rep ptr_i arr_i ty rep
      | isGcPtrRep rep = do
          t <- recurse ty $ (ptrArgs clos)!!ptr_i
          return (ptr_i + 1, arr_i, t)
      | otherwise = do
          -- This is a bit involved since we allow packing multiple fields
          -- within a single word. See also
          -- StgCmmLayout.mkVirtHeapOffsetsWithPadding
          dflags <- getDynFlags
          let word_size = wORD_SIZE dflags
              big_endian = wORDS_BIGENDIAN dflags
              size_b = primRepSizeB dflags rep
              -- Align the start offset (eg, 2-byte value should be 2-byte
              -- aligned). But not more than to a word. The offset calculation
              -- should be the same with the offset calculation in
              -- StgCmmLayout.mkVirtHeapOffsetsWithPadding.
              !aligned_idx = roundUpTo arr_i (min word_size size_b)
              !new_arr_i = aligned_idx + size_b
              ws | size_b < word_size =
                     [index size_b aligned_idx word_size big_endian]
                 | otherwise =
                     let (q, r) = size_b `quotRem` word_size
                     in ASSERT( r == 0 )
                        [ array!!i
                        | o <- [0.. q - 1]
                        , let i = (aligned_idx `quot` word_size) + o
                        ]
          return (ptr_i, new_arr_i, Prim ty ws)

    unboxedTupleTerm ty terms
      = Term ty (Right (tupleDataCon Unboxed (length terms)))
                (error "unboxedTupleTerm: no HValue for unboxed tuple") terms

    -- Extract a sub-word sized field from a word
    index item_size_b index_b word_size big_endian =
        (word .&. (mask `shiftL` moveBytes)) `shiftR` moveBytes
      where
        mask :: Word
        mask = case item_size_b of
            1 -> 0xFF
            2 -> 0xFFFF
            4 -> 0xFFFFFFFF
            _ -> panic ("Weird byte-index: " ++ show index_b)
        (q,r) = index_b `quotRem` word_size
        word = array!!q
        moveBytes = if big_endian
                    then word_size - (r + item_size_b) * 8
                    else r * 8


-- | Fast, breadth-first Type reconstruction
--
-- Given a heap object (`HValue`) and its (possibly polymorphic) type (usually
-- obtained in GHCi), try to reconstruct a more monomorphic type of the object.
-- This is used for improving type information in debugger. For example, if we
-- have a polymorphic function:
--
--     sumNumList :: Num a => [a] -> a
--     sumNumList [] = 0
--     sumNumList (x : xs) = x + sumList xs
--
-- and add a breakpoint to it:
--
--     ghci> break sumNumList
--     ghci> sumNumList ([0 .. 9] :: [Int])
--
-- ghci shows us more precise types than just `a`s:
--
--     Stopped in Main.sumNumList, debugger.hs:3:23-39
--     _result :: Int = _
--     x :: Int = 0
--     xs :: [Int] = _
--
cvReconstructType
    :: HscEnv
    -> Int       -- ^ How many times to recurse for subterms
    -> GhciType  -- ^ Type to refine
    -> ForeignHValue  -- ^ Refine the type using this value
    -> IO (Maybe Type)
cvReconstructType hsc_env max_depth old_ty hval = runTR_maybe hsc_env $ do
   traceTR (text "RTTI started with initial type " <> ppr old_ty)
   let sigma_old_ty@(old_tvs, _) = quantifyType old_ty
   new_ty <-
       if null old_tvs
        then return old_ty
        else do
          (old_ty', rev_subst) <- instScheme sigma_old_ty
          my_ty <- newOpenVar
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
  go :: Type -> ForeignHValue -> TR [(Type, ForeignHValue)]
  go my_ty a = do
    traceTR (text "go" <+> ppr my_ty)
    clos <- trIO $ GHCi.getClosure hsc_env a
    case clos of
      BlackholeClosure{indirectee=ind} -> go my_ty ind
      IndClosure{indirectee=ind} -> go my_ty ind
      MutVarClosure{var=contents} -> do
         tv'   <- newVar liftedTypeKind
         world <- newVar liftedTypeKind
         addConstraint my_ty (mkTyConApp mutVarPrimTyCon [world,tv'])
         return [(tv', contents)]
      ConstrClosure{ptrArgs=pArgs} -> do
        Right dcname <- liftIO $ constrClosToName hsc_env clos
        traceTR (text "Constr1" <+> ppr dcname)
        (_,mb_dc) <- tryTc (tcLookupDataCon dcname)
        case mb_dc of
          Nothing-> do
            forM pArgs $ \x -> do
              tv <- newVar liftedTypeKind
              return (tv, x)

          Just dc -> do
            arg_tys <- getDataConArgTys dc my_ty
            (_, itys) <- findPtrTyss 0 arg_tys
            traceTR (text "Constr2" <+> ppr dcname <+> ppr arg_tys)
            return $ zipWith (\(_,ty) x -> (ty, x)) itys pArgs
      _ -> return []

findPtrTys :: Int  -- Current pointer index
           -> Type -- Type
           -> TR (Int, [(Int, Type)])
findPtrTys i ty
  | Just (tc, elem_tys) <- tcSplitTyConApp_maybe ty
  , isUnboxedTupleTyCon tc
  = findPtrTyss i elem_tys

  | otherwise
  = case typePrimRep ty of
      [rep] | isGcPtrRep rep -> return (i + 1, [(i, ty)])
            | otherwise      -> return (i,     [])
      prim_reps              ->
        foldM (\(i, extras) prim_rep ->
                if isGcPtrRep prim_rep
                  then newVar liftedTypeKind >>= \tv -> return (i + 1, extras ++ [(i, tv)])
                  else return (i, extras))
              (i, []) prim_reps

findPtrTyss :: Int
            -> [Type]
            -> TR (Int, [(Int, Type)])
findPtrTyss i tys = foldM step (i, []) tys
  where step (i, discovered) elem_ty = do
          (i, extras) <- findPtrTys i elem_ty
          return (i, discovered ++ extras)


-- Compute the difference between a base type and the type found by RTTI
-- improveType <base_type> <rtti_type>
-- The types can contain skolem type variables, which need to be treated as normal vars.
-- In particular, we want them to unify with things.
improveRTTIType :: HscEnv -> RttiType -> RttiType -> Maybe TCvSubst
improveRTTIType _ base_ty new_ty = U.tcUnifyTyKi base_ty new_ty

getDataConArgTys :: DataCon -> Type -> TR [Type]
-- Given the result type ty of a constructor application (D a b c :: ty)
-- return the types of the arguments.  This is RTTI-land, so 'ty' might
-- not be fully known.  Moreover, the arg types might involve existentials;
-- if so, make up fresh RTTI type variables for them
--
-- I believe that con_app_ty should not have any enclosing foralls
getDataConArgTys dc con_app_ty
  = do { let rep_con_app_ty = unwrapType con_app_ty
       ; traceTR (text "getDataConArgTys 1" <+> (ppr con_app_ty $$ ppr rep_con_app_ty
                   $$ ppr (tcSplitTyConApp_maybe rep_con_app_ty)))
       ; ASSERT( all isTyVar ex_tvs ) return ()
                 -- ex_tvs can only be tyvars as data types in source
                 -- Haskell cannot mention covar yet (Aug 2018)
       ; (subst, _) <- instTyVars (univ_tvs ++ ex_tvs)
       ; addConstraint rep_con_app_ty (substTy subst (dataConOrigResTy dc))
              -- See Note [Constructor arg types]
       ; let con_arg_tys = substTys subst (map scaledThing $ dataConRepArgTys dc)
       ; traceTR (text "getDataConArgTys 2" <+> (ppr rep_con_app_ty $$ ppr con_arg_tys $$ ppr subst))
       ; return con_arg_tys }
  where
    univ_tvs = dataConUnivTyVars dc
    ex_tvs   = dataConExTyCoVars dc

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
   isHigherKind = not . null . fst . splitPiTys

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
    | Just (Scaled w1 l1,l2) <- splitFunTy_maybe l
    , Just (Scaled w2 r1,r2) <- splitFunTy_maybe r
    , w1 `eqMult` w2
    = do r2' <- go l2 r2
         r1' <- go l1 r1
         return (mkFunTy w1 r1' r2')
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
               (_, vars) <- instTyVars (tyConTyVars new_tycon)
               let ty' = mkTyConApp new_tycon (mkTyVarTys vars)
                   rep_ty = unwrapType ty'
               _ <- liftTcM (unifyType Nothing ty rep_ty)
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
-- by RuntimeUnk skolems, safely out of Meta-tyvar-land
zonkRttiType ty= do { ze <- mkEmptyZonkEnv RuntimeUnkFlexi
                    ; zonkTcTypeToTypeX ze ty }

--------------------------------------------------------------------------------
-- Restore Class predicates out of a representation type
dictsView :: Type -> Type
dictsView ty = ty


-- Use only for RTTI types
isMonomorphic :: RttiType -> Bool
isMonomorphic ty = noExistentials && noUniversals
 where (tvs, _, ty')  = tcSplitSigmaTy ty
       noExistentials = noFreeVarsOfType ty'
       noUniversals   = null tvs

-- Use only for RTTI types
isMonomorphicOnNonPhantomArgs :: RttiType -> Bool
isMonomorphicOnNonPhantomArgs ty
  | Just (tc, all_args) <- tcSplitTyConApp_maybe (unwrapType ty)
  , phantom_vars  <- tyConPhantomTyVars tc
  , concrete_args <- [ arg | (tyv,arg) <- tyConTyVars tc `zip` all_args
                           , tyv `notElem` phantom_vars]
  = all isMonomorphicOnNonPhantomArgs concrete_args
  | Just (ty1, ty2) <- splitFunTy_maybe ty
  = all isMonomorphicOnNonPhantomArgs [scaledThing ty1,ty2]
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

quantifyType ty = ( filter isTyVar $
                    tyCoVarsOfTypeWellScoped rho
                  , rho)
  where
    (_tvs, rho) = tcSplitForAllTys ty
