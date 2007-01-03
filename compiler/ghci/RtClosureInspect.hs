-----------------------------------------------------------------------------
--
-- GHC Interactive support for inspecting arbitrary closures at runtime
--
-- Pepe Iborra (supported by Google SoC) 2006
--
-----------------------------------------------------------------------------

module RtClosureInspect(
  
     cvObtainTerm,       -- :: HscEnv -> Bool -> Maybe Type -> HValue -> IO Term

     AddressEnv(..), 
     DataConEnv,
     extendAddressEnvList, 
     elemAddressEnv, 
     delFromAddressEnv, 
     emptyAddressEnv, 
     lookupAddressEnv, 

     ClosureType(..), 
     getClosureData, 
     Closure ( tipe, infoTable, ptrs, nonPtrs ), 
     getClosureType, 
     isConstr, 
     isIndirection,
     getInfoTablePtr, 

     Term(..), 
     printTerm, 
     customPrintTerm, 
     customPrintTermBase,
     termType,
     foldTerm, 
     TermFold(..), 
     idTermFold, 
     idTermFoldM,
     isFullyEvaluated, 
     isPointed,
     isFullyEvaluatedTerm,
--     unsafeDeepSeq, 
 ) where 

#include "HsVersions.h"

import ByteCodeItbls    ( StgInfoTable )
import qualified ByteCodeItbls as BCI( StgInfoTable(..) )
import ByteCodeLink     ( HValue )
import HscTypes         ( HscEnv )

import DataCon          
import Type             
import TcRnMonad        ( TcM, initTcPrintErrors, ioToTcRn, recoverM, writeMutVar )
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
import Unique
import {-#SOURCE#-} TcRnDriver ( tcRnRecoverDataCon )

import TysPrim		
import PrelNames
import TysWiredIn

import Constants        ( wORD_SIZE )
import Outputable
import Maybes
import Panic
import FiniteMap

import GHC.Arr          ( Array(..) )
import GHC.Ptr          ( Ptr(..), castPtr )
import GHC.Exts         
import GHC.Int          ( Int32(..),  Int64(..) )
import GHC.Word         ( Word32(..), Word64(..) )

import Control.Monad
import Data.Maybe
import Data.Array.Base
import Data.List        ( partition )
import Foreign.Storable

---------------------------------------------
-- * A representation of semi evaluated Terms
---------------------------------------------
{-
  A few examples in this representation:

  > Just 10 = Term Data.Maybe Data.Maybe.Just (Just 10) [Term Int I# (10) "10"]

  > (('a',_,_),_,('b',_,_)) = 
      Term ((Char,b,c),d,(Char,e,f)) (,,) (('a',_,_),_,('b',_,_))
          [ Term (Char, b, c) (,,) ('a',_,_) [Term Char C# "a", Thunk, Thunk]
          , Thunk
          , Term (Char, e, f) (,,) ('b',_,_) [Term Char C# "b", Thunk, Thunk]]
-}

data Term = Term { ty        :: Type 
                 , dc        :: DataCon 
                 , val       :: HValue 
                 , subTerms  :: [Term] }

          | Prim { ty        :: Type
                 , value     :: String }

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

instance Outputable (Term) where
 ppr = head . customPrintTerm customPrintTermBase

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
                       , infoTable    :: StgInfoTable
                       , ptrs         :: Array Int HValue
                        -- What would be the type here? HValue is ok? Should I build a Ptr?
                       , nonPtrs      :: ByteArray# 
                       }

instance Outputable ClosureType where
  ppr = text . show 

getInfoTablePtr :: a -> Ptr StgInfoTable
getInfoTablePtr x = 
    case infoPtr# x of
      itbl_ptr -> castPtr ( Ptr itbl_ptr )

getClosureType :: a -> IO ClosureType
getClosureType = liftM (readCType . BCI.tipe ) . peek . getInfoTablePtr

#include "../includes/ClosureTypes.h"

aP_CODE = AP
pAP_CODE = PAP
#undef AP
#undef PAP

getClosureData :: a -> IO Closure
getClosureData a = do
   itbl <- peek (getInfoTablePtr a)
   let tipe = readCType (BCI.tipe itbl)
   case closurePayload# a of 
     (# ptrs, nptrs #) -> 
           let elems = BCI.ptrs itbl 
               ptrsList = Array 0 (fromIntegral$ elems) ptrs
           in ptrsList `seq` return (Closure tipe itbl ptrsList nptrs)

readCType :: Integral a => a -> ClosureType
readCType i
 | i >= CONSTR && i <= CONSTR_NOCAF_STATIC = Constr
 | i >= FUN    && i <= FUN_STATIC          = Fun
 | i >= THUNK  && i < THUNK_SELECTOR       = Thunk (fromIntegral i)
 | i == THUNK_SELECTOR                     = ThunkSelector
 | i == BLACKHOLE                          = Blackhole
 | i >= IND    && i <= IND_STATIC          = Indirection (fromIntegral i)
 | fromIntegral i == aP_CODE               = AP
 | fromIntegral i == pAP_CODE              = PAP
 | otherwise                               = Other (fromIntegral i)

isConstr, isIndirection :: ClosureType -> Bool
isConstr Constr = True
isConstr    _   = False

isIndirection (Indirection _) = True
--isIndirection ThunkSelector = True
isIndirection _ = False

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
       unsafeDeepSeq1 i a b                -- 1st case avoids infinite loops for non reducible thunks
        | not (isConstr tipe) = seq a $! unsafeDeepSeq1 (i-1) a b     
     -- | unsafePerformIO (isFullyEvaluated a) = b
        | otherwise = case unsafePerformIO (getClosureData a) of
                        closure -> foldl' (flip unsafeDeepSeq) b (ptrs closure)
        where tipe = unsafePerformIO (getClosureType a)
-}
isPointed :: Type -> Bool
isPointed t | Just (t, _) <- splitTyConApp_maybe t = not$ isUnliftedTypeKind (tyConKind t)
isPointed _ = True

#define MKDECODER(offset,cons,builder) (offset, show$ cons (builder addr 0#))

extractUnboxed  :: [Type] -> ByteArray# -> [String]
extractUnboxed tt ba = helper tt (byteArrayContents# ba)
   where helper :: [Type] -> Addr# -> [String]
         helper (t:tt) addr 
          | Just ( tycon,_) <- splitTyConApp_maybe t 
          =  let (offset, txt) = decode tycon addr
                 (I# word_offset)   = offset*wORD_SIZE
             in txt : helper tt (plusAddr# addr word_offset)
          | otherwise 
          = -- ["extractUnboxed.helper: Urk. I got a " ++ showSDoc (ppr t)]
            panic$ "extractUnboxed.helper: Urk. I got a " ++ showSDoc (ppr t)
         helper [] addr = []
         decode :: TyCon -> Addr# -> (Int, String)
         decode t addr                             
           | t == charPrimTyCon   = MKDECODER(1,C#,indexCharOffAddr#)
           | t == intPrimTyCon    = MKDECODER(1,I#,indexIntOffAddr#)
           | t == wordPrimTyCon   = MKDECODER(1,W#,indexWordOffAddr#)
           | t == floatPrimTyCon  = MKDECODER(1,F#,indexFloatOffAddr#)
           | t == doublePrimTyCon = MKDECODER(2,D#,indexDoubleOffAddr#)
           | t == int32PrimTyCon  = MKDECODER(1,I32#,indexInt32OffAddr#)
           | t == word32PrimTyCon = MKDECODER(1,W32#,indexWord32OffAddr#)
           | t == int64PrimTyCon  = MKDECODER(2,I64#,indexInt64OffAddr#)
           | t == word64PrimTyCon = MKDECODER(2,W64#,indexWord64OffAddr#)
           | t == addrPrimTyCon   = MKDECODER(1,I#,(\x off-> addr2Int# (indexAddrOffAddr# x off)))  --OPT Improve the presentation of addresses
           | t == stablePtrPrimTyCon  = (1, "<stablePtr>")
           | t == stableNamePrimTyCon = (1, "<stableName>")
           | t == statePrimTyCon      = (1, "<statethread>")
           | t == realWorldTyCon      = (1, "<realworld>")
           | t == threadIdPrimTyCon   = (1, "<ThreadId>")
           | t == weakPrimTyCon       = (1, "<Weak>")
           | t == arrayPrimTyCon      = (1,"<array>")
           | t == byteArrayPrimTyCon  = (1,"<bytearray>")
           | t == mutableArrayPrimTyCon = (1, "<mutableArray>")
           | t == mutableByteArrayPrimTyCon = (1, "<mutableByteArray>")
           | t == mutVarPrimTyCon= (1, "<mutVar>")
           | t == mVarPrimTyCon  = (1, "<mVar>")
           | t == tVarPrimTyCon  = (1, "<tVar>")
           | otherwise = (1, showSDoc (char '<' <> ppr t <> char '>')) 
                 -- We cannot know the right offset in the otherwise case, so 1 is just a wild dangerous guess!
           -- TODO: Improve the offset handling in decode (make it machine dependant)

-----------------------------------
-- Boilerplate Fold code for Term
-----------------------------------

data TermFold a = TermFold { fTerm :: Type -> DataCon -> HValue -> [a] -> a
                           , fPrim :: Type -> String -> a
                           , fSuspension :: ClosureType -> Maybe Type -> HValue -> Maybe Name -> a
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

----------------------------------
-- Pretty printing of terms
----------------------------------

parensCond True  = parens
parensCond False = id
app_prec::Int
app_prec = 10

printTerm :: Term -> SDoc
printTerm Prim{value=value} = text value 
printTerm t@Term{} = printTerm1 0 t 
printTerm Suspension{bound_to=Nothing} =  char '_' -- <> ppr ct <> char '_'
printTerm Suspension{mb_ty=Just ty, bound_to=Just n} =
  parens$ ppr n <> text "::" <> ppr ty 

printTerm1 p Term{dc=dc, subTerms=tt} 
{-  | dataConIsInfix dc, (t1:t2:tt') <- tt 
  = parens (printTerm1 True t1 <+> ppr dc <+> printTerm1 True ppr t2) 
    <+> hsep (map (printTerm1 True) tt) 
-}
  | null tt   = ppr dc
  | otherwise = parensCond (p > app_prec) 
                     (ppr dc <+> sep (map (printTerm1 (app_prec+1)) tt))

  where fixity   = undefined 

printTerm1 _ t = printTerm t

customPrintTerm :: Monad m => ((Int->Term->m SDoc)->[Term->m (Maybe SDoc)]) -> Term -> m SDoc
customPrintTerm custom = let 
--  go :: Monad m => Int -> Term -> m SDoc
  go prec t@Term{subTerms=tt, dc=dc} = do
    mb_customDocs <- sequence$ sequence (custom go) t  -- Inner sequence is List monad
    case msum mb_customDocs of        -- msum is in Maybe monad
      Just doc -> return$ parensCond (prec>app_prec+1) doc
--    | dataConIsInfix dc, (t1:t2:tt') <- tt =
      Nothing  -> do pprSubterms <- mapM (go (app_prec+1)) tt
                     return$ parensCond (prec>app_prec+1) 
                                        (ppr dc <+> sep pprSubterms)
  go _ t = return$ printTerm t
  in go 0 
   where fixity = undefined 

customPrintTermBase :: Monad m => (Int->Term-> m SDoc)->[Term->m (Maybe SDoc)]
customPrintTermBase showP =
  [ 
    test isTupleDC (liftM (parens . cat . punctuate comma) . mapM (showP 0) . subTerms)
  , test (isDC consDataCon) (\Term{subTerms=[h,t]} -> doList h t)
  , test (isDC intDataCon)  (coerceShow$ \(a::Int)->a)
  , test (isDC charDataCon) (coerceShow$ \(a::Char)->a)
--  , test (isDC wordDataCon) (coerceShow$ \(a::Word)->a)
  , test (isDC floatDataCon) (coerceShow$ \(a::Float)->a)
  , test (isDC doubleDataCon) (coerceShow$ \(a::Double)->a)
  , test isIntegerDC (coerceShow$ \(a::Integer)->a)
  ] 
     where test pred f t = if pred t then liftM Just (f t) else return Nothing
           isIntegerDC Term{dc=dc} = 
              dataConName dc `elem` [ smallIntegerDataConName
                                    , largeIntegerDataConName] 
           isTupleDC Term{dc=dc}   = dc `elem` snd (unzip (elems boxedTupleArr))
           isDC a_dc Term{dc=dc}   = a_dc == dc
           coerceShow f Term{val=val} = return . text . show . f . unsafeCoerce# $ val
           --TODO pprinting of list terms is not lazy
           doList h t = do
               let elems = h : getListTerms t
                   isConsLast = isSuspension (last elems) && 
                                (mb_ty$ last elems) /= (termType h)
               init <- mapM (showP 0) (init elems) 
               last0 <- showP 0 (last elems)
               let last = case length elems of 
                            1 -> last0 
                            _ | isConsLast -> text " | " <> last0
                            _ -> comma <> last0
               return$ brackets (cat (punctuate comma init ++ [last]))

                where Just a /= Just b = not (a `coreEqType` b)
                      _      /=   _    = True
                      getListTerms Term{subTerms=[h,t]} = h : getListTerms t
                      getListTerms t@Term{subTerms=[]}  = []
                      getListTerms t@Suspension{}       = [t]
                      getListTerms t = pprPanic "getListTerms" (ppr t)

isFullyEvaluatedTerm :: Term -> Bool
isFullyEvaluatedTerm Term {subTerms=tt} = all isFullyEvaluatedTerm tt
isFullyEvaluatedTerm Suspension {}      = False
isFullyEvaluatedTerm Prim {}            = True


-----------------------------------
-- Type Reconstruction
-----------------------------------

-- The Type Reconstruction monad
type TR a = TcM a

runTR :: HscEnv -> TR Term -> IO Term
runTR hsc_env c = do 
  mb_term <- initTcPrintErrors hsc_env iNTERACTIVE (c >>= zonkTerm)
  case mb_term of 
    Nothing -> panic "Can't unify"
    Just term -> return term

trIO :: IO a -> TR a 
trIO = liftTcM . ioToTcRn

addConstraint :: TcType -> TcType -> TR ()
addConstraint t1 t2  = congruenceNewtypes t1 t2 >> unifyType t1 t2

-- A parallel fold over a Type value, replacing
-- in the right side reptypes for newtypes as found in the lhs
-- Sadly it doesn't cover all the possibilities. It does not always manage
-- to recover the highest level type. See test print016 for an example
congruenceNewtypes ::  TcType -> TcType -> TcM TcType
congruenceNewtypes lhs rhs
--    | pprTrace "Congruence" (ppr lhs $$ ppr rhs) False = undefined
 -- We have a tctyvar at the other side
    | Just tv <- getTyVar_maybe rhs 
--    , trace "congruence, entering tyvar" True
    = recoverM (return rhs) $ do  
         Indirect ty_v <- readMetaTyVar tv
         newtyped_tytv <- congruenceNewtypes lhs ty_v
         writeMutVar (metaTvRef tv) (Indirect newtyped_tytv)
         return newtyped_tytv
-- We have a function type: go on inductively
    | Just (r1,r2) <- splitFunTy_maybe rhs
    , Just (l1,l2) <- splitFunTy_maybe lhs
    = liftM2 mkFunTy ( congruenceNewtypes l1 r1)
                      (congruenceNewtypes l2 r2)
-- There is a newtype at the top level tycon and we can manage it
    | Just (tycon, args)    <- splitNewTyConApp_maybe lhs
    , isNewTyCon tycon
    , (tvs, realtipe)       <- newTyConRep tycon
    =   case tcUnifyTys (const BindMe) [realtipe] [rhs] of
          Just subst -> 
                let tvs' = substTys subst (map mkTyVarTy tvs) in
                liftM (mkTyConApp tycon) (zipWithM congruenceNewtypes args tvs')
          otherwise -> panic "congruenceNewtypes: Can't unify a newtype"
                                             
-- We have a TyconApp: go on inductively
    | Just (tycon, args)     <- splitNewTyConApp_maybe lhs
    , Just (tycon_v, args_v) <- splitNewTyConApp_maybe rhs
    = liftM (mkTyConApp tycon_v) (zipWithM congruenceNewtypes args args_v)

    | otherwise = return rhs


newVar :: Kind -> TR TcTyVar
newVar = liftTcM . newFlexiTyVar

liftTcM = id

instScheme :: Type -> TR TcType
instScheme ty = liftTcM$ liftM trd (tcInstType (liftM fst3 . tcInstTyVars) ty)
    where fst3 (x,y,z) = x
          trd  (x,y,z) = z

cvObtainTerm :: HscEnv -> Bool -> Maybe Type -> HValue -> IO Term
cvObtainTerm hsc_env force mb_ty a = 
 -- Obtain the term and tidy the type before returning it
     cvObtainTerm1 hsc_env force mb_ty a >>= return . tidyTypes 
   where 
         tidyTypes = foldTerm idTermFold {
            fTerm = \ty dc hval tt -> Term (tidy ty) dc hval tt,
            fSuspension = \ct mb_ty hval n -> 
                          Suspension ct (fmap tidy mb_ty) hval n
            }
         tidy ty = tidyType (emptyTidyOccEnv, tidyVarEnv ty) ty  
         tidyVarEnv ty = 
             mkVarEnv$ [ (v, setTyVarName v (tyVarName tv))
                         | (tv,v) <- zip alphaTyVars vars]
             where vars = varSetElems$ tyVarsOfType ty

cvObtainTerm1 :: HscEnv -> Bool -> Maybe Type -> HValue -> IO Term
cvObtainTerm1 hsc_env force mb_ty hval
  | Nothing <- mb_ty = runTR hsc_env . go argTypeKind $ hval
  | Just ty <- mb_ty = runTR hsc_env $ do
                 term <- go argTypeKind hval
                 ty'  <- instScheme ty
                 addConstraint ty' (fromMaybe (error "by definition") 
                                              (termType term)) 
                 return term
    where 
  go k a = do 
    ctype <- trIO$ getClosureType a
    case ctype of
-- Thunks we may want to force
      Thunk _ | force -> seq a $ go k a
-- We always follow indirections 
      _       | isIndirection ctype 
                      -> do
        clos   <- trIO$ getClosureData a
--      dflags <- getSessionDynFlags session
--      debugTraceMsg dflags 2 (text "Following an indirection")
        go k $! (ptrs clos ! 0)
 -- The interesting case
      Constr -> do
        m_dc <- trIO$ tcRnRecoverDataCon hsc_env a
        case m_dc of
          Nothing -> panic "Can't find the DataCon for a term"
          Just dc -> do 
            clos          <- trIO$ getClosureData a
            let extra_args = length(dataConRepArgTys dc) - length(dataConOrigArgTys dc)
                subTtypes  = drop extra_args (dataConRepArgTys dc)
                (subTtypesP, subTtypesNP) = partition isPointed subTtypes
                
            subTermsP <- mapM (\i->extractSubterm i (ptrs clos)
                                                    (subTtypesP!!(i-extra_args)))
                              [extra_args..extra_args + length subTtypesP - 1]
            let unboxeds   = extractUnboxed subTtypesNP (nonPtrs clos)
                subTermsNP = map (uncurry Prim) (zip subTtypesNP unboxeds)      
                subTerms   = reOrderTerms subTermsP subTermsNP subTtypes
            resType       <- liftM mkTyVarTy (newVar k)
            baseType      <- instScheme (dataConRepType dc)
            let myType     = mkFunTys (map (fromMaybe undefined . termType) 
                                       subTerms) 
                                  resType
            addConstraint baseType myType
            return (Term resType dc a subTerms)
-- The otherwise case: can be a Thunk,AP,PAP,etc.
      otherwise -> do
         x <- liftM mkTyVarTy (newVar k)
         return (Suspension ctype (Just x) a Nothing)

-- Access the array of pointers and recurse down. Needs to be done with
-- care of no introducing a thunk! or go will fail to do its job 
  extractSubterm (I# i#) ptrs ty = case ptrs of 
                 (Array _ _ ptrs#) -> case indexArray# ptrs# i# of 
                       (# e #) -> go (typeKind ty) e

-- This is used to put together pointed and nonpointed subterms in the 
--  correct order.
  reOrderTerms _ _ [] = []
  reOrderTerms pointed unpointed (ty:tys) 
   | isPointed ty = head pointed : reOrderTerms (tail pointed) unpointed tys
   | otherwise    = head unpointed : reOrderTerms pointed (tail unpointed) tys

zonkTerm :: Term -> TcM Term
zonkTerm = foldTerm idTermFoldM {
              fTerm = \ty dc v tt -> sequence tt      >>= \tt ->
                                     zonkTcType ty    >>= \ty' ->
                                     return (Term ty' dc v tt)
             ,fSuspension = \ct ty v b -> fmapMMaybe zonkTcType ty >>= \ty ->
                                          return (Suspension ct ty v b)}  

{-
Example of Type Reconstruction
--------------------------------
Suppose we have an existential type such as

data Opaque = forall a. Opaque a

And we have a term built as:

t = Opaque (map Just [[1,1],[2,2]])

The type of t as far as the typechecker goes is t :: Opaque
If we seq the head of t, we obtain:

t - O (_1::a) 

seq _1 ()

t - O ( (_3::b) : (_4::[b]) ) 

seq _3 ()

t - O ( (Just (_5::c)) : (_4::[b]) ) 

At this point, we know that b = (Maybe c)

seq _5 ()

t - O ( (Just ((_6::d) : (_7::[d]) )) : (_4::[b]) )

At this point, we know that c = [d]

seq _6 ()

t - O ( (Just (1 : (_7::[d]) )) : (_4::[b]) )

At this point, we know that d = Integer

The fully reconstructed expressions, with propagation, would be:

t - O ( (Just (_5::c)) : (_4::[Maybe c]) ) 
t - O ( (Just ((_6::d) : (_7::[d]) )) : (_4::[Maybe [d]]) )
t - O ( (Just (1 : (_7::[Integer]) )) : (_4::[Maybe [Integer]]) )


For reference, the type of the thing inside the opaque is 
map Just [[1,1],[2,2]] :: [Maybe [Integer]]

NOTE: (Num t) contexts have been manually replaced by Integer for clarity
-}

--------------------------------------------------------------------
-- The DataConEnv is used to store the addresses of datacons loaded
-- via the dynamic linker
--------------------------------------------------------------------

type DataConEnv   = AddressEnv StgInfoTable

-- Note that this AddressEnv and DataConEnv I wrote trying to follow 
-- conventions in ghc, but probably they make not much sense.

newtype AddressEnv a = AE {aenv:: FiniteMap (Ptr a) Name}
  deriving (Outputable)

emptyAddressEnv = AE emptyFM

extendAddressEnvList  :: AddressEnv a -> [(Ptr a, Name)] -> AddressEnv a
elemAddressEnv        :: Ptr a -> AddressEnv a -> Bool
delFromAddressEnv     :: AddressEnv a -> Ptr a -> AddressEnv a
nullAddressEnv        :: AddressEnv a -> Bool
lookupAddressEnv       :: AddressEnv a -> Ptr a -> Maybe Name

extendAddressEnvList  (AE env) = AE . addListToFM env 
elemAddressEnv   ptr  (AE env) = ptr `elemFM` env
delFromAddressEnv     (AE env) = AE . delFromFM env
nullAddressEnv                 = isEmptyFM . aenv
lookupAddressEnv      (AE env) = lookupFM env


instance Outputable (Ptr a) where
  ppr = text . show
