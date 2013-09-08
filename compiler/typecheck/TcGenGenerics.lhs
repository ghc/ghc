%
% (c) The University of Glasgow 2011
%

The deriving code for the Generic class
(equivalent to the code in TcGenDeriv, for other classes)

\begin{code}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details


module TcGenGenerics (canDoGenerics, canDoGenerics1,
                      GenericKind(..),
                      MetaTyCons, genGenericMetaTyCons,
                      gen_Generic_binds, get_gen1_constrained_tys) where

import DynFlags
import HsSyn
import Type
import TcType
import TcGenDeriv
import DataCon
import TyCon
import FamInstEnv       ( FamInst, FamFlavor(..), mkSingleCoAxiom )
import FamInst
import Module           ( Module, moduleName, moduleNameString )
import IfaceEnv         ( newGlobalBinder )
import Name      hiding ( varName )
import RdrName
import BasicTypes
import TysWiredIn
import PrelNames
import InstEnv
import TcEnv
import MkId
import TcRnMonad
import HscTypes
import BuildTyCl
import SrcLoc
import Bag
import VarSet (elemVarSet)
import Outputable 
import FastString
import Util

import Control.Monad (mplus,forM)
import qualified State as S

#include "HsVersions.h"
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Bindings for the new generic deriving mechanism}
%*                                                                      *
%************************************************************************

For the generic representation we need to generate:
\begin{itemize}
\item A Generic instance
\item A Rep type instance 
\item Many auxiliary datatypes and instances for them (for the meta-information)
\end{itemize}

\begin{code}
gen_Generic_binds :: GenericKind -> TyCon -> MetaTyCons -> Module
                 -> TcM (LHsBinds RdrName, FamInst)
gen_Generic_binds gk tc metaTyCons mod = do
  repTyInsts <- tc_mkRepFamInsts gk tc metaTyCons mod
  return (mkBindsRep gk tc, repTyInsts)

genGenericMetaTyCons :: TyCon -> Module -> TcM (MetaTyCons, BagDerivStuff)
genGenericMetaTyCons tc mod =
  do  loc <- getSrcSpanM
      let
        tc_name   = tyConName tc
        tc_cons   = tyConDataCons tc
        tc_arits  = map dataConSourceArity tc_cons

        tc_occ    = nameOccName tc_name
        d_occ     = mkGenD tc_occ
        c_occ m   = mkGenC tc_occ m
        s_occ m n = mkGenS tc_occ m n

        mkTyCon name = ASSERT( isExternalName name )
                       buildAlgTyCon name [] [] Nothing [] distinctAbstractTyConRhs
                                          NonRecursive 
                                          NotPromotable
                                          False          -- Not GADT syntax
                                          NoParentTyCon

      d_name  <- newGlobalBinder mod d_occ loc
      c_names <- forM (zip [0..] tc_cons) $ \(m,_) ->
                    newGlobalBinder mod (c_occ m) loc
      s_names <- forM (zip [0..] tc_arits) $ \(m,a) -> forM [0..a-1] $ \n ->
                    newGlobalBinder mod (s_occ m n) loc

      let metaDTyCon  = mkTyCon d_name
          metaCTyCons = map mkTyCon c_names
          metaSTyCons = map (map mkTyCon) s_names

          metaDts = MetaTyCons metaDTyCon metaCTyCons metaSTyCons

      -- pprTrace "rep0" (ppr rep0_tycon) $
      (,) metaDts `fmap` metaTyConsToDerivStuff tc metaDts

-- both the tycon declarations and related instances
metaTyConsToDerivStuff :: TyCon -> MetaTyCons -> TcM BagDerivStuff
metaTyConsToDerivStuff tc metaDts =
  do  loc <- getSrcSpanM
      dflags <- getDynFlags
      dClas <- tcLookupClass datatypeClassName
      let new_dfun_name clas tycon = newDFunName clas [mkTyConApp tycon []] loc
      d_dfun_name <- new_dfun_name dClas tc
      cClas <- tcLookupClass constructorClassName
      c_dfun_names <- sequence [ new_dfun_name cClas tc | _ <- metaC metaDts ]
      sClas <- tcLookupClass selectorClassName
      s_dfun_names <- sequence (map sequence [ [ new_dfun_name sClas tc 
                                               | _ <- x ] 
                                             | x <- metaS metaDts ])
      fix_env <- getFixityEnv

      let
        safeOverlap = safeLanguageOn dflags
        (dBinds,cBinds,sBinds) = mkBindsMetaD fix_env tc
        mk_inst clas tc dfun_name 
          = mkLocalInstance (mkDictFunId dfun_name [] [] clas tys)
                            (NoOverlap safeOverlap)
                            [] clas tys
          where
            tys = [mkTyConTy tc]
        
        -- Datatype
        d_metaTycon = metaD metaDts
        d_inst   = mk_inst dClas d_metaTycon d_dfun_name
        d_binds  = VanillaInst dBinds [] False
        d_mkInst = DerivInst (InstInfo { iSpec = d_inst, iBinds = d_binds })
        
        -- Constructor
        c_metaTycons = metaC metaDts
        c_insts = [ mk_inst cClas c ds
                  | (c, ds) <- myZip1 c_metaTycons c_dfun_names ]
        c_binds = [ VanillaInst c [] False | c <- cBinds ]
        c_mkInst = [ DerivInst (InstInfo { iSpec = is, iBinds = bs })
                   | (is,bs) <- myZip1 c_insts c_binds ]
        
        -- Selector
        s_metaTycons = metaS metaDts
        s_insts = map (map (\(s,ds) -> mk_inst sClas s ds))
                      (myZip2 s_metaTycons s_dfun_names)
        s_binds = [ [ VanillaInst s [] False | s <- ss ] | ss <- sBinds ]
        s_mkInst = map (map (\(is,bs) -> DerivInst (InstInfo { iSpec  = is
                                                             , iBinds = bs})))
                       (myZip2 s_insts s_binds)
       
        myZip1 :: [a] -> [b] -> [(a,b)]
        myZip1 l1 l2 = ASSERT(length l1 == length l2) zip l1 l2
        
        myZip2 :: [[a]] -> [[b]] -> [[(a,b)]]
        myZip2 l1 l2 =
          ASSERT(and (zipWith (>=) (map length l1) (map length l2)))
            [ zip x1 x2 | (x1,x2) <- zip l1 l2 ]
        
      return $ mapBag DerivTyCon (metaTyCons2TyCons metaDts)
               `unionBags` listToBag (d_mkInst : c_mkInst ++ concat s_mkInst)
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Generating representation types}
%*                                                                      *
%************************************************************************

\begin{code}
get_gen1_constrained_tys :: TyVar -> [Type] -> [Type]
-- called by TcDeriv.inferConstraints; generates a list of types, each of which
-- must be a Functor in order for the Generic1 instance to work.
get_gen1_constrained_tys argVar =
  concatMap $ argTyFold argVar $ ArgTyAlg {
    ata_rec0 = const [],
    ata_par1 = [], ata_rec1 = const [],
    ata_comp = (:)}



canDoGenerics :: TyCon -> [Type] -> Maybe SDoc
-- Called on source-code data types, to see if we should generate
-- generic functions for them.
-- Nothing == yes
-- Just s  == no, because of `s`

canDoGenerics tc tc_args
  = mergeErrors (
          -- We do not support datatypes with context
              (if (not (null (tyConStupidTheta tc)))
                then (Just (tc_name <+> text "must not have a datatype context"))
                else Nothing) :
          -- The type arguments should not be instantiated (see #5939)
          -- Data family indices can be instantiated; the `tc_args` here are the
          -- representation tycon args
              (if (all isTyVarTy (filterOut isKindTy tc_args))
                then Nothing
                else Just (tc_name <+> text "must not be instantiated;" <+>
                           text "try deriving `" <> tc_name <+> tc_tys <>
                           text "' instead"))
          -- See comment below
            : (map bad_con (tyConDataCons tc)))
  where
    -- The tc can be a representation tycon. When we want to display it to the
    -- user (in an error message) we should print its parent
    (tc_name, tc_tys) = case tyConParent tc of
        FamInstTyCon _ ptc tys -> (ppr ptc, hsep (map ppr
                                            (tys ++ drop (length tys) tc_args)))
        _                      -> (ppr tc, hsep (map ppr (tyConTyVars tc)))

        -- If any of the constructor has an unboxed type as argument,
        -- then we can't build the embedding-projection pair, because
        -- it relies on instantiating *polymorphic* sum and product types
        -- at the argument types of the constructors
    bad_con dc = if (any bad_arg_type (dataConOrigArgTys dc))
                  then (Just (ppr dc <+> text "must not have unlifted or polymorphic arguments"))
                  else (if (not (isVanillaDataCon dc))
                          then (Just (ppr dc <+> text "must be a vanilla data constructor"))
                          else Nothing)

	-- Nor can we do the job if it's an existential data constructor,
	-- Nor if the args are polymorphic types (I don't think)
    bad_arg_type ty = isUnLiftedType ty || not (isTauTy ty)

mergeErrors :: [Maybe SDoc] -> Maybe SDoc
mergeErrors []           = Nothing
mergeErrors ((Just s):t) = case mergeErrors t of
  Nothing -> Just s
  Just s' -> Just (s <> text ", and" $$ s')
mergeErrors (Nothing :t) = mergeErrors t

canDoGenerics1 :: TyCon -> [Type] -> Maybe SDoc
-- Called on source-code data types, to see if we should generate
-- generic functions for them.
-- Nothing == yes
-- Just s  == no, because of `s`

-- (derived from TcDeriv.cond_functorOK; also checks canDoGenerics)

-- OK for Generic1/Rep1
-- Currently: (a) at least one argument
--            (b) don't use argument contravariantly
--            (c) don't use argument in the wrong place, e.g. data T a = T (X a a)
--            (d) no "stupid context" on data type
--            (e) not instantiated (except for data family indices)
canDoGenerics1 tc tc_args = canDoGenerics tc tc_args
                              `mplus` S.evalState (canDoGenerics1_w tc) []

-- the state is which tycons we have entered; it avoids divergence when we
-- recur (robust against mutual recursion)
canDoGenerics1_w :: TyCon -> S.State [Name] (Maybe SDoc)
canDoGenerics1_w rep_tc
  | null tc_tvs
  = return $ Just (ptext (sLit "Data type") <+> quotes (ppr rep_tc)
          <+> ptext (sLit "must have some type parameters"))

  | not (null bad_stupid_theta)
  = return $ Just (ptext (sLit "Data type") <+> quotes (ppr rep_tc)
          <+> ptext (sLit "must not have a class context") <+> pprTheta bad_stupid_theta)

  | otherwise
  = (mergeErrors . concat) `fmap` mapM check_con data_cons
  where
    tc_tvs            = tyConTyVars rep_tc
    Just (_, last_tv) = snocView tc_tvs
    bad_stupid_theta  = filter is_bad (tyConStupidTheta rep_tc)
    is_bad pred       = last_tv `elemVarSet` tyVarsOfType pred

    data_cons = tyConDataCons rep_tc
    check_con con = case check_vanilla con of
      j@(Just _) -> return [j]
      Nothing -> mapM snd $ foldDataConArgs (ft_check con) con

    bad :: DataCon -> SDoc -> SDoc
    bad con msg = ptext (sLit "Constructor") <+> quotes (ppr con) <+> msg

    check_vanilla :: DataCon -> Maybe SDoc
    check_vanilla con | isVanillaDataCon con = Nothing
                      | otherwise            = Just (bad con existential)

    -- the Bool is if the parameter occurs in the type
    ft_check :: DataCon -> FFoldType (Bool, S.State [Name] (Maybe SDoc))
    ft_check con = FT { ft_triv = bmzero, ft_var = (True, return Nothing)
                      , ft_co_var = (True, return $ Just $ bad con covariant)
                        -- NB foldDataConArgs caters to Functor/Foldable/etc,
                        -- which treat applications of functions and tuples
                        -- specially. But we just treat them like normal
                        -- applications, so we must compensate with extra logic
                        -- to ensure that the variable only occurs as the last
                        -- argument.
                      , ft_fun = \x y -> if fst x then (True, return $ Just $ bad con wrong_arg)
                                         else x `bmplus` y
                      , ft_tup = \_ xs ->
                          if not (null xs) && any fst (init xs)
                          then (True, return $ Just $ bad con wrong_arg)
                          else foldr bmplus bmzero xs
                      , ft_ty_app = \ty x -> bmplus x $ (,) False $
                          if fst x then representable ty else return Nothing
                      , ft_bad_app = (True, return $ Just $ bad con wrong_arg)
                      , ft_forall = \_ -> id }

    bmzero = (False, return Nothing)
    bmplus (b1, m1) (b2, m2) = (b1 || b2, m1 >>= maybe m2 (return . Just))

    representable :: Type -> S.State [Name] (Maybe SDoc)
    representable ty = case tcSplitTyConApp_maybe ty of
      Nothing -> return Nothing
      -- if it's a type constructor, it has to be representable
      Just (tc, _) -> do
        let n = tyConName tc
        s <- S.get
        -- internally assume that recursive occurrences are OK
        if n `elem` s then return Nothing else do
          S.put (n : s)
          fmap {-maybe-} (\_ -> bad_app tc) -- don't give the message, just
                                            -- name what wasn't representable
            `fmap` {-state-} canDoGenerics1_w tc

    existential = (ptext . sLit) "must not have existential arguments"
    covariant   = (ptext . sLit) "must not use the last type parameter in a function argument"
    wrong_arg   = (ptext . sLit) "must use the last type parameter only as the last argument of a data type, newtype, or (->)"
    bad_app tc  = (ptext . sLit) "must not apply type constructors that cannot be represented with `Rep1' (such as `" <> ppr (tyConName tc)
                  <> (ptext . sLit) "') to arguments that involve the last type parameter"

\end{code}

%************************************************************************
%*									*
\subsection{Generating the RHS of a generic default method}
%*									*
%************************************************************************

\begin{code}
type US = Int	-- Local unique supply, just a plain Int
type Alt = (LPat RdrName, LHsExpr RdrName)

-- GenericKind serves to mark if a datatype derives Generic (Gen0) or
-- Generic1 (Gen1).
data GenericKind = Gen0 | Gen1

-- as above, but with a payload of the TyCon's name for "the" parameter
data GenericKind_ = Gen0_ | Gen1_ TyVar

-- as above, but using a single datacon's name for "the" parameter
data GenericKind_DC = Gen0_DC | Gen1_DC TyVar

forgetArgVar :: GenericKind_DC -> GenericKind
forgetArgVar Gen0_DC   = Gen0
forgetArgVar Gen1_DC{} = Gen1

-- When working only within a single datacon, "the" parameter's name should
-- match that datacon's name for it.
gk2gkDC :: GenericKind_ -> DataCon -> GenericKind_DC
gk2gkDC Gen0_   _ = Gen0_DC
gk2gkDC Gen1_{} d = Gen1_DC $ last $ dataConUnivTyVars d



-- Bindings for the Generic instance
mkBindsRep :: GenericKind -> TyCon -> LHsBinds RdrName
mkBindsRep gk tycon = 
    unitBag (L loc (mkFunBind (L loc from01_RDR) from_matches))
  `unionBags`
    unitBag (L loc (mkFunBind (L loc to01_RDR) to_matches))
      where
        from_matches  = [mkSimpleHsAlt pat rhs | (pat,rhs) <- from_alts]
        to_matches    = [mkSimpleHsAlt pat rhs | (pat,rhs) <- to_alts  ]
        loc           = srcLocSpan (getSrcLoc tycon)
        datacons      = tyConDataCons tycon

        (from01_RDR, to01_RDR) = case gk of
                                   Gen0 -> (from_RDR,  to_RDR)
                                   Gen1 -> (from1_RDR, to1_RDR)

        -- Recurse over the sum first
        from_alts, to_alts :: [Alt]
        (from_alts, to_alts) = mkSum gk_ (1 :: US) tycon datacons
          where gk_ = case gk of
                  Gen0 -> Gen0_
                  Gen1 -> ASSERT(length tyvars >= 1)
                          Gen1_ (last tyvars)
                    where tyvars = tyConTyVars tycon
        
--------------------------------------------------------------------------------
-- The type synonym instance and synonym
--       type instance Rep (D a b) = Rep_D a b
--       type Rep_D a b = ...representation type for D ...
--------------------------------------------------------------------------------

tc_mkRepFamInsts :: GenericKind     -- Gen0 or Gen1
               -> TyCon           -- The type to generate representation for
               -> MetaTyCons      -- Metadata datatypes to refer to
               -> Module          -- Used as the location of the new RepTy
               -> TcM (FamInst)   -- Generated representation0 coercion
tc_mkRepFamInsts gk tycon metaDts mod = 
       -- Consider the example input tycon `D`, where data D a b = D_ a
       -- Also consider `R:DInt`, where { data family D x y :: * -> *
       --                               ; data instance D Int a b = D_ a }
  do { -- `rep` = GHC.Generics.Rep or GHC.Generics.Rep1 (type family)
       fam_tc <- case gk of
         Gen0 -> tcLookupTyCon repTyConName
         Gen1 -> tcLookupTyCon rep1TyConName

     ; let -- `tyvars` = [a,b]
           (tyvars, gk_) = case gk of
             Gen0 -> (all_tyvars, Gen0_)
             Gen1 -> ASSERT(not $ null all_tyvars)
                     (init all_tyvars, Gen1_ $ last all_tyvars)
             where all_tyvars = tyConTyVars tycon

           tyvar_args = mkTyVarTys tyvars

           appT :: [Type]
           appT = case tyConFamInst_maybe tycon of
                     -- `appT` = D Int a b (data families case)
                     Just (famtycon, apps) ->
                       -- `fam` = D
                       -- `apps` = [Int, a]
                       let allApps = apps ++
                                     drop (length apps + length tyvars
                                           - tyConArity famtycon) tyvar_args
                       in [mkTyConApp famtycon allApps]
                     -- `appT` = D a b (normal case)
                     Nothing -> [mkTyConApp tycon tyvar_args]

       -- `repTy` = D1 ... (C1 ... (S1 ... (Rec0 a))) :: * -> *
     ; repTy <- tc_mkRepTy gk_ tycon metaDts
    
       -- `rep_name` is a name we generate for the synonym
     ; rep_name <- let mkGen = case gk of Gen0 -> mkGenR; Gen1 -> mkGen1R
                   in newGlobalBinder mod (mkGen (nameOccName (tyConName tycon)))
                        (nameSrcSpan (tyConName tycon))

     ; let axiom = mkSingleCoAxiom rep_name tyvars fam_tc appT repTy
     ; newFamInst SynFamilyInst axiom  }

--------------------------------------------------------------------------------
-- Type representation
--------------------------------------------------------------------------------

-- | See documentation of 'argTyFold'; that function uses the fields of this
-- type to interpret the structure of a type when that type is considered as an
-- argument to a constructor that is being represented with 'Rep1'.
data ArgTyAlg a = ArgTyAlg
  { ata_rec0 :: (Type -> a)
  , ata_par1 :: a, ata_rec1 :: (Type -> a)
  , ata_comp :: (Type -> a -> a)
  }

-- | @argTyFold@ implements a generalised and safer variant of the @arg@
-- function from Figure 3 in <http://dreixel.net/research/pdf/gdmh.pdf>. @arg@
-- is conceptually equivalent to:
--
-- > arg t = case t of
-- >   _ | isTyVar t         -> if (t == argVar) then Par1 else Par0 t
-- >   App f [t'] |
--       representable1 f &&
--       t' == argVar        -> Rec1 f
-- >   App f [t'] |
--       representable1 f &&
--       t' has tyvars       -> f :.: (arg t')
-- >   _                     -> Rec0 t
--
-- where @argVar@ is the last type variable in the data type declaration we are
-- finding the representation for.
--
-- @argTyFold@ is more general than @arg@ because it uses 'ArgTyAlg' to
-- abstract out the concrete invocations of @Par0@, @Rec0@, @Par1@, @Rec1@, and
-- @:.:@.
--
-- @argTyFold@ is safer than @arg@ because @arg@ would lead to a GHC panic for
-- some data types. The problematic case is when @t@ is an application of a
-- non-representable type @f@ to @argVar@: @App f [argVar]@ is caught by the
-- @_@ pattern, and ends up represented as @Rec0 t@. This type occurs /free/ in
-- the RHS of the eventual @Rep1@ instance, which is therefore ill-formed. Some
-- representable1 checks have been relaxed, and others were moved to
-- @canDoGenerics1@.
argTyFold :: forall a. TyVar -> ArgTyAlg a -> Type -> a
argTyFold argVar (ArgTyAlg {ata_rec0 = mkRec0,
                            ata_par1 = mkPar1, ata_rec1 = mkRec1,
                            ata_comp = mkComp}) =
  -- mkRec0 is the default; use it if there is no interesting structure
  -- (e.g. occurrences of parameters or recursive occurrences)
  \t -> maybe (mkRec0 t) id $ go t where
  go :: Type -> -- type to fold through
        Maybe a -- the result (e.g. representation type), unless it's trivial
  go t = isParam `mplus` isApp where

    isParam = do -- handles parameters
      t' <- getTyVar_maybe t
      Just $ if t' == argVar then mkPar1 -- moreover, it is "the" parameter
             else mkRec0 t -- NB mkRec0 instead of the conventional mkPar0

    isApp = do -- handles applications
      (phi, beta) <- tcSplitAppTy_maybe t

      let interesting = argVar `elemVarSet` exactTyVarsOfType beta

      -- Does it have no interesting structure to represent?
      if not interesting then Nothing
        else -- Is the argument the parameter? Special case for mkRec1.
          if Just argVar == getTyVar_maybe beta then Just $ mkRec1 phi
            else mkComp phi `fmap` go beta -- It must be a composition.


tc_mkRepTy ::  -- Gen0_ or Gen1_, for Rep or Rep1
               GenericKind_
              -- The type to generate representation for
            -> TyCon
               -- Metadata datatypes to refer to
            -> MetaTyCons 
               -- Generated representation0 type
            -> TcM Type
tc_mkRepTy gk_ tycon metaDts = 
  do
    d1    <- tcLookupTyCon d1TyConName
    c1    <- tcLookupTyCon c1TyConName
    s1    <- tcLookupTyCon s1TyConName
    nS1   <- tcLookupTyCon noSelTyConName
    rec0  <- tcLookupTyCon rec0TyConName
    rec1  <- tcLookupTyCon rec1TyConName
    par1  <- tcLookupTyCon par1TyConName
    u1    <- tcLookupTyCon u1TyConName
    v1    <- tcLookupTyCon v1TyConName
    plus  <- tcLookupTyCon sumTyConName
    times <- tcLookupTyCon prodTyConName
    comp  <- tcLookupTyCon compTyConName
    
    let mkSum' a b = mkTyConApp plus  [a,b]
        mkProd a b = mkTyConApp times [a,b]
        mkComp a b = mkTyConApp comp  [a,b]
        mkRec0 a   = mkTyConApp rec0  [a]
        mkRec1 a   = mkTyConApp rec1  [a]
        mkPar1     = mkTyConTy  par1
        mkD    a   = mkTyConApp d1    [metaDTyCon, sumP (tyConDataCons a)]
        mkC  i d a = mkTyConApp c1    [d, prod i (dataConInstOrigArgTys a $ mkTyVarTys $ tyConTyVars tycon)
                                                 (null (dataConFieldLabels a))]
        -- This field has no label
        mkS True  _ a = mkTyConApp s1 [mkTyConTy nS1, a]
        -- This field has a  label
        mkS False d a = mkTyConApp s1 [d, a]
        
        -- Sums and products are done in the same way for both Rep and Rep1
        sumP [] = mkTyConTy v1
        sumP l  = ASSERT(length metaCTyCons == length l)
                    foldBal mkSum' [ mkC i d a
                                   | (d,(a,i)) <- zip metaCTyCons (zip l [0..])]
        -- The Bool is True if this constructor has labelled fields
        prod :: Int -> [Type] -> Bool -> Type
        prod i [] _ = ASSERT(length metaSTyCons > i)
                        ASSERT(length (metaSTyCons !! i) == 0)
                          mkTyConTy u1
        prod i l b  = ASSERT(length metaSTyCons > i)
                        ASSERT(length l == length (metaSTyCons !! i))
                          foldBal mkProd [ arg d t b
                                         | (d,t) <- zip (metaSTyCons !! i) l ]
        
        arg :: Type -> Type -> Bool -> Type
        arg d t b = mkS b d $ case gk_ of 
            -- Here we previously used Par0 if t was a type variable, but we
            -- realized that we can't always guarantee that we are wrapping-up
            -- all type variables in Par0. So we decided to stop using Par0
            -- altogether, and use Rec0 all the time.
                      Gen0_        -> mkRec0 t
                      Gen1_ argVar -> argPar argVar t
          where
            -- Builds argument represention for Rep1 (more complicated due to
            -- the presence of composition).
            argPar argVar = argTyFold argVar $ ArgTyAlg
              {ata_rec0 = mkRec0, ata_par1 = mkPar1,
               ata_rec1 = mkRec1, ata_comp = mkComp}
        
       
        metaDTyCon  = mkTyConTy (metaD metaDts)
        metaCTyCons = map mkTyConTy (metaC metaDts)
        metaSTyCons = map (map mkTyConTy) (metaS metaDts)
        
    return (mkD tycon)

--------------------------------------------------------------------------------
-- Meta-information
--------------------------------------------------------------------------------

data MetaTyCons = MetaTyCons { -- One meta datatype per dataype
                               metaD :: TyCon
                               -- One meta datatype per constructor
                             , metaC :: [TyCon]
                               -- One meta datatype per selector per constructor
                             , metaS :: [[TyCon]] }
                             
instance Outputable MetaTyCons where
  ppr (MetaTyCons d c s) = ppr d $$ vcat (map ppr c) $$ vcat (map ppr (concat s))
                                   
metaTyCons2TyCons :: MetaTyCons -> Bag TyCon
metaTyCons2TyCons (MetaTyCons d c s) = listToBag (d : c ++ concat s)


-- Bindings for Datatype, Constructor, and Selector instances
mkBindsMetaD :: FixityEnv -> TyCon 
             -> ( LHsBinds RdrName      -- Datatype instance
                , [LHsBinds RdrName]    -- Constructor instances
                , [[LHsBinds RdrName]]) -- Selector instances
mkBindsMetaD fix_env tycon = (dtBinds, allConBinds, allSelBinds)
      where
        mkBag l = foldr1 unionBags 
                    [ unitBag (L loc (mkFunBind (L loc name) matches)) 
                        | (name, matches) <- l ]
        dtBinds       = mkBag ( [ (datatypeName_RDR, dtName_matches)
                                , (moduleName_RDR, moduleName_matches)]
                              ++ ifElseEmpty (isNewTyCon tycon)
                                [ (isNewtypeName_RDR, isNewtype_matches) ] )

        allConBinds   = map conBinds datacons
        conBinds c    = mkBag ( [ (conName_RDR, conName_matches c)]
                              ++ ifElseEmpty (dataConIsInfix c)
                                   [ (conFixity_RDR, conFixity_matches c) ]
                              ++ ifElseEmpty (length (dataConFieldLabels c) > 0)
                                   [ (conIsRecord_RDR, conIsRecord_matches c) ]
                              )

        ifElseEmpty p x = if p then x else []
        fixity c      = case lookupFixity fix_env (dataConName c) of
                          Fixity n InfixL -> buildFix n leftAssocDataCon_RDR
                          Fixity n InfixR -> buildFix n rightAssocDataCon_RDR
                          Fixity n InfixN -> buildFix n notAssocDataCon_RDR
        buildFix n assoc = nlHsApps infixDataCon_RDR [nlHsVar assoc
                                                     , nlHsIntLit (toInteger n)]

        allSelBinds   = map (map selBinds) datasels
        selBinds s    = mkBag [(selName_RDR, selName_matches s)]

        loc           = srcLocSpan (getSrcLoc tycon)
        mkStringLHS s = [mkSimpleHsAlt nlWildPat (nlHsLit (mkHsString s))]
        datacons      = tyConDataCons tycon
        datasels      = map dataConFieldLabels datacons

        tyConName_user = case tyConFamInst_maybe tycon of
                           Just (ptycon, _) -> tyConName ptycon
                           Nothing          -> tyConName tycon

        dtName_matches     = mkStringLHS . occNameString . nameOccName
                           $ tyConName_user
        moduleName_matches = mkStringLHS . moduleNameString . moduleName 
                           . nameModule . tyConName $ tycon
        isNewtype_matches  = [mkSimpleHsAlt nlWildPat (nlHsVar true_RDR)]

        conName_matches     c = mkStringLHS . occNameString . nameOccName
                              . dataConName $ c
        conFixity_matches   c = [mkSimpleHsAlt nlWildPat (fixity c)]
        conIsRecord_matches _ = [mkSimpleHsAlt nlWildPat (nlHsVar true_RDR)]

        selName_matches     s = mkStringLHS (occNameString (nameOccName s))


--------------------------------------------------------------------------------
-- Dealing with sums
--------------------------------------------------------------------------------

mkSum :: GenericKind_ -- Generic or Generic1?
      -> US          -- Base for generating unique names
      -> TyCon       -- The type constructor
      -> [DataCon]   -- The data constructors
      -> ([Alt],     -- Alternatives for the T->Trep "from" function
          [Alt])     -- Alternatives for the Trep->T "to" function

-- Datatype without any constructors
mkSum _ _ tycon [] = ([from_alt], [to_alt])
  where
    from_alt = (nlWildPat, mkM1_E (makeError errMsgFrom))
    to_alt   = (mkM1_P nlWildPat, makeError errMsgTo)
               -- These M1s are meta-information for the datatype
    makeError s = nlHsApp (nlHsVar error_RDR) (nlHsLit (mkHsString s))
    tyConStr   = occNameString (nameOccName (tyConName tycon))
    errMsgFrom = "No generic representation for empty datatype " ++ tyConStr
    errMsgTo   = "No values for empty datatype " ++ tyConStr

-- Datatype with at least one constructor
mkSum gk_ us _ datacons =
  -- switch the payload of gk_ to be datacon-centric instead of tycon-centric
 unzip [ mk1Sum (gk2gkDC gk_ d) us i (length datacons) d
           | (d,i) <- zip datacons [1..] ]

-- Build the sum for a particular constructor
mk1Sum :: GenericKind_DC -- Generic or Generic1?
       -> US        -- Base for generating unique names
       -> Int       -- The index of this constructor
       -> Int       -- Total number of constructors
       -> DataCon   -- The data constructor
       -> (Alt,     -- Alternative for the T->Trep "from" function
           Alt)     -- Alternative for the Trep->T "to" function
mk1Sum gk_ us i n datacon = (from_alt, to_alt)
  where
    gk = forgetArgVar gk_

    -- Existentials already excluded
    argTys = dataConOrigArgTys datacon
    n_args = dataConSourceArity datacon

    datacon_varTys = zip (map mkGenericLocal [us .. us+n_args-1]) argTys
    datacon_vars = map fst datacon_varTys
    us'          = us + n_args

    datacon_rdr  = getRdrName datacon
    
    from_alt     = (nlConVarPat datacon_rdr datacon_vars, from_alt_rhs)
    from_alt_rhs = mkM1_E (genLR_E i n (mkProd_E gk_ us' datacon_varTys))
    
    to_alt     = (mkM1_P (genLR_P i n (mkProd_P gk us' datacon_vars)), to_alt_rhs)
                 -- These M1s are meta-information for the datatype
    to_alt_rhs = case gk_ of
      Gen0_DC        -> nlHsVarApps datacon_rdr datacon_vars
      Gen1_DC argVar -> nlHsApps datacon_rdr $ map argTo datacon_varTys
        where
          argTo (var, ty) = converter ty `nlHsApp` nlHsVar var where
            converter = argTyFold argVar $ ArgTyAlg
              {ata_rec0 = const $ nlHsVar unK1_RDR,
               ata_par1 = nlHsVar unPar1_RDR,
               ata_rec1 = const $ nlHsVar unRec1_RDR,
               ata_comp = \_ cnv -> (nlHsVar fmap_RDR `nlHsApp` cnv)
                                    `nlHsCompose` nlHsVar unComp1_RDR}



-- Generates the L1/R1 sum pattern
genLR_P :: Int -> Int -> LPat RdrName -> LPat RdrName
genLR_P i n p
  | n == 0       = error "impossible"
  | n == 1       = p
  | i <= div n 2 = nlConPat l1DataCon_RDR [genLR_P i     (div n 2) p]
  | otherwise    = nlConPat r1DataCon_RDR [genLR_P (i-m) (n-m)     p]
                     where m = div n 2

-- Generates the L1/R1 sum expression
genLR_E :: Int -> Int -> LHsExpr RdrName -> LHsExpr RdrName
genLR_E i n e
  | n == 0       = error "impossible"
  | n == 1       = e
  | i <= div n 2 = nlHsVar l1DataCon_RDR `nlHsApp` genLR_E i     (div n 2) e
  | otherwise    = nlHsVar r1DataCon_RDR `nlHsApp` genLR_E (i-m) (n-m)     e
                     where m = div n 2

--------------------------------------------------------------------------------
-- Dealing with products
--------------------------------------------------------------------------------

-- Build a product expression
mkProd_E :: GenericKind_DC      -- Generic or Generic1?
         -> US	            -- Base for unique names
         -> [(RdrName, Type)] -- List of variables matched on the lhs and their types
	 -> LHsExpr RdrName -- Resulting product expression
mkProd_E _   _ []     = mkM1_E (nlHsVar u1DataCon_RDR)
mkProd_E gk_ _ varTys = mkM1_E (foldBal prod appVars)
                     -- These M1s are meta-information for the constructor
  where
    appVars = map (wrapArg_E gk_) varTys
    prod a b = prodDataCon_RDR `nlHsApps` [a,b]

wrapArg_E :: GenericKind_DC -> (RdrName, Type) -> LHsExpr RdrName
wrapArg_E Gen0_DC          (var, _)  = mkM1_E (k1DataCon_RDR `nlHsVarApps` [var])
                         -- This M1 is meta-information for the selector
wrapArg_E (Gen1_DC argVar) (var, ty) = mkM1_E $ converter ty `nlHsApp` nlHsVar var
                         -- This M1 is meta-information for the selector
  where converter = argTyFold argVar $ ArgTyAlg
          {ata_rec0 = const $ nlHsVar k1DataCon_RDR,
           ata_par1 = nlHsVar par1DataCon_RDR,
           ata_rec1 = const $ nlHsVar rec1DataCon_RDR,
           ata_comp = \_ cnv -> nlHsVar comp1DataCon_RDR `nlHsCompose`
                                  (nlHsVar fmap_RDR `nlHsApp` cnv)}



-- Build a product pattern
mkProd_P :: GenericKind   -- Gen0 or Gen1
         -> US		        -- Base for unique names
	       -> [RdrName]     -- List of variables to match
	       -> LPat RdrName  -- Resulting product pattern
mkProd_P _  _ []   = mkM1_P (nlNullaryConPat u1DataCon_RDR)
mkProd_P gk _ vars = mkM1_P (foldBal prod appVars)
                     -- These M1s are meta-information for the constructor
  where
    appVars = map (wrapArg_P gk) vars
    prod a b = prodDataCon_RDR `nlConPat` [a,b]

wrapArg_P :: GenericKind -> RdrName -> LPat RdrName
wrapArg_P Gen0 v = mkM1_P (k1DataCon_RDR `nlConVarPat` [v])
                   -- This M1 is meta-information for the selector
wrapArg_P Gen1 v = m1DataCon_RDR `nlConVarPat` [v]

mkGenericLocal :: US -> RdrName
mkGenericLocal u = mkVarUnqual (mkFastString ("g" ++ show u))

mkM1_E :: LHsExpr RdrName -> LHsExpr RdrName
mkM1_E e = nlHsVar m1DataCon_RDR `nlHsApp` e

mkM1_P :: LPat RdrName -> LPat RdrName
mkM1_P p = m1DataCon_RDR `nlConPat` [p]

nlHsCompose :: LHsExpr RdrName -> LHsExpr RdrName -> LHsExpr RdrName
nlHsCompose x y = compose_RDR `nlHsApps` [x, y]

-- | Variant of foldr1 for producing balanced lists
foldBal :: (a -> a -> a) -> [a] -> a
foldBal op = foldBal' op (error "foldBal: empty list")

foldBal' :: (a -> a -> a) -> a -> [a] -> a
foldBal' _  x []  = x
foldBal' _  _ [y] = y
foldBal' op x l   = let (a,b) = splitAt (length l `div` 2) l
                    in foldBal' op x a `op` foldBal' op x b

\end{code}
