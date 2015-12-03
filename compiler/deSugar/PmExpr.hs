{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Haskell expressions (as used by the pattern matching checker) and utilities.
-}

{-# LANGUAGE CPP #-}

module PmExpr (
        PmExpr(..), PmLit(..), SimpleEq, ComplexEq, eqPmLit,
        truePmExpr, falsePmExpr, isTruePmExpr, isFalsePmExpr, isNotPmExprOther,
        lhsExprToPmExpr, hsExprToPmExpr, substComplexEq, filterComplex,
        pprPmExprWithParens, runPmPprM
    ) where

#include "HsVersions.h"

import HsSyn
import Id
import DataCon
import TysWiredIn
import Outputable
import Util
import SrcLoc
import FastString -- sLit
import VarSet

import Data.Functor ((<$>))
import Data.Maybe (mapMaybe)
import Data.List (groupBy, sortBy)
import Control.Monad.Trans.State.Lazy

{-
%************************************************************************
%*                                                                      *
                         Lifted Expressions
%*                                                                      *
%************************************************************************
-}

{- Note [PmExprOther in PmExpr]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Since there is no plan to extend the (currently pretty naive) term oracle in
the near future, instead of playing with the verbose (HsExpr Id), we lift it to
PmExpr. All expressions the term oracle does not handle are wrapped by the
constructor PmExprOther. Note that we do not perform substitution in
PmExprOther. Because of this, we do not even print PmExprOther, since they may
refer to variables that are otherwise substituted away.
-}

-- ----------------------------------------------------------------------------
-- ** Types

-- | Lifted expressions for pattern match checking.
data PmExpr = PmExprVar   Id
            | PmExprCon   DataCon [PmExpr]
            | PmExprLit   PmLit
            | PmExprEq    PmExpr PmExpr  -- Syntactic equality
            | PmExprOther (HsExpr Id)    -- Note [PmExprOther in PmExpr]

-- | Literals (simple and overloaded ones) for pattern match checking.
data PmLit = PmSLit HsLit                                    -- simple
           | PmOLit Bool {- is it negated? -} (HsOverLit Id) -- overloaded

-- | PmLit equality. If both literals are overloaded, the equality check may be
-- inconclusive. Since an overloaded PmLit represents a function application
-- (e.g.  fromInteger 5), if two literals look the same they are the same but
-- if they don't, whether they are depends on the implementation of the
-- from-function.
eqPmLit :: PmLit -> PmLit -> Maybe Bool
eqPmLit (PmSLit    l1) (PmSLit l2   ) = Just (l1 == l2)
eqPmLit (PmOLit b1 l1) (PmOLit b2 l2) = if res then Just True else Nothing
  where res = b1 == b2 && l1 == l2
eqPmLit _ _ = Just False -- this should not even happen I think

nubPmLit :: [PmLit] -> [PmLit]
nubPmLit []     = []
nubPmLit [x]    = [x]
nubPmLit (x:xs) = x : nubPmLit (filter (neqPmLit x) xs)
  where neqPmLit l1 l2 = case eqPmLit l1 l2 of
          Just True  -> False
          Just False -> True
          Nothing    -> True

-- | Term equalities
type SimpleEq  = (Id, PmExpr) -- We always use this orientation
type ComplexEq = (PmExpr, PmExpr)

-- | Expression `True'
truePmExpr :: PmExpr
truePmExpr = PmExprCon trueDataCon []

-- | Expression `False'
falsePmExpr :: PmExpr
falsePmExpr = PmExprCon falseDataCon []

-- ----------------------------------------------------------------------------
-- ** Predicates on PmExpr

-- | Check if an expression is lifted or not
isNotPmExprOther :: PmExpr -> Bool
isNotPmExprOther (PmExprOther _) = False
isNotPmExprOther _expr           = True

-- | Check whether a literal is negated
isNegatedPmLit :: PmLit -> Bool
isNegatedPmLit (PmOLit b _) = b
isNegatedPmLit _other_lit   = False

-- | Check whether a PmExpr is syntactically equal to term `True'.
isTruePmExpr :: PmExpr -> Bool
isTruePmExpr (PmExprCon c []) = c == trueDataCon
isTruePmExpr _other_expr      = False

-- | Check whether a PmExpr is syntactically equal to term `False'.
isFalsePmExpr :: PmExpr -> Bool
isFalsePmExpr (PmExprCon c []) = c == falseDataCon
isFalsePmExpr _other_expr      = False

-- | Check whether a PmExpr is syntactically e
isNilPmExpr :: PmExpr -> Bool
isNilPmExpr (PmExprCon c _) = c == nilDataCon
isNilPmExpr _other_expr     = False

-- | Check whether a PmExpr is syntactically equal to (x == y).
-- Since (==) is overloaded and can have an arbitrary implementation, we use
-- the PmExprEq constructor to represent only equalities with non-overloaded
-- literals where it coincides with a syntactic equality check.
isPmExprEq :: PmExpr -> Maybe (PmExpr, PmExpr)
isPmExprEq (PmExprEq e1 e2) = Just (e1,e2)
isPmExprEq _other_expr      = Nothing

-- | Check if a DataCon is (:).
isConsDataCon :: DataCon -> Bool
isConsDataCon con = consDataCon == con

-- ----------------------------------------------------------------------------
-- ** Substitution in PmExpr

-- | We return a boolean along with the expression. Hence, if substitution was
-- a no-op, we know that the expression still cannot progress.
substPmExpr :: Id -> PmExpr -> PmExpr -> (PmExpr, Bool)
substPmExpr x e1 e =
  case e of
    PmExprVar z | x == z    -> (e1, True)
                | otherwise -> (e, False)
    PmExprCon c ps -> let (ps', bs) = mapAndUnzip (substPmExpr x e1) ps
                      in  (PmExprCon c ps', or bs)
    PmExprEq ex ey -> let (ex', bx) = substPmExpr x e1 ex
                          (ey', by) = substPmExpr x e1 ey
                      in  (PmExprEq ex' ey', bx || by)
    _other_expr    -> (e, False) -- The rest are terminals (We silently ignore
                                 -- Other). See Note [PmExprOther in PmExpr]

-- | Substitute in a complex equality. We return (Left eq) if the substitution
-- affected the equality or (Right eq) if nothing happened.
substComplexEq :: Id -> PmExpr -> ComplexEq -> Either ComplexEq ComplexEq
substComplexEq x e (ex, ey)
  | bx || by  = Left  (ex', ey')
  | otherwise = Right (ex', ey')
  where
    (ex', bx) = substPmExpr x e ex
    (ey', by) = substPmExpr x e ey

-- -----------------------------------------------------------------------
-- ** Lift source expressions (HsExpr Id) to PmExpr

lhsExprToPmExpr :: LHsExpr Id -> PmExpr
lhsExprToPmExpr (L _ e) = hsExprToPmExpr e

hsExprToPmExpr :: HsExpr Id -> PmExpr

hsExprToPmExpr (HsVar         x) = PmExprVar (unLoc x)
hsExprToPmExpr (HsOverLit  olit) = PmExprLit (PmOLit False olit)
hsExprToPmExpr (HsLit       lit) = PmExprLit (PmSLit lit)

hsExprToPmExpr e@(NegApp _ neg_e)
  | PmExprLit (PmOLit False ol) <- hsExprToPmExpr neg_e
  = PmExprLit (PmOLit True ol)
  | otherwise = PmExprOther e
hsExprToPmExpr (HsPar (L _ e)) = hsExprToPmExpr e

hsExprToPmExpr e@(ExplicitTuple ps boxity)
  | all tupArgPresent ps = PmExprCon tuple_con tuple_args
  | otherwise            = PmExprOther e
  where
    tuple_con  = tupleDataCon boxity (length ps)
    tuple_args = [ lhsExprToPmExpr e | L _ (Present e) <- ps ]

hsExprToPmExpr e@(ExplicitList _elem_ty mb_ol elems)
  | Nothing <- mb_ol = foldr cons nil (map lhsExprToPmExpr elems)
  | otherwise        = PmExprOther e {- overloaded list: No PmExprApp -}
  where
    cons x xs = PmExprCon consDataCon [x,xs]
    nil       = PmExprCon nilDataCon  []

hsExprToPmExpr (ExplicitPArr _elem_ty elems)
  = PmExprCon (parrFakeCon (length elems)) (map lhsExprToPmExpr elems)

-- we want this but we would have to make evrything monadic :/
-- ./compiler/deSugar/DsMonad.hs:397:dsLookupDataCon :: Name -> DsM DataCon
--
-- hsExprToPmExpr (RecordCon   c _ binds) = do
--   con  <- dsLookupDataCon (unLoc c)
--   args <- mapM lhsExprToPmExpr (hsRecFieldsArgs binds)
--   return (PmExprCon con args)
hsExprToPmExpr e@(RecordCon   _ _ _ _) = PmExprOther e

hsExprToPmExpr (HsTick            _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsBinTick       _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsTickPragma    _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsSCC           _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsCoreAnn       _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (ExprWithTySig     e _) = lhsExprToPmExpr e
hsExprToPmExpr (ExprWithTySigOut  e _) = lhsExprToPmExpr e
hsExprToPmExpr (HsWrap            _ e) =  hsExprToPmExpr e
hsExprToPmExpr e = PmExprOther e -- the rest are not handled by the oracle

{-
%************************************************************************
%*                                                                      *
                            Pretty printing
%*                                                                      *
%************************************************************************
-}

{- 1. Literals
~~~~~~~~~~~~~~
Starting with a function definition like:

    f :: Int -> Bool
    f 5 = True
    f 6 = True

The uncovered set looks like:
    { var |> False == (var == 5), False == (var == 6) }

Yet, we would like to print this nicely as follows:
   x , where x not one of {5,6}

Function `filterComplex' takes the set of residual constraints and packs
together the negative constraints that refer to the same variable so we can do
just this. Since these variables will be shown to the programmer, we also give
them better names (t1, t2, ..), hence the SDoc in PmNegLitCt.

2. Residual Constraints
~~~~~~~~~~~~~~~~~~~~~~~
Unhandled constraints that refer to HsExpr are typically ignored by the solver
(it does not even substitute in HsExpr so they are even printed as wildcards).
Additionally, the oracle returns a substitution if it succeeds so we apply this
substitution to the vectors before printing them out (see function `pprOne' in
Check.hs) to be more precice.
-}

-- -----------------------------------------------------------------------------
-- ** Transform residual constraints in appropriate form for pretty printing

type PmNegLitCt = (Id, (SDoc, [PmLit]))

filterComplex :: [ComplexEq] -> [PmNegLitCt]
filterComplex = zipWith rename nameList . map mkGroup
              . groupBy name . sortBy order . mapMaybe isNegLitCs
  where
    order x y = compare (fst x) (fst y)
    name  x y = fst x == fst y
    mkGroup l = (fst (head l), nubPmLit $ map snd l)
    rename new (old, lits) = (old, (new, lits))

    isNegLitCs (e1,e2)
      | isFalsePmExpr e1, Just (x,y) <- isPmExprEq e2 = isNegLitCs' x y
      | isFalsePmExpr e2, Just (x,y) <- isPmExprEq e1 = isNegLitCs' x y
      | otherwise = Nothing

    isNegLitCs' (PmExprVar x) (PmExprLit l) = Just (x, l)
    isNegLitCs' (PmExprLit l) (PmExprVar x) = Just (x, l)
    isNegLitCs' _ _             = Nothing

    -- Try nice names p,q,r,s,t before using the (ugly) t_i
    nameList :: [SDoc]
    nameList = map (ptext . sLit) ["p","q","r","s","t"] ++
                 [ ptext (sLit ('t':show u)) | u <- [(0 :: Int)..] ]

-- ----------------------------------------------------------------------------

runPmPprM :: PmPprM a -> [PmNegLitCt] -> (a, [(SDoc,[PmLit])])
runPmPprM m lit_env = (result, mapMaybe is_used lit_env)
  where
    (result, (_lit_env, used)) = runState m (lit_env, emptyVarSet)

    is_used (x,(name, lits))
      | elemVarSet x used = Just (name, lits)
      | otherwise         = Nothing

type PmPprM a = State ([PmNegLitCt], IdSet) a
-- (the first part of the state is read only. make it a reader?)

addUsed :: Id -> PmPprM ()
addUsed x = modify (\(negated, used) -> (negated, extendVarSet used x))

checkNegation :: Id -> PmPprM (Maybe SDoc) -- the clean name if it is negated
checkNegation x = do
  negated <- gets fst
  return $ case lookup x negated of
    Just (new, _) -> Just new
    Nothing       -> Nothing

-- | Pretty print a pmexpr, but remember to prettify the names of the variables
-- that refer to neg-literals. The ones that cannot be shown are printed as
-- underscores.
pprPmExpr :: PmExpr -> PmPprM SDoc
pprPmExpr (PmExprVar x) = do
  mb_name <- checkNegation x
  case mb_name of
    Just name -> addUsed x >> return name
    Nothing   -> return underscore

pprPmExpr (PmExprCon con args) = pprPmExprCon con args
pprPmExpr (PmExprLit l)        = return (ppr l)
pprPmExpr (PmExprEq _ _)       = return underscore -- don't show
pprPmExpr (PmExprOther _)      = return underscore -- don't show

needsParens :: PmExpr -> Bool
needsParens (PmExprVar   {}) = False
needsParens (PmExprLit    l) = isNegatedPmLit l
needsParens (PmExprEq    {}) = False -- will become a wildcard
needsParens (PmExprOther {}) = False -- will become a wildcard
needsParens (PmExprCon c es)
  | isTupleDataCon c || isPArrFakeCon c
  || isConsDataCon c || null es = False
  | otherwise                   = True

pprPmExprWithParens :: PmExpr -> PmPprM SDoc
pprPmExprWithParens expr
  | needsParens expr = parens <$> pprPmExpr expr
  | otherwise        =            pprPmExpr expr

pprPmExprCon :: DataCon -> [PmExpr] -> PmPprM SDoc
pprPmExprCon con args
  | isTupleDataCon con = mkTuple <$> mapM pprPmExpr args
  |  isPArrFakeCon con = mkPArr  <$> mapM pprPmExpr args
  |  isConsDataCon con = pretty_list
  | dataConIsInfix con = case args of
      [x, y] -> do x' <- pprPmExprWithParens x
                   y' <- pprPmExprWithParens y
                   return (x' <+> ppr con <+> y')
      -- can it be infix but have more than two arguments?
      list   -> pprPanic "pprPmExprCon:" (ppr list)
  | null args = return (ppr con)
  | otherwise = do args' <- mapM pprPmExprWithParens args
                   return (fsep (ppr con : args'))
  where
    mkTuple, mkPArr :: [SDoc] -> SDoc
    mkTuple = parens     . fsep . punctuate comma
    mkPArr  = paBrackets . fsep . punctuate comma

    -- lazily, to be used in the list case only
    pretty_list :: PmPprM SDoc
    pretty_list = case isNilPmExpr (last list) of
      True  -> brackets . fsep . punctuate comma <$> mapM pprPmExpr (init list)
      False -> parens   . hcat . punctuate colon <$> mapM pprPmExpr list

    list = list_elements args

    list_elements [x,y]
      | PmExprCon c es <- y,  nilDataCon == c = ASSERT (null es) [x,y]
      | PmExprCon c es <- y, consDataCon == c = x : list_elements es
      | otherwise = [x,y]
    list_elements list  = pprPanic "list_elements:" (ppr list)

instance Outputable PmLit where
  ppr (PmSLit     l) = pmPprHsLit l
  ppr (PmOLit neg l) = (if neg then char '-' else empty) <> ppr l

-- not really useful for pmexprs per se
instance Outputable PmExpr where
  ppr e = fst $ runPmPprM (pprPmExpr e) []

