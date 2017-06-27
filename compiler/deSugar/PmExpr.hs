{-
Author: George Karachalias <george.karachalias@cs.kuleuven.be>

Haskell expressions (as used by the pattern matching checker) and utilities.
-}

{-# LANGUAGE CPP #-}

module PmExpr (
        PmExpr(..), PmLit(..), SimpleEq, ComplexEq, toComplex, eqPmLit,
        truePmExpr, falsePmExpr, isTruePmExpr, isFalsePmExpr, isNotPmExprOther,
        lhsExprToPmExpr, hsExprToPmExpr, substComplexEq, filterComplex,
        pprPmExprWithParens, runPmPprM
    ) where

#include "HsVersions.h"

import HsSyn
import Id
import Name
import NameSet
import DataCon
import ConLike
import TysWiredIn
import Outputable
import Util
import SrcLoc

import Data.Maybe (mapMaybe)
import Data.List (groupBy, sortBy, nubBy)
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
data PmExpr = PmExprVar   Name
            | PmExprCon   ConLike [PmExpr]
            | PmExprLit   PmLit
            | PmExprEq    PmExpr PmExpr  -- Syntactic equality
            | PmExprOther (HsExpr GhcTc)  -- Note [PmExprOther in PmExpr]


mkPmExprData :: DataCon -> [PmExpr] -> PmExpr
mkPmExprData dc args = PmExprCon (RealDataCon dc) args

-- | Literals (simple and overloaded ones) for pattern match checking.
data PmLit = PmSLit (HsLit GhcTc)                               -- simple
           | PmOLit Bool {- is it negated? -} (HsOverLit GhcTc) -- overloaded

-- | Equality between literals for pattern match checking.
eqPmLit :: PmLit -> PmLit -> Bool
eqPmLit (PmSLit    l1) (PmSLit    l2) = l1 == l2
eqPmLit (PmOLit b1 l1) (PmOLit b2 l2) = b1 == b2 && l1 == l2
  -- See Note [Undecidable Equality for Overloaded Literals]
eqPmLit _              _              = False

{- Note [Undecidable Equality for Overloaded Literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Equality on overloaded literals is undecidable in the general case. Consider
the following example:

  instance Num Bool where
    ...
    fromInteger 0 = False -- C-like representation of booleans
    fromInteger _ = True

    f :: Bool -> ()
    f 1 = ()        -- Clause A
    f 2 = ()        -- Clause B

Clause B is redundant but to detect this, we should be able to solve the
constraint: False ~ (fromInteger 2 ~ fromInteger 1) which means that we
have to look through function `fromInteger`, whose implementation could
be anything. This poses difficulties for:

1. The expressive power of the check.
   We cannot expect a reasonable implementation of pattern matching to detect
   that fromInteger 2 ~ fromInteger 1 is True, unless we unfold function
   fromInteger. This puts termination at risk and is undecidable in the
   general case.

2. Performance.
   Having an unresolved constraint False ~ (fromInteger 2 ~ fromInteger 1)
   lying around could become expensive really fast. Ticket #11161 illustrates
   how heavy use of overloaded literals can generate plenty of those
   constraints, effectively undermining the term oracle's performance.

3. Error nessages/Warnings.
   What should our message for `f` above be? A reasonable approach would be
   to issue:

     Pattern matches are (potentially) redundant:
       f 2 = ...    under the assumption that 1 == 2

   but seems to complex and confusing for the user.

We choose to treat overloaded literals that look different as different. The
impact of this is the following:

  * Redundancy checking is rather conservative, since it cannot see that clause
    B above is redundant.

  * We have instant equality check for overloaded literals (we do not rely on
    the term oracle which is rather expensive, both in terms of performance and
    memory). This significantly improves the performance of functions `covered`
    `uncovered` and `divergent` in deSugar/Check.hs and effectively addresses
    #11161.

  * The warnings issued are simpler.

  * We do not play on the safe side, strictly speaking. The assumption that
    1 /= 2 makes the redundancy check more conservative but at the same time
    makes its dual (exhaustiveness check) unsafe. This we can live with, mainly
    for two reasons:
    1. At the moment we do not use the results of the check during compilation
       where this would be a disaster (could result in runtime errors even if
       our function was deemed exhaustive).
    2. Pattern matcing on literals can never be considered exhaustive unless we
       have a catch-all clause. Hence, this assumption affects mainly the
       appearance of the warnings and is, in practice safe.
-}

nubPmLit :: [PmLit] -> [PmLit]
nubPmLit = nubBy eqPmLit

-- | Term equalities
type SimpleEq  = (Id, PmExpr) -- We always use this orientation
type ComplexEq = (PmExpr, PmExpr)

-- | Lift a `SimpleEq` to a `ComplexEq`
toComplex :: SimpleEq -> ComplexEq
toComplex (x,e) = (PmExprVar (idName x), e)

-- | Expression `True'
truePmExpr :: PmExpr
truePmExpr = mkPmExprData trueDataCon []

-- | Expression `False'
falsePmExpr :: PmExpr
falsePmExpr = mkPmExprData falseDataCon []

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
isTruePmExpr (PmExprCon c []) = c == RealDataCon trueDataCon
isTruePmExpr _other_expr      = False

-- | Check whether a PmExpr is syntactically equal to term `False'.
isFalsePmExpr :: PmExpr -> Bool
isFalsePmExpr (PmExprCon c []) = c == RealDataCon falseDataCon
isFalsePmExpr _other_expr      = False

-- | Check whether a PmExpr is syntactically e
isNilPmExpr :: PmExpr -> Bool
isNilPmExpr (PmExprCon c _) = c == RealDataCon nilDataCon
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
substPmExpr :: Name -> PmExpr -> PmExpr -> (PmExpr, Bool)
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
substComplexEq :: Name -> PmExpr -> ComplexEq -> Either ComplexEq ComplexEq
substComplexEq x e (ex, ey)
  | bx || by  = Left  (ex', ey')
  | otherwise = Right (ex', ey')
  where
    (ex', bx) = substPmExpr x e ex
    (ey', by) = substPmExpr x e ey

-- -----------------------------------------------------------------------
-- ** Lift source expressions (HsExpr Id) to PmExpr

lhsExprToPmExpr :: LHsExpr GhcTc -> PmExpr
lhsExprToPmExpr (L _ e) = hsExprToPmExpr e

hsExprToPmExpr :: HsExpr GhcTc -> PmExpr

hsExprToPmExpr (HsVar         x) = PmExprVar (idName (unLoc x))
hsExprToPmExpr (HsConLikeOut  c) = PmExprVar (conLikeName c)
hsExprToPmExpr (HsOverLit  olit) = PmExprLit (PmOLit False olit)
hsExprToPmExpr (HsLit       lit) = PmExprLit (PmSLit lit)

hsExprToPmExpr e@(NegApp _ neg_e)
  | PmExprLit (PmOLit False ol) <- synExprToPmExpr neg_e
  = PmExprLit (PmOLit True ol)
  | otherwise = PmExprOther e
hsExprToPmExpr (HsPar (L _ e)) = hsExprToPmExpr e

hsExprToPmExpr e@(ExplicitTuple ps boxity)
  | all tupArgPresent ps = mkPmExprData tuple_con tuple_args
  | otherwise            = PmExprOther e
  where
    tuple_con  = tupleDataCon boxity (length ps)
    tuple_args = [ lhsExprToPmExpr e | L _ (Present e) <- ps ]

hsExprToPmExpr e@(ExplicitList _elem_ty mb_ol elems)
  | Nothing <- mb_ol = foldr cons nil (map lhsExprToPmExpr elems)
  | otherwise        = PmExprOther e {- overloaded list: No PmExprApp -}
  where
    cons x xs = mkPmExprData consDataCon [x,xs]
    nil       = mkPmExprData nilDataCon  []

hsExprToPmExpr (ExplicitPArr _elem_ty elems)
  = mkPmExprData (parrFakeCon (length elems)) (map lhsExprToPmExpr elems)


-- we want this but we would have to make everything monadic :/
-- ./compiler/deSugar/DsMonad.hs:397:dsLookupDataCon :: Name -> DsM DataCon
--
-- hsExprToPmExpr (RecordCon   c _ binds) = do
--   con  <- dsLookupDataCon (unLoc c)
--   args <- mapM lhsExprToPmExpr (hsRecFieldsArgs binds)
--   return (PmExprCon con args)
hsExprToPmExpr e@(RecordCon   _ _ _ _) = PmExprOther e

hsExprToPmExpr (HsTick            _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsBinTick       _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsTickPragma  _ _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsSCC           _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (HsCoreAnn       _ _ e) = lhsExprToPmExpr e
hsExprToPmExpr (ExprWithTySig     e _) = lhsExprToPmExpr e
hsExprToPmExpr (ExprWithTySigOut  e _) = lhsExprToPmExpr e
hsExprToPmExpr (HsWrap            _ e) =  hsExprToPmExpr e
hsExprToPmExpr e = PmExprOther e -- the rest are not handled by the oracle

synExprToPmExpr :: SyntaxExpr GhcTc -> PmExpr
synExprToPmExpr = hsExprToPmExpr . syn_expr  -- ignore the wrappers

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

type PmNegLitCt = (Name, (SDoc, [PmLit]))

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
    nameList = map text ["p","q","r","s","t"] ++
                 [ text ('t':show u) | u <- [(0 :: Int)..] ]

-- ----------------------------------------------------------------------------

runPmPprM :: PmPprM a -> [PmNegLitCt] -> (a, [(SDoc,[PmLit])])
runPmPprM m lit_env = (result, mapMaybe is_used lit_env)
  where
    (result, (_lit_env, used)) = runState m (lit_env, emptyNameSet)

    is_used (x,(name, lits))
      | elemNameSet x used = Just (name, lits)
      | otherwise         = Nothing

type PmPprM a = State ([PmNegLitCt], NameSet) a
-- (the first part of the state is read only. make it a reader?)

addUsed :: Name -> PmPprM ()
addUsed x = modify (\(negated, used) -> (negated, extendNameSet used x))

checkNegation :: Name -> PmPprM (Maybe SDoc) -- the clean name if it is negated
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
needsParens (PmExprCon (RealDataCon c) es)
  | isTupleDataCon c || isPArrFakeCon c
  || isConsDataCon c || null es = False
  | otherwise                   = True
needsParens (PmExprCon (PatSynCon _) es) = not (null es)

pprPmExprWithParens :: PmExpr -> PmPprM SDoc
pprPmExprWithParens expr
  | needsParens expr = parens <$> pprPmExpr expr
  | otherwise        =            pprPmExpr expr

pprPmExprCon :: ConLike -> [PmExpr] -> PmPprM SDoc
pprPmExprCon (RealDataCon con) args
  | isTupleDataCon con = mkTuple <$> mapM pprPmExpr args
  |  isPArrFakeCon con = mkPArr  <$> mapM pprPmExpr args
  |  isConsDataCon con = pretty_list
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
      | PmExprCon c es <- y,  RealDataCon nilDataCon == c
          = ASSERT(null es) [x,y]
      | PmExprCon c es <- y, RealDataCon consDataCon == c
          = x : list_elements es
      | otherwise = [x,y]
    list_elements list  = pprPanic "list_elements:" (ppr list)
pprPmExprCon cl args
  | conLikeIsInfix cl = case args of
      [x, y] -> do x' <- pprPmExprWithParens x
                   y' <- pprPmExprWithParens y
                   return (x' <+> ppr cl <+> y')
      -- can it be infix but have more than two arguments?
      list   -> pprPanic "pprPmExprCon:" (ppr list)
  | null args = return (ppr cl)
  | otherwise = do args' <- mapM pprPmExprWithParens args
                   return (fsep (ppr cl : args'))

instance Outputable PmLit where
  ppr (PmSLit     l) = pmPprHsLit l
  ppr (PmOLit neg l) = (if neg then char '-' else empty) <> ppr l

-- not really useful for pmexprs per se
instance Outputable PmExpr where
  ppr e = fst $ runPmPprM (pprPmExpr e) []
