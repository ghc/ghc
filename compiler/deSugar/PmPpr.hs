{-# LANGUAGE CPP #-}

-- | Provides factilities for pretty-printing 'PmExpr's in a way approriate for
-- user facing pattern match warnings.
module PmPpr (
        pprUncovered
    ) where

#include "HsVersions.h"

import GhcPrelude

import Name
import NameEnv
import NameSet
import UniqDFM
import UniqSet
import ConLike
import DataCon
import TysWiredIn
import Outputable
import Control.Monad.Trans.Reader
import Maybes
import Data.List.NonEmpty (toList)

import PmExpr
import TmOracle

-- | Pretty-print the guts of an uncovered value vector abstraction, i.e., its
-- components and refutable shapes associated to any mentioned variables.
--
-- Example for @([Just p, q], [p :-> [3,4], q :-> [0,5]]):
--
-- @
-- (Just p) q
--     where p is not one of {3, 4}
--           q is not one of {0, 5}
-- @
--
-- When the set of refutable shapes contains more than 3 elements, the
-- additional elements are indicated by "...".
pprUncovered :: ([PmExpr], TmState) -> SDoc
pprUncovered (expr_vec, tm_cs)
  | isNullUDFM refuts = fsep vec -- there are no refutations
  | otherwise         = hang (fsep vec) 4 $
                          text "where" <+> vcat (map (pprRefutableShapes . snd) (udfmToList refuts))
  where
    sdoc_vec = mapM pprPmExprWithParens expr_vec
    fvs      = unionNameSets (map pmExprFVs expr_vec)
    refuts   = prettifyRefuts fvs tm_cs
    vec      = runPmPpr sdoc_vec refuts

-- | Output refutable shapes of a variable in the form of @var is not one of {2,
-- Nothing, 3}@. Will never print more than 3 refutable shapes, the tail is
-- indicated by an ellipsis.
pprRefutableShapes :: (SDoc,[PmAltCon]) -> SDoc
pprRefutableShapes (var, alts)
  = var <+> text "is not one of" <+> format_alts alts
  where
    format_alts = braces . fsep . punctuate comma . shorten . map ppr_alt
    shorten (a:b:c:_:_)       = a:b:c:[text "..."]
    shorten xs                = xs
    ppr_alt (PmAltConLike cl) = ppr cl
    ppr_alt (PmAltLit lit)    = ppr lit

{- 1. Literals
~~~~~~~~~~~~~~
Starting with a function definition like:

    f :: Int -> Bool
    f 5 = True
    f 6 = True

The uncovered set looks like:
    { var |> var /= 5, var /= 6 }

Yet, we would like to print this nicely as follows:
   x , where x not one of {5,6}

Since these variables will be shown to the programmer, we give them better names
(t1, t2, ..) in 'prettifyRefuts', hence the SDoc in 'PrettyPmRefutEnv'.

2. Residual Constraints
~~~~~~~~~~~~~~~~~~~~~~~
Unhandled constraints that refer to HsExpr are typically ignored by the solver
(it does not even substitute in HsExpr so they are even printed as wildcards).
Additionally, the oracle returns a substitution if it succeeds so we apply this
substitution to the vectors before printing them out (see function `pprOne' in
Check.hs) to be more precise.
-}

-- | Extract and assigns pretty names to constraint variables with refutable
-- shapes.
prettifyRefuts :: UniqSet Name -> TmState -> DNameEnv (SDoc, [PmAltCon])
prettifyRefuts fvs
  = listToUDFM . zipWith rename nameList
  . filter ((`elemUniqSet_Directly` fvs) . fst) . udfmToList
  . wrapUpRefutableShapes
  where
    rename new (old, ncons) = (old, (new, ncons))
    -- Try nice names p,q,r,s,t before using the (ugly) t_i
    nameList :: [SDoc]
    nameList = map text ["p","q","r","s","t"] ++
                 [ text ('t':show u) | u <- [(0 :: Int)..] ]

type PmPprM a = Reader (DNameEnv (SDoc, [PmAltCon])) a

runPmPpr :: PmPprM a -> DNameEnv (SDoc, [PmAltCon]) -> a
runPmPpr = runReader

checkNegation :: Name -> PmPprM (Maybe SDoc) -- the clean name if it is negated
checkNegation x = do
  negated <- ask
  return $ case lookupDNameEnv negated x of
    Just (new, _) -> Just new
    Nothing       -> Nothing

-- | Pretty print a pmexpr, but remember to prettify the names of the variables
-- that refer to neg-literals. The ones that cannot be shown are printed as
-- underscores.
pprPmExpr :: PmExpr -> PmPprM SDoc
pprPmExpr (PmExprVar x)        = fromMaybe underscore <$> checkNegation x
pprPmExpr (PmExprCon con args) = pprPmExprCon con args
pprPmExpr (PmExprOther _)      = return underscore -- don't show

needsParens :: PmExpr -> Bool
needsParens (PmExprVar   {})            = False
needsParens (PmExprOther {})            = False -- will become a wildcard
needsParens (PmExprCon (PmAltLit l) _)  = isNegatedPmLit l
needsParens (PmExprCon (PmAltConLike (RealDataCon c)) _)
  | isTupleDataCon c || isConsDataCon c = False
needsParens (PmExprCon _ es)            = not (null es)

pprPmExprWithParens :: PmExpr -> PmPprM SDoc
pprPmExprWithParens expr
  | needsParens expr = parens <$> pprPmExpr expr
  | otherwise        =            pprPmExpr expr

pprPmExprCon :: PmAltCon -> [PmExpr] -> PmPprM SDoc
pprPmExprCon (PmAltConLike cl) args = pprConLike cl args
pprPmExprCon (PmAltLit l)      _    = pure (ppr l)

pprConLike :: ConLike -> [PmExpr] -> PmPprM SDoc
pprConLike cl args
  | Just pm_expr_list <- pmExprAsList (PmExprCon (PmAltConLike cl) args)
  = case pm_expr_list of
      NilTerminated list ->
        brackets . fsep . punctuate comma <$> mapM pprPmExpr list
      WcVarTerminated pref x ->
        parens   . fcat . punctuate colon <$> mapM pprPmExpr (toList pref ++ [PmExprVar x])
pprConLike (RealDataCon con) args
  | isTupleDataCon con
  = parens . fsep . punctuate comma <$> mapM pprPmExpr args
pprConLike cl args
  | conLikeIsInfix cl = case args of
      [x, y] -> do x' <- pprPmExprWithParens x
                   y' <- pprPmExprWithParens y
                   return (x' <+> ppr cl <+> y')
      -- can it be infix but have more than two arguments?
      list   -> pprPanic "pprPmExprCon:" (ppr list)
  | null args = return (ppr cl)
  | otherwise = do args' <- mapM pprPmExprWithParens args
                   return (fsep (ppr cl : args'))

-- | Check whether a literal is negated
isNegatedPmLit :: PmLit -> Bool
isNegatedPmLit (PmOLit b _) = b
isNegatedPmLit _other_lit   = False

-- | Check if a DataCon is (:).
isConsDataCon :: DataCon -> Bool
isConsDataCon con = consDataCon == con
