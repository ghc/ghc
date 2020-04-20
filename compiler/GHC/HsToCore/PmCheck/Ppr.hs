{-# LANGUAGE CPP, ViewPatterns #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Provides factilities for pretty-printing 'Delta's in a way appropriate for
-- user facing pattern match warnings.
module GHC.HsToCore.PmCheck.Ppr (
        pprUncovered
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Types.Basic
import GHC.Types.Id
import GHC.Types.Var.Env
import GHC.Types.Unique.DFM
import GHC.Core.ConLike
import GHC.Core.DataCon
import GHC.Builtin.Types
import GHC.Utils.Outputable
import Control.Monad.Trans.RWS.CPS
import GHC.Utils.Misc
import GHC.Data.Maybe
import Data.List.NonEmpty (NonEmpty, nonEmpty, toList)

import GHC.HsToCore.PmCheck.Types
import GHC.HsToCore.PmCheck.Oracle

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
pprUncovered :: Delta -> [Id] -> SDoc
pprUncovered delta vas
  | isNullUDFM refuts = fsep vec -- there are no refutations
  | otherwise         = hang (fsep vec) 4 $
                          text "where" <+> vcat (map (pprRefutableShapes . snd) (udfmToList refuts))
  where
    init_prec
      -- No outer parentheses when it's a unary pattern by assuming lowest
      -- precedence
      | [_] <- vas   = topPrec
      | otherwise    = appPrec
    ppr_action       = mapM (pprPmVar init_prec) vas
    (vec, renamings) = runPmPpr delta ppr_action
    refuts           = prettifyRefuts delta renamings

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
prettifyRefuts :: Delta -> DIdEnv SDoc -> DIdEnv (SDoc, [PmAltCon])
prettifyRefuts delta = listToUDFM . map attach_refuts . udfmToList
  where
    attach_refuts (u, sdoc) = (u, (sdoc, lookupRefuts delta u))


type PmPprM a = RWS Delta () (DIdEnv SDoc, [SDoc]) a

-- Try nice names p,q,r,s,t before using the (ugly) t_i
nameList :: [SDoc]
nameList = map text ["p","q","r","s","t"] ++
            [ text ('t':show u) | u <- [(0 :: Int)..] ]

runPmPpr :: Delta -> PmPprM a -> (a, DIdEnv SDoc)
runPmPpr delta m = case runRWS m delta (emptyDVarEnv, nameList) of
  (a, (renamings, _), _) -> (a, renamings)

-- | Allocates a new, clean name for the given 'Id' if it doesn't already have
-- one.
getCleanName :: Id -> PmPprM SDoc
getCleanName x = do
  (renamings, name_supply) <- get
  let (clean_name:name_supply') = name_supply
  case lookupDVarEnv renamings x of
    Just nm -> pure nm
    Nothing -> do
      put (extendDVarEnv renamings x clean_name, name_supply')
      pure clean_name

checkRefuts :: Id -> PmPprM (Maybe SDoc) -- the clean name if it has negative info attached
checkRefuts x = do
  delta <- ask
  case lookupRefuts delta x of
    [] -> pure Nothing -- Will just be a wildcard later on
    _  -> Just <$> getCleanName x

-- | Pretty print a variable, but remember to prettify the names of the variables
-- that refer to neg-literals. The ones that cannot be shown are printed as
-- underscores. Even with a type signature, if it's not too noisy.
pprPmVar :: PprPrec -> Id -> PmPprM SDoc
-- Type signature is "too noisy" by my definition if it needs to parenthesize.
-- I like           "not matched: _ :: Proxy (DIdEnv SDoc)",
-- but I don't like "not matched: (_ :: stuff) (_:_) (_ :: Proxy (DIdEnv SDoc))"
-- The useful information in the latter case is the constructor that we missed,
-- not the types of the wildcards in the places that aren't matched as a result.
pprPmVar prec x = do
  delta <- ask
  case lookupSolution delta x of
    Just (alt, _tvs, args) -> pprPmAltCon prec alt args
    Nothing          -> fromMaybe typed_wildcard <$> checkRefuts x
      where
        -- if we have no info about the parameter and would just print a
        -- wildcard, also show its type.
        typed_wildcard
          | prec <= sigPrec
          = underscore <+> text "::" <+> ppr (idType x)
          | otherwise
          = underscore

pprPmAltCon :: PprPrec -> PmAltCon -> [Id] -> PmPprM SDoc
pprPmAltCon _prec (PmAltLit l)      _    = pure (ppr l)
pprPmAltCon prec  (PmAltConLike cl) args = do
  delta <- ask
  pprConLike delta prec cl args

pprConLike :: Delta -> PprPrec -> ConLike -> [Id] -> PmPprM SDoc
pprConLike delta _prec cl args
  | Just pm_expr_list <- pmExprAsList delta (PmAltConLike cl) args
  = case pm_expr_list of
      NilTerminated list ->
        brackets . fsep . punctuate comma <$> mapM (pprPmVar appPrec) list
      WcVarTerminated pref x ->
        parens   . fcat . punctuate colon <$> mapM (pprPmVar appPrec) (toList pref ++ [x])
pprConLike _delta _prec (RealDataCon con) args
  | isUnboxedTupleCon con
  , let hash_parens doc = text "(#" <+> doc <+> text "#)"
  = hash_parens . fsep . punctuate comma <$> mapM (pprPmVar appPrec) args
  | isTupleDataCon con
  = parens . fsep . punctuate comma <$> mapM (pprPmVar appPrec) args
pprConLike _delta prec cl args
  | conLikeIsInfix cl = case args of
      [x, y] -> do x' <- pprPmVar funPrec x
                   y' <- pprPmVar funPrec y
                   return (cparen (prec > opPrec) (x' <+> ppr cl <+> y'))
      -- can it be infix but have more than two arguments?
      list   -> pprPanic "pprConLike:" (ppr list)
  | null args = return (ppr cl)
  | otherwise = do args' <- mapM (pprPmVar appPrec) args
                   return (cparen (prec > funPrec) (fsep (ppr cl : args')))

-- | The result of 'pmExprAsList'.
data PmExprList
  = NilTerminated [Id]
  | WcVarTerminated (NonEmpty Id) Id

-- | Extract a list of 'Id's out of a sequence of cons cells, optionally
-- terminated by a wildcard variable instead of @[]@. Some examples:
--
-- * @pmExprAsList (1:2:[]) == Just ('NilTerminated' [1,2])@, a regular,
--   @[]@-terminated list. Should be pretty-printed as @[1,2]@.
-- * @pmExprAsList (1:2:x) == Just ('WcVarTerminated' [1,2] x)@, a list prefix
--   ending in a wildcard variable x (of list type). Should be pretty-printed as
--   (1:2:_).
-- * @pmExprAsList [] == Just ('NilTerminated' [])@
pmExprAsList :: Delta -> PmAltCon -> [Id] -> Maybe PmExprList
pmExprAsList delta = go_con []
  where
    go_var rev_pref x
      | Just (alt, _tvs, args) <- lookupSolution delta x
      = go_con rev_pref alt args
    go_var rev_pref x
      | Just pref <- nonEmpty (reverse rev_pref)
      = Just (WcVarTerminated pref x)
    go_var _ _
      = Nothing

    go_con rev_pref (PmAltConLike (RealDataCon c)) es
      | c == nilDataCon
      = ASSERT( null es ) Just (NilTerminated (reverse rev_pref))
      | c == consDataCon
      = ASSERT( length es == 2 ) go_var (es !! 0 : rev_pref) (es !! 1)
    go_con _ _ _
      = Nothing
