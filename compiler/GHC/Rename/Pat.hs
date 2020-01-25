{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Renaming of patterns

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module GHC.Rename.Pat (-- main entry points
              rnPat, rnPats, rnBindPat, rnPatAndThen,

              NameMaker, applyNameMaker,     -- a utility for making names:
              localRecNameMaker, topRecNameMaker,  --   sometimes we want to make local names,
                                             --   sometimes we want to make top (qualified) names.
              isTopRecNameMaker,

              rnHsRecFields, HsRecFieldContext(..),
              rnHsRecUpdFields,

              -- CpsRn monad
              CpsRn, liftCps,

              -- Literals
              rnLit, rnOverLit,

             -- Pattern Error messages that are also used elsewhere
             checkTupSize, patSigErr
             ) where

-- ENH: thin imports to only what is necessary for patterns

import GhcPrelude

import {-# SOURCE #-} GHC.Rename.Expr ( rnLExpr )
import {-# SOURCE #-} GHC.Rename.Splice ( rnSplicePat )

#include "HsVersions.h"

import GHC.Hs
import TcRnMonad
import TcHsSyn             ( hsOverLitName )
import GHC.Rename.Env
import GHC.Rename.Fixity
import GHC.Rename.Utils    ( HsDocContext(..), newLocalBndrRn, bindLocalNames
                           , warnUnusedMatches, newLocalBndrRn
                           , checkUnusedRecordWildcard
                           , checkDupNames, checkDupAndShadowedNames
                           , checkTupSize , unknownSubordinateErr )
import GHC.Rename.Types
import PrelNames
import Name
import NameSet
import RdrName
import BasicTypes
import Util
import ListSetOps          ( removeDups )
import Outputable
import SrcLoc
import Literal             ( inCharRange )
import TysWiredIn          ( nilDataCon )
import DataCon
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad       ( when, ap, guard )
import qualified Data.List.NonEmpty as NE
import Data.Ratio

{-
*********************************************************
*                                                      *
        The CpsRn Monad
*                                                      *
*********************************************************

Note [CpsRn monad]
~~~~~~~~~~~~~~~~~~
The CpsRn monad uses continuation-passing style to support this
style of programming:

        do { ...
           ; ns <- bindNames rs
           ; ...blah... }

   where rs::[RdrName], ns::[Name]

The idea is that '...blah...'
  a) sees the bindings of ns
  b) returns the free variables it mentions
     so that bindNames can report unused ones

In particular,
    mapM rnPatAndThen [p1, p2, p3]
has a *left-to-right* scoping: it makes the binders in
p1 scope over p2,p3.
-}

newtype CpsRn b = CpsRn { unCpsRn :: forall r. (b -> RnM (r, FreeVars))
                                            -> RnM (r, FreeVars) }
        deriving (Functor)
        -- See Note [CpsRn monad]

instance Applicative CpsRn where
    pure x = CpsRn (\k -> k x)
    (<*>) = ap

instance Monad CpsRn where
  (CpsRn m) >>= mk = CpsRn (\k -> m (\v -> unCpsRn (mk v) k))

runCps :: CpsRn a -> RnM (a, FreeVars)
runCps (CpsRn m) = m (\r -> return (r, emptyFVs))

liftCps :: RnM a -> CpsRn a
liftCps rn_thing = CpsRn (\k -> rn_thing >>= k)

liftCpsFV :: RnM (a, FreeVars) -> CpsRn a
liftCpsFV rn_thing = CpsRn (\k -> do { (v,fvs1) <- rn_thing
                                     ; (r,fvs2) <- k v
                                     ; return (r, fvs1 `plusFV` fvs2) })

wrapSrcSpanCps :: (a -> CpsRn b) -> Located a -> CpsRn (Located b)
-- Set the location, and also wrap it around the value returned
wrapSrcSpanCps fn (L loc a)
  = CpsRn (\k -> setSrcSpan loc $
                 unCpsRn (fn a) $ \v ->
                 k (L loc v))

lookupConCps :: Located RdrName -> CpsRn (Located Name)
lookupConCps con_rdr
  = CpsRn (\k -> do { con_name <- lookupLocatedOccRn con_rdr
                    ; (r, fvs) <- k con_name
                    ; return (r, addOneFV fvs (unLoc con_name)) })
    -- We add the constructor name to the free vars
    -- See Note [Patterns are uses]

{-
Note [Patterns are uses]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  module Foo( f, g ) where
  data T = T1 | T2

  f T1 = True
  f T2 = False

  g _ = T1

Arguably we should report T2 as unused, even though it appears in a
pattern, because it never occurs in a constructed position.  See
#7336.
However, implementing this in the face of pattern synonyms would be
less straightforward, since given two pattern synonyms

  pattern P1 <- P2
  pattern P2 <- ()

we need to observe the dependency between P1 and P2 so that type
checking can be done in the correct order (just like for value
bindings). Dependencies between bindings is analyzed in the renamer,
where we don't know yet whether P2 is a constructor or a pattern
synonym. So for now, we do report conid occurrences in patterns as
uses.

*********************************************************
*                                                      *
        Name makers
*                                                      *
*********************************************************

Externally abstract type of name makers,
which is how you go from a RdrName to a Name
-}

data NameMaker
  = LamMk       -- Lambdas
      Bool      -- True <=> report unused bindings
                --   (even if True, the warning only comes out
                --    if -Wunused-matches is on)

  | LetMk       -- Let bindings, incl top level
                -- Do *not* check for unused bindings
      TopLevelFlag
      MiniFixityEnv

topRecNameMaker :: MiniFixityEnv -> NameMaker
topRecNameMaker fix_env = LetMk TopLevel fix_env

isTopRecNameMaker :: NameMaker -> Bool
isTopRecNameMaker (LetMk TopLevel _) = True
isTopRecNameMaker _ = False

localRecNameMaker :: MiniFixityEnv -> NameMaker
localRecNameMaker fix_env = LetMk NotTopLevel fix_env

matchNameMaker :: HsMatchContext a -> NameMaker
matchNameMaker ctxt = LamMk report_unused
  where
    -- Do not report unused names in interactive contexts
    -- i.e. when you type 'x <- e' at the GHCi prompt
    report_unused = case ctxt of
                      StmtCtxt GhciStmtCtxt -> False
                      -- also, don't warn in pattern quotes, as there
                      -- is no RHS where the variables can be used!
                      ThPatQuote            -> False
                      _                     -> True

rnHsSigCps :: LHsSigWcType GhcPs -> CpsRn (LHsSigWcType GhcRn)
rnHsSigCps sig = CpsRn (rnHsSigWcTypeScoped AlwaysBind PatCtx sig)

newPatLName :: NameMaker -> Located RdrName -> CpsRn (Located Name)
newPatLName name_maker rdr_name@(L loc _)
  = do { name <- newPatName name_maker rdr_name
       ; return (L loc name) }

newPatName :: NameMaker -> Located RdrName -> CpsRn Name
newPatName (LamMk report_unused) rdr_name
  = CpsRn (\ thing_inside ->
        do { name <- newLocalBndrRn rdr_name
           ; (res, fvs) <- bindLocalNames [name] (thing_inside name)
           ; when report_unused $ warnUnusedMatches [name] fvs
           ; return (res, name `delFV` fvs) })

newPatName (LetMk is_top fix_env) rdr_name
  = CpsRn (\ thing_inside ->
        do { name <- case is_top of
                       NotTopLevel -> newLocalBndrRn rdr_name
                       TopLevel    -> newTopSrcBinder rdr_name
           ; bindLocalNames [name] $       -- Do *not* use bindLocalNameFV here
                                        -- See Note [View pattern usage]
             addLocalFixities fix_env [name] $
             thing_inside name })

    -- Note: the bindLocalNames is somewhat suspicious
    --       because it binds a top-level name as a local name.
    --       however, this binding seems to work, and it only exists for
    --       the duration of the patterns and the continuation;
    --       then the top-level name is added to the global env
    --       before going on to the RHSes (see GHC.Rename.Source).

{-
Note [View pattern usage]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  let (r, (r -> x)) = x in ...
Here the pattern binds 'r', and then uses it *only* in the view pattern.
We want to "see" this use, and in let-bindings we collect all uses and
report unused variables at the binding level. So we must use bindLocalNames
here, *not* bindLocalNameFV.  #3943.


Note [Don't report shadowing for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is one special context where a pattern doesn't introduce any new binders -
pattern synonym declarations. Therefore we don't check to see if pattern
variables shadow existing identifiers as they are never bound to anything
and have no scope.

Without this check, there would be quite a cryptic warning that the `x`
in the RHS of the pattern synonym declaration shadowed the top level `x`.

```
x :: ()
x = ()

pattern P x = Just x
```

See #12615 for some more examples.

*********************************************************
*                                                      *
        External entry points
*                                                      *
*********************************************************

There are various entry points to renaming patterns, depending on
 (1) whether the names created should be top-level names or local names
 (2) whether the scope of the names is entirely given in a continuation
     (e.g., in a case or lambda, but not in a let or at the top-level,
      because of the way mutually recursive bindings are handled)
 (3) whether the a type signature in the pattern can bind
        lexically-scoped type variables (for unpacking existential
        type vars in data constructors)
 (4) whether we do duplicate and unused variable checking
 (5) whether there are fixity declarations associated with the names
     bound by the patterns that need to be brought into scope with them.

 Rather than burdening the clients of this module with all of these choices,
 we export the three points in this design space that we actually need:
-}

-- ----------- Entry point 1: rnPats -------------------
-- Binds local names; the scope of the bindings is entirely in the thing_inside
--   * allows type sigs to bind type vars
--   * local namemaker
--   * unused and duplicate checking
--   * no fixities
rnPats :: HsMatchContext GhcRn -- for error messages
       -> [LPat GhcPs]
       -> ([LPat GhcRn] -> RnM (a, FreeVars))
       -> RnM (a, FreeVars)
rnPats ctxt pats thing_inside
  = do  { envs_before <- getRdrEnvs

          -- (1) rename the patterns, bringing into scope all of the term variables
          -- (2) then do the thing inside.
        ; unCpsRn (rnLPatsAndThen (matchNameMaker ctxt) pats) $ \ pats' -> do
        { -- Check for duplicated and shadowed names
          -- Must do this *after* renaming the patterns
          -- See Note [Collect binders only after renaming] in GHC.Hs.Utils
          -- Because we don't bind the vars all at once, we can't
          --    check incrementally for duplicates;
          -- Nor can we check incrementally for shadowing, else we'll
          --    complain *twice* about duplicates e.g. f (x,x) = ...
          --
          -- See note [Don't report shadowing for pattern synonyms]
        ; let bndrs = collectPatsBinders pats'
        ; addErrCtxt doc_pat $
          if isPatSynCtxt ctxt
             then checkDupNames bndrs
             else checkDupAndShadowedNames envs_before bndrs
        ; thing_inside pats' } }
  where
    doc_pat = text "In" <+> pprMatchContext ctxt

rnPat :: HsMatchContext GhcRn -- for error messages
      -> LPat GhcPs
      -> (LPat GhcRn -> RnM (a, FreeVars))
      -> RnM (a, FreeVars)     -- Variables bound by pattern do not
                               -- appear in the result FreeVars
rnPat ctxt pat thing_inside
  = rnPats ctxt [pat] (\pats' -> let [pat'] = pats' in thing_inside pat')

applyNameMaker :: NameMaker -> Located RdrName -> RnM (Located Name)
applyNameMaker mk rdr = do { (n, _fvs) <- runCps (newPatLName mk rdr)
                           ; return n }

-- ----------- Entry point 2: rnBindPat -------------------
-- Binds local names; in a recursive scope that involves other bound vars
--      e.g let { (x, Just y) = e1; ... } in ...
--   * does NOT allows type sig to bind type vars
--   * local namemaker
--   * no unused and duplicate checking
--   * fixities might be coming in
rnBindPat :: NameMaker
          -> LPat GhcPs
          -> RnM (LPat GhcRn, FreeVars)
   -- Returned FreeVars are the free variables of the pattern,
   -- of course excluding variables bound by this pattern

rnBindPat name_maker pat = runCps (rnLPatAndThen name_maker pat)

{-
*********************************************************
*                                                      *
        The main event
*                                                      *
*********************************************************
-}

-- ----------- Entry point 3: rnLPatAndThen -------------------
-- General version: parametrized by how you make new names

rnLPatsAndThen :: NameMaker -> [LPat GhcPs] -> CpsRn [LPat GhcRn]
rnLPatsAndThen mk = mapM (rnLPatAndThen mk)
  -- Despite the map, the monad ensures that each pattern binds
  -- variables that may be mentioned in subsequent patterns in the list

--------------------
-- The workhorse
rnLPatAndThen :: NameMaker -> LPat GhcPs -> CpsRn (LPat GhcRn)
rnLPatAndThen nm lpat = wrapSrcSpanCps (rnPatAndThen nm) lpat

rnPatAndThen :: NameMaker -> Pat GhcPs -> CpsRn (Pat GhcRn)
rnPatAndThen _  (WildPat _)   = return (WildPat noExtField)
rnPatAndThen mk (ParPat x pat)  = do { pat' <- rnLPatAndThen mk pat
                                     ; return (ParPat x pat') }
rnPatAndThen mk (LazyPat x pat) = do { pat' <- rnLPatAndThen mk pat
                                     ; return (LazyPat x pat') }
rnPatAndThen mk (BangPat x pat) = do { pat' <- rnLPatAndThen mk pat
                                     ; return (BangPat x pat') }
rnPatAndThen mk (VarPat x (L l rdr))
    = do { loc <- liftCps getSrcSpanM
         ; name <- newPatName mk (L loc rdr)
         ; return (VarPat x (L l name)) }
     -- we need to bind pattern variables for view pattern expressions
     -- (e.g. in the pattern (x, x -> y) x needs to be bound in the rhs of the tuple)

rnPatAndThen mk (SigPat x pat sig)
  -- When renaming a pattern type signature (e.g. f (a :: T) = ...), it is
  -- important to rename its type signature _before_ renaming the rest of the
  -- pattern, so that type variables are first bound by the _outermost_ pattern
  -- type signature they occur in. This keeps the type checker happy when
  -- pattern type signatures happen to be nested (#7827)
  --
  -- f ((Just (x :: a) :: Maybe a)
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~^       `a' is first bound here
  -- ~~~~~~~~~~~~~~~^                   the same `a' then used here
  = do { sig' <- rnHsSigCps sig
       ; pat' <- rnLPatAndThen mk pat
       ; return (SigPat x pat' sig' ) }

rnPatAndThen mk (LitPat x lit)
  | HsString src s <- lit
  = do { ovlStr <- liftCps (xoptM LangExt.OverloadedStrings)
       ; if ovlStr
         then rnPatAndThen mk
                           (mkNPat (noLoc (mkHsIsString src s))
                                      Nothing)
         else normal_lit }
  | otherwise = normal_lit
  where
    normal_lit = do { liftCps (rnLit lit); return (LitPat x (convertLit lit)) }

rnPatAndThen _ (NPat x (L l lit) mb_neg _eq)
  = do { (lit', mb_neg') <- liftCpsFV $ rnOverLit lit
       ; mb_neg' -- See Note [Negative zero]
           <- let negative = do { (neg, fvs) <- lookupSyntax negateName
                                ; return (Just neg, fvs) }
                  positive = return (Nothing, emptyFVs)
              in liftCpsFV $ case (mb_neg , mb_neg') of
                                  (Nothing, Just _ ) -> negative
                                  (Just _ , Nothing) -> negative
                                  (Nothing, Nothing) -> positive
                                  (Just _ , Just _ ) -> positive
       ; eq' <- liftCpsFV $ lookupSyntax eqName
       ; return (NPat x (L l lit') mb_neg' eq') }

rnPatAndThen mk (NPlusKPat x rdr (L l lit) _ _ _ )
  = do { new_name <- newPatName mk rdr
       ; (lit', _) <- liftCpsFV $ rnOverLit lit -- See Note [Negative zero]
                                                -- We skip negateName as
                                                -- negative zero doesn't make
                                                -- sense in n + k patterns
       ; minus <- liftCpsFV $ lookupSyntax minusName
       ; ge    <- liftCpsFV $ lookupSyntax geName
       ; return (NPlusKPat x (L (nameSrcSpan new_name) new_name)
                             (L l lit') lit' ge minus) }
                -- The Report says that n+k patterns must be in Integral

rnPatAndThen mk (AsPat x rdr pat)
  = do { new_name <- newPatLName mk rdr
       ; pat' <- rnLPatAndThen mk pat
       ; return (AsPat x new_name pat') }

rnPatAndThen mk p@(ViewPat x expr pat)
  = do { liftCps $ do { vp_flag <- xoptM LangExt.ViewPatterns
                      ; checkErr vp_flag (badViewPat p) }
         -- Because of the way we're arranging the recursive calls,
         -- this will be in the right context
       ; expr' <- liftCpsFV $ rnLExpr expr
       ; pat' <- rnLPatAndThen mk pat
       -- Note: at this point the PreTcType in ty can only be a placeHolder
       -- ; return (ViewPat expr' pat' ty) }
       ; return (ViewPat x expr' pat') }

rnPatAndThen mk (ConPat con stuff NoExtField)
   -- rnConPatAndThen takes care of reconstructing the pattern
   -- The pattern for the empty list needs to be replaced by an empty explicit list pattern when overloaded lists is turned on.
  = case unLoc con == nameRdrName (dataConName nilDataCon) of
      True    -> do { ol_flag <- liftCps $ xoptM LangExt.OverloadedLists
                    ; if ol_flag then rnPatAndThen mk (ListPat noExtField [])
                                 else rnConPatAndThen mk con stuff}
      False   -> rnConPatAndThen mk con stuff

rnPatAndThen mk (ListPat _ pats)
  = do { opt_OverloadedLists <- liftCps $ xoptM LangExt.OverloadedLists
       ; pats' <- rnLPatsAndThen mk pats
       ; case opt_OverloadedLists of
          True -> do { (to_list_name,_) <- liftCps $ lookupSyntax toListName
                     ; return (ListPat (Just to_list_name) pats')}
          False -> return (ListPat Nothing pats') }

rnPatAndThen mk (TuplePat x pats boxed)
  = do { liftCps $ checkTupSize (length pats)
       ; pats' <- rnLPatsAndThen mk pats
       ; return (TuplePat x pats' boxed) }

rnPatAndThen mk (SumPat x pat alt arity)
  = do { pat <- rnLPatAndThen mk pat
       ; return (SumPat x pat alt arity)
       }

-- If a splice has been run already, just rename the result.
rnPatAndThen mk (SplicePat x (HsSpliced x2 mfs (HsSplicedPat pat)))
  = SplicePat x . HsSpliced x2 mfs . HsSplicedPat <$> rnPatAndThen mk pat

rnPatAndThen mk (SplicePat _ splice)
  = do { eith <- liftCpsFV $ rnSplicePat splice
       ; case eith of   -- See Note [rnSplicePat] in GHC.Rename.Splice
           Left  not_yet_renamed -> rnPatAndThen mk not_yet_renamed
           Right already_renamed -> return already_renamed }

rnPatAndThen _ pat = pprPanic "rnLPatAndThen" (ppr pat)


--------------------
rnConPatAndThen :: NameMaker
                -> Located RdrName    -- the constructor
                -> HsConPatDetails GhcPs
                -> CpsRn (Pat GhcRn)

rnConPatAndThen mk con (PrefixCon pats)
  = do  { con' <- lookupConCps con
        ; pats' <- rnLPatsAndThen mk pats
        ; return (ConPat con' (PrefixCon pats') NoExtField) }

rnConPatAndThen mk con (InfixCon pat1 pat2)
  = do  { con' <- lookupConCps con
        ; pat1' <- rnLPatAndThen mk pat1
        ; pat2' <- rnLPatAndThen mk pat2
        ; fixity <- liftCps $ lookupFixityRn (unLoc con')
        ; liftCps $ mkConOpPatRn con' fixity pat1' pat2' }

rnConPatAndThen mk con (RecCon rpats)
  = do  { con' <- lookupConCps con
        ; rpats' <- rnHsRecPatsAndThen mk con' rpats
        ; return (ConPat con' (RecCon rpats') NoExtField) }

checkUnusedRecordWildcardCps :: SrcSpan -> Maybe [Name] -> CpsRn ()
checkUnusedRecordWildcardCps loc dotdot_names =
  CpsRn (\thing -> do
                    (r, fvs) <- thing ()
                    checkUnusedRecordWildcard loc fvs dotdot_names
                    return (r, fvs) )
--------------------
rnHsRecPatsAndThen :: NameMaker
                   -> Located Name      -- Constructor
                   -> HsRecFields GhcPs (LPat GhcPs)
                   -> CpsRn (HsRecFields GhcRn (LPat GhcRn))
rnHsRecPatsAndThen mk (L _ con)
     hs_rec_fields@(HsRecFields { rec_dotdot = dd })
  = do { flds <- liftCpsFV $ rnHsRecFields (HsRecFieldPat con) mkVarPat
                                            hs_rec_fields
       ; flds' <- mapM rn_field (flds `zip` [1..])
       ; check_unused_wildcard (implicit_binders flds' <$> dd)
       ; return (HsRecFields { rec_flds = flds', rec_dotdot = dd }) }
  where
    mkVarPat l n = VarPat noExtField (L l n)
    rn_field (L l fld, n') =
      do { arg' <- rnLPatAndThen (nested_mk dd mk n') (hsRecFieldArg fld)
         ; return (L l (fld { hsRecFieldArg = arg' })) }

    loc = maybe noSrcSpan getLoc dd

    -- Get the arguments of the implicit binders
    implicit_binders fs (unLoc -> n) = collectPatsBinders implicit_pats
      where
        implicit_pats = map (hsRecFieldArg . unLoc) (drop n fs)

    -- Don't warn for let P{..} = ... in ...
    check_unused_wildcard = case mk of
                              LetMk{} -> const (return ())
                              LamMk{} -> checkUnusedRecordWildcardCps loc

        -- Suppress unused-match reporting for fields introduced by ".."
    nested_mk Nothing  mk                    _  = mk
    nested_mk (Just _) mk@(LetMk {})         _  = mk
    nested_mk (Just (unLoc -> n)) (LamMk report_unused) n'
      = LamMk (report_unused && (n' <= n))

{-
************************************************************************
*                                                                      *
        Record fields
*                                                                      *
************************************************************************
-}

data HsRecFieldContext
  = HsRecFieldCon Name
  | HsRecFieldPat Name
  | HsRecFieldUpd

rnHsRecFields
    :: forall arg.
       HsRecFieldContext
    -> (SrcSpan -> RdrName -> arg)
         -- When punning, use this to build a new field
    -> HsRecFields GhcPs (Located arg)
    -> RnM ([LHsRecField GhcRn (Located arg)], FreeVars)

-- This surprisingly complicated pass
--   a) looks up the field name (possibly using disambiguation)
--   b) fills in puns and dot-dot stuff
-- When we've finished, we've renamed the LHS, but not the RHS,
-- of each x=e binding
--
-- This is used for record construction and pattern-matching, but not updates.

rnHsRecFields ctxt mk_arg (HsRecFields { rec_flds = flds, rec_dotdot = dotdot })
  = do { pun_ok      <- xoptM LangExt.RecordPuns
       ; disambig_ok <- xoptM LangExt.DisambiguateRecordFields
       ; let parent = guard disambig_ok >> mb_con
       ; flds1  <- mapM (rn_fld pun_ok parent) flds
       ; mapM_ (addErr . dupFieldErr ctxt) dup_flds
       ; dotdot_flds <- rn_dotdot dotdot mb_con flds1
       ; let all_flds | null dotdot_flds = flds1
                      | otherwise        = flds1 ++ dotdot_flds
       ; return (all_flds, mkFVs (getFieldIds all_flds)) }
  where
    mb_con = case ctxt of
                HsRecFieldCon con  -> Just con
                HsRecFieldPat con  -> Just con
                _ {- update -}     -> Nothing

    rn_fld :: Bool -> Maybe Name -> LHsRecField GhcPs (Located arg)
           -> RnM (LHsRecField GhcRn (Located arg))
    rn_fld pun_ok parent (L l
                           (HsRecField
                              { hsRecFieldLbl =
                                  (L loc (FieldOcc _ (L ll lbl)))
                              , hsRecFieldArg = arg
                              , hsRecPun      = pun }))
      = do { sel <- setSrcSpan loc $ lookupRecFieldOcc parent lbl
           ; arg' <- if pun
                     then do { checkErr pun_ok (badPun (L loc lbl))
                               -- Discard any module qualifier (#11662)
                             ; let arg_rdr = mkRdrUnqual (rdrNameOcc lbl)
                             ; return (L loc (mk_arg loc arg_rdr)) }
                     else return arg
           ; return (L l (HsRecField
                             { hsRecFieldLbl = (L loc (FieldOcc
                                                          sel (L ll lbl)))
                             , hsRecFieldArg = arg'
                             , hsRecPun      = pun })) }
    rn_fld _ _ (L _ (HsRecField (L _ (XFieldOcc _)) _ _))
      = panic "rnHsRecFields"


    rn_dotdot :: Maybe (Located Int)      -- See Note [DotDot fields] in GHC.Hs.Pat
              -> Maybe Name -- The constructor (Nothing for an
                                --    out of scope constructor)
              -> [LHsRecField GhcRn (Located arg)] -- Explicit fields
              -> RnM ([LHsRecField GhcRn (Located arg)])   -- Field Labels we need to fill in
    rn_dotdot (Just (L loc n)) (Just con) flds -- ".." on record construction / pat match
      | not (isUnboundName con) -- This test is because if the constructor
                                -- isn't in scope the constructor lookup will add
                                -- an error but still return an unbound name. We
                                -- don't want that to screw up the dot-dot fill-in stuff.
      = ASSERT( flds `lengthIs` n )
        do { dd_flag <- xoptM LangExt.RecordWildCards
           ; checkErr dd_flag (needFlagDotDot ctxt)
           ; (rdr_env, lcl_env) <- getRdrEnvs
           ; con_fields <- lookupConstructorFields con
           ; when (null con_fields) (addErr (badDotDotCon con))
           ; let present_flds = mkOccSet $ map rdrNameOcc (getFieldLbls flds)

                   -- For constructor uses (but not patterns)
                   -- the arg should be in scope locally;
                   -- i.e. not top level or imported
                   -- Eg.  data R = R { x,y :: Int }
                   --      f x = R { .. }   -- Should expand to R {x=x}, not R{x=x,y=y}
                 arg_in_scope lbl = mkRdrUnqual lbl `elemLocalRdrEnv` lcl_env

                 (dot_dot_fields, dot_dot_gres)
                        = unzip [ (fl, gre)
                                | fl <- con_fields
                                , let lbl = mkVarOccFS (flLabel fl)
                                , not (lbl `elemOccSet` present_flds)
                                , Just gre <- [lookupGRE_FieldLabel rdr_env fl]
                                              -- Check selector is in scope
                                , case ctxt of
                                    HsRecFieldCon {} -> arg_in_scope lbl
                                    _other           -> True ]

           ; addUsedGREs dot_dot_gres
           ; return [ L loc (HsRecField
                        { hsRecFieldLbl = L loc (FieldOcc sel (L loc arg_rdr))
                        , hsRecFieldArg = L loc (mk_arg loc arg_rdr)
                        , hsRecPun      = False })
                    | fl <- dot_dot_fields
                    , let sel     = flSelector fl
                    , let arg_rdr = mkVarUnqual (flLabel fl) ] }

    rn_dotdot _dotdot _mb_con _flds
      = return []
      -- _dotdot = Nothing => No ".." at all
      -- _mb_con = Nothing => Record update
      -- _mb_con = Just unbound => Out of scope data constructor

    dup_flds :: [NE.NonEmpty RdrName]
        -- Each list represents a RdrName that occurred more than once
        -- (the list contains all occurrences)
        -- Each list in dup_fields is non-empty
    (_, dup_flds) = removeDups compare (getFieldLbls flds)


-- NB: Consider this:
--      module Foo where { data R = R { fld :: Int } }
--      module Odd where { import Foo; fld x = x { fld = 3 } }
-- Arguably this should work, because the reference to 'fld' is
-- unambiguous because there is only one field id 'fld' in scope.
-- But currently it's rejected.

rnHsRecUpdFields
    :: [LHsRecUpdField GhcPs]
    -> RnM ([LHsRecUpdField GhcRn], FreeVars)
rnHsRecUpdFields flds
  = do { pun_ok        <- xoptM LangExt.RecordPuns
       ; overload_ok   <- xoptM LangExt.DuplicateRecordFields
       ; (flds1, fvss) <- mapAndUnzipM (rn_fld pun_ok overload_ok) flds
       ; mapM_ (addErr . dupFieldErr HsRecFieldUpd) dup_flds

       -- Check for an empty record update  e {}
       -- NB: don't complain about e { .. }, because rn_dotdot has done that already
       ; when (null flds) $ addErr emptyUpdateErr

       ; return (flds1, plusFVs fvss) }
  where
    doc = text "constructor field name"

    rn_fld :: Bool -> Bool -> LHsRecUpdField GhcPs
           -> RnM (LHsRecUpdField GhcRn, FreeVars)
    rn_fld pun_ok overload_ok (L l (HsRecField { hsRecFieldLbl = L loc f
                                               , hsRecFieldArg = arg
                                               , hsRecPun      = pun }))
      = do { let lbl = rdrNameAmbiguousFieldOcc f
           ; sel <- setSrcSpan loc $
                      -- Defer renaming of overloaded fields to the typechecker
                      -- See Note [Disambiguating record fields] in TcExpr
                      if overload_ok
                          then do { mb <- lookupGlobalOccRn_overloaded
                                            overload_ok lbl
                                  ; case mb of
                                      Nothing ->
                                        do { addErr
                                               (unknownSubordinateErr doc lbl)
                                           ; return (Right []) }
                                      Just r  -> return r }
                          else fmap Left $ lookupGlobalOccRn lbl
           ; arg' <- if pun
                     then do { checkErr pun_ok (badPun (L loc lbl))
                               -- Discard any module qualifier (#11662)
                             ; let arg_rdr = mkRdrUnqual (rdrNameOcc lbl)
                             ; return (L loc (HsVar noExtField (L loc arg_rdr))) }
                     else return arg
           ; (arg'', fvs) <- rnLExpr arg'

           ; let fvs' = case sel of
                          Left sel_name -> fvs `addOneFV` sel_name
                          Right [sel_name] -> fvs `addOneFV` sel_name
                          Right _       -> fvs
                 lbl' = case sel of
                          Left sel_name ->
                                     L loc (Unambiguous sel_name   (L loc lbl))
                          Right [sel_name] ->
                                     L loc (Unambiguous sel_name   (L loc lbl))
                          Right _ -> L loc (Ambiguous   noExtField (L loc lbl))

           ; return (L l (HsRecField { hsRecFieldLbl = lbl'
                                     , hsRecFieldArg = arg''
                                     , hsRecPun      = pun }), fvs') }

    dup_flds :: [NE.NonEmpty RdrName]
        -- Each list represents a RdrName that occurred more than once
        -- (the list contains all occurrences)
        -- Each list in dup_fields is non-empty
    (_, dup_flds) = removeDups compare (getFieldUpdLbls flds)



getFieldIds :: [LHsRecField GhcRn arg] -> [Name]
getFieldIds flds = map (unLoc . hsRecFieldSel . unLoc) flds

getFieldLbls :: [LHsRecField id arg] -> [RdrName]
getFieldLbls flds
  = map (unLoc . rdrNameFieldOcc . unLoc . hsRecFieldLbl . unLoc) flds

getFieldUpdLbls :: [LHsRecUpdField GhcPs] -> [RdrName]
getFieldUpdLbls flds = map (rdrNameAmbiguousFieldOcc . unLoc . hsRecFieldLbl . unLoc) flds

needFlagDotDot :: HsRecFieldContext -> SDoc
needFlagDotDot ctxt = vcat [text "Illegal `..' in record" <+> pprRFC ctxt,
                            text "Use RecordWildCards to permit this"]

badDotDotCon :: Name -> SDoc
badDotDotCon con
  = vcat [ text "Illegal `..' notation for constructor" <+> quotes (ppr con)
         , nest 2 (text "The constructor has no labelled fields") ]

emptyUpdateErr :: SDoc
emptyUpdateErr = text "Empty record update"

badPun :: Located RdrName -> SDoc
badPun fld = vcat [text "Illegal use of punning for field" <+> quotes (ppr fld),
                   text "Use NamedFieldPuns to permit this"]

dupFieldErr :: HsRecFieldContext -> NE.NonEmpty RdrName -> SDoc
dupFieldErr ctxt dups
  = hsep [text "duplicate field name",
          quotes (ppr (NE.head dups)),
          text "in record", pprRFC ctxt]

pprRFC :: HsRecFieldContext -> SDoc
pprRFC (HsRecFieldCon {}) = text "construction"
pprRFC (HsRecFieldPat {}) = text "pattern"
pprRFC (HsRecFieldUpd {}) = text "update"

{-
************************************************************************
*                                                                      *
\subsubsection{Literals}
*                                                                      *
************************************************************************

When literals occur we have to make sure
that the types and classes they involve
are made available.
-}

rnLit :: HsLit p -> RnM ()
rnLit (HsChar _ c) = checkErr (inCharRange c) (bogusCharError c)
rnLit _ = return ()

-- Turn a Fractional-looking literal which happens to be an integer into an
-- Integer-looking literal.
generalizeOverLitVal :: OverLitVal -> OverLitVal
generalizeOverLitVal (HsFractional (FL {fl_text=src,fl_neg=neg,fl_value=val}))
    | denominator val == 1 = HsIntegral (IL { il_text=src
                                            , il_neg=neg
                                            , il_value=numerator val})
generalizeOverLitVal lit = lit

isNegativeZeroOverLit :: HsOverLit t -> Bool
isNegativeZeroOverLit lit
 = case ol_val lit of
        HsIntegral i   -> 0 == il_value i && il_neg i
        HsFractional f -> 0 == fl_value f && fl_neg f
        _              -> False

{-
Note [Negative zero]
~~~~~~~~~~~~~~~~~~~~~~~~~
There were problems with negative zero in conjunction with Negative Literals
extension. Numeric literal value is contained in Integer and Rational types
inside IntegralLit and FractionalLit. These types cannot represent negative
zero value. So we had to add explicit field 'neg' which would hold information
about literal sign. Here in rnOverLit we use it to detect negative zeroes and
in this case return not only literal itself but also negateName so that users
can apply it explicitly. In this case it stays negative zero.  #13211
-}

rnOverLit :: HsOverLit t ->
             RnM ((HsOverLit GhcRn, Maybe (HsExpr GhcRn)), FreeVars)
rnOverLit origLit
  = do  { opt_NumDecimals <- xoptM LangExt.NumDecimals
        ; let { lit@(OverLit {ol_val=val})
            | opt_NumDecimals = origLit {ol_val = generalizeOverLitVal (ol_val origLit)}
            | otherwise       = origLit
          }
        ; let std_name = hsOverLitName val
        ; (from_thing_name, fvs1) <- lookupSyntaxName std_name
        ; let rebindable = from_thing_name /= std_name
              lit' = lit { ol_witness = nl_HsVar from_thing_name
                         , ol_ext = rebindable }
        ; if isNegativeZeroOverLit lit'
          then do { (negate_name, fvs2) <- lookupSyntaxExpr negateName
                  ; return ((lit' { ol_val = negateOverLitVal val }, Just negate_name)
                                  , fvs1 `plusFV` fvs2) }
          else return ((lit', Nothing), fvs1) }

{-
************************************************************************
*                                                                      *
\subsubsection{Errors}
*                                                                      *
************************************************************************
-}

patSigErr :: Outputable a => a -> SDoc
patSigErr ty
  =  (text "Illegal signature in pattern:" <+> ppr ty)
        $$ nest 4 (text "Use ScopedTypeVariables to permit it")

bogusCharError :: Char -> SDoc
bogusCharError c
  = text "character literal out of range: '\\" <> char c  <> char '\''

badViewPat :: Pat GhcPs -> SDoc
badViewPat pat = vcat [text "Illegal view pattern: " <+> ppr pat,
                       text "Use ViewPatterns to enable view patterns"]
