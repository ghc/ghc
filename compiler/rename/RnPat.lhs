%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnPat]{Renaming of patterns}

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

{-# LANGUAGE ScopedTypeVariables #-}
module RnPat (-- main entry points
              rnPat, rnPats, rnBindPat,

              NameMaker, applyNameMaker,     -- a utility for making names:
              localRecNameMaker, topRecNameMaker,  --   sometimes we want to make local names,
                                             --   sometimes we want to make top (qualified) names.

              rnHsRecFields1, HsRecFieldContext(..),

              -- Literals
              rnLit, rnOverLit,     

             -- Pattern Error messages that are also used elsewhere
             checkTupSize, patSigErr
             ) where

-- ENH: thin imports to only what is necessary for patterns

import {-# SOURCE #-} RnExpr ( rnLExpr )
#ifdef GHCI
import {-# SOURCE #-} TcSplice ( runQuasiQuotePat )
#endif  /* GHCI */

#include "HsVersions.h"

import HsSyn            
import TcRnMonad
import TcHsSyn             ( hsOverLitName )
import RnEnv
import RnTypes
import DynFlags
import PrelNames
import Name
import NameSet
import RdrName
import BasicTypes
import Util
import ListSetOps          ( removeDups )
import Outputable
import SrcLoc
import FastString
import Literal             ( inCharRange )
import TysWiredIn          ( nilDataCon )
import DataCon             ( dataConName )
import Control.Monad       ( when, liftM, ap )
import Data.Ratio
\end{code}


%*********************************************************
%*                                                      *
        The CpsRn Monad
%*                                                      *
%*********************************************************

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

\begin{code}
newtype CpsRn b = CpsRn { unCpsRn :: forall r. (b -> RnM (r, FreeVars))
                                            -> RnM (r, FreeVars) }
        -- See Note [CpsRn monad]

instance Functor CpsRn where
    fmap = liftM

instance Applicative CpsRn where
    pure = return
    (<*>) = ap

instance Monad CpsRn where
  return x = CpsRn (\k -> k x)
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
                    ; k con_name })
    -- We do not add the constructor name to the free vars
    -- See Note [Patterns are not uses]
\end{code}

Note [Patterns are not uses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  module Foo( f, g ) where
  data T = T1 | T2

  f T1 = True
  f T2 = False

  g _ = T1

Arguaby we should report T2 as unused, even though it appears in a
pattern, because it never occurs in a constructed position.  See
Trac #7336.

%*********************************************************
%*                                                      *
        Name makers
%*                                                      *
%*********************************************************

Externally abstract type of name makers,
which is how you go from a RdrName to a Name

\begin{code}
data NameMaker 
  = LamMk       -- Lambdas 
      Bool      -- True <=> report unused bindings
                --   (even if True, the warning only comes out 
                --    if -fwarn-unused-matches is on)

  | LetMk       -- Let bindings, incl top level
                -- Do *not* check for unused bindings
      TopLevelFlag
      MiniFixityEnv

topRecNameMaker :: MiniFixityEnv -> NameMaker
topRecNameMaker fix_env = LetMk TopLevel fix_env

localRecNameMaker :: MiniFixityEnv -> NameMaker
localRecNameMaker fix_env = LetMk NotTopLevel fix_env 

matchNameMaker :: HsMatchContext a -> NameMaker
matchNameMaker ctxt = LamMk report_unused
  where
    -- Do not report unused names in interactive contexts
    -- i.e. when you type 'x <- e' at the GHCi prompt
    report_unused = case ctxt of
                      StmtCtxt GhciStmtCtxt -> False
                      _                     -> True

rnHsSigCps :: HsWithBndrs (LHsType RdrName) -> CpsRn (HsWithBndrs (LHsType Name))
rnHsSigCps sig 
  = CpsRn (rnHsBndrSig PatCtx sig)

newPatName :: NameMaker -> Located RdrName -> CpsRn Name
newPatName (LamMk report_unused) rdr_name
  = CpsRn (\ thing_inside -> 
        do { name <- newLocalBndrRn rdr_name
           ; (res, fvs) <- bindLocalName name (thing_inside name)
           ; when report_unused $ warnUnusedMatches [name] fvs
           ; return (res, name `delFV` fvs) })

newPatName (LetMk is_top fix_env) rdr_name
  = CpsRn (\ thing_inside -> 
        do { name <- case is_top of
                       NotTopLevel -> newLocalBndrRn rdr_name
                       TopLevel    -> newTopSrcBinder rdr_name
           ; bindLocalName name $       -- Do *not* use bindLocalNameFV here
                                        -- See Note [View pattern usage]
             addLocalFixities fix_env [name] $
             thing_inside name })
                          
    -- Note: the bindLocalName is somewhat suspicious
    --       because it binds a top-level name as a local name.
    --       however, this binding seems to work, and it only exists for
    --       the duration of the patterns and the continuation;
    --       then the top-level name is added to the global env
    --       before going on to the RHSes (see RnSource.lhs).
\end{code}

Note [View pattern usage]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  let (r, (r -> x)) = x in ...
Here the pattern binds 'r', and then uses it *only* in the view pattern.
We want to "see" this use, and in let-bindings we collect all uses and
report unused variables at the binding level. So we must use bindLocalName
here, *not* bindLocalNameFV.  Trac #3943.

%*********************************************************
%*                                                      *
        External entry points
%*                                                      *
%*********************************************************

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

\begin{code}
-- ----------- Entry point 1: rnPats -------------------
-- Binds local names; the scope of the bindings is entirely in the thing_inside
--   * allows type sigs to bind type vars
--   * local namemaker
--   * unused and duplicate checking
--   * no fixities
rnPats :: HsMatchContext Name -- for error messages
       -> [LPat RdrName] 
       -> ([LPat Name] -> RnM (a, FreeVars))
       -> RnM (a, FreeVars)
rnPats ctxt pats thing_inside
  = do  { envs_before <- getRdrEnvs

          -- (1) rename the patterns, bringing into scope all of the term variables
          -- (2) then do the thing inside.
        ; unCpsRn (rnLPatsAndThen (matchNameMaker ctxt) pats) $ \ pats' -> do
        { -- Check for duplicated and shadowed names 
          -- Must do this *after* renaming the patterns
          -- See Note [Collect binders only after renaming] in HsUtils
          -- Because we don't bind the vars all at once, we can't
          --    check incrementally for duplicates; 
          -- Nor can we check incrementally for shadowing, else we'll
          --    complain *twice* about duplicates e.g. f (x,x) = ...
        ; addErrCtxt doc_pat $ 
          checkDupAndShadowedNames envs_before $
          collectPatsBinders pats'
        ; thing_inside pats' } }
  where
    doc_pat = ptext (sLit "In") <+> pprMatchContext ctxt

rnPat :: HsMatchContext Name -- for error messages
      -> LPat RdrName 
      -> (LPat Name -> RnM (a, FreeVars))
      -> RnM (a, FreeVars)     -- Variables bound by pattern do not 
                               -- appear in the result FreeVars 
rnPat ctxt pat thing_inside 
  = rnPats ctxt [pat] (\pats' -> let [pat'] = pats' in thing_inside pat')

applyNameMaker :: NameMaker -> Located RdrName -> RnM Name
applyNameMaker mk rdr = do { (n, _fvs) <- runCps (newPatName mk rdr); return n }

-- ----------- Entry point 2: rnBindPat -------------------
-- Binds local names; in a recursive scope that involves other bound vars
--      e.g let { (x, Just y) = e1; ... } in ...
--   * does NOT allows type sig to bind type vars
--   * local namemaker
--   * no unused and duplicate checking
--   * fixities might be coming in
rnBindPat :: NameMaker
          -> LPat RdrName
          -> RnM (LPat Name, FreeVars)
   -- Returned FreeVars are the free variables of the pattern,
   -- of course excluding variables bound by this pattern 

rnBindPat name_maker pat = runCps (rnLPatAndThen name_maker pat)
\end{code}


%*********************************************************
%*                                                      *
        The main event
%*                                                      *
%*********************************************************

\begin{code}
-- ----------- Entry point 3: rnLPatAndThen -------------------
-- General version: parametrized by how you make new names

rnLPatsAndThen :: NameMaker -> [LPat RdrName] -> CpsRn [LPat Name]
rnLPatsAndThen mk = mapM (rnLPatAndThen mk)
  -- Despite the map, the monad ensures that each pattern binds
  -- variables that may be mentioned in subsequent patterns in the list

--------------------
-- The workhorse
rnLPatAndThen :: NameMaker -> LPat RdrName -> CpsRn (LPat Name)
rnLPatAndThen nm lpat = wrapSrcSpanCps (rnPatAndThen nm) lpat

rnPatAndThen :: NameMaker -> Pat RdrName -> CpsRn (Pat Name)
rnPatAndThen _  (WildPat _)   = return (WildPat placeHolderType)
rnPatAndThen mk (ParPat pat)  = do { pat' <- rnLPatAndThen mk pat; return (ParPat pat') }
rnPatAndThen mk (LazyPat pat) = do { pat' <- rnLPatAndThen mk pat; return (LazyPat pat') }
rnPatAndThen mk (BangPat pat) = do { pat' <- rnLPatAndThen mk pat; return (BangPat pat') }
rnPatAndThen mk (VarPat rdr)  = do { loc <- liftCps getSrcSpanM
                                   ; name <- newPatName mk (L loc rdr)
                                   ; return (VarPat name) }
     -- we need to bind pattern variables for view pattern expressions
     -- (e.g. in the pattern (x, x -> y) x needs to be bound in the rhs of the tuple)
                                     
rnPatAndThen mk (SigPatIn pat sig)
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
       ; return (SigPatIn pat' sig') }
       
rnPatAndThen mk (LitPat lit)
  | HsString s <- lit
  = do { ovlStr <- liftCps (xoptM Opt_OverloadedStrings)
       ; if ovlStr 
         then rnPatAndThen mk (mkNPat (mkHsIsString s placeHolderType) Nothing)
         else normal_lit }
  | otherwise = normal_lit
  where
    normal_lit = do { liftCps (rnLit lit); return (LitPat lit) }

rnPatAndThen _ (NPat lit mb_neg _eq)
  = do { lit'    <- liftCpsFV $ rnOverLit lit
       ; mb_neg' <- liftCpsFV $ case mb_neg of
                      Nothing -> return (Nothing, emptyFVs)
                      Just _  -> do { (neg, fvs) <- lookupSyntaxName negateName
                                    ; return (Just neg, fvs) }
       ; eq' <- liftCpsFV $ lookupSyntaxName eqName
       ; return (NPat lit' mb_neg' eq') }

rnPatAndThen mk (NPlusKPat rdr lit _ _)
  = do { new_name <- newPatName mk rdr
       ; lit'  <- liftCpsFV $ rnOverLit lit
       ; minus <- liftCpsFV $ lookupSyntaxName minusName
       ; ge    <- liftCpsFV $ lookupSyntaxName geName
       ; return (NPlusKPat (L (nameSrcSpan new_name) new_name) lit' ge minus) }
                -- The Report says that n+k patterns must be in Integral

rnPatAndThen mk (AsPat rdr pat)
  = do { new_name <- newPatName mk rdr
       ; pat' <- rnLPatAndThen mk pat
       ; return (AsPat (L (nameSrcSpan new_name) new_name) pat') }

rnPatAndThen mk p@(ViewPat expr pat ty)
  = do { liftCps $ do { vp_flag <- xoptM Opt_ViewPatterns
                      ; checkErr vp_flag (badViewPat p) }
         -- Because of the way we're arranging the recursive calls,
         -- this will be in the right context 
       ; expr' <- liftCpsFV $ rnLExpr expr 
       ; pat' <- rnLPatAndThen mk pat
       ; return (ViewPat expr' pat' ty) }

rnPatAndThen mk (ConPatIn con stuff)
   -- rnConPatAndThen takes care of reconstructing the pattern
   -- The pattern for the empty list needs to be replaced by an empty explicit list pattern when overloaded lists is turned on.
  = case unLoc con == nameRdrName (dataConName nilDataCon) of
      True    -> do { ol_flag <- liftCps $ xoptM Opt_OverloadedLists
                    ; if ol_flag then rnPatAndThen mk (ListPat [] placeHolderType Nothing)
                                 else rnConPatAndThen mk con stuff} 
      False   -> rnConPatAndThen mk con stuff

rnPatAndThen mk (ListPat pats _ _)
  = do { opt_OverloadedLists <- liftCps $ xoptM Opt_OverloadedLists
       ; pats' <- rnLPatsAndThen mk pats
       ; case opt_OverloadedLists of
          True -> do   { (to_list_name,_) <- liftCps $ lookupSyntaxName toListName
                       ; return (ListPat pats' placeHolderType (Just (placeHolderType, to_list_name)))}
          False -> return (ListPat pats' placeHolderType Nothing) }

rnPatAndThen mk (PArrPat pats _)
  = do { pats' <- rnLPatsAndThen mk pats
       ; return (PArrPat pats' placeHolderType) }

rnPatAndThen mk (TuplePat pats boxed _)
  = do { liftCps $ checkTupSize (length pats)
       ; pats' <- rnLPatsAndThen mk pats
       ; return (TuplePat pats' boxed placeHolderType) }

#ifndef GHCI
rnPatAndThen _ p@(QuasiQuotePat {}) 
  = pprPanic "Can't do QuasiQuotePat without GHCi" (ppr p)
#else
rnPatAndThen mk (QuasiQuotePat qq)
  = do { pat <- liftCps $ runQuasiQuotePat qq
         -- Wrap the result of the quasi-quoter in parens so that we don't
         -- lose the outermost location set by runQuasiQuote (#7918) 
       ; rnPatAndThen mk (ParPat pat) }
#endif  /* GHCI */

rnPatAndThen _ pat = pprPanic "rnLPatAndThen" (ppr pat)


--------------------
rnConPatAndThen :: NameMaker
                -> Located RdrName          -- the constructor
                -> HsConPatDetails RdrName 
                -> CpsRn (Pat Name)

rnConPatAndThen mk con (PrefixCon pats)
  = do  { con' <- lookupConCps con
        ; pats' <- rnLPatsAndThen mk pats
        ; return (ConPatIn con' (PrefixCon pats')) }

rnConPatAndThen mk con (InfixCon pat1 pat2)
  = do  { con' <- lookupConCps con
        ; pat1' <- rnLPatAndThen mk pat1
        ; pat2' <- rnLPatAndThen mk pat2
        ; fixity <- liftCps $ lookupFixityRn (unLoc con')
        ; liftCps $ mkConOpPatRn con' fixity pat1' pat2' }

rnConPatAndThen mk con (RecCon rpats)
  = do  { con' <- lookupConCps con
        ; rpats' <- rnHsRecPatsAndThen mk con' rpats
        ; return (ConPatIn con' (RecCon rpats')) }

--------------------
rnHsRecPatsAndThen :: NameMaker
                   -> Located Name      -- Constructor
                   -> HsRecFields RdrName (LPat RdrName)
                   -> CpsRn (HsRecFields Name (LPat Name))
rnHsRecPatsAndThen mk (L _ con) hs_rec_fields@(HsRecFields { rec_dotdot = dd })
  = do { flds <- liftCpsFV $ rnHsRecFields1 (HsRecFieldPat con) VarPat hs_rec_fields
       ; flds' <- mapM rn_field (flds `zip` [1..])
       ; return (HsRecFields { rec_flds = flds', rec_dotdot = dd }) }
  where 
    rn_field (fld, n') = do { arg' <- rnLPatAndThen (nested_mk dd mk n') 
                                                    (hsRecFieldArg fld)
                            ; return (fld { hsRecFieldArg = arg' }) }

        -- Suppress unused-match reporting for fields introduced by ".."
    nested_mk Nothing  mk                    _  = mk
    nested_mk (Just _) mk@(LetMk {})         _  = mk
    nested_mk (Just n) (LamMk report_unused) n' = LamMk (report_unused && (n' <= n))
\end{code}


%************************************************************************
%*                                                                      *
        Record fields
%*                                                                      *
%************************************************************************

\begin{code}
data HsRecFieldContext 
  = HsRecFieldCon Name
  | HsRecFieldPat Name
  | HsRecFieldUpd

rnHsRecFields1 
    :: forall arg. 
       HsRecFieldContext
    -> (RdrName -> arg) -- When punning, use this to build a new field
    -> HsRecFields RdrName (Located arg)
    -> RnM ([HsRecField Name (Located arg)], FreeVars)

-- This supprisingly complicated pass
--   a) looks up the field name (possibly using disambiguation)
--   b) fills in puns and dot-dot stuff
-- When we we've finished, we've renamed the LHS, but not the RHS,
-- of each x=e binding

rnHsRecFields1 ctxt mk_arg (HsRecFields { rec_flds = flds, rec_dotdot = dotdot })
  = do { pun_ok      <- xoptM Opt_RecordPuns
       ; disambig_ok <- xoptM Opt_DisambiguateRecordFields
       ; parent <- check_disambiguation disambig_ok mb_con
       ; flds1 <- mapM (rn_fld pun_ok parent) flds
       ; mapM_ (addErr . dupFieldErr ctxt) dup_flds
       ; dotdot_flds <- rn_dotdot dotdot mb_con flds1
       ; let all_flds | null dotdot_flds = flds1
                      | otherwise        = flds1 ++ dotdot_flds
       ; return (all_flds, mkFVs (getFieldIds all_flds)) }
  where
    mb_con = case ctxt of
                HsRecFieldCon con | not (isUnboundName con) -> Just con
                HsRecFieldPat con | not (isUnboundName con) -> Just con
                _other -> Nothing
           -- The unbound name test is because if the constructor 
           -- isn't in scope the constructor lookup will add an error
           -- add an error, but still return an unbound name. 
           -- We don't want that to screw up the dot-dot fill-in stuff.

    doc = case mb_con of
            Nothing  -> ptext (sLit "constructor field name")
            Just con -> ptext (sLit "field of constructor") <+> quotes (ppr con)

    rn_fld pun_ok parent (HsRecField { hsRecFieldId = fld
                                     , hsRecFieldArg = arg
                                     , hsRecPun = pun })
      = do { fld'@(L loc fld_nm) <- wrapLocM (lookupSubBndrOcc True parent doc) fld
           ; arg' <- if pun 
                     then do { checkErr pun_ok (badPun fld)
                             ; return (L loc (mk_arg (mkRdrUnqual (nameOccName fld_nm)))) }
                     else return arg
           ; return (HsRecField { hsRecFieldId = fld'
                                , hsRecFieldArg = arg'
                                , hsRecPun = pun }) }

    rn_dotdot :: Maybe Int      -- See Note [DotDot fields] in HsPat
              -> Maybe Name     -- The constructor (Nothing for an update
                                --    or out of scope constructor)
              -> [HsRecField Name (Located arg)]   -- Explicit fields
              -> RnM [HsRecField Name (Located arg)]   -- Filled in .. fields
    rn_dotdot Nothing _mb_con _flds     -- No ".." at all
      = return []
    rn_dotdot (Just {}) Nothing _flds   -- ".." on record update
      = do { addErr (badDotDot ctxt); return [] }
    rn_dotdot (Just n) (Just con) flds -- ".." on record construction / pat match
      = ASSERT( n == length flds )
        do { loc <- getSrcSpanM -- Rather approximate
           ; dd_flag <- xoptM Opt_RecordWildCards
           ; checkErr dd_flag (needFlagDotDot ctxt)
           ; (rdr_env, lcl_env) <- getRdrEnvs
           ; con_fields <- lookupConstructorFields con
           ; let present_flds = getFieldIds flds
                 parent_tc = find_tycon rdr_env con

                   -- For constructor uses (but not patterns)
                   -- the arg should be in scope (unqualified)
                   -- ignoring the record field itself
                   -- Eg.  data R = R { x,y :: Int }
                   --      f x = R { .. }   -- Should expand to R {x=x}, not R{x=x,y=y}
                 arg_in_scope fld 
                   = rdr `elemLocalRdrEnv` lcl_env
                   || notNull [ gre | gre <- lookupGRE_RdrName rdr rdr_env
                                    , case gre_par gre of
                                        ParentIs p -> p /= parent_tc
                                        _          -> True ]
                   where
                     rdr = mkRdrUnqual (nameOccName fld)

                 dot_dot_gres = [ head gres
                                | fld <- con_fields
                                , not (fld `elem` present_flds)
                                , let gres = lookupGRE_Name rdr_env fld
                                , not (null gres)  -- Check field is in scope
                                , case ctxt of
                                    HsRecFieldCon {} -> arg_in_scope fld
                                    _other           -> True ] 

           ; addUsedRdrNames (map greRdrName dot_dot_gres)
           ; return [ HsRecField
                        { hsRecFieldId  = L loc fld
                        , hsRecFieldArg = L loc (mk_arg arg_rdr)
                        , hsRecPun      = False }
                    | gre <- dot_dot_gres
                    , let fld     = gre_name gre
                          arg_rdr = mkRdrUnqual (nameOccName fld) ] }

    check_disambiguation :: Bool -> Maybe Name -> RnM Parent
    -- When disambiguation is on, 
    check_disambiguation disambig_ok mb_con
      | disambig_ok, Just con <- mb_con
      = do { env <- getGlobalRdrEnv; return (ParentIs (find_tycon env con)) }
      | otherwise = return NoParent
 
    find_tycon :: GlobalRdrEnv -> Name {- DataCon -} -> Name {- TyCon -}
    -- Return the parent *type constructor* of the data constructor
    -- That is, the parent of the data constructor.  
    -- That's the parent to use for looking up record fields.
    find_tycon env con 
      = case lookupGRE_Name env con of
          [GRE { gre_par = ParentIs p }] -> p
          gres  -> pprPanic "find_tycon" (ppr con $$ ppr gres)

    dup_flds :: [[RdrName]]
        -- Each list represents a RdrName that occurred more than once
        -- (the list contains all occurrences)
        -- Each list in dup_fields is non-empty
    (_, dup_flds) = removeDups compare (getFieldIds flds)

getFieldIds :: [HsRecField id arg] -> [id]
getFieldIds flds = map (unLoc . hsRecFieldId) flds

needFlagDotDot :: HsRecFieldContext -> SDoc
needFlagDotDot ctxt = vcat [ptext (sLit "Illegal `..' in record") <+> pprRFC ctxt,
                            ptext (sLit "Use RecordWildCards to permit this")]

badDotDot :: HsRecFieldContext -> SDoc
badDotDot ctxt = ptext (sLit "You cannot use `..' in a record") <+> pprRFC ctxt

badPun :: Located RdrName -> SDoc
badPun fld = vcat [ptext (sLit "Illegal use of punning for field") <+> quotes (ppr fld),
                   ptext (sLit "Use NamedFieldPuns to permit this")]

dupFieldErr :: HsRecFieldContext -> [RdrName] -> SDoc
dupFieldErr ctxt dups
  = hsep [ptext (sLit "duplicate field name"), 
          quotes (ppr (head dups)),
          ptext (sLit "in record"), pprRFC ctxt]

pprRFC :: HsRecFieldContext -> SDoc
pprRFC (HsRecFieldCon {}) = ptext (sLit "construction")
pprRFC (HsRecFieldPat {}) = ptext (sLit "pattern")
pprRFC (HsRecFieldUpd {}) = ptext (sLit "update")
\end{code}


%************************************************************************
%*                                                                      *
\subsubsection{Literals}
%*                                                                      *
%************************************************************************

When literals occur we have to make sure
that the types and classes they involve
are made available.

\begin{code}
rnLit :: HsLit -> RnM ()
rnLit (HsChar c) = checkErr (inCharRange c) (bogusCharError c)
rnLit _ = return ()

-- Turn a Fractional-looking literal which happens to be an integer into an
-- Integer-looking literal.
generalizeOverLitVal :: OverLitVal -> OverLitVal
generalizeOverLitVal (HsFractional (FL {fl_value=val}))
    | denominator val == 1 = HsIntegral (numerator val)
generalizeOverLitVal lit = lit

rnOverLit :: HsOverLit t -> RnM (HsOverLit Name, FreeVars)
rnOverLit origLit
  = do  { opt_NumDecimals <- xoptM Opt_NumDecimals
        ; let { lit@(OverLit {ol_val=val})
            | opt_NumDecimals = origLit {ol_val = generalizeOverLitVal (ol_val origLit)}
            | otherwise       = origLit
          }
        ; let std_name = hsOverLitName val
        ; (from_thing_name, fvs) <- lookupSyntaxName std_name
        ; let rebindable = case from_thing_name of
                                HsVar v -> v /= std_name
                                _       -> panic "rnOverLit"
        ; return (lit { ol_witness = from_thing_name
                      , ol_rebindable = rebindable }, fvs) }
\end{code}

%************************************************************************
%*                                                                      *
\subsubsection{Errors}
%*                                                                      *
%************************************************************************

\begin{code}
patSigErr :: Outputable a => a -> SDoc
patSigErr ty
  =  (ptext (sLit "Illegal signature in pattern:") <+> ppr ty)
        $$ nest 4 (ptext (sLit "Use ScopedTypeVariables to permit it"))

bogusCharError :: Char -> SDoc
bogusCharError c
  = ptext (sLit "character literal out of range: '\\") <> char c  <> char '\''

badViewPat :: Pat RdrName -> SDoc
badViewPat pat = vcat [ptext (sLit "Illegal view pattern: ") <+> ppr pat,
                       ptext (sLit "Use ViewPatterns to enable view patterns")]
\end{code}
