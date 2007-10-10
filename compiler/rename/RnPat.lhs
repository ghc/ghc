%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnPat]{Renaming of patterns}

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
{-# OPTIONS -w #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module RnPat (-- main entry points
              rnPatsAndThen_LocalRightwards, rnPat_LocalRec, rnPat_TopRec,

              NameMaker, applyNameMaker,     -- a utility for making names:
              localNameMaker, topNameMaker,  --   sometimes we want to make local names,
                                             --   sometimes we want to make top (qualified) names.

              rnHsRecFields_Con, rnHsRecFields_Update, --rename record fields in a constructor
                                                       --and in an update

	      -- Literals
	      rnLit, rnOverLit,     

             -- Pattern Error messages that are also used elsewhere
             checkTupSize, patSigErr
             ) where

-- ENH: thin imports to only what is necessary for patterns

import {-# SOURCE #-} RnExpr( rnLExpr, rnStmts)

#include "HsVersions.h"

import HsSyn            
import TcRnMonad
import RnEnv
import HscTypes         ( availNames )
import RnNames		( getLocalDeclBinders, extendRdrEnvRn )
import RnTypes		( rnHsTypeFVs, 
			  mkOpFormRn, mkOpAppRn, mkNegAppRn, checkSectionPrec, mkConOpPatRn
			   )
import DynFlags		( DynFlag(..) )
import BasicTypes	( FixityDirection(..) )
import SrcLoc           ( SrcSpan )
import PrelNames	( thFAKE, hasKey, assertIdKey, assertErrorName,
			  loopAName, choiceAName, appAName, arrAName, composeAName, firstAName,
			  negateName, thenMName, bindMName, failMName,
                        eqClassName, integralClassName, geName, eqName,
		  	  negateName, minusName, lengthPName, indexPName,
			  plusIntegerName, fromIntegerName, timesIntegerName,
			  ratioDataConName, fromRationalName, fromStringName )
import Constants	( mAX_TUPLE_SIZE )
import Name		( Name, nameOccName, nameIsLocalOrFrom, getOccName, nameSrcSpan )
import NameSet
import UniqFM
import RdrName        ( RdrName, extendLocalRdrEnv, lookupLocalRdrEnv, hideSomeUnquals, mkRdrUnqual, nameRdrName )
import LoadIface	( loadInterfaceForName )
import UniqFM		( isNullUFM )
import UniqSet		( emptyUniqSet )
import List		( nub )
import Util		( isSingleton )
import ListSetOps	( removeDups, minusList )
import Maybes		( expectJust )
import Outputable
import SrcLoc		( Located(..), unLoc, getLoc, cmpLocated, noLoc )
import FastString
import Literal		( inIntRange, inCharRange )
import List		( unzip4 )
import Bag            (foldrBag)

import ErrUtils       (Message)
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
-- externally abstract type of name makers,
-- which is how you go from a RdrName to a Name
data NameMaker = NM (Located RdrName -> RnM Name)
localNameMaker = NM (\name -> do [newname] <- newLocalsRn [name]
                                 return newname)

topNameMaker = NM (\name -> do mod <- getModule
                               newTopSrcBinder mod name)

applyNameMaker :: NameMaker -> Located RdrName -> RnM Name
applyNameMaker (NM f) x = f x


-- There are various entry points to renaming patterns, depending on
--  (1) whether the names created should be top-level names or local names
--  (2) whether the scope of the names is entirely given in a continuation
--      (e.g., in a case or lambda, but not in a let or at the top-level,
--       because of the way mutually recursive bindings are handled)
--  (3) whether the type signatures can bind variables
--      (for unpacking existential type vars in data constructors)
--  (4) whether we do duplicate and unused variable checking
--  (5) whether there are fixity declarations associated with the names
--      bound by the patterns that need to be brought into scope with them.
--      
--  Rather than burdening the clients of this module with all of these choices,
--  we export the three points in this design space that we actually need:

-- entry point 1:
-- binds local names; the scope of the bindings is entirely in the thing_inside
--   allows type sigs to bind vars
--   local namemaker
--   unused and duplicate checking
--   no fixities
rnPatsAndThen_LocalRightwards :: HsMatchContext Name -- for error messages
                              -> [LPat RdrName] 
                              -- the continuation gets:
                              --    the list of renamed patterns
                              --    the (overall) free vars of all of them
                              -> (([LPat Name], FreeVars) -> RnM (a, FreeVars))
                              -> RnM (a, FreeVars)

rnPatsAndThen_LocalRightwards ctxt pats thing_inside = 
 -- (0) bring into scope all of the type variables bound by the patterns
    bindPatSigTyVarsFV (collectSigTysFromPats pats) $ 
 -- (1) rename the patterns, bringing into scope all of the term variables
    rnLPatsAndThen localNameMaker emptyUFM pats	       $ \ (pats', pat_fvs) ->
 -- (2) then do the thing inside.
    thing_inside (pats', pat_fvs)	       `thenM` \ (res, res_fvs) ->
    let
        -- walk again to collect the names bound by the pattern
        new_bndrs 	= collectPatsBinders pats'

        -- uses now include both pattern uses and thing_inside uses
        used = res_fvs `plusFV` pat_fvs
        unused_binders = filter (not . (`elemNameSet` used)) new_bndrs

        -- restore the locations and rdrnames of the new_bndrs
        -- lets us use the existing checkDupNames, rather than reimplementing
        -- the error reporting for names
        new_bndrs_rdr = map (\ n -> (L (nameSrcSpan n) 
                                        (mkRdrUnqual (getOccName n)))) new_bndrs
    in 
 -- (3) check for duplicates explicitly
 -- (because we don't bind the vars all at once, it doesn't happen
 -- for free in the binding)
    checkDupNames doc_pat new_bndrs_rdr `thenM_`
 -- (4) warn about unused binders
    warnUnusedMatches unused_binders   `thenM_`
 -- (5) return; note that the fvs are pruned by the rnLPatsAndThen
    returnM (res, res_fvs `plusFV` pat_fvs)
  where
    doc_pat     = ptext SLIT("In") <+> pprMatchContext ctxt


-- entry point 2:
-- binds local names; in a recursive scope that involves other bound vars
--   allows type sigs to bind vars
--   local namemaker
--   no unused and duplicate checking
--   fixities might be coming in
rnPat_LocalRec :: UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                          -- these fixities need to be brought into scope with the names
               -> LPat RdrName
               -> RnM (LPat Name, 
                       -- free variables of the pattern,
                       -- but not including variables bound by this pattern 
                       FreeVars)

rnPat_LocalRec fix_env pat = 
    bindPatSigTyVarsFV (collectSigTysFromPats [pat]) $ 
    rnLPatsAndThen localNameMaker fix_env [pat]	       $ \ ([pat'], pat_fvs) ->
        return (pat', pat_fvs)


-- entry point 3:
-- binds top names; in a recursive scope that involves other bound vars
--   does NOT allow type sigs to bind vars
--   top namemaker
--   no unused and duplicate checking
--   fixities might be coming in
rnPat_TopRec ::  UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                         -- these fixities need to be brought into scope with the names
               -> LPat RdrName
               -> RnM (LPat Name, 
                       -- free variables of the pattern,
                       -- but not including variables bound by this pattern 
                       FreeVars)

rnPat_TopRec fix_env pat = 
    rnLPatsAndThen topNameMaker fix_env [pat]	       $ \ ([pat'], pat_fvs) ->
        return (pat', pat_fvs)


-- general version: parametrized by how you make new names
-- invariant: what-to-do continuation only gets called with a list whose length is the same as
--            the part of the pattern we're currently renaming
rnLPatsAndThen :: NameMaker -- how to make a new variable
               -> UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                          -- these fixities need to be brought into scope with the names
               -> [LPat RdrName]   -- part of pattern we're currently renaming
               -> (([LPat Name],FreeVars) -> RnM (a, FreeVars)) -- what to do afterwards
               -> RnM (a, FreeVars) -- renaming of the whole thing
               
rnLPatsAndThen var fix_env = mapFvRnCPS (rnLPatAndThen var fix_env)


-- the workhorse
rnLPatAndThen :: NameMaker
              -> UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                         -- these fixities need to be brought into scope with the names
              -> LPat RdrName   -- part of pattern we're currently renaming
              -> ((LPat Name, FreeVars) -> RnM (a, FreeVars)) -- what to do afterwards
              -> RnM (a, FreeVars) -- renaming of the whole thing
rnLPatAndThen var@(NM varf) fix_env (L loc p) cont = 
    setSrcSpan loc $ 
      let reloc = L loc 
          lcont = \ (unlocated, fv) -> cont (reloc unlocated, fv)

          -- Note: this is somewhat suspicious because it sometimes
          --       binds a top-level name as a local name (when the NameMaker
          --       returns a top-level name).
          --       however, this binding seems to work, and it only exists for
          --       the duration of the patterns and the continuation;
          --       then the top-level name is added to the global env
          --       before going on to the RHSes (see RnSource.lhs).
          --
          --       and doing things this way saves us from having to parametrize
          --       by the environment extender, repeating the FreeVar handling,
          --       etc.
          bind n = bindLocalNamesFV_WithFixities [n] fix_env
      in
       case p of
         WildPat _ -> lcont (WildPat placeHolderType, emptyFVs)
         
         VarPat name -> do
               newBoundName <- varf (reloc name)
               -- we need to bind pattern variables for view pattern expressions
               -- (e.g. in the pattern (x, x -> y) x needs to be bound in the rhs of the tuple)
               bind newBoundName $ 
                 (lcont (VarPat newBoundName, emptyFVs))
                                     
         SigPatIn pat ty ->
             doptM Opt_PatternSignatures `thenM` \ patsigs ->
             if patsigs
             then rnLPatAndThen var fix_env pat
                      (\ (pat', fvs1) ->
                           rnHsTypeFVs tvdoc ty `thenM` \ (ty',  fvs2) ->
                           lcont (SigPatIn pat' ty', fvs1 `plusFV` fvs2))
             else addErr (patSigErr ty) `thenM_`
                  rnLPatAndThen var fix_env pat cont 
           where
             tvdoc = text "In a pattern type-signature"
       
         LitPat lit@(HsString s) -> 
             do ovlStr <- doptM Opt_OverloadedStrings
                if ovlStr 
                 then rnLPatAndThen var fix_env (reloc $ mkNPat (mkHsIsString s placeHolderType) Nothing) cont
                 else do 
                   rnLit lit
                   lcont (LitPat lit, emptyFVs)   -- Same as below
      
         LitPat lit -> do 
              rnLit lit
              lcont (LitPat lit, emptyFVs)

         NPat lit mb_neg eq ->
            rnOverLit lit			`thenM` \ (lit', fvs1) ->
            (case mb_neg of
        	Nothing -> returnM (Nothing, emptyFVs)
        	Just _  -> lookupSyntaxName negateName	`thenM` \ (neg, fvs) ->
        		   returnM (Just neg, fvs)
            )					`thenM` \ (mb_neg', fvs2) ->
            lookupSyntaxName eqName		`thenM` \ (eq', fvs3) -> 
            lcont (NPat lit' mb_neg' eq',
        	     fvs1 `plusFV` fvs2 `plusFV` fvs3)	
        	-- Needed to find equality on pattern

         NPlusKPat name lit _ _ -> do
              new_name <- varf name 
              bind new_name $  
                rnOverLit lit `thenM` \ (lit', fvs1) ->
                    lookupSyntaxName minusName		`thenM` \ (minus, fvs2) ->
                    lookupSyntaxName geName		`thenM` \ (ge, fvs3) ->
                    lcont (NPlusKPat (L (nameSrcSpan new_name) new_name) lit' ge minus,
	                   fvs1 `plusFV` fvs2 `plusFV` fvs3)
   	-- The Report says that n+k patterns must be in Integral

         LazyPat pat ->
             rnLPatAndThen var fix_env pat $ \ (pat', fvs) -> lcont (LazyPat pat', fvs)

         BangPat pat ->
             rnLPatAndThen var fix_env pat $ \ (pat', fvs) -> lcont (BangPat pat', fvs)

         AsPat name pat -> do
             new_name <- varf name 
             bind new_name $ 
                 rnLPatAndThen var fix_env pat $ \ (pat', fvs) -> 
                     lcont (AsPat (L (nameSrcSpan new_name) new_name) pat', fvs)

         ViewPat expr pat ty -> 
             do vp_flag <- doptM Opt_ViewPatterns
                checkErr vp_flag (badViewPat p)
                -- because of the way we're arranging the recursive calls,
                -- this will be in the right context 
                (expr', fvExpr) <- rnLExpr expr 
                rnLPatAndThen var fix_env pat $ \ (pat', fvPat) ->
                    lcont (ViewPat expr' pat' ty, fvPat `plusFV` fvExpr)

         ConPatIn con stuff -> 
             -- rnConPatAndThen takes care of reconstructing the pattern
             rnConPatAndThen var fix_env con stuff cont

         ParPat pat -> rnLPatAndThen var fix_env pat $ 
                       \ (pat', fv') -> lcont (ParPat pat', fv')

         ListPat pats _ -> 
           rnLPatsAndThen var fix_env pats $ \ (patslist, fvs) ->
               lcont (ListPat patslist placeHolderType, fvs)

         PArrPat pats _ -> 
           rnLPatsAndThen var fix_env pats $ \ (patslist, fvs) ->
               lcont (PArrPat patslist placeHolderType, 
	               fvs `plusFV` implicit_fvs)
           where
             implicit_fvs = mkFVs [lengthPName, indexPName]

         TuplePat pats boxed _ -> 
             checkTupSize (length pats) `thenM_`
              (rnLPatsAndThen var fix_env pats $ \ (patslist, fvs) ->
                   lcont (TuplePat patslist boxed placeHolderType, fvs))

         TypePat name -> 
             rnHsTypeFVs (text "In a type pattern") name	`thenM` \ (name', fvs) ->
                 lcont (TypePat name', fvs)


-- helper for renaming constructor patterns
rnConPatAndThen :: NameMaker
                -> UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                           -- these fixities need to be brought into scope with the names
                -> Located RdrName          -- the constructor
                -> HsConPatDetails RdrName 
                -> ((LPat Name, FreeVars) -> RnM (a, FreeVars)) -- what to do afterwards
                -> RnM (a, FreeVars)

rnConPatAndThen var fix_env (con@(L loc _)) (PrefixCon pats) cont
  = do	con' <- lookupLocatedOccRn con
	rnLPatsAndThen var fix_env pats $ 
         \ (pats', fvs) -> 
             cont (L loc $ ConPatIn con' (PrefixCon pats'),
                   fvs `addOneFV` unLoc con')

rnConPatAndThen var fix_env (con@(L loc _)) (InfixCon pat1 pat2) cont
    = do con' <- lookupLocatedOccRn con
         (rnLPatAndThen var fix_env pat1 $
          (\ (pat1', fvs1) -> 
	   rnLPatAndThen var fix_env pat2 $ 
           (\ (pat2', fvs2) -> do 
              fixity <- lookupFixityRn (unLoc con')
              pat' <- mkConOpPatRn con' fixity pat1' pat2'
              cont (L loc pat', fvs1 `plusFV` fvs2 `addOneFV` unLoc con'))))

rnConPatAndThen var fix_env (con@(L loc _)) (RecCon rpats) cont = do
  con' <- lookupLocatedOccRn con
  rnHsRecFieldsAndThen_Pattern con' var fix_env rpats $ \ (rpats', fvs) -> 
      cont (L loc $ ConPatIn con' (RecCon rpats'), fvs `addOneFV` unLoc con')


-- what kind of record expression we're doing
-- the first two tell the name of the datatype constructor in question
-- and give a way of creating a variable to fill in a ..
data RnHsRecFieldsChoice a = Constructor (Located Name) (RdrName -> a)
                           | Pattern  (Located Name) (RdrName -> a)
                           | Update

choiceToMessage (Constructor _ _) = "construction"
choiceToMessage (Pattern _ _) = "pattern"
choiceToMessage Update = "update"

doDotDot (Constructor a b) = Just (a,b)
doDotDot (Pattern a b) = Just (a,b)
doDotDot Update        = Nothing

getChoiceName (Constructor n _) = Just n
getChoiceName (Pattern n _) = Just n
getChoiceName (Update) = Nothing



-- helper for renaming record patterns;
-- parameterized so that it can also be used for expressions
rnHsRecFieldsAndThen :: RnHsRecFieldsChoice field
                     -- how to rename the fields (CPSed)
                     -> (Located field -> ((Located field', FreeVars) -> RnM (c, FreeVars)) 
                                       -> RnM (c, FreeVars)) 
                     -- the actual fields 
                     -> HsRecFields RdrName (Located field)  
                     -- what to do in the scope of the field vars
                     -> ((HsRecFields Name (Located field'), FreeVars) -> RnM (c, FreeVars)) 
                     -> RnM (c, FreeVars)
-- Haddock comments for record fields are renamed to Nothing here
rnHsRecFieldsAndThen choice rn_thing (HsRecFields fields dd) cont = 
    let

        -- helper to collect and report duplicate record fields
        reportDuplicateFields doingstr fields = 
            let 
                -- each list represents a RdrName that occurred more than once
                -- (the list contains all occurrences)
                -- invariant: each list in dup_fields is non-empty
                (_, dup_fields :: [[RdrName]]) = removeDups compare
                                                 (map (unLoc . hsRecFieldId) fields)
                                             
                -- duplicate field reporting function
                field_dup_err dup_group = addErr (dupFieldErr doingstr (head dup_group))
            in
              mappM_ field_dup_err dup_fields

        -- helper to rename each field
        rn_field pun_ok (HsRecField field inside pun) cont = do 
          fieldname <- lookupRecordBndr (getChoiceName choice) field
          checkErr (not pun || pun_ok) (badPun field)
          rn_thing inside $ \ (inside', fvs) -> 
              cont (HsRecField fieldname inside' pun, 
                    fvs `addOneFV` unLoc fieldname)

        -- Compute the extra fields to be filled in by the dot-dot notation
        dot_dot_fields fs con mk_field cont = do 
            con_fields <- lookupConstructorFields (unLoc con)
            let missing_fields = con_fields `minusList` fs
            loc <- getSrcSpanM	-- Rather approximate
            -- it's important that we make the RdrName fields that we morally wrote
            -- and then rename them in the usual manner
            -- (rather than trying to make the result of renaming directly)
            -- because, for patterns, renaming can bind vars in the continuation
            mapFvRnCPS rn_thing 
             (map (L loc . mk_field . mkRdrUnqual . getOccName) missing_fields) $
              \ (rhss, fvs_s) -> 
                  let new_fs = [ HsRecField (L loc f) r False
		                 | (f, r) <- missing_fields `zip` rhss ]
                  in 
                    cont (new_fs, fvs_s)

   in do
       -- report duplicate fields
       let doingstr = choiceToMessage choice
       reportDuplicateFields doingstr fields

       -- rename the records as written
       -- check whether punning (implicit x=x) is allowed
       pun_flag <- doptM Opt_RecordPuns
       -- rename the fields
       mapFvRnCPS (rn_field pun_flag) fields $ \ (fields1, fvs1) ->

           -- handle ..
           case dd of
             Nothing -> cont (HsRecFields fields1 dd, fvs1)
             Just n  -> ASSERT( n == length fields ) do
                          dd_flag <- doptM Opt_RecordWildCards
                          checkErr dd_flag (needFlagDotDot doingstr)
                          let fld_names1 = map (unLoc . hsRecFieldId) fields1
                          case doDotDot choice of 
                                Nothing -> addErr (badDotDot doingstr) `thenM_` 
                                           -- we return a junk value here so that error reporting goes on
                                           cont (HsRecFields fields1 dd, fvs1)
                                Just (con, mk_field) ->
                                    dot_dot_fields fld_names1 con mk_field $
                                      \ (fields2, fvs2) -> 
                                          cont (HsRecFields (fields1 ++ fields2) dd, 
                                                            fvs1 `plusFV` fvs2)

needFlagDotDot str = vcat [ptext SLIT("Illegal `..' in record") <+> text str,
			  ptext SLIT("Use -XRecordWildCards to permit this")]

badDotDot str = ptext SLIT("You cannot use `..' in record") <+> text str

badPun fld = vcat [ptext SLIT("Illegal use of punning for field") <+> quotes (ppr fld),
		   ptext SLIT("Use -XRecordPuns to permit this")]


-- wrappers
rnHsRecFieldsAndThen_Pattern :: Located Name
                             -> NameMaker -- new name maker
                             -> UniqFM (Located Fixity) -- mini fixity env for the names we're about to bind
                                                        -- these fixities need to be brought into scope with the names
                             -> HsRecFields RdrName (LPat RdrName)  
                             -> ((HsRecFields Name (LPat Name), FreeVars) -> RnM (c, FreeVars)) 
                             -> RnM (c, FreeVars)
rnHsRecFieldsAndThen_Pattern n var fix_env = rnHsRecFieldsAndThen (Pattern n VarPat) (rnLPatAndThen var fix_env)


-- wrapper to use rnLExpr in CPS style;
-- because it does not bind any vars going forward, it does not need
-- to be written that way
rnLExprAndThen :: (LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars))
               -> LHsExpr RdrName 
               -> ((LHsExpr Name, FreeVars) -> RnM (c, FreeVars)) 
               -> RnM (c, FreeVars) 
rnLExprAndThen f e cont = do {x <- f e; cont x}


-- non-CPSed because exprs don't leave anything bound
rnHsRecFields_Con :: Located Name
                  -> (LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars))
                  -> HsRecFields RdrName (LHsExpr RdrName)  
                  -> RnM (HsRecFields Name (LHsExpr Name), FreeVars)
rnHsRecFields_Con n rnLExpr fields = rnHsRecFieldsAndThen (Constructor n HsVar) 
                                     (rnLExprAndThen rnLExpr) fields return

rnHsRecFields_Update :: (LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars))
                     -> HsRecFields RdrName (LHsExpr RdrName)  
                     -> RnM (HsRecFields Name (LHsExpr Name), FreeVars)
rnHsRecFields_Update rnLExpr fields = rnHsRecFieldsAndThen Update
                                      (rnLExprAndThen rnLExpr) fields return
\end{code}



%************************************************************************
%*									*
\subsubsection{Literals}
%*									*
%************************************************************************

When literals occur we have to make sure
that the types and classes they involve
are made available.

\begin{code}
rnLit :: HsLit -> RnM ()
rnLit (HsChar c) = checkErr (inCharRange c) (bogusCharError c)
rnLit other	 = returnM ()

rnOverLit (HsIntegral i _ _)
  = lookupSyntaxName fromIntegerName	`thenM` \ (from_integer_name, fvs) ->
    if inIntRange i then
	returnM (HsIntegral i from_integer_name placeHolderType, fvs)
    else let
	extra_fvs = mkFVs [plusIntegerName, timesIntegerName]
	-- Big integer literals are built, using + and *, 
	-- out of small integers (DsUtils.mkIntegerLit)
	-- [NB: plusInteger, timesInteger aren't rebindable... 
	--	they are used to construct the argument to fromInteger, 
	--	which is the rebindable one.]
    in
    returnM (HsIntegral i from_integer_name placeHolderType, fvs `plusFV` extra_fvs)

rnOverLit (HsFractional i _ _)
  = lookupSyntaxName fromRationalName		`thenM` \ (from_rat_name, fvs) ->
    let
	extra_fvs = mkFVs [ratioDataConName, plusIntegerName, timesIntegerName]
	-- We have to make sure that the Ratio type is imported with
	-- its constructor, because literals of type Ratio t are
	-- built with that constructor.
	-- The Rational type is needed too, but that will come in
	-- as part of the type for fromRational.
	-- The plus/times integer operations may be needed to construct the numerator
	-- and denominator (see DsUtils.mkIntegerLit)
    in
    returnM (HsFractional i from_rat_name placeHolderType, fvs `plusFV` extra_fvs)

rnOverLit (HsIsString s _ _)
  = lookupSyntaxName fromStringName	`thenM` \ (from_string_name, fvs) ->
	returnM (HsIsString s from_string_name placeHolderType, fvs)
\end{code}


%************************************************************************
%*									*
\subsubsection{Errors}
%*									*
%************************************************************************

\begin{code}
checkTupSize :: Int -> RnM ()
checkTupSize tup_size
  | tup_size <= mAX_TUPLE_SIZE 
  = returnM ()
  | otherwise		       
  = addErr (sep [ptext SLIT("A") <+> int tup_size <> ptext SLIT("-tuple is too large for GHC"),
		 nest 2 (parens (ptext SLIT("max size is") <+> int mAX_TUPLE_SIZE)),
		 nest 2 (ptext SLIT("Workaround: use nested tuples or define a data type"))])

patSigErr ty
  =  (ptext SLIT("Illegal signature in pattern:") <+> ppr ty)
	$$ nest 4 (ptext SLIT("Use -fglasgow-exts to permit it"))

dupFieldErr str dup
  = hsep [ptext SLIT("duplicate field name"), 
          quotes (ppr dup),
	  ptext SLIT("in record"), text str]

bogusCharError c
  = ptext SLIT("character literal out of range: '\\") <> char c  <> char '\''

badViewPat pat = vcat [ptext SLIT("Illegal view pattern: ") <+> ppr pat,
                       ptext SLIT("Use -XViewPatterns to enalbe view patterns")]

\end{code}
