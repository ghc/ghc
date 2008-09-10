%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnPat]{Renaming of patterns}

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
module RnPat (-- main entry points
              rnPatsAndThen_LocalRightwards, rnBindPat,

              NameMaker, applyNameMaker,     -- a utility for making names:
              localRecNameMaker, topRecNameMaker,  --   sometimes we want to make local names,
                                             --   sometimes we want to make top (qualified) names.

              rnHsRecFields_Con, rnHsRecFields_Update, --rename record fields in a constructor
                                                       --and in an update

	      -- Literals
	      rnLit, rnOverLit,     

	      -- Quasiquotation
	      rnQuasiQuote,

             -- Pattern Error messages that are also used elsewhere
             checkTupSize, patSigErr
             ) where

-- ENH: thin imports to only what is necessary for patterns

import {-# SOURCE #-} RnExpr ( rnLExpr )
#ifdef GHCI
import {-# SOURCE #-} TcSplice ( runQuasiQuotePat )
#endif 	/* GHCI */

#include "HsVersions.h"

import HsSyn            
import TcRnMonad
import TcHsSyn		( hsOverLitName )
import RnEnv
import RnTypes
import DynFlags		( DynFlag(..) )
import PrelNames
import Constants	( mAX_TUPLE_SIZE )
import Name
import NameSet
import RdrName
import ListSetOps	( removeDups, minusList )
import Outputable
import SrcLoc
import FastString
import Literal		( inCharRange )
\end{code}


%*********************************************************
%*							*
\subsection{Patterns}
%*							*
%*********************************************************

\begin{code}
-- externally abstract type of name makers,
-- which is how you go from a RdrName to a Name
data NameMaker = NM (forall a. Located RdrName -> (Name -> RnM (a, FreeVars))
			                       -> RnM (a, FreeVars))

matchNameMaker :: NameMaker
matchNameMaker
  = NM (\ rdr_name thing_inside -> 
	do { names@[name] <- newLocalsRn [rdr_name]
	   ; bindLocalNamesFV names $ do
	   { (res, fvs) <- thing_inside name
	   ; warnUnusedMatches names fvs
	   ; return (res, fvs) }})
			  
topRecNameMaker, localRecNameMaker
  :: MiniFixityEnv -> NameMaker

-- topNameMaker and localBindMaker do not check for unused binding
localRecNameMaker fix_env
  = NM (\ rdr_name thing_inside -> 
	do { [name] <- newLocalsRn [rdr_name]
	   ; bindLocalNamesFV_WithFixities [name] fix_env $
	     thing_inside name })
  
topRecNameMaker fix_env
  = NM (\rdr_name thing_inside -> 
        do { mod <- getModule
           ; name <- newTopSrcBinder mod rdr_name
	   ; bindLocalNamesFV_WithFixities [name] fix_env $
	     thing_inside name })
	        -- Note: the bindLocalNamesFV_WithFixities is somewhat suspicious 
        	--       because it binds a top-level name as a local name.
	        --       however, this binding seems to work, and it only exists for
	        --       the duration of the patterns and the continuation;
	        --       then the top-level name is added to the global env
	        --       before going on to the RHSes (see RnSource.lhs).

applyNameMaker :: NameMaker -> Located RdrName
	       -> (Name -> RnM (a,FreeVars)) -> RnM (a, FreeVars)
applyNameMaker (NM f) = f


-- There are various entry points to renaming patterns, depending on
--  (1) whether the names created should be top-level names or local names
--  (2) whether the scope of the names is entirely given in a continuation
--      (e.g., in a case or lambda, but not in a let or at the top-level,
--       because of the way mutually recursive bindings are handled)
--  (3) whether the a type signature in the pattern can bind 
--	lexically-scoped type variables (for unpacking existential 
--	type vars in data constructors)
--  (4) whether we do duplicate and unused variable checking
--  (5) whether there are fixity declarations associated with the names
--      bound by the patterns that need to be brought into scope with them.
--      
--  Rather than burdening the clients of this module with all of these choices,
--  we export the three points in this design space that we actually need:

-- entry point 1:
-- binds local names; the scope of the bindings is entirely in the thing_inside
--   allows type sigs to bind type vars
--   local namemaker
--   unused and duplicate checking
--   no fixities
rnPatsAndThen_LocalRightwards :: HsMatchContext Name -- for error messages
                              -> [LPat RdrName] 
                              -- the continuation gets:
                              --    the list of renamed patterns
                              --    the (overall) free vars of all of them
                              -> ([LPat Name] -> RnM (a, FreeVars))
                              -> RnM (a, FreeVars)

rnPatsAndThen_LocalRightwards ctxt pats thing_inside
  = do	{ envs_before <- getRdrEnvs

	  -- (0) bring into scope all of the type variables bound by the patterns
	  -- (1) rename the patterns, bringing into scope all of the term variables
	  -- (2) then do the thing inside.
	; bindPatSigTyVarsFV (collectSigTysFromPats pats) $ 
	  rnLPatsAndThen matchNameMaker pats	$ \ pats' ->
            do { -- Check for duplicated and shadowed names 
	         -- Because we don't bind the vars all at once, we can't
	         -- 	check incrementally for duplicates; 
	         -- Nor can we check incrementally for shadowing, else we'll
	         -- 	complain *twice* about duplicates e.g. f (x,x) = ...
            ; let names = collectPatsBinders pats'
            ; checkDupNames doc_pat names
	    ; checkShadowedNames doc_pat envs_before
				 [(nameSrcSpan name, nameOccName name) | name <- names]
            ; thing_inside pats' } }
  where
    doc_pat = ptext (sLit "In") <+> pprMatchContext ctxt


-- entry point 2:
-- binds local names; in a recursive scope that involves other bound vars
--	e.g let { (x, Just y) = e1; ... } in ...
--   does NOT allows type sig to bind type vars
--   local namemaker
--   no unused and duplicate checking
--   fixities might be coming in
rnBindPat :: NameMaker
          -> LPat RdrName
          -> RnM (LPat Name, 
                       -- free variables of the pattern,
                       -- but not including variables bound by this pattern 
                   FreeVars)

rnBindPat name_maker pat
  = rnLPatsAndThen name_maker [pat] $ \ [pat'] ->
    return (pat', emptyFVs)


-- general version: parametrized by how you make new names
-- invariant: what-to-do continuation only gets called with a list whose length is the same as
--            the part of the pattern we're currently renaming
rnLPatsAndThen :: NameMaker -- how to make a new variable
               -> [LPat RdrName]   -- part of pattern we're currently renaming
               -> ([LPat Name] -> RnM (a, FreeVars)) -- what to do afterwards
               -> RnM (a, FreeVars) -- renaming of the whole thing
               
rnLPatsAndThen var = mapFvRnCPS (rnLPatAndThen var)


-- the workhorse
rnLPatAndThen :: NameMaker
              -> LPat RdrName   -- part of pattern we're currently renaming
              -> (LPat Name -> RnM (a, FreeVars)) -- what to do afterwards
              -> RnM (a, FreeVars) -- renaming of the whole thing
rnLPatAndThen var@(NM varf) (L loc p) cont = 
    setSrcSpan loc $ 
      let reloc = L loc 
          lcont = \ unlocated -> cont (reloc unlocated)
      in
       case p of
         WildPat _   -> lcont (WildPat placeHolderType)

         ParPat pat  -> rnLPatAndThen var pat $ \ pat' -> lcont (ParPat pat')
         LazyPat pat -> rnLPatAndThen var pat $ \ pat' -> lcont (LazyPat pat')
         BangPat pat -> rnLPatAndThen var pat $ \ pat' -> lcont (BangPat pat')
         
         VarPat name -> 
	    varf (reloc name) $ \ newBoundName -> 
	    lcont (VarPat newBoundName)
               -- we need to bind pattern variables for view pattern expressions
               -- (e.g. in the pattern (x, x -> y) x needs to be bound in the rhs of the tuple)
                                     
         SigPatIn pat ty -> do
             patsigs <- doptM Opt_ScopedTypeVariables
             if patsigs
              then rnLPatAndThen var pat
                      (\ pat' -> do { (ty', fvs1) <- rnHsTypeFVs tvdoc ty
				    ; (res, fvs2) <- lcont (SigPatIn pat' ty')
				    ; return (res, fvs1 `plusFV` fvs2) })
              else do addErr (patSigErr ty)
                      rnLPatAndThen var pat cont
           where
             tvdoc = text "In a pattern type-signature"
       
         LitPat lit@(HsString s) -> 
             do ovlStr <- doptM Opt_OverloadedStrings
                if ovlStr 
                 then rnLPatAndThen var (reloc $ mkNPat (mkHsIsString s placeHolderType) Nothing) cont
                 else do { rnLit lit; lcont (LitPat lit) }   -- Same as below
      
         LitPat lit -> do { rnLit lit; lcont (LitPat lit) }

         NPat lit mb_neg _eq ->
           do { (lit', fvs1) <- rnOverLit lit
	      ;	(mb_neg', fvs2) <- case mb_neg of
			             Nothing -> return (Nothing, emptyFVs)
			             Just _  -> do { (neg, fvs) <- lookupSyntaxName negateName
						   ; return (Just neg, fvs) }
	      ; (eq', fvs3) <- lookupSyntaxName eqName
	      ; (res, fvs4) <- lcont (NPat lit' mb_neg' eq')
	      ; return (res, fvs1 `plusFV` fvs2 `plusFV` fvs3 `plusFV` fvs4) }
	       	-- Needed to find equality on pattern

         NPlusKPat name lit _ _ ->
   	   varf name $ \ new_name ->
	   do { (lit', fvs1) <- rnOverLit lit
  	      ; (minus, fvs2) <- lookupSyntaxName minusName
              ; (ge, fvs3) <- lookupSyntaxName geName
              ; (res, fvs4) <- lcont (NPlusKPat (L (nameSrcSpan new_name) new_name) lit' ge minus)
	      ; return (res, fvs1 `plusFV` fvs2 `plusFV` fvs3 `plusFV` fvs4) }
	   	-- The Report says that n+k patterns must be in Integral

         AsPat name pat ->
   	   varf name $ \ new_name ->
           rnLPatAndThen var pat $ \ pat' -> 
           lcont (AsPat (L (nameSrcSpan new_name) new_name) pat')

         ViewPat expr pat ty -> 
	   do { vp_flag <- doptM Opt_ViewPatterns
              ; checkErr vp_flag (badViewPat p)
                -- because of the way we're arranging the recursive calls,
                -- this will be in the right context 
              ; (expr', fv_expr) <- rnLExpr expr 
              ; (res, fvs_res) <- rnLPatAndThen var pat $ \ pat' ->
		                  lcont (ViewPat expr' pat' ty)
	      ; return (res, fvs_res `plusFV` fv_expr) }

#ifndef GHCI
         (QuasiQuotePat _) -> pprPanic "Can't do QuasiQuotePat without GHCi" (ppr p)
#else
         QuasiQuotePat qq -> do
             (qq', _) <- rnQuasiQuote qq
             pat' <- runQuasiQuotePat qq'
             rnLPatAndThen var pat' $ \ (L _ pat'') ->
                 lcont pat''
#endif 	/* GHCI */

         ConPatIn con stuff -> 
             -- rnConPatAndThen takes care of reconstructing the pattern
             rnConPatAndThen var con stuff cont

         ListPat pats _ -> 
           rnLPatsAndThen var pats $ \ patslist ->
               lcont (ListPat patslist placeHolderType)

         PArrPat pats _ -> 
	   do { (res, res_fvs) <- rnLPatsAndThen var pats $ \ patslist ->
			          lcont (PArrPat patslist placeHolderType)
	      ; return (res, res_fvs `plusFV` implicit_fvs) }
           where
             implicit_fvs = mkFVs [lengthPName, indexPName]

         TuplePat pats boxed _ -> 
           do { checkTupSize (length pats)
              ; rnLPatsAndThen var pats $ \ patslist ->
                lcont (TuplePat patslist boxed placeHolderType) }

         TypePat ty -> 
           do { (ty', fvs1) <- rnHsTypeFVs (text "In a type pattern") ty
	      ; (res, fvs2) <- lcont (TypePat ty')
	      ; return (res, fvs1 `plusFV` fvs2) }

         p -> pprPanic "rnLPatAndThen" (ppr p)


-- helper for renaming constructor patterns
rnConPatAndThen :: NameMaker
                -> Located RdrName          -- the constructor
                -> HsConPatDetails RdrName 
                -> (LPat Name -> RnM (a, FreeVars)) -- what to do afterwards
                -> RnM (a, FreeVars)

rnConPatAndThen var (con@(L loc _)) (PrefixCon pats) cont
  = do	{ con' <- lookupLocatedOccRn con
	; (res, res_fvs) <- rnLPatsAndThen var pats $ \ pats' ->
		            cont (L loc $ ConPatIn con' (PrefixCon pats'))
        ; return (res, res_fvs `addOneFV` unLoc con') }

rnConPatAndThen var (con@(L loc _)) (InfixCon pat1 pat2) cont
  = do	{ con' <- lookupLocatedOccRn con
   	; (res, res_fvs) <- rnLPatAndThen var pat1 $ \ pat1' -> 
			    rnLPatAndThen var pat2 $ \ pat2' ->
			    do { fixity <- lookupFixityRn (unLoc con')
		               ; pat' <- mkConOpPatRn con' fixity pat1' pat2'
			       ; cont (L loc pat') }
        ; return (res, res_fvs `addOneFV` unLoc con') }

rnConPatAndThen var (con@(L loc _)) (RecCon rpats) cont
  = do	{ con' <- lookupLocatedOccRn con
  	; (res, res_fvs) <- rnHsRecFieldsAndThen_Pattern con' var rpats $ \ rpats' -> 
			    cont (L loc $ ConPatIn con' (RecCon rpats'))
        ; return (res, res_fvs `addOneFV` unLoc con') }

-- what kind of record expression we're doing
-- the first two tell the name of the datatype constructor in question
-- and give a way of creating a variable to fill in a ..
data RnHsRecFieldsChoice a = Constructor (Located Name) (RdrName -> a)
                           | Pattern  (Located Name) (RdrName -> a)
                           | Update

choiceToMessage :: RnHsRecFieldsChoice t -> String
choiceToMessage (Constructor _ _) = "construction"
choiceToMessage (Pattern _ _) = "pattern"
choiceToMessage Update = "update"

doDotDot :: RnHsRecFieldsChoice t -> Maybe (Located Name, RdrName -> t)
doDotDot (Constructor a b) = Just (a,b)
doDotDot (Pattern a b) = Just (a,b)
doDotDot Update        = Nothing

getChoiceName :: RnHsRecFieldsChoice field -> Maybe (Located Name)
getChoiceName (Constructor n _) = Just n
getChoiceName (Pattern n _) = Just n
getChoiceName (Update) = Nothing



-- helper for renaming record patterns;
-- parameterized so that it can also be used for expressions
rnHsRecFieldsAndThen :: RnHsRecFieldsChoice field
                     -- how to rename the fields (CPSed)
                     -> (Located field -> (Located field' -> RnM (c, FreeVars)) 
                                       -> RnM (c, FreeVars)) 
                     -- the actual fields 
                     -> HsRecFields RdrName (Located field)  
                     -- what to do in the scope of the field vars
                     -> (HsRecFields Name (Located field') -> RnM (c, FreeVars)) 
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
                dup_fields :: [[RdrName]]
                (_, dup_fields) = removeDups compare
                                                 (map (unLoc . hsRecFieldId) fields)
                                             
                -- duplicate field reporting function
                field_dup_err dup_group = addErr (dupFieldErr doingstr (head dup_group))
            in
              mapM_ field_dup_err dup_fields

        -- helper to rename each field
        rn_field pun_ok (HsRecField field inside pun) cont = do 
          fieldname <- lookupRecordBndr (getChoiceName choice) field
          checkErr (not pun || pun_ok) (badPun field)
          (res, res_fvs) <- rn_thing inside $ \ inside' -> 
		            cont (HsRecField fieldname inside' pun) 
          return (res, res_fvs `addOneFV` unLoc fieldname)

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
              \ rhss -> 
                  let new_fs = [ HsRecField (L loc f) r False
		                 | (f, r) <- missing_fields `zip` rhss ]
                  in 
                  cont new_fs

   in do
       -- report duplicate fields
       let doingstr = choiceToMessage choice
       reportDuplicateFields doingstr fields

       -- rename the records as written
       -- check whether punning (implicit x=x) is allowed
       pun_flag <- doptM Opt_RecordPuns
       -- rename the fields
       mapFvRnCPS (rn_field pun_flag) fields $ \ fields1 ->

           -- handle ..
           case dd of
             Nothing -> cont (HsRecFields fields1 dd)
             Just n  -> ASSERT( n == length fields ) do
                          dd_flag <- doptM Opt_RecordWildCards
                          checkErr dd_flag (needFlagDotDot doingstr)
                          let fld_names1 = map (unLoc . hsRecFieldId) fields1
                          case doDotDot choice of 
                                Nothing -> do addErr (badDotDot doingstr)
                                              -- we return a junk value here so that error reporting goes on
                                              cont (HsRecFields fields1 dd)
                                Just (con, mk_field) ->
                                    dot_dot_fields fld_names1 con mk_field $
                                      \ fields2 -> 
                                          cont (HsRecFields (fields1 ++ fields2) dd)

needFlagDotDot :: String -> SDoc
needFlagDotDot str = vcat [ptext (sLit "Illegal `..' in record") <+> text str,
			  ptext (sLit "Use -XRecordWildCards to permit this")]

badDotDot :: String -> SDoc
badDotDot str = ptext (sLit "You cannot use `..' in record") <+> text str

badPun :: Located RdrName -> SDoc
badPun fld = vcat [ptext (sLit "Illegal use of punning for field") <+> quotes (ppr fld),
		   ptext (sLit "Use -XNamedFieldPuns to permit this")]


-- wrappers
rnHsRecFieldsAndThen_Pattern :: Located Name
                             -> NameMaker -- new name maker
                             -> HsRecFields RdrName (LPat RdrName)  
                             -> (HsRecFields Name (LPat Name) -> RnM (c, FreeVars)) 
                             -> RnM (c, FreeVars)
rnHsRecFieldsAndThen_Pattern n var
  = rnHsRecFieldsAndThen (Pattern n VarPat) (rnLPatAndThen var)


-- wrapper to use rnLExpr in CPS style;
-- because it does not bind any vars going forward, it does not need
-- to be written that way
rnLExprAndThen :: (LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars))
               -> LHsExpr RdrName 
               -> (LHsExpr Name -> RnM (c, FreeVars)) 
               -> RnM (c, FreeVars) 
rnLExprAndThen f e cont = do { (x, fvs1) <- f e
			     ; (res, fvs2) <- cont x
			     ; return (res, fvs1 `plusFV` fvs2) }


-- non-CPSed because exprs don't leave anything bound
rnHsRecFields_Con :: Located Name
                  -> (LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars))
                  -> HsRecFields RdrName (LHsExpr RdrName)  
                  -> RnM (HsRecFields Name (LHsExpr Name), FreeVars)
rnHsRecFields_Con n rnLExpr fields = rnHsRecFieldsAndThen (Constructor n HsVar) 
                                     (rnLExprAndThen rnLExpr) fields $ \ res ->
				     return (res, emptyFVs)

rnHsRecFields_Update :: (LHsExpr RdrName -> RnM (LHsExpr Name, FreeVars))
                     -> HsRecFields RdrName (LHsExpr RdrName)  
                     -> RnM (HsRecFields Name (LHsExpr Name), FreeVars)
rnHsRecFields_Update rnLExpr fields = rnHsRecFieldsAndThen Update
                                      (rnLExprAndThen rnLExpr) fields $ \ res -> 
				      return (res, emptyFVs)
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
rnLit _ = return ()

rnOverLit :: HsOverLit t -> RnM (HsOverLit Name, FreeVars)
rnOverLit lit@(OverLit {ol_val=val})
  = do	{ let std_name = hsOverLitName val
	; (from_thing_name, fvs) <- lookupSyntaxName std_name
	; let rebindable = case from_thing_name of
				HsVar v -> v /= std_name
				_	-> panic "rnOverLit"
	; return (lit { ol_witness = from_thing_name
		      , ol_rebindable = rebindable }, fvs) }
\end{code}

----------------------------------------------------------------
-- Old code returned extra free vars need in desugarer
-- but that is no longer necessary, I believe
--     if inIntRange i then
--        return (HsIntegral i from_integer_name placeHolderType, fvs)
--     else let
--	extra_fvs = mkFVs [plusIntegerName, timesIntegerName]
-- Big integer literals are built, using + and *, 
-- out of small integers (DsUtils.mkIntegerLit)
-- [NB: plusInteger, timesInteger aren't rebindable... 
--	they are used to construct the argument to fromInteger, 
--	which is the rebindable one.]

-- (HsFractional i _ _) = do
--	extra_fvs = mkFVs [ratioDataConName, plusIntegerName, timesIntegerName]
-- We have to make sure that the Ratio type is imported with
-- its constructor, because literals of type Ratio t are
-- built with that constructor.
-- The Rational type is needed too, but that will come in
-- as part of the type for fromRational.
-- The plus/times integer operations may be needed to construct the numerator
-- and denominator (see DsUtils.mkIntegerLit)

%************************************************************************
%*									*
\subsubsection{Quasiquotation}
%*									*
%************************************************************************

See Note [Quasi-quote overview] in TcSplice.

\begin{code}
rnQuasiQuote :: HsQuasiQuote RdrName -> RnM (HsQuasiQuote Name, FreeVars)
rnQuasiQuote (HsQuasiQuote n quoter quoteSpan quote)
  = do	{ loc  <- getSrcSpanM
   	; [n'] <- newLocalsRn [L loc n]
   	; quoter' <-  (lookupOccRn quoter)
		-- If 'quoter' is not in scope, proceed no further
		-- Otherwise lookupOcc adds an error messsage and returns 
		-- an "unubound name", which makes the subsequent attempt to
		-- run the quote fail
   	; return (HsQuasiQuote n' quoter' quoteSpan quote, unitFV quoter') }
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
  = return ()
  | otherwise		       
  = addErr (sep [ptext (sLit "A") <+> int tup_size <> ptext (sLit "-tuple is too large for GHC"),
		 nest 2 (parens (ptext (sLit "max size is") <+> int mAX_TUPLE_SIZE)),
		 nest 2 (ptext (sLit "Workaround: use nested tuples or define a data type"))])

patSigErr :: Outputable a => a -> SDoc
patSigErr ty
  =  (ptext (sLit "Illegal signature in pattern:") <+> ppr ty)
	$$ nest 4 (ptext (sLit "Use -XScopedTypeVariables to permit it"))

dupFieldErr :: String -> RdrName -> SDoc
dupFieldErr str dup
  = hsep [ptext (sLit "duplicate field name"), 
          quotes (ppr dup),
	  ptext (sLit "in record"), text str]

bogusCharError :: Char -> SDoc
bogusCharError c
  = ptext (sLit "character literal out of range: '\\") <> char c  <> char '\''

badViewPat :: Pat RdrName -> SDoc
badViewPat pat = vcat [ptext (sLit "Illegal view pattern: ") <+> ppr pat,
                       ptext (sLit "Use -XViewPatterns to enable view patterns")]

\end{code}
