%
% (c) The GRASP/AQUA Project, Glasgow University, 1998
%
\section[UConSet]{UsageSP constraint solver}

This code is (based on) PhD work of Keith Wansbrough <kw217@cl.cam.ac.uk>,
February 1998 .. April 1999.

Keith Wansbrough 1998-02-16..1999-04-29

\begin{code}
module UConSet ( UConSet, 
                 emptyUConSet,
                 eqManyUConSet,
		 eqUConSet,
		 leqUConSet,
                 unionUCS,
		 unionUCSs,
                 solveUCS,
	       ) where

#include "HsVersions.h"

import VarEnv
import Type		( UsageAnn(..) )
import Var		( UVar )
import Monad		( foldM )
import Bag              ( Bag, unitBag, emptyBag, unionBags, foldlBag, bagToList )
import Outputable
import PprType
\end{code}

======================================================================

The data type:
~~~~~~~~~~~~~~

First, individual constraints on particular variables.  This is
private to the implementation.

\begin{code}
data UCon = UCEq           UVar UVar    --         j = k  (equivalence)
          | UCBound [UVar] UVar [UVar]  -- {..} <= j <= {..}
          | UCUsOnce       UVar         --         j = 1
          | UCUsMany       UVar         --         j = omega
\end{code}

Next, the public (but abstract) data type for a usage constraint set:
either a bag of mappings from @UVar@ to @UCon@, or an error message
for an inconsistent constraint set.

\begin{code}
data UConSet = UConSet (Bag (VarEnv UCon))
	     | UConFail SDoc
\end{code}

The idea is that the @VarEnv@s (which will eventually be merged into a
single @VarEnv@) are union-find data structures: a variable is either
equal to another variable, or it is bounded or has a value.  The
equalities form a forest pointing to a root node for each equality
class, on which is found the bound or value for that class.

The @Bag@ enables two-phase operation: we merely collect constraints
in the first phase, an donly union them at solution time.  This gives
a much more efficient algorithm, as we make only a single pass over
the constraints.

Note that the absence of a variable from the @VarEnv@ is exactly
equivalent to it being mapped to @UCBound [] _ []@.


The interface:
~~~~~~~~~~~~~~

@emptyUConSet@ gives an empty constraint set.
@eqManyUConSet@ constrains an annotation to be Many.
@eqUConSet@ constrains two annotations to be equal.
@leqUConSet@ constrains one annotation to be less than or equal to
another (with Once < Many).

\begin{code}
mkUCS = UConSet . unitBag  -- helper function not exported

emptyUConSet :: UConSet
emptyUConSet  = UConSet emptyBag

eqManyUConSet :: UsageAnn -> UConSet

eqManyUConSet UsOnce     = UConFail (text "Once /= Many")
eqManyUConSet UsMany     = emptyUConSet
eqManyUConSet (UsVar uv) = mkUCS $ unitVarEnv uv (UCUsMany uv)

eqUConSet :: UsageAnn -> UsageAnn -> UConSet

eqUConSet UsOnce     UsOnce      = emptyUConSet
eqUConSet UsOnce     (UsVar uv)  = mkUCS $ unitVarEnv uv (UCUsOnce uv)
eqUConSet UsMany     UsMany      = emptyUConSet
eqUConSet UsMany     (UsVar uv)  = mkUCS $ unitVarEnv uv (UCUsMany uv)
eqUConSet (UsVar uv) UsOnce      = mkUCS $ unitVarEnv uv (UCUsOnce uv)
eqUConSet (UsVar uv) UsMany      = mkUCS $ unitVarEnv uv (UCUsMany uv)
eqUConSet (UsVar uv) (UsVar uv') = if uv==uv'
			           then emptyUConSet
				   else mkUCS $ unitVarEnv uv (UCEq uv uv')
eqUConSet UsMany     UsOnce      = UConFail (text "Many /= Once")
eqUConSet UsOnce     UsMany      = UConFail (text "Once /= Many")

leqUConSet :: UsageAnn -> UsageAnn -> UConSet

leqUConSet UsOnce     _           = emptyUConSet
leqUConSet _          UsMany      = emptyUConSet
leqUConSet UsMany     UsOnce      = UConFail (text "Many /<= Once")
leqUConSet UsMany     (UsVar uv)  = mkUCS $ unitVarEnv uv (UCUsMany uv)
leqUConSet (UsVar uv) UsOnce      = mkUCS $ unitVarEnv uv (UCUsOnce uv)
leqUConSet (UsVar uv) (UsVar uv') = mkUCS $ mkVarEnv [(uv, UCBound []   uv  [uv']),
						      (uv',UCBound [uv] uv' []   )]
\end{code}

@unionUCS@ forms the union of two @UConSet@s.
@unionUCSs@ forms the `big union' of a list of @UConSet@s.

\begin{code}
unionUCS :: UConSet -> UConSet -> UConSet

unionUCS     (UConSet b1)      (UConSet b2) = UConSet (b1 `unionBags` b2)
unionUCS ucs@(UConFail _)                _  = ucs  -- favour first error
unionUCS     (UConSet  _)  ucs@(UConFail _) = ucs

unionUCSs :: [UConSet] -> UConSet

unionUCSs ucss = foldl unionUCS emptyUConSet ucss
\end{code}


@solveUCS@ finds the minimal solution to the constraint set, returning
it as @Just@ a substitution function taking usage variables to usage
annotations (@UsOnce@ or @UsMany@).  If this is not possible (for an
inconsistent constraint set), @solveUCS@ returns @Nothing@.

The minimal solution is found by simply reading off the known
variables, and for unknown ones substituting @UsOnce@.

\begin{code}
solveUCS :: UConSet -> Maybe (UVar -> UsageAnn)

solveUCS (UConSet css)
  = case foldlBag (\cs1 jcs2 -> foldVarEnv addUCS cs1 jcs2)
                  (Left emptyVarEnv)
                  css of
      Left cs   -> let cs'    = mapVarEnv conToSub cs
                       sub uv = case lookupVarEnv cs' uv of
	                          Just u  -> u
	                          Nothing -> UsOnce
                       conToSub (UCEq       _ uv')    = case lookupVarEnv cs uv' of
			                                  Nothing   -> UsOnce
				                          Just con' -> conToSub con'
                       conToSub (UCUsOnce   _    )    = UsOnce
                       conToSub (UCUsMany   _    )    = UsMany
                       conToSub (UCBound  _ _ _  )    = UsOnce
                   in  Just sub
      Right err -> solveUCS (UConFail err)

solveUCS (UConFail why) = 
#ifdef DEBUG
                          pprTrace "UConFail:" why $
#endif
                          Nothing
\end{code}

======================================================================

The internals:
~~~~~~~~~~~~~~

In the internals, we use the @VarEnv UCon@ explicitly, or occasionally
@Either (VarEnv UCon) SDoc@.  In other words, the @Bag@ is no longer
used.

@findUCon@ finds the root of an equivalence class.
@changeUConUVar@ copies a constraint, but changes the variable constrained.

\begin{code}
findUCon :: VarEnv UCon -> UVar -> UVar

findUCon cs uv
  = case lookupVarEnv cs uv of
      Just (UCEq _ uv') -> findUCon cs uv'
      Just _            -> uv
      Nothing           -> uv

changeUConUVar :: UCon -> UVar -> UCon

changeUConUVar (UCEq       _ v ) uv' = (UCEq       uv' v )
changeUConUVar (UCBound us _ vs) uv' = (UCBound us uv' vs)
changeUConUVar (UCUsOnce   _   ) uv' = (UCUsOnce   uv'   )
changeUConUVar (UCUsMany   _   ) uv' = (UCUsMany   uv'   )
\end{code}

@mergeUVars@ tests to see if a set of @UVar@s can be constrained.  If
they can, it returns the set of root @UVar@s represented (with no
duplicates); if they can't, it returns @Nothing@.

\begin{code}
mergeUVars :: VarEnv UCon    -- current constraint set
           -> Bool           -- True/False = try to constrain to Many/Once
           -> [UVar]         -- list of UVars to constrain
           -> Maybe [UVar]   -- Just [root uvars to force], or Nothing if conflict

mergeUVars cs isMany vs = foldl muv (Just []) vs
  where
    muv :: Maybe [UVar] -> UVar -> Maybe [UVar]
    muv Nothing      _
      = Nothing
    muv jvs@(Just vs) v
      = let rv = findUCon cs v
        in  if elem rv vs
            then
              jvs
            else
              case lookupVarEnv cs rv of  -- never UCEq
                Nothing              -> Just (rv:vs)
                Just (UCBound _ _ _) -> Just (rv:vs)
		Just (UCUsOnce _)    -> if isMany then Nothing else jvs
		Just (UCUsMany _)    -> if isMany then jvs else Nothing
\end{code}

@addUCS@ adds an individual @UCon@ on a @UVar@ to a @UConSet@.  This
is the core of the algorithm.  As such, it could probably use some
optimising.

\begin{code}
addUCS :: UCon                        -- constraint to add
       -> Either (VarEnv UCon) SDoc   -- old constraint set or error
       -> Either (VarEnv UCon) SDoc   -- new constraint set or error

addUCS _ jcs@(Right _) = jcs  -- propagate errors

addUCS (UCEq uv1 uv2) jcs@(Left cs)
  = let ruv1 = findUCon cs uv1
        ruv2 = findUCon cs uv2
    in  if ruv1==ruv2
        then jcs  -- no change if already equal
        else let cs' = Left $ extendVarEnv cs ruv1 (UCEq ruv1 ruv2)  -- merge trees
             in  case lookupVarEnv cs ruv1 of
                   Just uc'
                     -> addUCS (changeUConUVar uc' ruv2) cs'  -- merge old constraints
                   Nothing
                     -> cs'

addUCS (UCBound us uv1 vs) jcs@(Left cs)
  = let ruv1 = findUCon cs uv1
    in  case lookupWithDefaultVarEnv cs (UCBound [] ruv1 []) ruv1 of  -- never UCEq
          UCBound us' _ vs'
            -> case (mergeUVars cs False (us'++us),
                     mergeUVars cs True  (vs'++vs)) of
                 (Just us'',Just vs'')  -- update
                   -> Left $ extendVarEnv cs ruv1 (UCBound us'' ruv1 vs'')
                 (Nothing,  Just vs'')  -- set
                   -> addUCS (UCUsMany ruv1)
                             (forceUVars UCUsMany vs'' jcs)
                 (Just us'',Nothing)    -- set
                   -> addUCS (UCUsOnce ruv1)
                             (forceUVars UCUsOnce us'' jcs)
                 (Nothing,  Nothing)    -- fail
                   -> Right (text "union failed[B] at" <+> ppr uv1)
          UCUsOnce _
            -> forceUVars UCUsOnce us jcs
          UCUsMany _
            -> forceUVars UCUsMany vs jcs

addUCS (UCUsOnce uv1) jcs@(Left cs)
  = let ruv1 = findUCon cs uv1
    in  case lookupWithDefaultVarEnv cs (UCBound [] ruv1 []) ruv1 of  -- never UCEq
          UCBound us _ vs
            -> forceUVars UCUsOnce us (Left $ extendVarEnv cs ruv1 (UCUsOnce ruv1))
          UCUsOnce _
            -> jcs
          UCUsMany _
            -> Right (text "union failed[O] at" <+> ppr uv1)

addUCS (UCUsMany uv1) jcs@(Left cs)
  = let ruv1 = findUCon cs uv1
    in  case lookupWithDefaultVarEnv cs (UCBound [] ruv1 []) ruv1 of  -- never UCEq
          UCBound us _ vs
            -> forceUVars UCUsMany vs (Left $ extendVarEnv cs ruv1 (UCUsMany ruv1))
          UCUsOnce _
            -> Right (text "union failed[M] at" <+> ppr uv1)
          UCUsMany _
            -> jcs

-- helper function forcing a set of UVars to either Once or Many:
forceUVars :: (UVar -> UCon)
           -> [UVar]
           -> Either (VarEnv UCon) SDoc
           -> Either (VarEnv UCon) SDoc
forceUVars uc uvs cs0 = foldl (\cs uv -> addUCS (uc uv) cs) cs0 uvs
\end{code}

======================================================================

Pretty-printing:
~~~~~~~~~~~~~~~~

\begin{code}
-- Printing a usage constraint.

pprintUCon :: VarEnv UCon -> UCon -> SDoc

pprintUCon fm (UCEq uv1 uv2)
  = ppr uv1 <+> text "=" <+> ppr uv2 <> text ":"
    <+> let uv2' = findUCon fm uv2
        in  case lookupVarEnv fm uv2' of
              Just uc -> pprintUCon fm uc
              Nothing -> text "unconstrained"

pprintUCon fm (UCBound us uv vs)
  = lbrace <> hcat (punctuate comma (map ppr us)) <> rbrace
    <+> text "<=" <+> ppr uv <+> text "<="
    <+> lbrace <> hcat (punctuate comma (map ppr vs)) <> rbrace

pprintUCon fm (UCUsOnce uv)
  = ppr uv <+> text "=" <+> ppr UsOnce

pprintUCon fm (UCUsMany uv)
  = ppr uv <+> text "=" <+> ppr UsMany

-- Printing a usage constraint set.

instance Outputable UConSet where
  ppr (UConSet bfm)
    = text "UConSet:" <+> lbrace
      $$ vcat (map (\fm -> nest 2 (vcat (map (pprintUCon fm) (rngVarEnv fm))))
                   (bagToList bfm))
      $$ rbrace

  ppr (UConFail d)
    = hang (text "UConSet inconsistent:")
        4 d
\end{code}

======================================================================

EOF
