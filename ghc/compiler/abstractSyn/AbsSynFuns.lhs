%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[AbsSynFuns]{Abstract syntax: help functions}

\begin{code}
#include "HsVersions.h"

module AbsSynFuns (
	collectTopLevelBinders,
	collectBinders, collectTypedBinders,
	collectMonoBinders,
	collectMonoBindersAndLocs,
	collectPatBinders,
	collectQualBinders,
	collectTypedPatBinders,
#ifdef DPH
	collectParQualBinders,
#endif {- Data Parallel Haskell -}
	cmpInstanceTypes,
	extractMonoTyNames,
{-OLD:-}getMentionedVars, -- MENTIONED
	getNonPrelOuterTyCon,
	mkDictApp,
	mkDictLam,
	mkTyApp,
	mkTyLam,

	PreludeNameFun(..)
    ) where

IMPORT_Trace

import AbsSyn

import HsTypes		( cmpMonoType )
import Id		( Id, DictVar(..), DictFun(..) )
import Maybes		( Maybe(..) )
import ProtoName	( ProtoName(..), cmpProtoName )
import Rename		( PreludeNameFun(..) )
import Util
\end{code}

%************************************************************************
%*									*
\subsection[AbsSynFuns-MonoBinds]{Bindings: @MonoBinds@}
%*									*
%************************************************************************

Get all the binders in some @ProtoNameMonoBinds@, IN THE ORDER OF
APPEARANCE; e.g., in:
\begin{verbatim}
...
where
  (x, y) = ...
  f i j  = ...
  [a, b] = ...
\end{verbatim}
it should return @[x, y, f, a, b]@ (remember, order important).

\begin{code}
collectTopLevelBinders :: Binds name (InPat name) -> [name]
collectTopLevelBinders EmptyBinds     = []
collectTopLevelBinders (SingleBind b) = collectBinders b
collectTopLevelBinders (BindWith b _) = collectBinders b
collectTopLevelBinders (ThenBinds b1 b2) 
 = (collectTopLevelBinders b1) ++ (collectTopLevelBinders b2)

collectBinders :: Bind name (InPat name) -> [name]
collectBinders EmptyBind 	      = []
collectBinders (NonRecBind monobinds) = collectMonoBinders monobinds
collectBinders (RecBind monobinds)    = collectMonoBinders monobinds

collectTypedBinders :: TypecheckedBind -> [Id]
collectTypedBinders EmptyBind 	      = []
collectTypedBinders (NonRecBind monobinds) = collectTypedMonoBinders monobinds
collectTypedBinders (RecBind monobinds)    = collectTypedMonoBinders monobinds

collectMonoBinders :: MonoBinds name (InPat name) -> [name]
collectMonoBinders EmptyMonoBinds		     = []
collectMonoBinders (PatMonoBind pat grhss_w_binds _) = collectPatBinders pat
collectMonoBinders (FunMonoBind f matches _)	     = [f]
collectMonoBinders (VarMonoBind v expr) 	     = error "collectMonoBinders"
collectMonoBinders (AndMonoBinds bs1 bs2)
 = (collectMonoBinders bs1) ++ (collectMonoBinders bs2)

collectTypedMonoBinders :: TypecheckedMonoBinds -> [Id]
collectTypedMonoBinders EmptyMonoBinds		          = []
collectTypedMonoBinders (PatMonoBind pat grhss_w_binds _) = collectTypedPatBinders pat
collectTypedMonoBinders (FunMonoBind f matches _)	  = [f]
collectTypedMonoBinders (VarMonoBind v expr) 	          = [v]
collectTypedMonoBinders (AndMonoBinds bs1 bs2)
 = (collectTypedMonoBinders bs1) ++ (collectTypedMonoBinders bs2)

-- We'd like the binders -- and where they came from --
-- so we can make new ones with equally-useful origin info.

collectMonoBindersAndLocs
	:: MonoBinds name (InPat name) -> [(name, SrcLoc)]

collectMonoBindersAndLocs EmptyMonoBinds = []

collectMonoBindersAndLocs (AndMonoBinds bs1 bs2)
  = collectMonoBindersAndLocs bs1 ++ collectMonoBindersAndLocs bs2

collectMonoBindersAndLocs (PatMonoBind pat grhss_w_binds locn) 
  = collectPatBinders pat `zip` repeat locn

collectMonoBindersAndLocs (FunMonoBind f matches locn) = [(f, locn)]

collectMonoBindersAndLocs (VarMonoBind v expr)
  = trace "collectMonoBindersAndLocs:VarMonoBind" []
	-- ToDo: this is dubious, i.e., wrong, but harmless?
\end{code}

%************************************************************************
%*									*
\subsection[AbsSynFuns-Expr]{Help functions: @Expr@}
%*									*
%************************************************************************

And some little help functions that remove redundant redundancy:
\begin{code}
mkTyApp :: TypecheckedExpr -> [UniType] -> TypecheckedExpr
mkTyApp expr []  = expr
mkTyApp expr tys = TyApp expr tys

mkDictApp :: TypecheckedExpr -> [DictVar] -> TypecheckedExpr
mkDictApp expr []	  = expr
mkDictApp expr dict_vars = DictApp expr dict_vars

mkTyLam :: [TyVar] -> TypecheckedExpr -> TypecheckedExpr
mkTyLam []     expr = expr
mkTyLam tyvars expr = TyLam tyvars expr

mkDictLam :: [DictVar] -> TypecheckedExpr -> TypecheckedExpr
mkDictLam [] expr = expr
mkDictLam dicts expr = DictLam dicts expr
\end{code}

%************************************************************************
%*									*
\subsection[AbsSynFuns-Qual]{Help functions: @Quals@}
%*									*
%************************************************************************

\begin{code}
#ifdef DPH
collectParQualBinders :: RenamedParQuals -> [Name]
collectParQualBinders (AndParQuals q1 q2) 
   = collectParQualBinders q1 ++ collectParQualBinders q2

collectParQualBinders (DrawnGenIn pats pat expr) 
   = concat ((map collectPatBinders pats)++[collectPatBinders pat])

collectParQualBinders (IndexGen exprs pat expr) 
   = (collectPatBinders pat)

collectParQualBinders (ParFilter expr) = []
#endif {- Data Parallel HAskell -}
\end{code}

%************************************************************************
%*									*
\subsection[AbsSynFuns-ParQuals]{Help functions: @ParQuals@}
%*									*
%************************************************************************

\begin{code}
collectQualBinders :: [RenamedQual] -> [Name]

collectQualBinders quals
  = concat (map collect quals)
  where
    collect (GeneratorQual pat expr) = collectPatBinders pat
    collect (FilterQual expr) = []
\end{code}

%************************************************************************
%*									*
\subsection[AbsSynFuns-pats]{Help functions: patterns}
%*									*
%************************************************************************

With un-parameterised patterns, we have to have ``duplicate'' copies
of one or two functions:
\begin{code}
collectPatBinders :: InPat a -> [a]
collectPatBinders (VarPatIn var)	    = [var]
collectPatBinders (LazyPatIn pat)    = collectPatBinders pat
collectPatBinders (AsPatIn a pat)    = a : (collectPatBinders pat)
collectPatBinders (ConPatIn c pats)  = concat (map collectPatBinders pats)
collectPatBinders (ConOpPatIn p1 c p2)= (collectPatBinders p1) ++ (collectPatBinders p2)
collectPatBinders (ListPatIn pats)   = concat (map collectPatBinders pats)
collectPatBinders (TuplePatIn pats)  = concat (map collectPatBinders pats)
collectPatBinders (NPlusKPatIn n _)  = [n]
#ifdef DPH
collectPatBinders (ProcessorPatIn pats pat)
   = concat (map collectPatBinders pats) ++ (collectPatBinders pat)
#endif
collectPatBinders any_other_pat	    = [ {-no binders-} ]
\end{code}

Nota bene: DsBinds relies on the fact that at least for simple
tuple patterns @collectTypedPatBinders@ returns the binders in 
the same order as they appear in the tuple.

\begin{code}
collectTypedPatBinders :: TypecheckedPat -> [Id]
collectTypedPatBinders (VarPat var)	    = [var]
collectTypedPatBinders (LazyPat pat)	    = collectTypedPatBinders pat
collectTypedPatBinders (AsPat a pat)	    = a : (collectTypedPatBinders pat)
collectTypedPatBinders (ConPat _ _ pats)    = concat (map collectTypedPatBinders pats)
collectTypedPatBinders (ConOpPat p1 _ p2 _) = (collectTypedPatBinders p1) ++ (collectTypedPatBinders p2)
collectTypedPatBinders (ListPat t pats)    = concat (map collectTypedPatBinders pats)
collectTypedPatBinders (TuplePat pats)	    = concat (map collectTypedPatBinders pats)
collectTypedPatBinders (NPlusKPat n _ _ _ _ _) = [n]
#ifdef DPH
collectTypedPatBinders (ProcessorPat pats _ pat) 
  = (concat (map collectTypedPatBinders pats)) ++ 
    (collectTypedPatBinders pat)
#endif {- Data Parallel Haskell -}
collectTypedPatBinders any_other_pat	    = [ {-no binders-} ]
\end{code}

%************************************************************************
%*									*
\subsection[AbsSynFuns-MonoType]{Help functions: @MonoType@}
%*									*
%************************************************************************

Get the type variable names from a @MonoType@.  Don't use class @Eq@
because @ProtoNames@ aren't in it.

\begin{code}
extractMonoTyNames :: (name -> name -> Bool) -> MonoType name -> [name]

extractMonoTyNames eq monotype
  = get monotype []
  where
    get (MonoTyVar name) acc | name `is_elem` acc = acc
			       | otherwise	      = name : acc
    get (MonoTyCon con tys) acc = foldr get acc tys
    get (ListMonoTy ty)	    acc = get ty acc
    get (FunMonoTy ty1 ty2) acc = get ty1 (get ty2 acc)
    get (TupleMonoTy tys)   acc
      = foldr get_poly acc tys
      where
	get_poly (UnoverloadedTy ty)    acc = get ty acc
	get_poly (ForAllTy _ ty)	acc = get ty acc
	get_poly (OverloadedTy ctxt ty) acc = panic "extractMonoTyNames"
    get (MonoDict _ ty)	    acc = get ty acc
    get (MonoTyVarTemplate _) acc = acc
#ifdef DPH
    get (MonoTyProc tys ty) acc = foldr get (get ty acc) tys
    get (MonoTyPod  ty)	    acc = get ty acc
#endif {- Data Parallel Haskell -}

    is_elem n []     = False
    is_elem n (x:xs) = n `eq` x || n `is_elem` xs
\end{code}

@cmpInstanceTypes@ compares two @MonoType@s which are being used as
``instance types.''  This is used when comparing as-yet-unrenamed
instance decls to eliminate duplicates.  We allow things (e.g.,
overlapping instances) which standard Haskell doesn't, so we must
cater for that.  Generally speaking, the instance-type
``shape''-checker in @tcInstDecl@ will catch any mischief later on.

All we do is call @cmpMonoType@, passing it a tyvar-comparing function
that always claims that tyvars are ``equal;'' the result is that we
end up comparing the non-tyvar-ish structure of the two types.

\begin{code}
cmpInstanceTypes :: ProtoNameMonoType -> ProtoNameMonoType -> TAG_

cmpInstanceTypes ty1 ty2
  = cmpMonoType funny_cmp ty1 ty2
  where
    funny_cmp :: ProtoName -> ProtoName -> TAG_

    {- The only case we are really trying to catch
       is when both types are tyvars: which are both
       "Unk"s and names that start w/ a lower-case letter! (Whew.)
    -}
    funny_cmp (Unk u1) (Unk u2)
      | isLower s1 && isLower s2 = EQ_
      where
	s1 = _HEAD_ u1
	s2 = _HEAD_ u2

    funny_cmp x y = cmpProtoName x y -- otherwise completely normal
\end{code}

@getNonPrelOuterTyCon@ is a yukky function required when deciding
whether to import an instance decl.  If the class name or type
constructor are ``wanted'' then we should import it, otherwise not.
But the built-in core constructors for lists, tuples and arrows are
never ``wanted'' in this sense.  @getNonPrelOuterTyCon@ catches just a
user-defined tycon and returns it.

\begin{code}
getNonPrelOuterTyCon :: ProtoNameMonoType -> Maybe ProtoName

getNonPrelOuterTyCon (MonoTyCon con _)   = Just con
getNonPrelOuterTyCon _			 = Nothing
\end{code}

%************************************************************************
%*									*
\subsection[AbsSynFuns-mentioned-vars]{Collect mentioned variables}
%*									*
%************************************************************************

This is just a {\em hack} whichs collects, from a module body, all the
variables that are ``mentioned,'' either as top-level binders or as
free variables.  We can then use this list when walking over
interfaces, using it to avoid imported variables that are patently of
no interest.

We have to be careful to look out for \tr{M..} constructs in the
export list; if so, the game is up (and we must so report).

\begin{code}
{- OLD:MENTIONED-}
getMentionedVars :: PreludeNameFun	-- a prelude-name lookup function, so
					-- we can avoid recording prelude things
					-- as "mentioned"
		 -> [IE]{-exports-}	-- All the bits of the module body to 
		 -> [ProtoNameFixityDecl]-- look in for "mentioned" vars.
		 -> [ProtoNameClassDecl]
		 -> [ProtoNameInstDecl]
		 -> ProtoNameBinds

		 -> (Bool,		-- True <=> M.. construct in exports
		     [FAST_STRING])	-- list of vars "mentioned" in the module body

getMentionedVars val_nf exports fixes class_decls inst_decls binds
  = case (mention_IE exports) of { (module_dotdot_seen, export_mentioned) ->
    (module_dotdot_seen,
     concat [export_mentioned,
	     mention_Fixity	       fixes,
	     mention_ClassDecls	val_nf class_decls,
	     mention_InstDecls	val_nf inst_decls,
	     mention_Binds	val_nf True{-top-level-} binds])
    }
\end{code}

\begin{code}
mention_IE :: [IE] -> (Bool, [FAST_STRING])

mention_IE exps
  = foldr men (False, []) exps
  where
    men (IEVar str) (dotdot_seen, so_far) = (dotdot_seen, str : so_far)
    men (IEModuleContents _)  (_, so_far) = (True, so_far)
    men other_ie    	      acc   	  = acc
\end{code}

\begin{code}
mention_Fixity :: [ProtoNameFixityDecl] -> [FAST_STRING]

mention_Fixity fixity_decls = []
    -- ToDo: if we ever do something proper with fixity declarations,
    -- this might need to do something.
\end{code}

\begin{code}
mention_ClassDecls :: PreludeNameFun -> [ProtoNameClassDecl] -> [FAST_STRING]

mention_ClassDecls val_nf [] = []
mention_ClassDecls val_nf (ClassDecl _ _ _ _ binds _ _ : rest)
  = mention_MonoBinds val_nf True{-toplev-} binds
    ++ mention_ClassDecls val_nf rest
\end{code}

\begin{code}
mention_InstDecls :: PreludeNameFun -> [ProtoNameInstDecl] -> [FAST_STRING]

mention_InstDecls val_nf [] = []
mention_InstDecls val_nf (InstDecl _ _ _ binds _ _ _ _ _ _ : rest)
  = mention_MonoBinds val_nf True{-toplev-} binds
    ++ mention_InstDecls val_nf rest
\end{code}

\begin{code}
mention_Binds :: PreludeNameFun -> Bool -> ProtoNameBinds -> [FAST_STRING]

mention_Binds val_nf toplev EmptyBinds = []
mention_Binds val_nf toplev (ThenBinds a b)
  = mention_Binds val_nf toplev a ++ mention_Binds val_nf toplev b
mention_Binds val_nf toplev (SingleBind a) = mention_Bind val_nf toplev a
mention_Binds val_nf toplev (BindWith a _) = mention_Bind val_nf toplev a
\end{code}

\begin{code}
mention_Bind :: PreludeNameFun -> Bool -> ProtoNameBind -> [FAST_STRING]

mention_Bind val_nf toplev EmptyBind 	    = []
mention_Bind val_nf toplev (NonRecBind a)   = mention_MonoBinds val_nf toplev a
mention_Bind val_nf toplev (RecBind a)	    = mention_MonoBinds val_nf toplev a
\end{code}

\begin{code}
mention_MonoBinds :: PreludeNameFun -> Bool -> ProtoNameMonoBinds -> [FAST_STRING]

mention_MonoBinds val_nf toplev EmptyMonoBinds = []
mention_MonoBinds val_nf toplev (AndMonoBinds a b)
  = mention_MonoBinds val_nf toplev a ++ mention_MonoBinds val_nf toplev b
mention_MonoBinds val_nf toplev (PatMonoBind p gb _)
  = let
	rest = mention_GRHSsAndBinds val_nf gb
    in
    if toplev
    then (map stringify (collectPatBinders p)) ++ rest
    else rest

mention_MonoBinds val_nf toplev (FunMonoBind v ms _)
  = let
	rest = concat (map (mention_Match val_nf) ms)
    in
    if toplev then (stringify v) : rest else rest

stringify :: ProtoName -> FAST_STRING
stringify (Unk s) = s
\end{code}

\begin{code}
mention_Match :: PreludeNameFun -> ProtoNameMatch -> [FAST_STRING]

mention_Match val_nf (PatMatch _ m) = mention_Match val_nf m
mention_Match val_nf (GRHSMatch gb) = mention_GRHSsAndBinds val_nf gb
\end{code}

\begin{code}
mention_GRHSsAndBinds :: PreludeNameFun -> ProtoNameGRHSsAndBinds -> [FAST_STRING]

mention_GRHSsAndBinds val_nf (GRHSsAndBindsIn gs bs)
  = mention_GRHSs val_nf gs ++ mention_Binds val_nf False bs
\end{code}

\begin{code}
mention_GRHSs :: PreludeNameFun -> [ProtoNameGRHS] -> [FAST_STRING]

mention_GRHSs val_nf grhss
  = concat (map mention_grhs grhss)
  where
    mention_grhs (OtherwiseGRHS e _) = mention_Expr val_nf [] e
    mention_grhs (GRHS g e _)
      = mention_Expr val_nf [] g  ++ mention_Expr val_nf [] e
\end{code}

\begin{code}
mention_Expr :: PreludeNameFun -> [FAST_STRING] -> ProtoNameExpr -> [FAST_STRING]

mention_Expr val_nf acc (Var v)
  = case v of
      Unk str | _LENGTH_ str >= 3
	-> case (val_nf str) of
	     Nothing -> str : acc
	     Just _  -> acc
      other -> acc

mention_Expr val_nf acc (Lit _) = acc
mention_Expr val_nf acc (Lam m) = acc ++ (mention_Match val_nf m)
mention_Expr val_nf acc (App a b) = mention_Expr val_nf (mention_Expr val_nf acc a) b
mention_Expr val_nf acc (OpApp a b c) = mention_Expr val_nf (mention_Expr val_nf (mention_Expr val_nf acc a) b) c
mention_Expr val_nf acc (SectionL a b) = mention_Expr val_nf (mention_Expr val_nf acc a) b
mention_Expr val_nf acc (SectionR a b) = mention_Expr val_nf (mention_Expr val_nf acc a) b
mention_Expr val_nf acc (CCall _ es _ _ _) = mention_Exprs val_nf acc es
mention_Expr val_nf acc (SCC _ e) = mention_Expr val_nf acc e
mention_Expr val_nf acc (Case e ms) = mention_Expr val_nf acc e ++ concat (map (mention_Match val_nf) ms)
mention_Expr val_nf acc (ListComp e q) = mention_Expr val_nf acc e ++ mention_Quals val_nf q
mention_Expr val_nf acc (Let b e) = (mention_Expr val_nf acc e) ++ (mention_Binds val_nf False{-not toplev-} b)
mention_Expr val_nf acc (ExplicitList es)  = mention_Exprs val_nf acc es
mention_Expr val_nf acc (ExplicitTuple es) = mention_Exprs val_nf acc es
mention_Expr val_nf acc (ExprWithTySig e _) = mention_Expr val_nf acc e
mention_Expr val_nf acc (If b t e) = mention_Expr val_nf (mention_Expr val_nf (mention_Expr val_nf acc b) t) e
mention_Expr val_nf acc (ArithSeqIn s) = mention_ArithSeq val_nf acc s
#ifdef DPH
mention_Expr val_nf acc (ParallelZF e q) = (mention_Expr val_nf acc e) ++ 
					   (mention_ParQuals val_nf q)
mention_Expr val_nf acc (ExplicitPodIn es) = mention_Exprs val_nf acc es
mention_Expr val_nf acc (ExplicitProcessor es e) = mention_Expr val_nf (mention_Exprs val_nf acc es) e
#endif {- Data Parallel Haskell -}
\end{code}

\begin{code}
mention_Exprs :: PreludeNameFun -> [FAST_STRING] -> [ProtoNameExpr] -> [FAST_STRING]

mention_Exprs val_nf acc []     = acc
mention_Exprs val_nf acc (e:es) = mention_Exprs val_nf (mention_Expr val_nf acc e) es
\end{code}

\begin{code}
mention_ArithSeq :: PreludeNameFun -> [FAST_STRING] -> ProtoNameArithSeqInfo -> [FAST_STRING]

mention_ArithSeq val_nf acc (From	e1)
  = mention_Expr val_nf acc e1
mention_ArithSeq val_nf acc (FromThen   e1 e2)
  = mention_Expr val_nf (mention_Expr val_nf acc e1) e2
mention_ArithSeq val_nf acc (FromTo     e1 e2)
  = mention_Expr val_nf (mention_Expr val_nf acc e1) e2
mention_ArithSeq val_nf acc (FromThenTo e1 e2 e3)
  = mention_Expr val_nf (mention_Expr val_nf (mention_Expr val_nf acc e1) e2) e3
\end{code}

\begin{code}
mention_Quals :: PreludeNameFun -> [ProtoNameQual] -> [FAST_STRING]

mention_Quals val_nf quals
  = concat (map mention quals)
  where
    mention (GeneratorQual _ e) = mention_Expr val_nf [] e
    mention (FilterQual e)  	= mention_Expr val_nf [] e
\end{code}

\begin{code}
#ifdef DPH
mention_ParQuals :: PreludeNameFun -> ProtoNameParQuals -> [FAST_STRING]
mention_ParQuals val_nf (ParFilter e)      = mention_Expr val_nf [] e
mention_ParQuals val_nf (DrawnGenIn _ _ e) = mention_Expr val_nf [] e
mention_ParQuals val_nf (AndParQuals a b)  = mention_ParQuals val_nf a ++ 
					     mention_ParQuals val_nf b
mention_ParQuals val_nf (IndexGen es _ e)  = mention_Exprs val_nf [] es 
					     ++ mention_Expr val_nf [] e
#endif {- Data Parallel Haskell -}

{- END OLD:MENTIONED -}
\end{code}
