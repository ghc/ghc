%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[RdrHsSyn]{Specialisations of the @HsSyn@ syntax for the reader}

(Well, really, for specialisations involving @ProtoName@s, even if
they are used somewhat later on in the compiler...)

\begin{code}
#include "HsVersions.h"

module RdrHsSyn (
	cmpInstanceTypes,
	eqMonoType,
	getMentionedVars,
	getNonPrelOuterTyCon,
	ExportListInfo(..),
	getImportees,
	getExportees,
	getRawImportees,
	getRawExportees,

	ProtoNameArithSeqInfo(..),
	ProtoNameBind(..),
	ProtoNameClassDecl(..),
	ProtoNameClassOpPragmas(..),
	ProtoNameClassOpSig(..),
	ProtoNameClassPragmas(..),
	ProtoNameConDecl(..),
	ProtoNameContext(..),
	ProtoNameCoreExpr(..),
	ProtoNameDataPragmas(..),
	ProtoNameSpecDataSig(..),
	ProtoNameDefaultDecl(..),
	ProtoNameFixityDecl(..),
	ProtoNameGRHS(..),
	ProtoNameGRHSsAndBinds(..),
	ProtoNameGenPragmas(..),
	ProtoNameHsBinds(..),
	ProtoNameHsExpr(..),
	ProtoNameHsModule(..),
	ProtoNameIE(..),
	ProtoNameImportedInterface(..),
	ProtoNameInstDecl(..),
	ProtoNameInstancePragmas(..),
	ProtoNameInterface(..),
	ProtoNameMatch(..),
	ProtoNameMonoBinds(..),
	ProtoNameMonoType(..),
	ProtoNamePat(..),
	ProtoNamePolyType(..),
	ProtoNameQual(..),
	ProtoNameSig(..),
	ProtoNameSpecInstSig(..),
	ProtoNameStmt(..),
	ProtoNameTyDecl(..),
	ProtoNameUnfoldingCoreExpr(..)
    ) where

import Ubiq{-uitous-}

import Bag		( emptyBag, snocBag, unionBags, listToBag, Bag )
import FiniteMap	( mkSet, listToFM, emptySet, emptyFM, FiniteSet(..), FiniteMap )
import HsSyn
import Outputable	( ExportFlag(..) )
import ProtoName	( cmpProtoName, ProtoName(..) )
import Util		( panic{-ToDo:rm eventually-} )
\end{code}

\begin{code}
type ProtoNameArithSeqInfo	= ArithSeqInfo		Fake Fake ProtoName ProtoNamePat
type ProtoNameBind		= Bind			Fake Fake ProtoName ProtoNamePat
type ProtoNameClassDecl		= ClassDecl		Fake Fake ProtoName ProtoNamePat
type ProtoNameClassOpPragmas	= ClassOpPragmas	ProtoName
type ProtoNameClassOpSig	= Sig			ProtoName
type ProtoNameClassPragmas	= ClassPragmas		ProtoName
type ProtoNameConDecl		= ConDecl		ProtoName
type ProtoNameContext		= Context 		ProtoName
type ProtoNameCoreExpr		= UnfoldingCoreExpr	ProtoName
type ProtoNameDataPragmas	= DataPragmas		ProtoName
type ProtoNameSpecDataSig	= SpecDataSig		ProtoName
type ProtoNameDefaultDecl	= DefaultDecl		ProtoName
type ProtoNameFixityDecl	= FixityDecl		ProtoName
type ProtoNameGRHS		= GRHS			Fake Fake ProtoName ProtoNamePat
type ProtoNameGRHSsAndBinds	= GRHSsAndBinds		Fake Fake ProtoName ProtoNamePat
type ProtoNameGenPragmas	= GenPragmas		ProtoName
type ProtoNameHsBinds		= HsBinds		Fake Fake ProtoName ProtoNamePat
type ProtoNameHsExpr		= HsExpr		Fake Fake ProtoName ProtoNamePat
type ProtoNameHsModule		= HsModule		Fake Fake ProtoName ProtoNamePat
type ProtoNameIE		= IE			ProtoName
type ProtoNameImportedInterface	= ImportedInterface	Fake Fake ProtoName ProtoNamePat
type ProtoNameInstDecl		= InstDecl		Fake Fake ProtoName ProtoNamePat
type ProtoNameInstancePragmas	= InstancePragmas	ProtoName
type ProtoNameInterface		= Interface		Fake Fake ProtoName ProtoNamePat
type ProtoNameMatch		= Match			Fake Fake ProtoName ProtoNamePat
type ProtoNameMonoBinds		= MonoBinds		Fake Fake ProtoName ProtoNamePat
type ProtoNameMonoType		= MonoType		ProtoName
type ProtoNamePat		= InPat			ProtoName
type ProtoNamePolyType		= PolyType		ProtoName
type ProtoNameQual		= Qual			Fake Fake ProtoName ProtoNamePat
type ProtoNameSig		= Sig			ProtoName
type ProtoNameSpecInstSig	= SpecInstSig 		ProtoName
type ProtoNameStmt		= Stmt			Fake Fake ProtoName ProtoNamePat
type ProtoNameTyDecl		= TyDecl		ProtoName
type ProtoNameUnfoldingCoreExpr = UnfoldingCoreExpr	ProtoName
\end{code}

\begin{code}
eqMonoType :: ProtoNameMonoType -> ProtoNameMonoType -> Bool

eqMonoType a b = case (cmpMonoType cmpProtoName a b) of { EQ_ -> True; _ -> False }
\end{code}


@cmpInstanceTypes@ compares two @PolyType@s which are being used as
``instance types.''  This is used when comparing as-yet-unrenamed
instance decls to eliminate duplicates.  We allow things (e.g.,
overlapping instances) which standard Haskell doesn't, so we must
cater for that.  Generally speaking, the instance-type
``shape''-checker in @tcInstDecl@ will catch any mischief later on.

All we do is call @cmpMonoType@, passing it a tyvar-comparing function
that always claims that tyvars are ``equal;'' the result is that we
end up comparing the non-tyvar-ish structure of the two types.

\begin{code}
cmpInstanceTypes :: ProtoNamePolyType -> ProtoNamePolyType -> TAG_

cmpInstanceTypes (HsPreForAllTy _ ty1) (HsPreForAllTy _ ty2)
  = cmpMonoType funny_cmp ty1 ty2 -- Hey! ignore those contexts!
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

getNonPrelOuterTyCon (MonoTyApp con _)   = Just con
getNonPrelOuterTyCon _			 = Nothing
\end{code}

%************************************************************************
%*									*
\subsection{Grabbing importees and exportees}
%*									*
%************************************************************************

We want to know what names are exported (the first list of the result)
and what modules are exported (the second list of the result).
\begin{code}
type ExportListInfo
  = Maybe -- Nothing => no export list
    ( FiniteMap FAST_STRING ExportFlag,
			-- Assoc list of im/exported things &
			-- their "export" flags (im/exported
			-- abstractly, concretely, etc.)
			-- Hmm... slight misnomer there (WDP 95/02)
      FiniteSet FAST_STRING )
			-- List of modules to be exported
			-- entirely; NB: *not* everything with
			-- original names in these modules;
			-- but: everything that these modules'
			-- interfaces told us about.
			-- Note: This latter component can
			-- only arise on export lists.

getImportees    :: [ProtoNameIE] -> FiniteSet FAST_STRING
getExportees    :: Maybe [ProtoNameIE] -> ExportListInfo

getRawImportees :: [ProtoNameIE] ->  [FAST_STRING]
getRawExportees :: Maybe [ProtoNameIE] -> ([(ProtoName, ExportFlag)], [FAST_STRING])
  -- "Raw" gives the raw lists of things; we need this for
  -- checking for duplicates.

getImportees []   = emptySet
getImportees imps = mkSet (getRawImportees imps)

getExportees Nothing = Nothing
getExportees exps
  = case (getRawExportees exps) of { (pairs, mods) ->
    Just (panic "RdrHsSyn.getExportees" {-listToFM pairs-}, mkSet mods) }

getRawImportees imps
  = foldr do_imp [] imps
  where
    do_imp (IEVar (Unk n))	acc = n:acc
    do_imp (IEThingAbs (Unk n)) acc = n:acc
    do_imp (IEThingAll (Unk n)) acc = n:acc

getRawExportees Nothing     = ([], [])
getRawExportees (Just exps)
  = foldr do_exp ([],[]) exps
  where
    do_exp (IEVar n)		(prs, mods) = ((n, ExportAll):prs, mods)
    do_exp (IEThingAbs n)	(prs, mods) = ((n, ExportAbs):prs, mods)
    do_exp (IEThingAll n)	(prs, mods) = ((n, ExportAll):prs, mods)
    do_exp (IEModuleContents n) (prs, mods) = (prs, n : mods)
\end{code}

%************************************************************************
%*									*
\subsection{Collect mentioned variables}
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
type NameMapper a = FAST_STRING -> Maybe a
		    -- For our purposes here, we don't care *what*
		    -- they are mapped to; only if the names are
		    -- in the mapper

getMentionedVars :: NameMapper any	-- a prelude-name lookup function, so
					-- we can avoid recording prelude things
					-- as "mentioned"
		 -> Maybe [IE ProtoName]{-exports-}	-- All the bits of the module body to
		 -> [ProtoNameFixityDecl]-- look in for "mentioned" vars.
		 -> [ProtoNameClassDecl]
		 -> [ProtoNameInstDecl]
		 -> ProtoNameHsBinds

		 -> (Bool,		-- True <=> M.. construct in exports
		     Bag FAST_STRING)	-- list of vars "mentioned" in the module body

getMentionedVars val_nf exports fixes class_decls inst_decls binds
  = panic "getMentionedVars (RdrHsSyn)"
{- TO THE END
  = case (mention_IE exports) of { (module_dotdot_seen, export_mentioned) ->
    (module_dotdot_seen,
     initMentioned val_nf export_mentioned (
--	mapMent fixity    fixes		`thenMent_` -- see note below.
	mapMent classDecl class_decls	`thenMent_`
	mapMent instDecl  inst_decls	`thenMent_`
	bindsDecls True{-top-level-} binds )
    )}
\end{code}
ToDo: if we ever do something proper with fixity declarations,
we will need to create a @fixities@ function and make it do something.

Here's relevant bit of monad fluff: hides carrying around
the NameMapper function (down only) and passing along an
accumulator:
\begin{code}
type MentionM nm a = NameMapper nm -> Bag FAST_STRING -> Bag FAST_STRING

initMentioned :: NameMapper nm -> Bag FAST_STRING -> MentionM nm a -> Bag FAST_STRING
thenMent_  :: MentionM nm a -> MentionM nm b -> MentionM nm b
returnNothing :: MentionM nm a
mapMent	   :: (a -> MentionM nm b) -> [a] -> MentionM nm b
mentionedName  :: FAST_STRING   -> MentionM nm a
mentionedNames :: [FAST_STRING] -> MentionM nm a
lookupAndAdd   :: ProtoName -> MentionM nm a

initMentioned val_nf acc action = action val_nf acc

returnNothing val_nf acc = acc

thenMent_ act1 act2 val_nf acc
  = act2 val_nf (act1 val_nf acc)

mapMent f []     = returnNothing
mapMent f (x:xs)
  = f x		    `thenMent_`
    mapMent f xs

mentionedName name val_nf acc
  = acc `snocBag` name

mentionedNames names val_nf acc
  = acc `unionBags` listToBag names

lookupAndAdd (Unk str) val_nf acc
  | _LENGTH_ str >= 3 -- simply don't bother w/ very short names...
  = case (val_nf str) of
      Nothing -> acc `snocBag` str
      Just _  -> acc

lookupAndAdd _ _ acc = acc -- carry on with what we had
\end{code}

\begin{code}
mention_IE :: [IE ProtoName] -> (Bool, Bag FAST_STRING)

mention_IE exps
  = foldr men (False, emptyBag) exps
  where
    men (IEVar str) (dotdot_seen, so_far) = (dotdot_seen, so_far `snocBag` str)
    men (IEModuleContents _)  (_, so_far) = (True, so_far)
    men other_ie    	      acc   	  = acc
\end{code}

\begin{code}
classDecl (ClassDecl _ _ _ _ binds _ _)  = monoBinds True{-toplev-} binds
instDecl  (InstDecl _ _ binds _ _ _ _ _) = monoBinds True{-toplev-} binds
\end{code}

\begin{code}
bindsDecls toplev EmptyBinds	 = returnNothing
bindsDecls toplev (ThenBinds a b)= bindsDecls toplev a `thenMent_` bindsDecls toplev b
bindsDecls toplev (SingleBind a) = bindDecls toplev a
bindsDecls toplev (BindWith a _) = bindDecls toplev a

bindDecls toplev EmptyBind 	 = returnNothing
bindDecls toplev (NonRecBind a)  = monoBinds toplev a
bindDecls toplev (RecBind a)	 = monoBinds toplev a

monoBinds toplev EmptyMonoBinds  = returnNothing
monoBinds toplev (AndMonoBinds a b) = monoBinds toplev a `thenMent_` monoBinds toplev b
monoBinds toplev (PatMonoBind p gb _)
  = (if toplev
    then mentionedNames (map stringify (collectPatBinders p))
    else returnNothing)	`thenMent_`
    grhssAndBinds gb

monoBinds toplev (FunMonoBind v ms _)
  = (if toplev
    then mentionedName (stringify v)
    else returnNothing) `thenMent_`
    mapMent match ms

stringify :: ProtoName -> FAST_STRING
stringify (Unk s) = s
\end{code}

\begin{code}
match (PatMatch _ m) = match m
match (GRHSMatch gb) = grhssAndBinds gb

grhssAndBinds (GRHSsAndBindsIn gs bs)
  = mapMent grhs gs `thenMent_` bindsDecls False bs

grhs (OtherwiseGRHS e _) = expr e
grhs (GRHS g e _)	 = expr g  `thenMent_` expr e
\end{code}

\begin{code}
expr (HsVar v)  = lookupAndAdd v

expr (HsLit _) = returnNothing
expr (HsLam m) = match m
expr (HsApp a b)    = expr a `thenMent_` expr b
expr (OpApp a b c)  = expr a `thenMent_` expr b `thenMent_` expr c
expr (SectionL a b) = expr a `thenMent_` expr b
expr (SectionR a b) = expr a `thenMent_` expr b
expr (CCall _ es _ _ _) = mapMent expr es
expr (HsSCC _ e)    = expr e
expr (HsCase e ms _)= expr e `thenMent_` mapMent match ms
expr (HsLet b e)    = expr e `thenMent_` bindsDecls False{-not toplev-} b
expr (HsDo bs _)    = panic "mentioned_whatnot:RdrHsSyn:HsDo"
expr (ListComp e q) = expr e `thenMent_` mapMent qual  q
expr (ExplicitList es)   = mapMent expr es
expr (ExplicitTuple es)  = mapMent expr es
expr (RecordCon con  rbinds) = panic "mentioned:RdrHsSyn:RecordCon"
expr (RecordUpd aexp rbinds) = panic "mentioned:RdrHsSyn:RecordUpd"
expr (ExprWithTySig e _) = expr e
expr (HsIf b t e _) = expr b `thenMent_` expr t `thenMent_` expr e
expr (ArithSeqIn s) = arithSeq s

arithSeq (From	     a)	    = expr a
arithSeq (FromThen   a b)   = expr a `thenMent_` expr b
arithSeq (FromTo     a b)   = expr a `thenMent_` expr b
arithSeq (FromThenTo a b c) = expr a `thenMent_` expr b `thenMent_` expr c

qual (GeneratorQual _ e) = expr e
qual (FilterQual e)  	 = expr e
qual (LetQual bs)	 = bindsDecls False{-not toplev-} bs
-}
\end{code}
