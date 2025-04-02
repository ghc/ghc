module GHC.Runtime.Context
   ( InteractiveContext (..)
   , InteractiveImport (..)
   , emptyInteractiveContext
   , extendInteractiveContext
   , extendInteractiveContextWithIds
   , setInteractivePrintName
   , substInteractiveContext
   , replaceImportEnv
   , icReaderEnv
   , icExtendGblRdrEnv
   , icInteractiveModule
   , icInScopeTTs
   , icNamePprCtx
   )
where

import GHC.Prelude

import GHC.Hs

import GHC.Driver.DynFlags
import {-# SOURCE #-} GHC.Driver.Plugins

import GHC.Runtime.Eval.Types ( IcGlobalRdrEnv(..), Resume )

import GHC.Unit
import GHC.Unit.Env

import GHC.Core.FamInstEnv
import GHC.Core.InstEnv
import GHC.Core.Type

import GHC.Types.DefaultEnv ( DefaultEnv, emptyDefaultEnv )
import GHC.Types.Fixity.Env
import GHC.Types.Id.Info ( IdDetails(..) )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Name.Ppr
import GHC.Types.TyThing
import GHC.Types.Var

import GHC.Builtin.Names ( ioTyConName, printName, mkInteractiveModule )

import GHC.Utils.Outputable

{-
Note [The interactive package]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type, class, and value declarations at the command prompt are treated
as if they were defined in modules
   interactive:Ghci1
   interactive:Ghci2
   ...etc...
with each bunch of declarations using a new module, all sharing a
common package 'interactive' (see Module.interactiveUnitId, and
GHC.Builtin.Names.mkInteractiveModule).

This scheme deals well with shadowing.  For example:

   ghci> data T = A
   ghci> data T = B
   ghci> :i A
   data Ghci1.T = A  -- Defined at <interactive>:2:10

Here we must display info about constructor A, but its type T has been
shadowed by the second declaration.  But it has a respectable
qualified name (Ghci1.T), and its source location says where it was
defined, and it can also be used with the qualified name.

So the main invariant continues to hold, that in any session an
original name M.T only refers to one unique thing.  (In a previous
iteration both the T's above were called :Interactive.T, albeit with
different uniques, which gave rise to all sorts of trouble.)

The details are a bit tricky though:

 * The field ic_mod_index counts which Ghci module we've got up to.
   It is incremented when extending ic_tythings

 * ic_tythings contains only things from the 'interactive' package.

 * Module from the 'interactive' package (Ghci1, Ghci2 etc) never go
   in the Home Package Table (HPT).  When you say :load, that's when we
   extend the HPT.

 * The 'homeUnitId' field of DynFlags is *not* set to 'interactive'.
   It stays as 'main' (or whatever -this-unit-id says), and is the
   package to which :load'ed modules are added to.

 * So how do we arrange that declarations at the command prompt get to
   be in the 'interactive' package?  Simply by setting the tcg_mod
   field of the TcGblEnv to "interactive:Ghci1".  This is done by the
   call to initTc in initTcInteractive, which in turn get the module
   from it 'icInteractiveModule' field of the interactive context.

   The 'homeUnitId' field stays as 'main' (or whatever -this-unit-id says).

 * The main trickiness is that the type environment (tcg_type_env) and
   fixity envt (tcg_fix_env), now contain entities from all the
   interactive-package modules (Ghci1, Ghci2, ...) together, rather
   than just a single module as is usually the case.  So you can't use
   "nameIsLocalOrFrom" to decide whether to look in the TcGblEnv vs
   the HPT/PTE.  This is a change, but not a problem provided you
   know.

* However, the tcg_binds, tcg_sigs, tcg_insts, tcg_fam_insts, etc fields
  of the TcGblEnv, which collect "things defined in this module", all
  refer to stuff define in a single GHCi command, *not* all the commands
  so far.

  In contrast, tcg_inst_env, tcg_fam_inst_env, have instances from
  all GhciN modules, which makes sense -- they are all "home package"
  modules.


Note [Relation between the 'InteractiveContext' and 'interactiveGhciUnitId']
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The 'InteractiveContext' is used to store 'DynFlags', 'Plugins' and similar
information about the so-called interactive "home unit". We are using
quotes here, since, originally, GHC wasn't aware of more than one 'HomeUnitEnv's.
So the 'InteractiveContext' was a hack/solution to have 'DynFlags' and 'Plugins'
independent of the 'DynFlags' and 'Plugins' stored in 'HscEnv'.
Nowadays, GHC has support for multiple home units via the 'HomeUnitGraph', thus,
this part of the 'InteractiveContext' is strictly speaking redundant, as we
can simply manage one 'HomeUnitEnv' for the 'DynFlags' and 'Plugins' that are
currently stored in the 'InteractiveContext'.

As a matter of fact, that's exactly what we do nowadays.
That means, we can also lift other restrictions in the future, for example
allowing @:seti@ commands to modify the package-flags, since we now have a
separate 'UnitState' for the interactive session.
However, we did not rip out 'ic_dflags' and 'ic_plugins', yet, as it makes
it easier to access them for functions that want to use the interactive 'DynFlags',
such as 'runInteractiveHsc' and 'mkInteractiveHscEnv', without having to look that
information up in the 'HomeUnitGraph'.
It is reasonable to change this in the future, and remove 'ic_dflags' and 'ic_plugins'.

We keep 'ic_dflags' and 'ic_plugins' around, but we also store a 'HomeUnitEnv'
for the 'DynFlags' and 'Plugins' of the interactive session.

It is important to keep the 'DynFlags' in these two places consistent.

In other words, whenever you update the 'DynFlags' of the 'interactiveGhciUnitId'
in the 'HscEnv', then you also need to update the 'DynFlags' of the
'InteractiveContext'.
The easiest way to update them is via 'setInteractiveDynFlags'.
However, careful, footgun! It is very easy to call 'setInteractiveDynFlags'
and forget to call 'normaliseInteractiveDynFlags' on the 'DynFlags' in the
'HscEnv'! This is important, because you may, accidentally, have enabled
Language Extensions that are not supported in the interactive ghc session,
which we do not want.

To summarise, the 'ic_dflags' and 'ic_plugins' are currently used to
conveniently cache them for easy access.
The 'ic_dflags' must be identical to the 'DynFlags' stored in the 'HscEnv'
for the 'HomeUnitEnv' identified by 'interactiveGhciUnitId'.

See Note [Multiple Home Units aware GHCi] for the design and rationale for
the current 'interactiveGhciUnitId'.

Note [Interactively-bound Ids in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The Ids bound by previous Stmts in GHCi are currently
        a) GlobalIds, with
        b) An External Name, like Ghci4.foo
           See Note [The interactive package] above
        c) A tidied type

 (a) They must be GlobalIds (not LocalIds) otherwise when we come to
     compile an expression using these ids later, the byte code
     generator will consider the occurrences to be free rather than
     global.

 (b) Having an External Name is important because of Note
     [GlobalRdrEnv shadowing] in GHC.Types.Names.RdrName

 (c) Their types are tidied. This is important, because :info may ask
     to look at them, and :info expects the things it looks up to have
     tidy types

Where do interactively-bound Ids come from?

  - GHCi REPL Stmts   e.g.
         ghci> let foo x = x+1
    These start with an Internal Name because a Stmt is a local
    construct, so the renamer naturally builds an Internal name for
    each of its binders.  Then in tcRnStmt they are externalised via
    GHC.Tc.Module.externaliseAndTidyId, so they get Names like Ghic4.foo.

  - Ids bound by the debugger etc have Names constructed by
    GHC.Iface.Env.newInteractiveBinder; at the call sites it is followed by
    mkVanillaGlobal or mkVanillaGlobalWithInfo.  So again, they are
    all Global, External.

  - TyCons, Classes, and Ids bound by other top-level declarations in
    GHCi (eg foreign import, record selectors) also get External
    Names, with Ghci9 (or 8, or 7, etc) as the module name.


Note [ic_tythings]
~~~~~~~~~~~~~~~~~~
The ic_tythings field contains
  * The TyThings declared by the user at the command prompt
    (eg Ids, TyCons, Classes)

  * The user-visible Ids that arise from such things, which
    *don't* come from 'implicitTyThings', notably:
       - record selectors
       - class ops
    The implicitTyThings are readily obtained from the TyThings
    but record selectors etc are not

It does *not* contain
  * DFunIds (they can be gotten from ic_instances)
  * CoAxioms (ditto)

See also Note [Interactively-bound Ids in GHCi]

Note [Override identical instances in GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you declare a new instance in GHCi that is identical to a previous one,
we simply override the previous one; we don't regard it as overlapping.
e.g.    Prelude> data T = A | B
        Prelude> instance Eq T where ...
        Prelude> instance Eq T where ...   -- This one overrides

It's exactly the same for type-family instances.  See #7102

Note [icReaderEnv recalculation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GlobalRdrEnv describing what’s in scope at the prompts consists
of all the imported things, followed by all the things defined on the prompt,
with shadowing. Defining new things on the prompt is easy: we shadow as needed,
and then extend the environment.

But changing the set of imports, which can happen later as well, is tricky
we need to re-apply the shadowing from all the things defined at the prompt!

For example:

    ghci> let empty = True
    ghci> import Data.IntMap.Strict     -- Exports 'empty'
    ghci> empty   -- Still gets the 'empty' defined at the prompt
    True

It would be correct to re-construct the env from scratch based on
`ic_tythings`, but that'd be quite expensive if there are many entries in
`ic_tythings` that shadow each other.

Therefore we keep around a `GlobalRdrEnv` in `igre_prompt_env` that contains
_just_ the things defined at the prompt, and use that in `replaceImportEnv` to
rebuild the full env.  Conveniently, `shadowNames` takes such an `OccEnv`
to denote the set of names to shadow.

INVARIANT: Every `OccName` in `igre_prompt_env` is present unqualified as well
(else it would not be right to pass `igre_prompt_env` to `shadowNames`.)

The definition of the IcGlobalRdrEnv type should conceptually be in this module,
and made abstract, but it’s used in `Resume`, so it lives in GHC.Runtime.Eval.Type.

-}

-- | Interactive context, recording information about the state of the
-- context in which statements are executed in a GHCi session.
data InteractiveContext
  = InteractiveContext {
         ic_dflags     :: DynFlags,
             -- ^ The 'DynFlags' used to evaluate interactive expressions
             -- and statements.

         ic_mod_index :: Int,
             -- ^ Each GHCi stmt or declaration brings some new things into
             -- scope. We give them names like interactive:Ghci9.T,
             -- where the ic_index is the '9'.  The ic_mod_index is
             -- incremented whenever we add something to ic_tythings
             -- See Note [The interactive package]

         ic_imports :: [InteractiveImport],
             -- ^ The GHCi top-level scope (icReaderEnv) is extended with
             -- these imports
             --
             -- This field is only stored here so that the client
             -- can retrieve it with GHC.getContext. GHC itself doesn't
             -- use it, but does reset it to empty sometimes (such
             -- as before a GHC.load). The context is set with GHC.setContext.

         ic_tythings   :: [TyThing],
             -- ^ TyThings defined by the user, in reverse order of
             -- definition (ie most recent at the front).
             -- Also used in GHC.Tc.Module.runTcInteractive to fill the type
             -- checker environment.
             -- See Note [ic_tythings]

         ic_gre_cache :: IcGlobalRdrEnv,
             -- ^ Essentially the cached 'GlobalRdrEnv'.
             --
             -- The GlobalRdrEnv contains everything in scope at the command
             -- line, both imported and everything in ic_tythings, with the
             -- correct shadowing.
             --
             -- The IcGlobalRdrEnv contains extra data to allow efficient
             -- recalculation when the set of imports change.
             -- See Note [icReaderEnv recalculation]

         ic_instances  :: (InstEnv, [FamInst]),
             -- ^ All instances and family instances created during
             -- this session.  These are grabbed en masse after each
             -- update to be sure that proper overlapping is retained.
             -- That is, rather than re-check the overlapping each
             -- time we update the context, we just take the results
             -- from the instance code that already does that.

         ic_fix_env :: FixityEnv,
            -- ^ Fixities declared in let statements

         ic_default :: DefaultEnv,
             -- ^ The current default classes and types, set by 'default' declarations

         ic_resume :: [Resume],
             -- ^ The stack of breakpoint contexts

         ic_monad      :: Name,
             -- ^ The monad that GHCi is executing in

         ic_int_print  :: Name,
             -- ^ The function that is used for printing results
             -- of expressions in ghci and -e mode.

         ic_cwd :: Maybe FilePath,
             -- ^ virtual CWD of the program

         ic_plugins :: !Plugins
             -- ^ Cache of loaded plugins. We store them here to avoid having to
             -- load them every time we switch to the interactive context.
    }

data InteractiveImport
  = IIDecl (ImportDecl GhcPs)
      -- ^ Bring the exports of a particular module
      -- (filtered by an import decl) into scope

  | IIModule Module
      -- ^ Bring into scope the entire top-level envt of
      -- of this module, including the things imported
      -- into it.

emptyIcGlobalRdrEnv :: IcGlobalRdrEnv
emptyIcGlobalRdrEnv = IcGlobalRdrEnv
    { igre_env = emptyGlobalRdrEnv
    , igre_prompt_env = emptyGlobalRdrEnv
    }

-- | Constructs an empty InteractiveContext.
emptyInteractiveContext :: DynFlags -> InteractiveContext
emptyInteractiveContext dflags
  = InteractiveContext {
       ic_dflags     = dflags,
       ic_imports    = [],
       ic_gre_cache  = emptyIcGlobalRdrEnv,
       ic_mod_index  = 1,
       ic_tythings   = [],
       ic_instances  = (emptyInstEnv,[]),
       ic_fix_env    = emptyNameEnv,
       ic_monad      = ioTyConName,  -- IO monad by default
       ic_int_print  = printName,    -- System.IO.print by default
       ic_default    = emptyDefaultEnv,
       ic_resume     = [],
       ic_cwd        = Nothing,
       ic_plugins    = emptyPlugins
       }

icReaderEnv :: InteractiveContext -> GlobalRdrEnv
icReaderEnv = igre_env . ic_gre_cache

icInteractiveModule :: InteractiveContext -> Module
icInteractiveModule (InteractiveContext { ic_mod_index = index })
  = mkInteractiveModule (show index)

-- | This function returns the list of visible TyThings (useful for
-- e.g. showBindings).
--
-- It picks only those TyThings that are not shadowed by later definitions on the interpreter,
-- to not clutter :showBindings with shadowed ids, which would show up as Ghci9.foo.
--
-- Some TyThings define many names; we include them if _any_ name is still
-- available unqualified.
icInScopeTTs :: InteractiveContext -> [TyThing]
icInScopeTTs ictxt = filter in_scope_unqualified (ic_tythings ictxt)
  where
    in_scope_unqualified thing = or
        [ unQualOK gre
        | gre <- tyThingLocalGREs thing
        , let name = greName gre
        , Just gre <- [lookupGRE_Name (icReaderEnv ictxt) name]
        ]

-- | Get the NamePprCtx function based on the flags and this InteractiveContext
icNamePprCtx :: UnitEnv -> InteractiveContext -> NamePprCtx
icNamePprCtx unit_env ictxt = mkNamePprCtx ptc unit_env (icReaderEnv ictxt)
  where ptc = initPromotionTickContext (ic_dflags ictxt)

-- | extendInteractiveContext is called with new TyThings recently defined to update the
-- InteractiveContext to include them. By putting new things first, unqualified
-- use will pick the most recently defined thing with a given name, while
-- still keeping the old names in scope in their qualified form (Ghci1.foo).
extendInteractiveContext :: InteractiveContext
                         -> [TyThing]
                         -> InstEnv -> [FamInst]
                         -> DefaultEnv
                         -> FixityEnv
                         -> InteractiveContext
extendInteractiveContext ictxt new_tythings new_cls_insts new_fam_insts defaults fix_env
  = ictxt { ic_mod_index  = ic_mod_index ictxt + 1
                            -- Always bump this; even instances should create
                            -- a new mod_index (#9426)
          , ic_tythings   = new_tythings ++ ic_tythings ictxt
          , ic_gre_cache  = ic_gre_cache ictxt `icExtendIcGblRdrEnv` new_tythings
          , ic_instances  = ( new_cls_insts `unionInstEnv` old_cls_insts
                            , new_fam_insts ++ fam_insts )
                            -- we don't shadow old family instances (#7102),
                            -- so don't need to remove them here
          , ic_default    = defaults
          , ic_fix_env    = fix_env  -- See Note [Fixity declarations in GHCi]
          }
  where
    -- Discard old instances that have been fully overridden
    -- See Note [Override identical instances in GHCi]
    (cls_insts, fam_insts) = ic_instances ictxt
    old_cls_insts = filterInstEnv (\i -> not $ anyInstEnv (identicalClsInstHead i) new_cls_insts) cls_insts

extendInteractiveContextWithIds :: InteractiveContext -> [Id] -> InteractiveContext
-- Just a specialised version
extendInteractiveContextWithIds ictxt new_ids
  | null new_ids = ictxt
  | otherwise
  = ictxt { ic_mod_index  = ic_mod_index ictxt + 1
          , ic_tythings   = new_tythings ++ ic_tythings ictxt
          , ic_gre_cache  = ic_gre_cache ictxt `icExtendIcGblRdrEnv` new_tythings
          }
  where
    new_tythings = map AnId new_ids

setInteractivePrintName :: InteractiveContext -> Name -> InteractiveContext
setInteractivePrintName ic n = ic{ic_int_print = n}

icExtendIcGblRdrEnv :: IcGlobalRdrEnv -> [TyThing] -> IcGlobalRdrEnv
icExtendIcGblRdrEnv igre tythings = IcGlobalRdrEnv
    { igre_env        = icExtendGblRdrEnv False (igre_env igre)        tythings
    , igre_prompt_env = icExtendGblRdrEnv True  (igre_prompt_env igre) tythings
        -- Pass 'True' <=> drop names that are only available qualified.
        -- This is done to maintain the invariant of Note [icReaderEnv recalculation]
        -- that igre_prompt_env should only contain Names that are available unqualified.
    }

-- This is used by setContext in GHC.Runtime.Eval when the set of imports
-- changes, and recalculates the GlobalRdrEnv. See Note [icReaderEnv recalculation]
replaceImportEnv :: IcGlobalRdrEnv -> GlobalRdrEnv -> IcGlobalRdrEnv
replaceImportEnv igre import_env = igre { igre_env = new_env }
  where
    import_env_shadowed = shadowNames False import_env (igre_prompt_env igre)
    new_env = import_env_shadowed `plusGlobalRdrEnv` igre_prompt_env igre

-- | Add 'TyThings' to the 'GlobalRdrEnv', earlier ones in the list shadowing
-- later ones, and shadowing existing entries in the 'GlobalRdrEnv'.
icExtendGblRdrEnv :: Bool -- ^ discard names that are only available qualified?
                  -> GlobalRdrEnv -> [TyThing] -> GlobalRdrEnv
icExtendGblRdrEnv drop_only_qualified env tythings
  = foldr add env tythings  -- Foldr makes things in the front of
                            -- the list shadow things at the back
  where
    -- One at a time, to ensure each shadows the previous ones
    add thing env
       | is_sub_bndr thing
       = env
       | otherwise
       = foldl' extendGlobalRdrEnv env1 new_gres
       where
          new_gres = tyThingLocalGREs thing
          env1     = shadowNames drop_only_qualified env $ mkGlobalRdrEnv new_gres

    -- Ugh! The new_tythings may include record selectors, since they
    -- are not implicit-ids, and must appear in the TypeEnv.  But they
    -- will also be brought into scope by the corresponding (ATyCon
    -- tc).  And we want the latter, because that has the correct
    -- parent (#10520)
    is_sub_bndr (AnId f) = case idDetails f of
                             RecSelId {}  -> True
                             ClassOpId {} -> True
                             _            -> False
    is_sub_bndr _ = False

substInteractiveContext :: InteractiveContext -> Subst -> InteractiveContext
substInteractiveContext ictxt@InteractiveContext{ ic_tythings = tts } subst
  | isEmptyTCvSubst subst = ictxt
  | otherwise             = ictxt { ic_tythings = map subst_ty tts }
  where
    subst_ty (AnId id)
      = AnId $ updateIdTypeAndMult (substTyAddInScope subst) id
      -- Variables in the interactive context *can* mention free type variables
      -- because of the runtime debugger. Otherwise you'd expect all
      -- variables bound in the interactive context to be closed.
    subst_ty tt
      = tt

instance Outputable InteractiveImport where
  ppr (IIModule m) = char '*' <> ppr m
  ppr (IIDecl d)   = ppr d
