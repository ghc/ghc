module GHC.Runtime.Context
   ( InteractiveContext (..)
   , InteractiveImport (..)
   , emptyInteractiveContext
   , extendInteractiveContext
   , extendInteractiveContextWithIds
   , setInteractivePrintName
   , substInteractiveContext
   , icExtendGblRdrEnv
   , icInteractiveModule
   , icInScopeTTs
   , icPrintUnqual
   )
where

import GHC.Prelude

import GHC.Hs

import GHC.Driver.Session
import {-# SOURCE #-} GHC.Driver.Plugins

import GHC.Runtime.Eval.Types ( Resume )

import GHC.Unit
import GHC.Unit.Env

import GHC.Core.FamInstEnv
import GHC.Core.InstEnv ( ClsInst, identicalClsInstHead )
import GHC.Core.Type

import GHC.Types.Avail
import GHC.Types.Fixity.Env
import GHC.Types.Id ( isRecordSelector )
import GHC.Types.Id.Info ( IdDetails(..) )
import GHC.Types.Name
import GHC.Types.Name.Env
import GHC.Types.Name.Reader
import GHC.Types.Name.Ppr
import GHC.Types.TyThing
import GHC.Types.Var

import GHC.Builtin.Names ( ioTyConName, printName, mkInteractiveModule )

import GHC.Utils.Outputable
import GHC.Utils.Misc

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
defined.

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

   The 'homeUnitId' field stays as 'main' (or whatever -this-unit-id says.

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
             -- ^ The GHCi top-level scope (ic_rn_gbl_env) is extended with
             -- these imports
             --
             -- This field is only stored here so that the client
             -- can retrieve it with GHC.getContext. GHC itself doesn't
             -- use it, but does reset it to empty sometimes (such
             -- as before a GHC.load). The context is set with GHC.setContext.

         ic_tythings   :: [TyThing],
             -- ^ TyThings defined by the user, in reverse order of
             -- definition (ie most recent at the front)
             -- See Note [ic_tythings]

         ic_rn_gbl_env :: GlobalRdrEnv,
             -- ^ The cached 'GlobalRdrEnv', built by
             -- 'GHC.Runtime.Eval.setContext' and updated regularly
             -- It contains everything in scope at the command line,
             -- including everything in ic_tythings

         ic_instances  :: ([ClsInst], [FamInst]),
             -- ^ All instances and family instances created during
             -- this session.  These are grabbed en masse after each
             -- update to be sure that proper overlapping is retained.
             -- That is, rather than re-check the overlapping each
             -- time we update the context, we just take the results
             -- from the instance code that already does that.

         ic_fix_env :: FixityEnv,
            -- ^ Fixities declared in let statements

         ic_default :: Maybe [Type],
             -- ^ The current default types, set by a 'default' declaration

          ic_resume :: [Resume],
             -- ^ The stack of breakpoint contexts

         ic_monad      :: Name,
             -- ^ The monad that GHCi is executing in

         ic_int_print  :: Name,
             -- ^ The function that is used for printing results
             -- of expressions in ghci and -e mode.

         ic_cwd :: Maybe FilePath,
             -- ^ virtual CWD of the program

         ic_plugins :: ![LoadedPlugin]
             -- ^ Cache of loaded plugins. We store them here to avoid having to
             -- load them everytime we switch to the interctive context.
    }

data InteractiveImport
  = IIDecl (ImportDecl GhcPs)
      -- ^ Bring the exports of a particular module
      -- (filtered by an import decl) into scope

  | IIModule ModuleName
      -- ^ Bring into scope the entire top-level envt of
      -- of this module, including the things imported
      -- into it.


-- | Constructs an empty InteractiveContext.
emptyInteractiveContext :: DynFlags -> InteractiveContext
emptyInteractiveContext dflags
  = InteractiveContext {
       ic_dflags     = dflags,
       ic_imports    = [],
       ic_rn_gbl_env = emptyGlobalRdrEnv,
       ic_mod_index  = 1,
       ic_tythings   = [],
       ic_instances  = ([],[]),
       ic_fix_env    = emptyNameEnv,
       ic_monad      = ioTyConName,  -- IO monad by default
       ic_int_print  = printName,    -- System.IO.print by default
       ic_default    = Nothing,
       ic_resume     = [],
       ic_cwd        = Nothing,
       ic_plugins    = []
       }

icInteractiveModule :: InteractiveContext -> Module
icInteractiveModule (InteractiveContext { ic_mod_index = index })
  = mkInteractiveModule index

-- | This function returns the list of visible TyThings (useful for
-- e.g. showBindings)
icInScopeTTs :: InteractiveContext -> [TyThing]
icInScopeTTs = ic_tythings

-- | Get the PrintUnqualified function based on the flags and this InteractiveContext
icPrintUnqual :: UnitEnv -> InteractiveContext -> PrintUnqualified
icPrintUnqual unit_env InteractiveContext{ ic_rn_gbl_env = grenv } =
    mkPrintUnqualified unit_env grenv

-- | extendInteractiveContext is called with new TyThings recently defined to update the
-- InteractiveContext to include them.  Ids are easily removed when shadowed,
-- but Classes and TyCons are not.  Some work could be done to determine
-- whether they are entirely shadowed, but as you could still have references
-- to them (e.g. instances for classes or values of the type for TyCons), it's
-- not clear whether removing them is even the appropriate behavior.
extendInteractiveContext :: InteractiveContext
                         -> [TyThing]
                         -> [ClsInst] -> [FamInst]
                         -> Maybe [Type]
                         -> FixityEnv
                         -> InteractiveContext
extendInteractiveContext ictxt new_tythings new_cls_insts new_fam_insts defaults fix_env
  = ictxt { ic_mod_index  = ic_mod_index ictxt + 1
                            -- Always bump this; even instances should create
                            -- a new mod_index (#9426)
          , ic_tythings   = new_tythings ++ old_tythings
          , ic_rn_gbl_env = ic_rn_gbl_env ictxt `icExtendGblRdrEnv` new_tythings
          , ic_instances  = ( new_cls_insts ++ old_cls_insts
                            , new_fam_insts ++ fam_insts )
                            -- we don't shadow old family instances (#7102),
                            -- so don't need to remove them here
          , ic_default    = defaults
          , ic_fix_env    = fix_env  -- See Note [Fixity declarations in GHCi]
          }
  where
    new_ids = [id | AnId id <- new_tythings]
    old_tythings = filterOut (shadowed_by new_ids) (ic_tythings ictxt)

    -- Discard old instances that have been fully overridden
    -- See Note [Override identical instances in GHCi]
    (cls_insts, fam_insts) = ic_instances ictxt
    old_cls_insts = filterOut (\i -> any (identicalClsInstHead i) new_cls_insts) cls_insts

extendInteractiveContextWithIds :: InteractiveContext -> [Id] -> InteractiveContext
-- Just a specialised version
extendInteractiveContextWithIds ictxt new_ids
  | null new_ids = ictxt
  | otherwise    = ictxt { ic_mod_index  = ic_mod_index ictxt + 1
                         , ic_tythings   = new_tythings ++ old_tythings
                         , ic_rn_gbl_env = ic_rn_gbl_env ictxt `icExtendGblRdrEnv` new_tythings }
  where
    new_tythings = map AnId new_ids
    old_tythings = filterOut (shadowed_by new_ids) (ic_tythings ictxt)

shadowed_by :: [Id] -> TyThing -> Bool
shadowed_by ids = shadowed
  where
    -- Keep record selectors because they might be needed by HasField (#19322)
    shadowed (AnId id) | isRecordSelector id = False
    shadowed tything = getOccName tything `elemOccSet` new_occs
    new_occs = mkOccSet (map getOccName ids)

setInteractivePrintName :: InteractiveContext -> Name -> InteractiveContext
setInteractivePrintName ic n = ic{ic_int_print = n}

    -- ToDo: should not add Ids to the gbl env here

-- | Add TyThings to the GlobalRdrEnv, earlier ones in the list shadowing
-- later ones, and shadowing existing entries in the GlobalRdrEnv.
icExtendGblRdrEnv :: GlobalRdrEnv -> [TyThing] -> GlobalRdrEnv
icExtendGblRdrEnv env tythings
  = foldr add env tythings  -- Foldr makes things in the front of
                            -- the list shadow things at the back
  where
    -- One at a time, to ensure each shadows the previous ones
    add thing env
       | is_sub_bndr thing
       = env
       | otherwise
       = foldl' extendGlobalRdrEnv env1 (concatMap localGREsFromAvail avail)
       where
          env1  = shadowNames env (concatMap availGreNames avail)
          avail = tyThingAvailInfo thing

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

substInteractiveContext :: InteractiveContext -> TCvSubst -> InteractiveContext
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

