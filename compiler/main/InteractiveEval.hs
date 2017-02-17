{-# LANGUAGE CPP, MagicHash, NondecreasingIndentation, UnboxedTuples,
    RecordWildCards, BangPatterns #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2007
--
-- Running statements interactively
--
-- -----------------------------------------------------------------------------

module InteractiveEval (
        Resume(..), History(..),
        execStmt, ExecOptions(..), execOptions, ExecResult(..), resumeExec,
        runDecls, runDeclsWithLocation,
        isStmt, hasImport, isImport, isDecl,
        parseImportDecl, SingleStep(..),
        abandon, abandonAll,
        getResumeContext,
        getHistorySpan,
        getModBreaks,
        getHistoryModule,
        back, forward,
        setContext, getContext,
        availsToGlobalRdrEnv,
        getNamesInScope,
        getRdrNamesInScope,
        moduleIsInterpreted,
        getInfo,
        exprType,
        typeKind,
        parseName,
        showModule,
        isModuleInterpreted,
        parseExpr, compileParsedExpr,
        compileExpr, dynCompileExpr,
        compileExprRemote, compileParsedExprRemote,
        Term(..), obtainTermFromId, obtainTermFromVal, reconstructType
        ) where

#include "HsVersions.h"

import InteractiveEvalTypes

import GHCi
import GHCi.Message
import GHCi.RemoteTypes
import GhcMonad
import HscMain
import HsSyn
import HscTypes
import InstEnv
import IfaceEnv   ( newInteractiveBinder )
import FamInstEnv ( FamInst )
import CoreFVs    ( orphNamesOfFamInst )
import TyCon
import Type             hiding( typeKind )
import RepType
import TcType           hiding( typeKind )
import Var
import Id
import Name             hiding ( varName )
import NameSet
import Avail
import RdrName
import VarEnv
import ByteCodeTypes
import Linker
import DynFlags
import Unique
import UniqSupply
import MonadUtils
import Module
import PrelNames  ( toDynName, pretendNameIsInScope )
import Panic
import Maybes
import ErrUtils
import SrcLoc
import RtClosureInspect
import Outputable
import FastString
import Bag
import qualified Lexer (P (..), ParseResult(..), unP, mkPState)
import qualified Parser (parseStmt, parseModule, parseDeclaration, parseImport)

import System.Directory
import Data.Dynamic
import Data.Either
import qualified Data.IntMap as IntMap
import Data.List (find,intercalate)
import StringBuffer (stringToStringBuffer)
import Control.Monad
import GHC.Exts
import Data.Array
import Exception

-- -----------------------------------------------------------------------------
-- running a statement interactively

getResumeContext :: GhcMonad m => m [Resume]
getResumeContext = withSession (return . ic_resume . hsc_IC)

mkHistory :: HscEnv -> ForeignHValue -> BreakInfo -> History
mkHistory hsc_env hval bi = History hval bi (findEnclosingDecls hsc_env bi)

getHistoryModule :: History -> Module
getHistoryModule = breakInfo_module . historyBreakInfo

getHistorySpan :: HscEnv -> History -> SrcSpan
getHistorySpan hsc_env History{..} =
  let BreakInfo{..} = historyBreakInfo in
  case lookupHpt (hsc_HPT hsc_env) (moduleName breakInfo_module) of
    Just hmi -> modBreaks_locs (getModBreaks hmi) ! breakInfo_number
    _ -> panic "getHistorySpan"

getModBreaks :: HomeModInfo -> ModBreaks
getModBreaks hmi
  | Just linkable <- hm_linkable hmi,
    [BCOs cbc _] <- linkableUnlinked linkable
  = fromMaybe emptyModBreaks (bc_breaks cbc)
  | otherwise
  = emptyModBreaks -- probably object code

{- | Finds the enclosing top level function name -}
-- ToDo: a better way to do this would be to keep hold of the decl_path computed
-- by the coverage pass, which gives the list of lexically-enclosing bindings
-- for each tick.
findEnclosingDecls :: HscEnv -> BreakInfo -> [String]
findEnclosingDecls hsc_env (BreakInfo modl ix) =
   let hmi = expectJust "findEnclosingDecls" $
             lookupHpt (hsc_HPT hsc_env) (moduleName modl)
       mb = getModBreaks hmi
   in modBreaks_decls mb ! ix

-- | Update fixity environment in the current interactive context.
updateFixityEnv :: GhcMonad m => FixityEnv -> m ()
updateFixityEnv fix_env = do
  hsc_env <- getSession
  let ic = hsc_IC hsc_env
  setSession $ hsc_env { hsc_IC = ic { ic_fix_env = fix_env } }

-- -----------------------------------------------------------------------------
-- execStmt

-- | default ExecOptions
execOptions :: ExecOptions
execOptions = ExecOptions
  { execSingleStep = RunToCompletion
  , execSourceFile = "<interactive>"
  , execLineNumber = 1
  , execWrap = EvalThis -- just run the statement, don't wrap it in anything
  }

-- | Run a statement in the current interactive context.
execStmt
  :: GhcMonad m
  => String             -- ^ a statement (bind or expression)
  -> ExecOptions
  -> m ExecResult
execStmt stmt ExecOptions{..} = do
    hsc_env <- getSession

    -- Turn off -fwarn-unused-local-binds when running a statement, to hide
    -- warnings about the implicit bindings we introduce.
    let ic       = hsc_IC hsc_env -- use the interactive dflags
        idflags' = ic_dflags ic `wopt_unset` Opt_WarnUnusedLocalBinds
        hsc_env' = hsc_env{ hsc_IC = ic{ ic_dflags = idflags' } }

    -- compile to value (IO [HValue]), don't run
    r <- liftIO $ hscStmtWithLocation hsc_env' stmt
                    execSourceFile execLineNumber

    case r of
      -- empty statement / comment
      Nothing -> return (ExecComplete (Right []) 0)

      Just (ids, hval, fix_env) -> do
        updateFixityEnv fix_env

        status <-
          withVirtualCWD $
            liftIO $
              evalStmt hsc_env' (isStep execSingleStep) (execWrap hval)

        let ic = hsc_IC hsc_env
            bindings = (ic_tythings ic, ic_rn_gbl_env ic)

            size = ghciHistSize idflags'

        handleRunStatus execSingleStep stmt bindings ids
                        status (emptyHistory size)


runDecls :: GhcMonad m => String -> m [Name]
runDecls = runDeclsWithLocation "<interactive>" 1

-- | Run some declarations and return any user-visible names that were brought
-- into scope.
runDeclsWithLocation :: GhcMonad m => String -> Int -> String -> m [Name]
runDeclsWithLocation source linenumber expr =
  do
    hsc_env <- getSession
    (tyThings, ic) <- liftIO $ hscDeclsWithLocation hsc_env expr source linenumber

    setSession $ hsc_env { hsc_IC = ic }
    hsc_env <- getSession
    hsc_env' <- liftIO $ rttiEnvironment hsc_env
    setSession hsc_env'
    return $ filter (not . isDerivedOccName . nameOccName)
             -- For this filter, see Note [What to show to users]
           $ map getName tyThings

{- Note [What to show to users]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We don't want to display internally-generated bindings to users.
Things like the coercion axiom for newtypes. These bindings all get
OccNames that users can't write, to avoid the possiblity of name
clashes (in linker symbols).  That gives a convenient way to suppress
them. The relevant predicate is OccName.isDerivedOccName.
See Trac #11051 for more background and examples.
-}

withVirtualCWD :: GhcMonad m => m a -> m a
withVirtualCWD m = do
  hsc_env <- getSession

    -- a virtual CWD is only necessary when we're running interpreted code in
    -- the same process as the compiler.
  if gopt Opt_ExternalInterpreter (hsc_dflags hsc_env) then m else do

  let ic = hsc_IC hsc_env
  let set_cwd = do
        dir <- liftIO $ getCurrentDirectory
        case ic_cwd ic of
           Just dir -> liftIO $ setCurrentDirectory dir
           Nothing  -> return ()
        return dir

      reset_cwd orig_dir = do
        virt_dir <- liftIO $ getCurrentDirectory
        hsc_env <- getSession
        let old_IC = hsc_IC hsc_env
        setSession hsc_env{  hsc_IC = old_IC{ ic_cwd = Just virt_dir } }
        liftIO $ setCurrentDirectory orig_dir

  gbracket set_cwd reset_cwd $ \_ -> m

parseImportDecl :: GhcMonad m => String -> m (ImportDecl RdrName)
parseImportDecl expr = withSession $ \hsc_env -> liftIO $ hscImport hsc_env expr

emptyHistory :: Int -> BoundedList History
emptyHistory size = nilBL size

handleRunStatus :: GhcMonad m
                => SingleStep -> String-> ([TyThing],GlobalRdrEnv) -> [Id]
                -> EvalStatus_ [ForeignHValue] [HValueRef]
                -> BoundedList History
                -> m ExecResult

handleRunStatus step expr bindings final_ids status history
  | RunAndLogSteps <- step = tracing
  | otherwise              = not_tracing
 where
  tracing
    | EvalBreak is_exception apStack_ref ix mod_uniq resume_ctxt _ccs <- status
    , not is_exception
    = do
       hsc_env <- getSession
       let hmi = expectJust "handleRunStatus" $
                   lookupHptDirectly (hsc_HPT hsc_env)
                                     (mkUniqueGrimily mod_uniq)
           modl = mi_module (hm_iface hmi)
           breaks = getModBreaks hmi

       b <- liftIO $
              breakpointStatus hsc_env (modBreaks_flags breaks) ix
       if b
         then not_tracing
           -- This breakpoint is explicitly enabled; we want to stop
           -- instead of just logging it.
         else do
           apStack_fhv <- liftIO $ mkFinalizedHValue hsc_env apStack_ref
           let bi = BreakInfo modl ix
               !history' = mkHistory hsc_env apStack_fhv bi `consBL` history
                 -- history is strict, otherwise our BoundedList is pointless.
           fhv <- liftIO $ mkFinalizedHValue hsc_env resume_ctxt
           status <- liftIO $ GHCi.resumeStmt hsc_env True fhv
           handleRunStatus RunAndLogSteps expr bindings final_ids
                           status history'
    | otherwise
    = not_tracing

  not_tracing
    -- Hit a breakpoint
    | EvalBreak is_exception apStack_ref ix mod_uniq resume_ctxt ccs <- status
    = do
         hsc_env <- getSession
         resume_ctxt_fhv <- liftIO $ mkFinalizedHValue hsc_env resume_ctxt
         apStack_fhv <- liftIO $ mkFinalizedHValue hsc_env apStack_ref
         let hmi = expectJust "handleRunStatus" $
                     lookupHptDirectly (hsc_HPT hsc_env)
                                       (mkUniqueGrimily mod_uniq)
             modl = mi_module (hm_iface hmi)
             bp | is_exception = Nothing
                | otherwise = Just (BreakInfo modl ix)
         (hsc_env1, names, span, decl) <- liftIO $
           bindLocalsAtBreakpoint hsc_env apStack_fhv bp
         let
           resume = Resume
             { resumeStmt = expr, resumeContext = resume_ctxt_fhv
             , resumeBindings = bindings, resumeFinalIds = final_ids
             , resumeApStack = apStack_fhv
             , resumeBreakInfo = bp
             , resumeSpan = span, resumeHistory = toListBL history
             , resumeDecl = decl
             , resumeCCS = ccs
             , resumeHistoryIx = 0 }
           hsc_env2 = pushResume hsc_env1 resume

         setSession hsc_env2
         return (ExecBreak names bp)

    -- Completed successfully
    | EvalComplete allocs (EvalSuccess hvals) <- status
    = do hsc_env <- getSession
         let final_ic = extendInteractiveContextWithIds (hsc_IC hsc_env) final_ids
             final_names = map getName final_ids
         liftIO $ Linker.extendLinkEnv (zip final_names hvals)
         hsc_env' <- liftIO $ rttiEnvironment hsc_env{hsc_IC=final_ic}
         setSession hsc_env'
         return (ExecComplete (Right final_names) allocs)

    -- Completed with an exception
    | EvalComplete alloc (EvalException e) <- status
    = return (ExecComplete (Left (fromSerializableException e)) alloc)

    | otherwise
    = panic "not_tracing" -- actually exhaustive, but GHC can't tell


resumeExec :: GhcMonad m => (SrcSpan->Bool) -> SingleStep -> m ExecResult
resumeExec canLogSpan step
 = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic

   case resume of
     [] -> liftIO $
           throwGhcExceptionIO (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        -- unbind the temporary locals by restoring the TypeEnv from
        -- before the breakpoint, and drop this Resume from the
        -- InteractiveContext.
        let (resume_tmp_te,resume_rdr_env) = resumeBindings r
            ic' = ic { ic_tythings = resume_tmp_te,
                       ic_rn_gbl_env = resume_rdr_env,
                       ic_resume   = rs }
        setSession hsc_env{ hsc_IC = ic' }

        -- remove any bindings created since the breakpoint from the
        -- linker's environment
        let old_names = map getName resume_tmp_te
            new_names = [ n | thing <- ic_tythings ic
                            , let n = getName thing
                            , not (n `elem` old_names) ]
        liftIO $ Linker.deleteFromLinkEnv new_names

        case r of
          Resume { resumeStmt = expr, resumeContext = fhv
                 , resumeBindings = bindings, resumeFinalIds = final_ids
                 , resumeApStack = apStack, resumeBreakInfo = mb_brkpt
                 , resumeSpan = span
                 , resumeHistory = hist } -> do
               withVirtualCWD $ do
                status <- liftIO $ GHCi.resumeStmt hsc_env (isStep step) fhv
                let prevHistoryLst = fromListBL 50 hist
                    hist' = case mb_brkpt of
                       Nothing -> prevHistoryLst
                       Just bi
                         | not $canLogSpan span -> prevHistoryLst
                         | otherwise -> mkHistory hsc_env apStack bi `consBL`
                                                        fromListBL 50 hist
                handleRunStatus step expr bindings final_ids status hist'

back :: GhcMonad m => Int -> m ([Name], Int, SrcSpan, String)
back n = moveHist (+n)

forward :: GhcMonad m => Int -> m ([Name], Int, SrcSpan, String)
forward n = moveHist (subtract n)

moveHist :: GhcMonad m => (Int -> Int) -> m ([Name], Int, SrcSpan, String)
moveHist fn = do
  hsc_env <- getSession
  case ic_resume (hsc_IC hsc_env) of
     [] -> liftIO $
           throwGhcExceptionIO (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        let ix = resumeHistoryIx r
            history = resumeHistory r
            new_ix = fn ix
        --
        when (new_ix > length history) $ liftIO $
           throwGhcExceptionIO (ProgramError "no more logged breakpoints")
        when (new_ix < 0) $ liftIO $
           throwGhcExceptionIO (ProgramError "already at the beginning of the history")

        let
          update_ic apStack mb_info = do
            (hsc_env1, names, span, decl) <-
              liftIO $ bindLocalsAtBreakpoint hsc_env apStack mb_info
            let ic = hsc_IC hsc_env1
                r' = r { resumeHistoryIx = new_ix }
                ic' = ic { ic_resume = r':rs }

            setSession hsc_env1{ hsc_IC = ic' }

            return (names, new_ix, span, decl)

        -- careful: we want apStack to be the AP_STACK itself, not a thunk
        -- around it, hence the cases are carefully constructed below to
        -- make this the case.  ToDo: this is v. fragile, do something better.
        if new_ix == 0
           then case r of
                   Resume { resumeApStack = apStack,
                            resumeBreakInfo = mb_brkpt } ->
                          update_ic apStack mb_brkpt
           else case history !! (new_ix - 1) of
                   History{..} ->
                     update_ic historyApStack (Just historyBreakInfo)


-- -----------------------------------------------------------------------------
-- After stopping at a breakpoint, add free variables to the environment

result_fs :: FastString
result_fs = fsLit "_result"

bindLocalsAtBreakpoint
        :: HscEnv
        -> ForeignHValue
        -> Maybe BreakInfo
        -> IO (HscEnv, [Name], SrcSpan, String)

-- Nothing case: we stopped when an exception was raised, not at a
-- breakpoint.  We have no location information or local variables to
-- bind, all we can do is bind a local variable to the exception
-- value.
bindLocalsAtBreakpoint hsc_env apStack Nothing = do
   let exn_occ = mkVarOccFS (fsLit "_exception")
       span    = mkGeneralSrcSpan (fsLit "<unknown>")
   exn_name <- newInteractiveBinder hsc_env exn_occ span

   let e_fs    = fsLit "e"
       e_name  = mkInternalName (getUnique e_fs) (mkTyVarOccFS e_fs) span
       e_tyvar = mkRuntimeUnkTyVar e_name liftedTypeKind
       exn_id  = Id.mkVanillaGlobal exn_name (mkTyVarTy e_tyvar)

       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContextWithIds ictxt0 [exn_id]
   --
   Linker.extendLinkEnv [(exn_name, apStack)]
   return (hsc_env{ hsc_IC = ictxt1 }, [exn_name], span, "<exception thrown>")

-- Just case: we stopped at a breakpoint, we have information about the location
-- of the breakpoint and the free variables of the expression.
bindLocalsAtBreakpoint hsc_env apStack_fhv (Just BreakInfo{..}) = do
   let
       hmi       = expectJust "bindLocalsAtBreakpoint" $
                     lookupHpt (hsc_HPT hsc_env) (moduleName breakInfo_module)
       breaks    = getModBreaks hmi
       info      = expectJust "bindLocalsAtBreakpoint2" $
                     IntMap.lookup breakInfo_number (modBreaks_breakInfo breaks)
       vars      = cgb_vars info
       result_ty = cgb_resty info
       occs      = modBreaks_vars breaks ! breakInfo_number
       span      = modBreaks_locs breaks ! breakInfo_number
       decl      = intercalate "." $ modBreaks_decls breaks ! breakInfo_number

           -- Filter out any unboxed ids;
           -- we can't bind these at the prompt
       pointers = filter (\(id,_) -> isPointer id) vars
       isPointer id | [rep] <- typePrimRep (idType id)
                    , isGcPtrRep rep                   = True
                    | otherwise                        = False

       (ids, offsets) = unzip pointers

       free_tvs = tyCoVarsOfTypesList (result_ty:map idType ids)

   -- It might be that getIdValFromApStack fails, because the AP_STACK
   -- has been accidentally evaluated, or something else has gone wrong.
   -- So that we don't fall over in a heap when this happens, just don't
   -- bind any free variables instead, and we emit a warning.
   mb_hValues <-
      mapM (getBreakpointVar hsc_env apStack_fhv . fromIntegral) offsets
   when (any isNothing mb_hValues) $
      debugTraceMsg (hsc_dflags hsc_env) 1 $
          text "Warning: _result has been evaluated, some bindings have been lost"

   us <- mkSplitUniqSupply 'I'   -- Dodgy; will give the same uniques every time
   let tv_subst     = newTyVars us free_tvs
       filtered_ids = [ id | (id, Just _hv) <- zip ids mb_hValues ]
       (_,tidy_tys) = tidyOpenTypes emptyTidyEnv $
                      map (substTy tv_subst . idType) filtered_ids

   new_ids     <- zipWith3M mkNewId occs tidy_tys filtered_ids
   result_name <- newInteractiveBinder hsc_env (mkVarOccFS result_fs) span

   let result_id = Id.mkVanillaGlobal result_name
                     (substTy tv_subst result_ty)
       result_ok = isPointer result_id

       final_ids | result_ok = result_id : new_ids
                 | otherwise = new_ids
       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContextWithIds ictxt0 final_ids
       names  = map idName new_ids

   let fhvs = catMaybes mb_hValues
   Linker.extendLinkEnv (zip names fhvs)
   when result_ok $ Linker.extendLinkEnv [(result_name, apStack_fhv)]
   hsc_env1 <- rttiEnvironment hsc_env{ hsc_IC = ictxt1 }
   return (hsc_env1, if result_ok then result_name:names else names, span, decl)
  where
        -- We need a fresh Unique for each Id we bind, because the linker
        -- state is single-threaded and otherwise we'd spam old bindings
        -- whenever we stop at a breakpoint.  The InteractveContext is properly
        -- saved/restored, but not the linker state.  See #1743, test break026.
   mkNewId :: OccName -> Type -> Id -> IO Id
   mkNewId occ ty old_id
     = do { name <- newInteractiveBinder hsc_env occ (getSrcSpan old_id)
          ; return (Id.mkVanillaGlobalWithInfo name ty (idInfo old_id)) }

   newTyVars :: UniqSupply -> [TcTyVar] -> TCvSubst
     -- Similarly, clone the type variables mentioned in the types
     -- we have here, *and* make them all RuntimeUnk tyvars
   newTyVars us tvs
     = mkTvSubstPrs [ (tv, mkTyVarTy (mkRuntimeUnkTyVar name (tyVarKind tv)))
                    | (tv, uniq) <- tvs `zip` uniqsFromSupply us
                    , let name = setNameUnique (tyVarName tv) uniq ]

rttiEnvironment :: HscEnv -> IO HscEnv
rttiEnvironment hsc_env@HscEnv{hsc_IC=ic} = do
   let tmp_ids = [id | AnId id <- ic_tythings ic]
       incompletelyTypedIds =
           [id | id <- tmp_ids
               , not $ noSkolems id
               , (occNameFS.nameOccName.idName) id /= result_fs]
   hsc_env' <- foldM improveTypes hsc_env (map idName incompletelyTypedIds)
   return hsc_env'
    where
     noSkolems = noFreeVarsOfType . idType
     improveTypes hsc_env@HscEnv{hsc_IC=ic} name = do
      let tmp_ids = [id | AnId id <- ic_tythings ic]
          Just id = find (\i -> idName i == name) tmp_ids
      if noSkolems id
         then return hsc_env
         else do
           mb_new_ty <- reconstructType hsc_env 10 id
           let old_ty = idType id
           case mb_new_ty of
             Nothing -> return hsc_env
             Just new_ty -> do
              case improveRTTIType hsc_env old_ty new_ty of
               Nothing -> return $
                        WARN(True, text (":print failed to calculate the "
                                           ++ "improvement for a type")) hsc_env
               Just subst -> do
                 let dflags = hsc_dflags hsc_env
                 when (dopt Opt_D_dump_rtti dflags) $
                      printInfoForUser dflags alwaysQualify $
                      fsep [text "RTTI Improvement for", ppr id, equals, ppr subst]

                 let ic' = substInteractiveContext ic subst
                 return hsc_env{hsc_IC=ic'}

pushResume :: HscEnv -> Resume -> HscEnv
pushResume hsc_env resume = hsc_env { hsc_IC = ictxt1 }
  where
        ictxt0 = hsc_IC hsc_env
        ictxt1 = ictxt0 { ic_resume = resume : ic_resume ictxt0 }

-- -----------------------------------------------------------------------------
-- Abandoning a resume context

abandon :: GhcMonad m => m Bool
abandon = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic
   case resume of
      []    -> return False
      r:rs  -> do
         setSession hsc_env{ hsc_IC = ic { ic_resume = rs } }
         liftIO $ abandonStmt hsc_env (resumeContext r)
         return True

abandonAll :: GhcMonad m => m Bool
abandonAll = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic
   case resume of
      []  -> return False
      rs  -> do
         setSession hsc_env{ hsc_IC = ic { ic_resume = [] } }
         liftIO $ mapM_ (abandonStmt hsc_env. resumeContext) rs
         return True

-- -----------------------------------------------------------------------------
-- Bounded list, optimised for repeated cons

data BoundedList a = BL
                        {-# UNPACK #-} !Int  -- length
                        {-# UNPACK #-} !Int  -- bound
                        [a] -- left
                        [a] -- right,  list is (left ++ reverse right)

nilBL :: Int -> BoundedList a
nilBL bound = BL 0 bound [] []

consBL :: a -> BoundedList a -> BoundedList a
consBL a (BL len bound left right)
  | len < bound = BL (len+1) bound (a:left) right
  | null right  = BL len     bound [a]      $! tail (reverse left)
  | otherwise   = BL len     bound (a:left) $! tail right

toListBL :: BoundedList a -> [a]
toListBL (BL _ _ left right) = left ++ reverse right

fromListBL :: Int -> [a] -> BoundedList a
fromListBL bound l = BL (length l) bound l []

-- lenBL (BL len _ _ _) = len

-- -----------------------------------------------------------------------------
-- | Set the interactive evaluation context.
--
-- (setContext imports) sets the ic_imports field (which in turn
-- determines what is in scope at the prompt) to 'imports', and
-- constructs the ic_rn_glb_env environment to reflect it.
--
-- We retain in scope all the things defined at the prompt, and kept
-- in ic_tythings.  (Indeed, they shadow stuff from ic_imports.)

setContext :: GhcMonad m => [InteractiveImport] -> m ()
setContext imports
  = do { hsc_env <- getSession
       ; let dflags = hsc_dflags hsc_env
       ; all_env_err <- liftIO $ findGlobalRdrEnv hsc_env imports
       ; case all_env_err of
           Left (mod, err) ->
               liftIO $ throwGhcExceptionIO (formatError dflags mod err)
           Right all_env -> do {
       ; let old_ic         = hsc_IC hsc_env
             !final_rdr_env = all_env `icExtendGblRdrEnv` ic_tythings old_ic
       ; setSession
         hsc_env{ hsc_IC = old_ic { ic_imports    = imports
                                  , ic_rn_gbl_env = final_rdr_env }}}}
  where
    formatError dflags mod err = ProgramError . showSDoc dflags $
      text "Cannot add module" <+> ppr mod <+>
      text "to context:" <+> text err

findGlobalRdrEnv :: HscEnv -> [InteractiveImport]
                 -> IO (Either (ModuleName, String) GlobalRdrEnv)
-- Compute the GlobalRdrEnv for the interactive context
findGlobalRdrEnv hsc_env imports
  = do { idecls_env <- hscRnImportDecls hsc_env idecls
                    -- This call also loads any orphan modules
       ; return $ case partitionEithers (map mkEnv imods) of
           ([], imods_env) -> Right (foldr plusGlobalRdrEnv idecls_env imods_env)
           (err : _, _)    -> Left err }
  where
    idecls :: [LImportDecl RdrName]
    idecls = [noLoc d | IIDecl d <- imports]

    imods :: [ModuleName]
    imods = [m | IIModule m <- imports]

    mkEnv mod = case mkTopLevEnv (hsc_HPT hsc_env) mod of
      Left err -> Left (mod, err)
      Right env -> Right env

availsToGlobalRdrEnv :: ModuleName -> [AvailInfo] -> GlobalRdrEnv
availsToGlobalRdrEnv mod_name avails
  = mkGlobalRdrEnv (gresFromAvails (Just imp_spec) avails)
  where
      -- We're building a GlobalRdrEnv as if the user imported
      -- all the specified modules into the global interactive module
    imp_spec = ImpSpec { is_decl = decl, is_item = ImpAll}
    decl = ImpDeclSpec { is_mod = mod_name, is_as = mod_name,
                         is_qual = False,
                         is_dloc = srcLocSpan interactiveSrcLoc }

mkTopLevEnv :: HomePackageTable -> ModuleName -> Either String GlobalRdrEnv
mkTopLevEnv hpt modl
  = case lookupHpt hpt modl of
      Nothing -> Left "not a home module"
      Just details ->
         case mi_globals (hm_iface details) of
                Nothing  -> Left "not interpreted"
                Just env -> Right env

-- | Get the interactive evaluation context, consisting of a pair of the
-- set of modules from which we take the full top-level scope, and the set
-- of modules from which we take just the exports respectively.
getContext :: GhcMonad m => m [InteractiveImport]
getContext = withSession $ \HscEnv{ hsc_IC=ic } ->
             return (ic_imports ic)

-- | Returns @True@ if the specified module is interpreted, and hence has
-- its full top-level scope available.
moduleIsInterpreted :: GhcMonad m => Module -> m Bool
moduleIsInterpreted modl = withSession $ \h ->
 if moduleUnitId modl /= thisPackage (hsc_dflags h)
        then return False
        else case lookupHpt (hsc_HPT h) (moduleName modl) of
                Just details       -> return (isJust (mi_globals (hm_iface details)))
                _not_a_home_module -> return False

-- | Looks up an identifier in the current interactive context (for :info)
-- Filter the instances by the ones whose tycons (or clases resp)
-- are in scope (qualified or otherwise).  Otherwise we list a whole lot too many!
-- The exact choice of which ones to show, and which to hide, is a judgement call.
--      (see Trac #1581)
getInfo :: GhcMonad m => Bool -> Name -> m (Maybe (TyThing,Fixity,[ClsInst],[FamInst]))
getInfo allInfo name
  = withSession $ \hsc_env ->
    do mb_stuff <- liftIO $ hscTcRnGetInfo hsc_env name
       case mb_stuff of
         Nothing -> return Nothing
         Just (thing, fixity, cls_insts, fam_insts) -> do
           let rdr_env = ic_rn_gbl_env (hsc_IC hsc_env)

           -- Filter the instances based on whether the constituent names of their
           -- instance heads are all in scope.
           let cls_insts' = filter (plausible rdr_env . orphNamesOfClsInst) cls_insts
               fam_insts' = filter (plausible rdr_env . orphNamesOfFamInst) fam_insts
           return (Just (thing, fixity, cls_insts', fam_insts'))
  where
    plausible rdr_env names
          -- Dfun involving only names that are in ic_rn_glb_env
        = allInfo
       || nameSetAll ok names
        where   -- A name is ok if it's in the rdr_env,
                -- whether qualified or not
          ok n | n == name              = True
                       -- The one we looked for in the first place!
               | pretendNameIsInScope n = True
               | isBuiltInSyntax n      = True
               | isExternalName n       = isJust (lookupGRE_Name rdr_env n)
               | otherwise              = True

-- | Returns all names in scope in the current interactive context
getNamesInScope :: GhcMonad m => m [Name]
getNamesInScope = withSession $ \hsc_env -> do
  return (map gre_name (globalRdrEnvElts (ic_rn_gbl_env (hsc_IC hsc_env))))

-- | Returns all 'RdrName's in scope in the current interactive
-- context, excluding any that are internally-generated.
getRdrNamesInScope :: GhcMonad m => m [RdrName]
getRdrNamesInScope = withSession $ \hsc_env -> do
  let
      ic = hsc_IC hsc_env
      gbl_rdrenv = ic_rn_gbl_env ic
      gbl_names = concatMap greRdrNames $ globalRdrEnvElts gbl_rdrenv
  -- Exclude internally generated names; see e.g. Trac #11328
  return (filter (not . isDerivedOccName . rdrNameOcc) gbl_names)


-- | Parses a string as an identifier, and returns the list of 'Name's that
-- the identifier can refer to in the current interactive context.
parseName :: GhcMonad m => String -> m [Name]
parseName str = withSession $ \hsc_env -> liftIO $
   do { lrdr_name <- hscParseIdentifier hsc_env str
      ; hscTcRnLookupRdrName hsc_env $ unLEmb lrdr_name }

-- | Returns @True@ if passed string is a statement.
isStmt :: DynFlags -> String -> Bool
isStmt dflags stmt =
  case parseThing Parser.parseStmt dflags stmt of
    Lexer.POk _ _ -> True
    Lexer.PFailed _ _ -> False

-- | Returns @True@ if passed string has an import declaration.
hasImport :: DynFlags -> String -> Bool
hasImport dflags stmt =
  case parseThing Parser.parseModule dflags stmt of
    Lexer.POk _ thing -> hasImports thing
    Lexer.PFailed _ _ -> False
  where
    hasImports = not . null . hsmodImports . unLoc

-- | Returns @True@ if passed string is an import declaration.
isImport :: DynFlags -> String -> Bool
isImport dflags stmt =
  case parseThing Parser.parseImport dflags stmt of
    Lexer.POk _ _ -> True
    Lexer.PFailed _ _ -> False

-- | Returns @True@ if passed string is a declaration but __/not a splice/__.
isDecl :: DynFlags -> String -> Bool
isDecl dflags stmt = do
  case parseThing Parser.parseDeclaration dflags stmt of
    Lexer.POk _ thing ->
      case unLoc thing of
        SpliceD _ -> False
        _ -> True
    Lexer.PFailed _ _ -> False

parseThing :: Lexer.P thing -> DynFlags -> String -> Lexer.ParseResult thing
parseThing parser dflags stmt = do
  let buf = stringToStringBuffer stmt
      loc = mkRealSrcLoc (fsLit "<interactive>") 1 1

  Lexer.unP parser (Lexer.mkPState dflags buf loc)

-- -----------------------------------------------------------------------------
-- Getting the type of an expression

-- | Get the type of an expression
-- Returns the type as described by 'TcRnExprMode'
exprType :: GhcMonad m => TcRnExprMode -> String -> m Type
exprType mode expr = withSession $ \hsc_env -> do
   ty <- liftIO $ hscTcExpr hsc_env mode expr
   return $ tidyType emptyTidyEnv ty

-- -----------------------------------------------------------------------------
-- Getting the kind of a type

-- | Get the kind of a  type
typeKind  :: GhcMonad m => Bool -> String -> m (Type, Kind)
typeKind normalise str = withSession $ \hsc_env -> do
   liftIO $ hscKcType hsc_env normalise str

-----------------------------------------------------------------------------
-- Compile an expression, run it and deliver the result

-- | Parse an expression, the parsed expression can be further processed and
-- passed to compileParsedExpr.
parseExpr :: GhcMonad m => String -> m (LHsExpr RdrName)
parseExpr expr = withSession $ \hsc_env -> do
  liftIO $ runInteractiveHsc hsc_env $ hscParseExpr expr

-- | Compile an expression, run it and deliver the resulting HValue.
compileExpr :: GhcMonad m => String -> m HValue
compileExpr expr = do
  parsed_expr <- parseExpr expr
  compileParsedExpr parsed_expr

-- | Compile an expression, run it and deliver the resulting HValue.
compileExprRemote :: GhcMonad m => String -> m ForeignHValue
compileExprRemote expr = do
  parsed_expr <- parseExpr expr
  compileParsedExprRemote parsed_expr

-- | Compile an parsed expression (before renaming), run it and deliver
-- the resulting HValue.
compileParsedExprRemote :: GhcMonad m => LHsExpr RdrName -> m ForeignHValue
compileParsedExprRemote expr@(L loc _) = withSession $ \hsc_env -> do
  -- > let _compileParsedExpr = expr
  -- Create let stmt from expr to make hscParsedStmt happy.
  -- We will ignore the returned [Id], namely [expr_id], and not really
  -- create a new binding.
  let expr_fs = fsLit "_compileParsedExpr"
      expr_name = mkInternalName (getUnique expr_fs) (mkTyVarOccFS expr_fs) loc
      let_stmt = L loc . LetStmt . L loc . HsValBinds $
        ValBindsIn (unitBag $ mkHsVarBind loc (getRdrName expr_name) expr) []

  Just ([_id], hvals_io, fix_env) <- liftIO $ hscParsedStmt hsc_env let_stmt
  updateFixityEnv fix_env
  status <- liftIO $ evalStmt hsc_env False (EvalThis hvals_io)
  case status of
    EvalComplete _ (EvalSuccess [hval]) -> return hval
    EvalComplete _ (EvalException e) ->
      liftIO $ throwIO (fromSerializableException e)
    _ -> panic "compileParsedExpr"

compileParsedExpr :: GhcMonad m => LHsExpr RdrName -> m HValue
compileParsedExpr expr = do
   fhv <- compileParsedExprRemote expr
   dflags <- getDynFlags
   liftIO $ wormhole dflags fhv

-- | Compile an expression, run it and return the result as a Dynamic.
dynCompileExpr :: GhcMonad m => String -> m Dynamic
dynCompileExpr expr = do
  parsed_expr <- parseExpr expr
  -- > Data.Dynamic.toDyn expr
  let loc = getLoc parsed_expr
      to_dyn_expr = mkHsApp (L loc . HsVar . L loc $ EName
                                                        $ getRdrName toDynName)
                            parsed_expr
  hval <- compileParsedExpr to_dyn_expr
  return (unsafeCoerce# hval :: Dynamic)

-----------------------------------------------------------------------------
-- show a module and it's source/object filenames

showModule :: GhcMonad m => ModSummary -> m String
showModule mod_summary =
    withSession $ \hsc_env -> do
        interpreted <- isModuleInterpreted mod_summary
        let dflags = hsc_dflags hsc_env
        return (showModMsg dflags (hscTarget dflags) interpreted mod_summary)

isModuleInterpreted :: GhcMonad m => ModSummary -> m Bool
isModuleInterpreted mod_summary = withSession $ \hsc_env ->
  case lookupHpt (hsc_HPT hsc_env) (ms_mod_name mod_summary) of
        Nothing       -> panic "missing linkable"
        Just mod_info -> return (not obj_linkable)
                      where
                         obj_linkable = isObjectLinkable (expectJust "showModule" (hm_linkable mod_info))

----------------------------------------------------------------------------
-- RTTI primitives

obtainTermFromVal :: HscEnv -> Int -> Bool -> Type -> a -> IO Term
obtainTermFromVal hsc_env bound force ty x =
              cvObtainTerm hsc_env bound force ty (unsafeCoerce# x)

obtainTermFromId :: HscEnv -> Int -> Bool -> Id -> IO Term
obtainTermFromId hsc_env bound force id =  do
  let dflags = hsc_dflags hsc_env
  hv <- Linker.getHValue hsc_env (varName id) >>= wormhole dflags
  cvObtainTerm hsc_env bound force (idType id) hv

-- Uses RTTI to reconstruct the type of an Id, making it less polymorphic
reconstructType :: HscEnv -> Int -> Id -> IO (Maybe Type)
reconstructType hsc_env bound id = do
  let dflags = hsc_dflags hsc_env
  hv <- Linker.getHValue hsc_env (varName id) >>= wormhole dflags
  cvReconstructType hsc_env bound (idType id) hv

mkRuntimeUnkTyVar :: Name -> Kind -> TyVar
mkRuntimeUnkTyVar name kind = mkTcTyVar name kind RuntimeUnk
