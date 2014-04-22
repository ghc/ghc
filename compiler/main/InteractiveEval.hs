-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2005-2007
--
-- Running statements interactively
--
-- -----------------------------------------------------------------------------

module InteractiveEval (
#ifdef GHCI
        RunResult(..), Status(..), Resume(..), History(..),
        runStmt, runStmtWithLocation, runDecls, runDeclsWithLocation,
        parseImportDecl, SingleStep(..),
        resume,
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
        compileExpr, dynCompileExpr,
        Term(..), obtainTermFromId, obtainTermFromVal, reconstructType
#endif
        ) where

#ifdef GHCI

#include "HsVersions.h"

import InteractiveEvalTypes

import GhcMonad
import HscMain
import HsSyn
import HscTypes
import BasicTypes ( HValue )
import InstEnv
import FamInstEnv ( FamInst, orphNamesOfFamInst )
import TyCon
import Type     hiding( typeKind )
import TcType           hiding( typeKind )
import Var
import Id
import Name             hiding ( varName )
import NameSet
import Avail
import RdrName
import TcRnMonad
import VarSet
import VarEnv
import ByteCodeInstr
import Linker
import DynFlags
import Unique
import UniqSupply
import Module
import Panic
import UniqFM
import Maybes
import ErrUtils
import SrcLoc
import BreakArray
import RtClosureInspect
import Outputable
import FastString

import System.Mem.Weak
import System.Directory
import Data.Dynamic
import Data.Either
import Data.List (find)
import Control.Monad
import Foreign.Safe
import Foreign.C
import GHC.Exts
import Data.Array
import Exception
import Control.Concurrent
import System.IO.Unsafe

-- -----------------------------------------------------------------------------
-- running a statement interactively

getResumeContext :: GhcMonad m => m [Resume]
getResumeContext = withSession (return . ic_resume . hsc_IC)

data SingleStep
   = RunToCompletion
   | SingleStep
   | RunAndLogSteps

isStep :: SingleStep -> Bool
isStep RunToCompletion = False
isStep _ = True

mkHistory :: HscEnv -> HValue -> BreakInfo -> History
mkHistory hsc_env hval bi = let
    decls = findEnclosingDecls hsc_env bi
    in History hval bi decls


getHistoryModule :: History -> Module
getHistoryModule = breakInfo_module . historyBreakInfo

getHistorySpan :: HscEnv -> History -> SrcSpan
getHistorySpan hsc_env hist =
   let inf = historyBreakInfo hist
       num = breakInfo_number inf
   in case lookupUFM (hsc_HPT hsc_env) (moduleName (breakInfo_module inf)) of
       Just hmi -> modBreaks_locs (getModBreaks hmi) ! num
       _ -> panic "getHistorySpan"

getModBreaks :: HomeModInfo -> ModBreaks
getModBreaks hmi
  | Just linkable <- hm_linkable hmi,
    [BCOs _ modBreaks] <- linkableUnlinked linkable
  = modBreaks
  | otherwise
  = emptyModBreaks -- probably object code

{- | Finds the enclosing top level function name -}
-- ToDo: a better way to do this would be to keep hold of the decl_path computed
-- by the coverage pass, which gives the list of lexically-enclosing bindings
-- for each tick.
findEnclosingDecls :: HscEnv -> BreakInfo -> [String]
findEnclosingDecls hsc_env inf =
   let hmi = expectJust "findEnclosingDecls" $
             lookupUFM (hsc_HPT hsc_env) (moduleName $ breakInfo_module inf)
       mb = getModBreaks hmi
   in modBreaks_decls mb ! breakInfo_number inf

-- | Update fixity environment in the current interactive context.
updateFixityEnv :: GhcMonad m => FixityEnv -> m ()
updateFixityEnv fix_env = do
  hsc_env <- getSession
  let ic = hsc_IC hsc_env
  setSession $ hsc_env { hsc_IC = ic { ic_fix_env = fix_env } }

-- | Run a statement in the current interactive context.  Statement
-- may bind multple values.
runStmt :: GhcMonad m => String -> SingleStep -> m RunResult
runStmt = runStmtWithLocation "<interactive>" 1

-- | Run a statement in the current interactive context.  Passing debug information
--   Statement may bind multple values.
runStmtWithLocation :: GhcMonad m => String -> Int ->
                       String -> SingleStep -> m RunResult
runStmtWithLocation source linenumber expr step =
  do
    hsc_env <- getSession

    breakMVar  <- liftIO $ newEmptyMVar  -- wait on this when we hit a breakpoint
    statusMVar <- liftIO $ newEmptyMVar  -- wait on this when a computation is running

    -- Turn off -fwarn-unused-bindings when running a statement, to hide
    -- warnings about the implicit bindings we introduce.
    let ic       = hsc_IC hsc_env -- use the interactive dflags
        idflags' = ic_dflags ic `wopt_unset` Opt_WarnUnusedBinds
        hsc_env' = hsc_env{ hsc_IC = ic{ ic_dflags = idflags' } }

    -- compile to value (IO [HValue]), don't run
    r <- liftIO $ hscStmtWithLocation hsc_env' expr source linenumber

    case r of
      -- empty statement / comment
      Nothing -> return (RunOk [])

      Just (tyThings, hval, fix_env) -> do
        updateFixityEnv fix_env

        status <-
          withVirtualCWD $
            withBreakAction (isStep step) idflags' breakMVar statusMVar $ do
                liftIO $ sandboxIO idflags' statusMVar hval

        let ic = hsc_IC hsc_env
            bindings = (ic_tythings ic, ic_rn_gbl_env ic)

            size = ghciHistSize idflags'

        handleRunStatus step expr bindings tyThings
                        breakMVar statusMVar status (emptyHistory size)

runDecls :: GhcMonad m => String -> m [Name]
runDecls = runDeclsWithLocation "<interactive>" 1

runDeclsWithLocation :: GhcMonad m => String -> Int -> String -> m [Name]
runDeclsWithLocation source linenumber expr =
  do
    hsc_env <- getSession
    (tyThings, ic) <- liftIO $ hscDeclsWithLocation hsc_env expr source linenumber

    setSession $ hsc_env { hsc_IC = ic }
    hsc_env <- getSession
    hsc_env' <- liftIO $ rttiEnvironment hsc_env
    modifySession (\_ -> hsc_env')
    return (map getName tyThings)


withVirtualCWD :: GhcMonad m => m a -> m a
withVirtualCWD m = do
  hsc_env <- getSession
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
                -> MVar () -> MVar Status -> Status -> BoundedList History
                -> m RunResult

handleRunStatus step expr bindings final_ids
               breakMVar statusMVar status history
  | RunAndLogSteps <- step = tracing
  | otherwise              = not_tracing
 where
  tracing
    | Break is_exception apStack info tid <- status
    , not is_exception
    = do
       hsc_env <- getSession
       b <- liftIO $ isBreakEnabled hsc_env info
       if b
         then not_tracing
           -- This breakpoint is explicitly enabled; we want to stop
           -- instead of just logging it.
         else do
           let history' = mkHistory hsc_env apStack info `consBL` history
                 -- probably better make history strict here, otherwise
                 -- our BoundedList will be pointless.
           _ <- liftIO $ evaluate history'
           status <- withBreakAction True (hsc_dflags hsc_env)
                                     breakMVar statusMVar $ do
                     liftIO $ mask_ $ do
                        putMVar breakMVar ()  -- awaken the stopped thread
                        redirectInterrupts tid $
                          takeMVar statusMVar   -- and wait for the result
           handleRunStatus RunAndLogSteps expr bindings final_ids
                           breakMVar statusMVar status history'
    | otherwise
    = not_tracing

  not_tracing
    -- Hit a breakpoint
    | Break is_exception apStack info tid <- status
    = do
         hsc_env <- getSession
         let mb_info | is_exception = Nothing
                     | otherwise    = Just info
         (hsc_env1, names, span) <- liftIO $
           bindLocalsAtBreakpoint hsc_env apStack mb_info
         let
           resume = Resume
             { resumeStmt = expr, resumeThreadId = tid
             , resumeBreakMVar = breakMVar, resumeStatMVar = statusMVar
             , resumeBindings = bindings, resumeFinalIds = final_ids
             , resumeApStack = apStack, resumeBreakInfo = mb_info
             , resumeSpan = span, resumeHistory = toListBL history
             , resumeHistoryIx = 0 }
           hsc_env2 = pushResume hsc_env1 resume
  
         modifySession (\_ -> hsc_env2)
         return (RunBreak tid names mb_info)
  
    -- Completed with an exception
    | Complete (Left e) <- status
    = return (RunException e)
  
    -- Completed successfully
    | Complete (Right hvals) <- status
    = do hsc_env <- getSession
         let final_ic = extendInteractiveContext (hsc_IC hsc_env)
                                                 (map AnId final_ids)
             final_names = map getName final_ids
         liftIO $ Linker.extendLinkEnv (zip final_names hvals)
         hsc_env' <- liftIO $ rttiEnvironment hsc_env{hsc_IC=final_ic}
         modifySession (\_ -> hsc_env')
         return (RunOk final_names)
  
    | otherwise
    = panic "handleRunStatus"  -- The above cases are in fact exhaustive

isBreakEnabled :: HscEnv -> BreakInfo -> IO Bool
isBreakEnabled hsc_env inf =
   case lookupUFM (hsc_HPT hsc_env) (moduleName (breakInfo_module inf)) of
       Just hmi -> do
         w <- getBreak (hsc_dflags hsc_env)
                       (modBreaks_flags (getModBreaks hmi))
                       (breakInfo_number inf)
         case w of Just n -> return (n /= 0); _other -> return False
       _ ->
         return False


foreign import ccall "&rts_stop_next_breakpoint" stepFlag      :: Ptr CInt
foreign import ccall "&rts_stop_on_exception"    exceptionFlag :: Ptr CInt

setStepFlag :: IO ()
setStepFlag = poke stepFlag 1
resetStepFlag :: IO ()
resetStepFlag = poke stepFlag 0

-- this points to the IO action that is executed when a breakpoint is hit
foreign import ccall "&rts_breakpoint_io_action"
   breakPointIOAction :: Ptr (StablePtr (Bool -> BreakInfo -> HValue -> IO ()))

-- When running a computation, we redirect ^C exceptions to the running
-- thread.  ToDo: we might want a way to continue even if the target
-- thread doesn't die when it receives the exception... "this thread
-- is not responding".
--
-- Careful here: there may be ^C exceptions flying around, so we start the new
-- thread blocked (forkIO inherits mask from the parent, #1048), and unblock
-- only while we execute the user's code.  We can't afford to lose the final
-- putMVar, otherwise deadlock ensues. (#1583, #1922, #1946)
sandboxIO :: DynFlags -> MVar Status -> IO [HValue] -> IO Status
sandboxIO dflags statusMVar thing =
   mask $ \restore -> -- fork starts blocked
     let runIt = liftM Complete $ try (restore $ rethrow dflags thing)
     in if gopt Opt_GhciSandbox dflags
        then do tid <- forkIO $ do res <- runIt
                                   putMVar statusMVar res -- empty: can't block
                redirectInterrupts tid $
                  takeMVar statusMVar

        else -- GLUT on OS X needs to run on the main thread. If you
             -- try to use it from another thread then you just get a
             -- white rectangle rendered. For this, or anything else
             -- with such restrictions, you can turn the GHCi sandbox off
             -- and things will be run in the main thread.
             --
             -- BUT, note that the debugging features (breakpoints,
             -- tracing, etc.) need the expression to be running in a
             -- separate thread, so debugging is only enabled when
             -- using the sandbox.
             runIt

--
-- While we're waiting for the sandbox thread to return a result, if
-- the current thread receives an asynchronous exception we re-throw
-- it at the sandbox thread and continue to wait.
--
-- This is for two reasons:
--
--  * So that ^C interrupts runStmt (e.g. in GHCi), allowing the
--    computation to run its exception handlers before returning the
--    exception result to the caller of runStmt.
--
--  * clients of the GHC API can terminate a runStmt in progress
--    without knowing the ThreadId of the sandbox thread (#1381)
--
-- NB. use a weak pointer to the thread, so that the thread can still
-- be considered deadlocked by the RTS and sent a BlockedIndefinitely
-- exception.  A symptom of getting this wrong is that conc033(ghci)
-- will hang.
--
redirectInterrupts :: ThreadId -> IO a -> IO a
redirectInterrupts target wait
  = do wtid <- mkWeakThreadId target
       wait `catch` \e -> do
          m <- deRefWeak wtid
          case m of
            Nothing -> wait
            Just target -> do throwTo target (e :: SomeException); wait

-- We want to turn ^C into a break when -fbreak-on-exception is on,
-- but it's an async exception and we only break for sync exceptions.
-- Idea: if we catch and re-throw it, then the re-throw will trigger
-- a break.  Great - but we don't want to re-throw all exceptions, because
-- then we'll get a double break for ordinary sync exceptions (you'd have
-- to :continue twice, which looks strange).  So if the exception is
-- not "Interrupted", we unset the exception flag before throwing.
--
rethrow :: DynFlags -> IO a -> IO a
rethrow dflags io = Exception.catch io $ \se -> do
                   -- If -fbreak-on-error, we break unconditionally,
                   --  but with care of not breaking twice
                if gopt Opt_BreakOnError dflags &&
                   not (gopt Opt_BreakOnException dflags)
                    then poke exceptionFlag 1
                    else case fromException se of
                         -- If it is a "UserInterrupt" exception, we allow
                         --  a possible break by way of -fbreak-on-exception
                         Just UserInterrupt -> return ()
                         -- In any other case, we don't want to break
                         _ -> poke exceptionFlag 0

                Exception.throwIO se

-- This function sets up the interpreter for catching breakpoints, and
-- resets everything when the computation has stopped running.  This
-- is a not-very-good way to ensure that only the interactive
-- evaluation should generate breakpoints.
withBreakAction :: (ExceptionMonad m, MonadIO m) =>
                   Bool -> DynFlags -> MVar () -> MVar Status -> m a -> m a
withBreakAction step dflags breakMVar statusMVar act
 = gbracket (liftIO setBreakAction) (liftIO . resetBreakAction) (\_ -> act)
 where
   setBreakAction = do
     stablePtr <- newStablePtr onBreak
     poke breakPointIOAction stablePtr
     when (gopt Opt_BreakOnException dflags) $ poke exceptionFlag 1
     when step $ setStepFlag
     return stablePtr
        -- Breaking on exceptions is not enabled by default, since it
        -- might be a bit surprising.  The exception flag is turned off
        -- as soon as it is hit, or in resetBreakAction below.

   onBreak is_exception info apStack = do
     tid <- myThreadId
     putMVar statusMVar (Break is_exception apStack info tid)
     takeMVar breakMVar

   resetBreakAction stablePtr = do
     poke breakPointIOAction noBreakStablePtr
     poke exceptionFlag 0
     resetStepFlag
     freeStablePtr stablePtr

noBreakStablePtr :: StablePtr (Bool -> BreakInfo -> HValue -> IO ())
noBreakStablePtr = unsafePerformIO $ newStablePtr noBreakAction

noBreakAction :: Bool -> BreakInfo -> HValue -> IO ()
noBreakAction False _ _ = putStrLn "*** Ignoring breakpoint"
noBreakAction True  _ _ = return () -- exception: just continue

resume :: GhcMonad m => (SrcSpan->Bool) -> SingleStep -> m RunResult
resume canLogSpan step
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
        modifySession (\_ -> hsc_env{ hsc_IC = ic' })

        -- remove any bindings created since the breakpoint from the
        -- linker's environment
        let new_names = map getName (filter (`notElem` resume_tmp_te)
                                           (ic_tythings ic))
        liftIO $ Linker.deleteFromLinkEnv new_names

        when (isStep step) $ liftIO setStepFlag
        case r of
          Resume { resumeStmt = expr, resumeThreadId = tid
                 , resumeBreakMVar = breakMVar, resumeStatMVar = statusMVar
                 , resumeBindings = bindings, resumeFinalIds = final_ids
                 , resumeApStack = apStack, resumeBreakInfo = info, resumeSpan = span
                 , resumeHistory = hist } -> do
               withVirtualCWD $ do
                withBreakAction (isStep step) (hsc_dflags hsc_env)
                                        breakMVar statusMVar $ do
                status <- liftIO $ mask_ $ do
                             putMVar breakMVar ()
                                      -- this awakens the stopped thread...
                             redirectInterrupts tid $
                               takeMVar statusMVar
                                      -- and wait for the result
                let prevHistoryLst = fromListBL 50 hist
                    hist' = case info of
                       Nothing -> prevHistoryLst
                       Just i
                         | not $canLogSpan span -> prevHistoryLst
                         | otherwise -> mkHistory hsc_env apStack i `consBL`
                                                        fromListBL 50 hist
                handleRunStatus step expr bindings final_ids
                                breakMVar statusMVar status hist'

back :: GhcMonad m => m ([Name], Int, SrcSpan)
back  = moveHist (+1)

forward :: GhcMonad m => m ([Name], Int, SrcSpan)
forward  = moveHist (subtract 1)

moveHist :: GhcMonad m => (Int -> Int) -> m ([Name], Int, SrcSpan)
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
            (hsc_env1, names, span) <- liftIO $ bindLocalsAtBreakpoint hsc_env
                                                apStack mb_info
            let ic = hsc_IC hsc_env1
                r' = r { resumeHistoryIx = new_ix }
                ic' = ic { ic_resume = r':rs }

            modifySession (\_ -> hsc_env1{ hsc_IC = ic' })

            return (names, new_ix, span)

        -- careful: we want apStack to be the AP_STACK itself, not a thunk
        -- around it, hence the cases are carefully constructed below to
        -- make this the case.  ToDo: this is v. fragile, do something better.
        if new_ix == 0
           then case r of
                   Resume { resumeApStack = apStack,
                            resumeBreakInfo = mb_info } ->
                          update_ic apStack mb_info
           else case history !! (new_ix - 1) of
                   History apStack info _ ->
                          update_ic apStack (Just info)

-- -----------------------------------------------------------------------------
-- After stopping at a breakpoint, add free variables to the environment
result_fs :: FastString
result_fs = fsLit "_result"

bindLocalsAtBreakpoint
        :: HscEnv
        -> HValue
        -> Maybe BreakInfo
        -> IO (HscEnv, [Name], SrcSpan)

-- Nothing case: we stopped when an exception was raised, not at a
-- breakpoint.  We have no location information or local variables to
-- bind, all we can do is bind a local variable to the exception
-- value.
bindLocalsAtBreakpoint hsc_env apStack Nothing = do
   let exn_fs    = fsLit "_exception"
       exn_name  = mkInternalName (getUnique exn_fs) (mkVarOccFS exn_fs) span
       e_fs      = fsLit "e"
       e_name    = mkInternalName (getUnique e_fs) (mkTyVarOccFS e_fs) span
       e_tyvar   = mkRuntimeUnkTyVar e_name liftedTypeKind
       exn_id    = AnId $ Id.mkVanillaGlobal exn_name (mkTyVarTy e_tyvar)

       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContext ictxt0 [exn_id]

       span = mkGeneralSrcSpan (fsLit "<exception thrown>")
   --
   Linker.extendLinkEnv [(exn_name, unsafeCoerce# apStack)]
   return (hsc_env{ hsc_IC = ictxt1 }, [exn_name], span)

-- Just case: we stopped at a breakpoint, we have information about the location
-- of the breakpoint and the free variables of the expression.
bindLocalsAtBreakpoint hsc_env apStack (Just info) = do

   let
       mod_name  = moduleName (breakInfo_module info)
       hmi       = expectJust "bindLocalsAtBreakpoint" $
                        lookupUFM (hsc_HPT hsc_env) mod_name
       breaks    = getModBreaks hmi
       index     = breakInfo_number info
       vars      = breakInfo_vars info
       result_ty = breakInfo_resty info
       occs      = modBreaks_vars breaks ! index
       span      = modBreaks_locs breaks ! index

           -- Filter out any unboxed ids;
           -- we can't bind these at the prompt
       pointers = filter (\(id,_) -> isPointer id) vars
       isPointer id | UnaryRep ty <- repType (idType id)
                    , PtrRep <- typePrimRep ty = True
                    | otherwise                = False

       (ids, offsets) = unzip pointers

       free_tvs = foldr (unionVarSet . tyVarsOfType . idType)
                        (tyVarsOfType result_ty) ids

   -- It might be that getIdValFromApStack fails, because the AP_STACK
   -- has been accidentally evaluated, or something else has gone wrong.
   -- So that we don't fall over in a heap when this happens, just don't
   -- bind any free variables instead, and we emit a warning.
   mb_hValues <- mapM (getIdValFromApStack apStack) (map fromIntegral offsets)
   let filtered_ids = [ id | (id, Just _hv) <- zip ids mb_hValues ]
   when (any isNothing mb_hValues) $
      debugTraceMsg (hsc_dflags hsc_env) 1 $
          text "Warning: _result has been evaluated, some bindings have been lost"

   us <- mkSplitUniqSupply 'I'
   let (us1, us2) = splitUniqSupply us
       tv_subst   = newTyVars us1 free_tvs
       new_ids    = zipWith3 (mkNewId tv_subst) occs filtered_ids (uniqsFromSupply us2)
       names      = map idName new_ids

   -- make an Id for _result.  We use the Unique of the FastString "_result";
   -- we don't care about uniqueness here, because there will only be one
   -- _result in scope at any time.
   let result_name = mkInternalName (getUnique result_fs)
                          (mkVarOccFS result_fs) span
       result_id   = Id.mkVanillaGlobal result_name (substTy tv_subst result_ty)

   -- for each Id we're about to bind in the local envt:
   --    - tidy the type variables
   --    - globalise the Id (Ids are supposed to be Global, apparently).
   --
   let result_ok = isPointer result_id

       all_ids | result_ok = result_id : new_ids
               | otherwise = new_ids
       id_tys = map idType all_ids
       (_,tidy_tys) = tidyOpenTypes emptyTidyEnv id_tys
       final_ids = zipWith setIdType all_ids tidy_tys
       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContext ictxt0 (map AnId final_ids)

   Linker.extendLinkEnv [ (name,hval) | (name, Just hval) <- zip names mb_hValues ]
   when result_ok $ Linker.extendLinkEnv [(result_name, unsafeCoerce# apStack)]
   hsc_env1 <- rttiEnvironment hsc_env{ hsc_IC = ictxt1 }
   return (hsc_env1, if result_ok then result_name:names else names, span)
  where
        -- We need a fresh Unique for each Id we bind, because the linker
        -- state is single-threaded and otherwise we'd spam old bindings
        -- whenever we stop at a breakpoint.  The InteractveContext is properly
        -- saved/restored, but not the linker state.  See #1743, test break026.
   mkNewId :: TvSubst -> OccName -> Id -> Unique -> Id
   mkNewId tv_subst occ id uniq
     = Id.mkVanillaGlobalWithInfo name ty (idInfo id)
     where
         loc    = nameSrcSpan (idName id)
         name   = mkInternalName uniq occ loc
         ty     = substTy tv_subst (idType id)

   newTyVars :: UniqSupply -> TcTyVarSet -> TvSubst
     -- Similarly, clone the type variables mentioned in the types
     -- we have here, *and* make them all RuntimeUnk tyars
   newTyVars us tvs
     = mkTopTvSubst [ (tv, mkTyVarTy (mkRuntimeUnkTyVar name (tyVarKind tv)))
                    | (tv, uniq) <- varSetElems tvs `zip` uniqsFromSupply us
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
     noSkolems = isEmptyVarSet . tyVarsOfType . idType
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

                 let ic' = extendInteractiveContext
                               (substInteractiveContext ic subst) []
                 return hsc_env{hsc_IC=ic'}

getIdValFromApStack :: HValue -> Int -> IO (Maybe HValue)
getIdValFromApStack apStack (I# stackDepth) = do
   case getApStackVal# apStack (stackDepth +# 1#) of
                                -- The +1 is magic!  I don't know where it comes
                                -- from, but this makes things line up.  --SDM
        (# ok, result #) ->
            case ok of
              0# -> return Nothing -- AP_STACK not found
              _  -> return (Just (unsafeCoerce# result))

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
         modifySession $ \_ -> hsc_env{ hsc_IC = ic { ic_resume = rs } }
         liftIO $ abandon_ r
         return True

abandonAll :: GhcMonad m => m Bool
abandonAll = do
   hsc_env <- getSession
   let ic = hsc_IC hsc_env
       resume = ic_resume ic
   case resume of
      []  -> return False
      rs  -> do
         modifySession $ \_ -> hsc_env{ hsc_IC = ic { ic_resume = [] } }
         liftIO $ mapM_ abandon_ rs
         return True

-- when abandoning a computation we have to
--      (a) kill the thread with an async exception, so that the
--          computation itself is stopped, and
--      (b) fill in the MVar.  This step is necessary because any
--          thunks that were under evaluation will now be updated
--          with the partial computation, which still ends in takeMVar,
--          so any attempt to evaluate one of these thunks will block
--          unless we fill in the MVar.
--      (c) wait for the thread to terminate by taking its status MVar.  This
--          step is necessary to prevent race conditions with
--          -fbreak-on-exception (see #5975).
--  See test break010.
abandon_ :: Resume -> IO ()
abandon_ r = do
  killThread (resumeThreadId r)
  putMVar (resumeBreakMVar r) ()
  _ <- takeMVar (resumeStatMVar r)
  return ()

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
       ; let old_ic        = hsc_IC hsc_env
             final_rdr_env = all_env `icExtendGblRdrEnv` ic_tythings old_ic
       ; modifySession $ \_ ->
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
  = mkGlobalRdrEnv (gresFromAvails imp_prov avails)
  where
      -- We're building a GlobalRdrEnv as if the user imported
      -- all the specified modules into the global interactive module
    imp_prov = Imported [ImpSpec { is_decl = decl, is_item = ImpAll}]
    decl = ImpDeclSpec { is_mod = mod_name, is_as = mod_name,
                         is_qual = False,
                         is_dloc = srcLocSpan interactiveSrcLoc }

mkTopLevEnv :: HomePackageTable -> ModuleName -> Either String GlobalRdrEnv
mkTopLevEnv hpt modl
  = case lookupUFM hpt modl of
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
 if modulePackageId modl /= thisPackage (hsc_dflags h)
        then return False
        else case lookupUFM (hsc_HPT h) (moduleName modl) of
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
       || all ok (nameSetToList names)
        where   -- A name is ok if it's in the rdr_env,
                -- whether qualified or not
          ok n | n == name         = True       -- The one we looked for in the first place!
               | isBuiltInSyntax n = True
               | isExternalName n  = any ((== n) . gre_name)
                                         (lookupGRE_Name rdr_env n)
               | otherwise         = True

-- | Returns all names in scope in the current interactive context
getNamesInScope :: GhcMonad m => m [Name]
getNamesInScope = withSession $ \hsc_env -> do
  return (map gre_name (globalRdrEnvElts (ic_rn_gbl_env (hsc_IC hsc_env))))

getRdrNamesInScope :: GhcMonad m => m [RdrName]
getRdrNamesInScope = withSession $ \hsc_env -> do
  let
      ic = hsc_IC hsc_env
      gbl_rdrenv = ic_rn_gbl_env ic
      gbl_names = concatMap greToRdrNames $ globalRdrEnvElts gbl_rdrenv
  return gbl_names


-- ToDo: move to RdrName
greToRdrNames :: GlobalRdrElt -> [RdrName]
greToRdrNames GRE{ gre_name = name, gre_prov = prov }
  = case prov of
     LocalDef -> [unqual]
     Imported specs -> concat (map do_spec (map is_decl specs))
  where
    occ = nameOccName name
    unqual = Unqual occ
    do_spec decl_spec
        | is_qual decl_spec = [qual]
        | otherwise         = [unqual,qual]
        where qual = Qual (is_as decl_spec) occ

-- | Parses a string as an identifier, and returns the list of 'Name's that
-- the identifier can refer to in the current interactive context.
parseName :: GhcMonad m => String -> m [Name]
parseName str = withSession $ \hsc_env -> do
   (L _ rdr_name) <- liftIO $ hscParseIdentifier hsc_env str
   liftIO $ hscTcRnLookupRdrName hsc_env rdr_name

-- -----------------------------------------------------------------------------
-- Getting the type of an expression

-- | Get the type of an expression
-- Returns its most general type
exprType :: GhcMonad m => String -> m Type
exprType expr = withSession $ \hsc_env -> do
   ty <- liftIO $ hscTcExpr hsc_env expr
   return $ tidyType emptyTidyEnv ty

-- -----------------------------------------------------------------------------
-- Getting the kind of a type

-- | Get the kind of a  type
typeKind  :: GhcMonad m => Bool -> String -> m (Type, Kind)
typeKind normalise str = withSession $ \hsc_env -> do
   liftIO $ hscKcType hsc_env normalise str

-----------------------------------------------------------------------------
-- Compile an expression, run it and deliver the resulting HValue

compileExpr :: GhcMonad m => String -> m HValue
compileExpr expr = withSession $ \hsc_env -> do
  Just (ids, hval, fix_env) <- liftIO $ hscStmt hsc_env ("let __cmCompileExpr = "++expr)
  updateFixityEnv fix_env
  hvals <- liftIO hval
  case (ids,hvals) of
    ([_],[hv]) -> return hv
    _          -> panic "compileExpr"

-- -----------------------------------------------------------------------------
-- Compile an expression, run it and return the result as a dynamic

dynCompileExpr :: GhcMonad m => String -> m Dynamic
dynCompileExpr expr = do
    iis <- getContext
    let importDecl = ImportDecl {
                         ideclName = noLoc (mkModuleName "Data.Dynamic"),
                         ideclPkgQual = Nothing,
                         ideclSource = False,
                         ideclSafe = False,
                         ideclQualified = True,
                         ideclImplicit = False,
                         ideclAs = Nothing,
                         ideclHiding = Nothing
                     }
    setContext (IIDecl importDecl : iis)
    let stmt = "let __dynCompileExpr = Data.Dynamic.toDyn (" ++ expr ++ ")"
    Just (ids, hvals, fix_env) <- withSession $ \hsc_env ->
                           liftIO $ hscStmt hsc_env stmt
    setContext iis
    updateFixityEnv fix_env

    vals <- liftIO (unsafeCoerce# hvals :: IO [Dynamic])
    case (ids,vals) of
        (_:[], v:[]) -> return v
        _            -> panic "dynCompileExpr"

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
  case lookupUFM (hsc_HPT hsc_env) (ms_mod_name mod_summary) of
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
              hv <- Linker.getHValue hsc_env (varName id)
              cvObtainTerm hsc_env bound force (idType id) hv

-- Uses RTTI to reconstruct the type of an Id, making it less polymorphic
reconstructType :: HscEnv -> Int -> Id -> IO (Maybe Type)
reconstructType hsc_env bound id = do
              hv <- Linker.getHValue hsc_env (varName id)
              cvReconstructType hsc_env bound (idType id) hv

mkRuntimeUnkTyVar :: Name -> Kind -> TyVar
mkRuntimeUnkTyVar name kind = mkTcTyVar name kind RuntimeUnk
#endif /* GHCI */

