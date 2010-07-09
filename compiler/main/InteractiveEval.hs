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
	runStmt, parseImportDecl, SingleStep(..),
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
	lookupName,
        Term(..), obtainTermFromId, obtainTermFromVal, reconstructType,
        skolemiseSubst, skolemiseTy
#endif
        ) where

#ifdef GHCI

#include "HsVersions.h"

import HscMain          hiding (compileExpr)
import HsSyn (ImportDecl)
import HscTypes
import TcRnDriver
import TcRnMonad (initTc)
import RnNames		(gresFromAvails, rnImports)
import InstEnv
import Type
import TcType		hiding( typeKind )
import Var
import Id
import Name             hiding ( varName )
import NameSet
import RdrName
import PrelNames (pRELUDE)
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
import Util
import SrcLoc
import BreakArray
import RtClosureInspect
import BasicTypes
import Outputable
import FastString
import MonadUtils

import System.Directory
import Data.Dynamic
import Data.List (find, partition)
import Control.Monad
import Foreign
import Foreign.C
import GHC.Exts
import Data.Array
import Exception
import Control.Concurrent
import Data.List (sortBy)
-- import Foreign.StablePtr
import System.IO

-- -----------------------------------------------------------------------------
-- running a statement interactively

data RunResult
  = RunOk [Name] 		-- ^ names bound by this evaluation
  | RunFailed	 		-- ^ statement failed compilation
  | RunException SomeException	-- ^ statement raised an exception
  | RunBreak ThreadId [Name] (Maybe BreakInfo)

data Status
   = Break Bool HValue BreakInfo ThreadId
          -- ^ the computation hit a breakpoint (Bool <=> was an exception)
   | Complete (Either SomeException [HValue])
          -- ^ the computation completed with either an exception or a value

data Resume
   = Resume {
       resumeStmt      :: String,       -- the original statement
       resumeThreadId  :: ThreadId,     -- thread running the computation
       resumeBreakMVar :: MVar (),   
       resumeStatMVar  :: MVar Status,
       resumeBindings  :: ([Id], TyVarSet),
       resumeFinalIds  :: [Id],         -- [Id] to bind on completion
       resumeApStack   :: HValue,       -- The object from which we can get
                                        -- value of the free variables.
       resumeBreakInfo :: Maybe BreakInfo,    
                                        -- the breakpoint we stopped at
                                        -- (Nothing <=> exception)
       resumeSpan      :: SrcSpan,      -- just a cache, otherwise it's a pain
                                        -- to fetch the ModDetails & ModBreaks
                                        -- to get this.
       resumeHistory   :: [History],
       resumeHistoryIx :: Int           -- 0 <==> at the top of the history
   }

getResumeContext :: GhcMonad m => m [Resume]
getResumeContext = withSession (return . ic_resume . hsc_IC)

data SingleStep
   = RunToCompletion
   | SingleStep
   | RunAndLogSteps

isStep :: SingleStep -> Bool
isStep RunToCompletion = False
isStep _ = True

data History
   = History {
        historyApStack   :: HValue,
        historyBreakInfo :: BreakInfo,
        historyEnclosingDecl :: Id
         -- ^^ A cache of the enclosing top level declaration, for convenience
   }

mkHistory :: HscEnv -> HValue -> BreakInfo -> History
mkHistory hsc_env hval bi = let
    h    = History hval bi decl
    decl = findEnclosingDecl hsc_env (getHistoryModule h)
                                     (getHistorySpan hsc_env h)
    in h

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
findEnclosingDecl :: HscEnv -> Module -> SrcSpan -> Id
findEnclosingDecl hsc_env mod span =
   case lookupUFM (hsc_HPT hsc_env) (moduleName mod) of
         Nothing -> panic "findEnclosingDecl"
         Just hmi -> let
             globals   = typeEnvIds (md_types (hm_details hmi))
             Just decl = 
                 find (\id -> let n = idName id in 
                               nameSrcSpan n < span && isExternalName n)
                      (reverse$ sortBy (compare `on` (nameSrcSpan.idName))
                                       globals)
           in decl

-- | Run a statement in the current interactive context.  Statement
-- may bind multple values.
runStmt :: GhcMonad m => String -> SingleStep -> m RunResult
runStmt expr step =
  do
    hsc_env <- getSession

    breakMVar  <- liftIO $ newEmptyMVar  -- wait on this when we hit a breakpoint
    statusMVar <- liftIO $ newEmptyMVar  -- wait on this when a computation is running

    -- Turn off -fwarn-unused-bindings when running a statement, to hide
    -- warnings about the implicit bindings we introduce.
    let dflags'  = dopt_unset (hsc_dflags hsc_env) Opt_WarnUnusedBinds
        hsc_env' = hsc_env{ hsc_dflags = dflags' }

    r <- hscStmt hsc_env' expr

    case r of
      Nothing -> return RunFailed -- empty statement / comment

      Just (ids, hval) -> do
          -- XXX: This is the only place we can print warnings before the
          -- result.  Is this really the right thing to do?  It's fine for
          -- GHCi, but what's correct for other GHC API clients?  We could
          -- introduce a callback argument.
        warns <- getWarnings
        liftIO $ printBagOfWarnings dflags' warns
        clearWarnings

        status <-
          withVirtualCWD $
            withBreakAction (isStep step) dflags' breakMVar statusMVar $ do
                let thing_to_run = unsafeCoerce# hval :: IO [HValue]
                liftIO $ sandboxIO dflags' statusMVar thing_to_run
              
        let ic = hsc_IC hsc_env
            bindings = (ic_tmp_ids ic, ic_tyvars ic)

        case step of
          RunAndLogSteps ->
              traceRunStatus expr bindings ids
                             breakMVar statusMVar status emptyHistory
          _other ->
              handleRunStatus expr bindings ids
                               breakMVar statusMVar status emptyHistory

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
parseImportDecl expr = withSession $ \hsc_env -> hscImport hsc_env expr

emptyHistory :: BoundedList History
emptyHistory = nilBL 50 -- keep a log of length 50

handleRunStatus :: GhcMonad m =>
                   String-> ([Id], TyVarSet) -> [Id]
                -> MVar () -> MVar Status -> Status -> BoundedList History
                -> m RunResult
handleRunStatus expr bindings final_ids breakMVar statusMVar status
                history =
   case status of  
      -- did we hit a breakpoint or did we complete?
      (Break is_exception apStack info tid) -> do
        hsc_env <- getSession
        let mb_info | is_exception = Nothing
                    | otherwise    = Just info
        (hsc_env1, names, span) <- liftIO $ bindLocalsAtBreakpoint hsc_env apStack
                                                               mb_info
        let
            resume = Resume expr tid breakMVar statusMVar 
                              bindings final_ids apStack mb_info span 
                              (toListBL history) 0
            hsc_env2 = pushResume hsc_env1 resume
        --
        modifySession (\_ -> hsc_env2)
        return (RunBreak tid names mb_info)
      (Complete either_hvals) ->
	case either_hvals of
	    Left e -> return (RunException e)
	    Right hvals -> do
                hsc_env <- getSession
                let final_ic = extendInteractiveContext (hsc_IC hsc_env)
                                        final_ids emptyVarSet
                        -- the bound Ids never have any free TyVars
                    final_names = map idName final_ids
                liftIO $ Linker.extendLinkEnv (zip final_names hvals)
                hsc_env' <- liftIO $ rttiEnvironment hsc_env{hsc_IC=final_ic}
                modifySession (\_ -> hsc_env')
                return (RunOk final_names)

traceRunStatus :: GhcMonad m =>
                  String -> ([Id], TyVarSet) -> [Id]
               -> MVar () -> MVar Status -> Status -> BoundedList History
               -> m RunResult
traceRunStatus expr bindings final_ids
               breakMVar statusMVar status history = do
  hsc_env <- getSession
  case status of
     -- when tracing, if we hit a breakpoint that is not explicitly
     -- enabled, then we just log the event in the history and continue.
     (Break is_exception apStack info tid) | not is_exception -> do
        b <- liftIO $ isBreakEnabled hsc_env info
        if b
           then handle_normally
           else do
             let history' = mkHistory hsc_env apStack info `consBL` history
                -- probably better make history strict here, otherwise
                -- our BoundedList will be pointless.
             _ <- liftIO $ evaluate history'
             status <-
                 withBreakAction True (hsc_dflags hsc_env)
                                      breakMVar statusMVar $ do
                   liftIO $ withInterruptsSentTo tid $ do
                       putMVar breakMVar ()  -- awaken the stopped thread
                       takeMVar statusMVar   -- and wait for the result
             traceRunStatus expr bindings final_ids
                            breakMVar statusMVar status history'
     _other ->
        handle_normally
  where
        handle_normally = handleRunStatus expr bindings final_ids
                                          breakMVar statusMVar status history


isBreakEnabled :: HscEnv -> BreakInfo -> IO Bool
isBreakEnabled hsc_env inf =
   case lookupUFM (hsc_HPT hsc_env) (moduleName (breakInfo_module inf)) of
       Just hmi -> do
         w <- getBreak (modBreaks_flags (getModBreaks hmi))
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
   mask $ \restore -> do  -- fork starts blocked
     id <- forkIO $ do res <- Exception.try (restore $ rethrow dflags thing)
                       putMVar statusMVar (Complete res) -- empty: can't block
     withInterruptsSentTo id $ takeMVar statusMVar


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
                if dopt Opt_BreakOnError dflags &&
                   not (dopt Opt_BreakOnException dflags)
                    then poke exceptionFlag 1
                    else case fromException se of
                         -- If it is a "UserInterrupt" exception, we allow
                         --  a possible break by way of -fbreak-on-exception
                         Just UserInterrupt -> return ()
                         -- In any other case, we don't want to break
                         _ -> poke exceptionFlag 0

                Exception.throwIO se

withInterruptsSentTo :: ThreadId -> IO r -> IO r
withInterruptsSentTo thread get_result = do
  bracket (modifyMVar_ interruptTargetThread (return . (thread:)))
          (\_ -> modifyMVar_ interruptTargetThread (\tl -> return $! tail tl))
          (\_ -> get_result)

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
     when (dopt Opt_BreakOnException dflags) $ poke exceptionFlag 1
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
     [] -> ghcError (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        -- unbind the temporary locals by restoring the TypeEnv from
        -- before the breakpoint, and drop this Resume from the
        -- InteractiveContext.
        let (resume_tmp_ids, resume_tyvars) = resumeBindings r
            ic' = ic { ic_tmp_ids  = resume_tmp_ids,
                       ic_tyvars   = resume_tyvars,
                       ic_resume   = rs }
        modifySession (\_ -> hsc_env{ hsc_IC = ic' })
        
        -- remove any bindings created since the breakpoint from the 
        -- linker's environment
        let new_names = map idName (filter (`notElem` resume_tmp_ids)
                                           (ic_tmp_ids ic))
        liftIO $ Linker.deleteFromLinkEnv new_names
        
        when (isStep step) $ liftIO setStepFlag
        case r of 
          Resume expr tid breakMVar statusMVar bindings 
              final_ids apStack info span hist _ -> do
               withVirtualCWD $ do
                withBreakAction (isStep step) (hsc_dflags hsc_env) 
                                        breakMVar statusMVar $ do
                status <- liftIO $ withInterruptsSentTo tid $ do
                             putMVar breakMVar ()
                                      -- this awakens the stopped thread...
                             takeMVar statusMVar
                                      -- and wait for the result 
                let prevHistoryLst = fromListBL 50 hist
                    hist' = case info of
                       Nothing -> prevHistoryLst
                       Just i
                         | not $canLogSpan span -> prevHistoryLst
                         | otherwise -> mkHistory hsc_env apStack i `consBL`
                                                        fromListBL 50 hist
                case step of
                  RunAndLogSteps -> 
                        traceRunStatus expr bindings final_ids
                                       breakMVar statusMVar status hist'
                  _other ->
                        handleRunStatus expr bindings final_ids
                                        breakMVar statusMVar status hist'

back :: GhcMonad m => m ([Name], Int, SrcSpan)
back  = moveHist (+1)

forward :: GhcMonad m => m ([Name], Int, SrcSpan)
forward  = moveHist (subtract 1)

moveHist :: GhcMonad m => (Int -> Int) -> m ([Name], Int, SrcSpan)
moveHist fn = do
  hsc_env <- getSession
  case ic_resume (hsc_IC hsc_env) of
     [] -> ghcError (ProgramError "not stopped at a breakpoint")
     (r:rs) -> do
        let ix = resumeHistoryIx r
            history = resumeHistory r
            new_ix = fn ix
        --
        when (new_ix > length history) $
           ghcError (ProgramError "no more logged breakpoints")
        when (new_ix < 0) $
           ghcError (ProgramError "already at the beginning of the history")

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
       e_tyvar   = mkTcTyVar e_name liftedTypeKind (SkolemTv RuntimeUnkSkol)
       exn_id    = Id.mkVanillaGlobal exn_name (mkTyVarTy e_tyvar)
       new_tyvars = unitVarSet e_tyvar

       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContext ictxt0 [exn_id] new_tyvars

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

   -- filter out any unboxed ids; we can't bind these at the prompt
   let pointers = filter (\(id,_) -> isPointer id) vars
       isPointer id | PtrRep <- idPrimRep id = True
                    | otherwise              = False

   let (ids, offsets) = unzip pointers

   -- It might be that getIdValFromApStack fails, because the AP_STACK
   -- has been accidentally evaluated, or something else has gone wrong.
   -- So that we don't fall over in a heap when this happens, just don't
   -- bind any free variables instead, and we emit a warning.
   mb_hValues <- mapM (getIdValFromApStack apStack) (map fromIntegral offsets)
   let filtered_ids = [ id | (id, Just _hv) <- zip ids mb_hValues ]
   when (any isNothing mb_hValues) $
      debugTraceMsg (hsc_dflags hsc_env) 1 $
	  text "Warning: _result has been evaluated, some bindings have been lost"

   new_ids <- zipWithM mkNewId occs filtered_ids
   let names = map idName new_ids

   -- make an Id for _result.  We use the Unique of the FastString "_result";
   -- we don't care about uniqueness here, because there will only be one
   -- _result in scope at any time.
   let result_name = mkInternalName (getUnique result_fs)
                          (mkVarOccFS result_fs) span
       result_id   = Id.mkVanillaGlobal result_name result_ty 

   -- for each Id we're about to bind in the local envt:
   --    - skolemise the type variables in its type, so they can't
   --      be randomly unified with other types.  These type variables
   --      can only be resolved by type reconstruction in RtClosureInspect
   --    - tidy the type variables
   --    - globalise the Id (Ids are supposed to be Global, apparently).
   --
   let result_ok = isPointer result_id
                    && not (isUnboxedTupleType (idType result_id))

       all_ids | result_ok = result_id : new_ids
               | otherwise = new_ids
       (id_tys, tyvarss) = mapAndUnzip (skolemiseTy.idType) all_ids
       (_,tidy_tys) = tidyOpenTypes emptyTidyEnv id_tys
       new_tyvars = unionVarSets tyvarss             
       final_ids = zipWith setIdType all_ids tidy_tys
       ictxt0 = hsc_IC hsc_env
       ictxt1 = extendInteractiveContext ictxt0 final_ids new_tyvars

   Linker.extendLinkEnv [ (name,hval) | (name, Just hval) <- zip names mb_hValues ]
   when result_ok $ Linker.extendLinkEnv [(result_name, unsafeCoerce# apStack)]
   hsc_env1 <- rttiEnvironment hsc_env{ hsc_IC = ictxt1 }
   return (hsc_env1, if result_ok then result_name:names else names, span)
  where
   mkNewId :: OccName -> Id -> IO Id
   mkNewId occ id = do
     us <- mkSplitUniqSupply 'I'
        -- we need a fresh Unique for each Id we bind, because the linker
        -- state is single-threaded and otherwise we'd spam old bindings
        -- whenever we stop at a breakpoint.  The InteractveContext is properly
        -- saved/restored, but not the linker state.  See #1743, test break026.
     let 
         uniq = uniqFromSupply us
         loc = nameSrcSpan (idName id)
         name = mkInternalName uniq occ loc
         ty = idType id
         new_id = Id.mkVanillaGlobalWithInfo name ty (idInfo id)
     return new_id

rttiEnvironment :: HscEnv -> IO HscEnv 
rttiEnvironment hsc_env@HscEnv{hsc_IC=ic} = do
   let InteractiveContext{ic_tmp_ids=tmp_ids} = ic
       incompletelyTypedIds = 
           [id | id <- tmp_ids
               , not $ noSkolems id
               , (occNameFS.nameOccName.idName) id /= result_fs]
   hsc_env' <- foldM improveTypes hsc_env (map idName incompletelyTypedIds)
   return hsc_env'
    where
     noSkolems = null . filter isSkolemTyVar . varSetElems . tyVarsOfType . idType
     improveTypes hsc_env@HscEnv{hsc_IC=ic} name = do
      let InteractiveContext{ic_tmp_ids=tmp_ids} = ic
          Just id = find (\i -> idName i == name) tmp_ids
      if noSkolems id
         then return hsc_env
         else do
           mb_new_ty <- reconstructType hsc_env 10 id
           let old_ty = idType id
           case mb_new_ty of
             Nothing -> return hsc_env
             Just new_ty -> do
              mb_subst <- improveRTTIType hsc_env old_ty new_ty
              case mb_subst of
               Nothing -> return $
                        WARN(True, text (":print failed to calculate the "
                                           ++ "improvement for a type")) hsc_env
               Just subst -> do
                 when (dopt Opt_D_dump_rtti (hsc_dflags hsc_env)) $
                      printForUser stderr alwaysQualify $
                      fsep [text "RTTI Improvement for", ppr id, equals, ppr subst]

                 let (subst', skols) = skolemiseSubst subst
                     ic' = extendInteractiveContext
                               (substInteractiveContext ic subst') [] skols
                 return hsc_env{hsc_IC=ic'}

skolemiseSubst :: TvSubst -> (TvSubst, TyVarSet)
skolemiseSubst subst = let
    varenv               = getTvSubstEnv subst
    all_together         = mapVarEnv skolemiseTy varenv
    (varenv', skol_vars) = ( mapVarEnv fst all_together
                           , map snd (varEnvElts all_together))
    in (subst `setTvSubstEnv` varenv', unionVarSets skol_vars)
                        

skolemiseTy :: Type -> (Type, TyVarSet)
skolemiseTy ty = (substTy subst ty, mkVarSet new_tyvars)
  where env           = mkVarEnv (zip tyvars new_tyvar_tys)
        subst         = mkTvSubst emptyInScopeSet env
        tyvars        = varSetElems (tyVarsOfType ty)
        new_tyvars    = map skolemiseTyVar tyvars
        new_tyvar_tys = map mkTyVarTy new_tyvars

skolemiseTyVar :: TyVar -> TyVar
skolemiseTyVar tyvar = mkTcTyVar (tyVarName tyvar) (tyVarKind tyvar) 
                                 (SkolemTv RuntimeUnkSkol)

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
--  See test break010.
abandon_ :: Resume -> IO ()
abandon_ r = do
  killThread (resumeThreadId r)
  putMVar (resumeBreakMVar r) () 

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
-- Setting the context doesn't throw away any bindings; the bindings
-- we've built up in the InteractiveContext simply move to the new
-- module.  They always shadow anything in scope in the current context.
setContext :: GhcMonad m =>
        [Module]	-- ^ entire top level scope of these modules
        -> [(Module, Maybe (ImportDecl RdrName))]	-- ^ exports of these modules
        -> m ()
setContext toplev_mods other_mods = do
    hsc_env <- getSession
    let old_ic  = hsc_IC     hsc_env
        hpt     = hsc_HPT    hsc_env
        (decls,mods)   = partition (isJust . snd) other_mods -- time for tracing
        export_mods = map fst mods
        imprt_decls = map noLoc (catMaybes (map snd decls))
    --
    export_env  <- liftIO $ mkExportEnv hsc_env export_mods
    import_env  <-
        if null imprt_decls then return emptyGlobalRdrEnv else do
            let imports = rnImports imprt_decls
                this_mod = if null toplev_mods then pRELUDE else head toplev_mods
            (_, env, _,_) <-
                ioMsgMaybe $ liftIO $ initTc hsc_env HsSrcFile False this_mod imports
            return env
    toplev_envs <- liftIO $ mapM (mkTopLevEnv hpt) toplev_mods
    let all_env = foldr plusGlobalRdrEnv (plusGlobalRdrEnv export_env import_env) toplev_envs
    modifySession $ \_ ->
        hsc_env{ hsc_IC = old_ic { ic_toplev_scope = toplev_mods,
      			 ic_exports      = other_mods,
      			 ic_rn_gbl_env   = all_env }}

-- Make a GlobalRdrEnv based on the exports of the modules only.
mkExportEnv :: HscEnv -> [Module] -> IO GlobalRdrEnv
mkExportEnv hsc_env mods
  = do { stuff <- mapM (getModuleExports hsc_env) mods
       ; let (_msgs, mb_name_sets) = unzip stuff
	     envs = [ availsToGlobalRdrEnv (moduleName mod) avails
                    | (Just avails, mod) <- zip mb_name_sets mods ]
       ; return $! foldr plusGlobalRdrEnv emptyGlobalRdrEnv envs }

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

mkTopLevEnv :: HomePackageTable -> Module -> IO GlobalRdrEnv
mkTopLevEnv hpt modl
  = case lookupUFM hpt (moduleName modl) of
      Nothing -> ghcError (ProgramError ("mkTopLevEnv: not a home module " ++ 
                                                showSDoc (ppr modl)))
      Just details ->
	 case mi_globals (hm_iface details) of
		Nothing  -> 
		   ghcError (ProgramError ("mkTopLevEnv: not interpreted " 
						++ showSDoc (ppr modl)))
		Just env -> return env

-- | Get the interactive evaluation context, consisting of a pair of the
-- set of modules from which we take the full top-level scope, and the set
-- of modules from which we take just the exports respectively.
getContext :: GhcMonad m => m ([Module],[(Module, Maybe (ImportDecl RdrName))])
getContext = withSession $ \HscEnv{ hsc_IC=ic } ->
	       return (ic_toplev_scope ic, ic_exports ic)

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
-- 	(see Trac #1581)
getInfo :: GhcMonad m => Name -> m (Maybe (TyThing,Fixity,[Instance]))
getInfo name
  = withSession $ \hsc_env ->
    do mb_stuff <- ioMsg $ tcRnGetInfo hsc_env name
       case mb_stuff of
         Nothing -> return Nothing
         Just (thing, fixity, ispecs) -> do
           let rdr_env = ic_rn_gbl_env (hsc_IC hsc_env)
           return (Just (thing, fixity, filter (plausible rdr_env) ispecs))
  where
    plausible rdr_env ispec	-- Dfun involving only names that are in ic_rn_glb_env
	= all ok $ nameSetToList $ tyClsNamesOfType $ idType $ instanceDFunId ispec
	where	-- A name is ok if it's in the rdr_env, 
		-- whether qualified or not
	  ok n | n == name	   = True	-- The one we looked for in the first place!
	       | isBuiltInSyntax n = True
	       | isExternalName n  = any ((== n) . gre_name)
					 (lookupGRE_Name rdr_env n)
	       | otherwise	   = True

-- | Returns all names in scope in the current interactive context
getNamesInScope :: GhcMonad m => m [Name]
getNamesInScope = withSession $ \hsc_env -> do
  return (map gre_name (globalRdrEnvElts (ic_rn_gbl_env (hsc_IC hsc_env))))

getRdrNamesInScope :: GhcMonad m => m [RdrName]
getRdrNamesInScope = withSession $ \hsc_env -> do
  let 
      ic = hsc_IC hsc_env
      gbl_rdrenv = ic_rn_gbl_env ic
      ids = ic_tmp_ids ic
      gbl_names = concat (map greToRdrNames (globalRdrEnvElts gbl_rdrenv))
      lcl_names = map (mkRdrUnqual.nameOccName.idName) ids
  --
  return (gbl_names ++ lcl_names)


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
   (L _ rdr_name) <- hscParseIdentifier (hsc_dflags hsc_env) str
   ioMsgMaybe $ tcRnLookupRdrName hsc_env rdr_name

-- | Returns the 'TyThing' for a 'Name'.  The 'Name' may refer to any
-- entity known to GHC, including 'Name's defined using 'runStmt'.
lookupName :: GhcMonad m => Name -> m (Maybe TyThing)
lookupName name = withSession $ \hsc_env -> do
  mb_tything <- ioMsg $ tcRnLookupName hsc_env name
  return mb_tything
  -- XXX: calls panic in some circumstances;  is that ok?

-- -----------------------------------------------------------------------------
-- Getting the type of an expression

-- | Get the type of an expression
exprType :: GhcMonad m => String -> m Type
exprType expr = withSession $ \hsc_env -> do
   ty <- hscTcExpr hsc_env expr
   return $ tidyType emptyTidyEnv ty

-- -----------------------------------------------------------------------------
-- Getting the kind of a type

-- | Get the kind of a  type
typeKind  :: GhcMonad m => String -> m Kind
typeKind str = withSession $ \hsc_env -> do
   hscKcType hsc_env str

-----------------------------------------------------------------------------
-- cmCompileExpr: compile an expression and deliver an HValue

compileExpr :: GhcMonad m => String -> m HValue
compileExpr expr = withSession $ \hsc_env -> do
  Just (ids, hval) <- hscStmt hsc_env ("let __cmCompileExpr = "++expr)
 		 -- Run it!
  hvals <- liftIO (unsafeCoerce# hval :: IO [HValue])

  case (ids,hvals) of
    ([_],[hv]) -> return hv
    _ 	     -> panic "compileExpr"

-- -----------------------------------------------------------------------------
-- Compile an expression into a dynamic

dynCompileExpr :: GhcMonad m => String -> m Dynamic
dynCompileExpr expr = do
    (full,exports) <- getContext
    setContext full $
        (mkModule
            (stringToPackageId "base") (mkModuleName "Data.Dynamic")
        ,Nothing):exports
    let stmt = "let __dynCompileExpr = Data.Dynamic.toDyn (" ++ expr ++ ")"
    Just (ids, hvals) <- withSession (flip hscStmt stmt)
    setContext full exports
    vals <- liftIO (unsafeCoerce# hvals :: IO [Dynamic])
    case (ids,vals) of
        (_:[], v:[])    -> return v
        _               -> panic "dynCompileExpr"

-----------------------------------------------------------------------------
-- show a module and it's source/object filenames

showModule :: GhcMonad m => ModSummary -> m String
showModule mod_summary =
    withSession $ \hsc_env -> do
        interpreted <- isModuleInterpreted mod_summary
        return (showModMsg (hscTarget(hsc_dflags hsc_env)) interpreted mod_summary)

isModuleInterpreted :: GhcMonad m => ModSummary -> m Bool
isModuleInterpreted mod_summary = withSession $ \hsc_env ->
  case lookupUFM (hsc_HPT hsc_env) (ms_mod_name mod_summary) of
	Nothing	      -> panic "missing linkable"
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

#endif /* GHCI */

