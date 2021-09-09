{-# LANGUAGE CPP,
             GADTs,
             OverloadedStrings,
             LambdaCase,
             TupleSections,
             ScopedTypeVariables,
             ViewPatterns,
             PackageImports
  #-}

module Gen2.TH where

{-
  Template Haskell support through Node.js
-}

import           Compiler.Settings

import qualified Gen2.Generator      as Gen2
import qualified Gen2.Linker         as Gen2
import qualified Gen2.ClosureInfo    as Gen2
import qualified Gen2.Shim           as Gen2
import qualified Gen2.Object         as Gen2
import qualified Gen2.Cache          as Gen2
import qualified Gen2.Rts            as Gen2
import Prelude

import           GHC.Hs.Extension
import           BasicTypes
import           CoreToStg
import           CoreUtils
import           CorePrep
import           Name
import           Id
import           Outputable          hiding ((<>))
import           CoreSyn
import           SrcLoc
import           Module
import           DynFlags
import           TcRnMonad
import           HscTypes
import           Packages
import           Maybes
import           SimplStg
import           GHC.Serialized
import           GHC.ThToHs
import           Bag
import           StaticPtrTable
import           Finder
import           Avail
import           ConLike
import           StgSyn
import           CostCentre
import           NameEnv

import           Control.Concurrent
import qualified Control.Exception              as E
import           Control.Monad

import qualified Data.IntMap                    as IM
import qualified Data.Map                       as M

import           Data.Text                      (Text)
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import qualified Data.ByteString.Base16         as B16
import qualified Data.ByteString.Lazy           as BL
import qualified Data.List                      as L
import qualified Data.Set                       as S
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import qualified Data.Text.Lazy.Encoding        as TL

import qualified GHCJS.Prim.TH.Types            as TH

-- #if defined(MIN_VERSION_template_haskell_ghcjs)
-- import qualified "template-haskell-ghcjs" Language.Haskell.TH            as TH
-- import qualified "template-haskell-ghcjs" Language.Haskell.TH.Syntax     as TH
-- #else

import qualified "template-haskell"       Language.Haskell.TH            as TH
import qualified "template-haskell"       Language.Haskell.TH.Syntax     as TH
-- #endif

import           System.Process
  (terminateProcess, waitForProcess)

import           System.FilePath
import           System.IO
import           System.IO.Error
import           System.Timeout

import           ErrUtils
import           GHC.Hs.Expr
import           DsMonad
import           DsExpr
import           GHC.Hs.Pat
import           GHC.Hs.Types
import           GHC.Hs.Decls
import           TcSplice ()
import           UniqDFM

#include "HsVersions.h"

convertE :: SrcSpan -> ByteString -> TcM (LHsExpr GhcPs)
convertE = convertTH (get :: Get TH.Exp)   convertToHsExpr

convertP :: SrcSpan -> ByteString -> TcM (LPat GhcPs)
convertP = convertTH (get :: Get TH.Pat)   convertToPat

convertT :: SrcSpan -> ByteString -> TcM (LHsType GhcPs)
convertT = convertTH (get :: Get TH.Type)  convertToHsType

convertD :: SrcSpan -> ByteString -> TcM [LHsDecl GhcPs]
convertD = convertTH (get :: Get [TH.Dec]) convertToHsDecls

convertTH :: Binary a
          => Get a
          -> (Origin -> SrcSpan -> a -> Either MsgDoc b)
          -> SrcSpan
          -> ByteString
          -> TcM b
convertTH g f s b
  = do
    th_origin <- getThSpliceOrigin
    case f th_origin s (runGet g (BL.fromStrict b)) of
      Left msg -> failWithTc msg
      Right x  -> return x

-- | We only want to produce warnings for TH-splices if the user requests so.
-- See Note [Warnings for TH splices].
getThSpliceOrigin :: TcM Origin
getThSpliceOrigin = do
  warn <- goptM Opt_EnableThSpliceWarnings
  if warn then return FromSource else return Generated

convertAnn :: SrcSpan -> ByteString -> TcM Serialized
convertAnn _ bs = return (toSerialized B.unpack bs)

ghcjsRunMeta :: GhcjsEnv
             -> GhcjsSettings
             -> MetaRequest
             -> LHsExpr GhcTc
             -> TcM MetaResult
ghcjsRunMeta js_env js_settings req expr =
  let m :: (hs_syn -> MetaResult) -> String -> TH.THResultType
        -> Bool -> (hs_syn -> SDoc)
        -> (SrcSpan -> ByteString -> TcM hs_syn)
        -> TcM MetaResult
      m r desc th_type show_code ppr_code convert_res
        = r <$> ghcjsRunMeta' js_env
                              js_settings
                              desc
                              th_type
                              show_code
                              ppr_code
                              convert_res
                              expr
  in case req of
    MetaE  r -> m r "expression"   TH.THExp        True  ppr convertE
    MetaP  r -> m r "pattern"      TH.THPat        True  ppr convertP
    MetaT  r -> m r "type"         TH.THType       True  ppr convertT
    MetaD  r -> m r "declarations" TH.THDec        True  ppr convertD
    MetaAW r -> m r "annotation"   TH.THAnnWrapper False ppr convertAnn

ghcjsRunMeta' :: GhcjsEnv
              -> GhcjsSettings
              -> String
              -> TH.THResultType
              -> Bool
              -> (hs_syn -> SDoc)
              -> (SrcSpan -> ByteString -> TcM hs_syn)
              -> LHsExpr GhcTc
              -> TcM hs_syn
ghcjsRunMeta' js_env js_settings _descr tht _show_code _ppr_code cvt expr = do
  traceTc "About to run" (ppr expr)
  recordThSpliceUse -- seems to be the best place to do this,
                    -- we catch all kinds of splices and annotations.
  failIfErrsM
  ds_expr  <- initDsTc (dsLExpr expr)
  dflags   <- getDynFlags
  hsc_env  <- getTopEnv
  src_span <- getSrcSpanM
  traceTc "About to run (desugared)" (ppr ds_expr)
  (js_code, symb) <-
    compileExpr js_env js_settings hsc_env dflags src_span ds_expr
  gbl_env  <- getGblEnv
  r        <- getThRunner js_env hsc_env dflags (tcg_mod gbl_env)
  base     <- liftIO $ takeMVar (thrBase r)
  let pkgs     = L.nub $
                 (S.toList . imp_dep_pkgs . tcg_imports $ gbl_env) ++
                 concatMap (map fst . dep_pkgs .  mi_deps . hm_iface)
                           (eltsUDFM $ hsc_HPT hsc_env)
      settings = thSettings { gsUseBase = BaseState base }
  lr       <- liftIO $ linkTh js_env
                              settings
                              []
                              dflags
                              pkgs
                              (hsc_HPT hsc_env)
                              (Just js_code)
  ext <- liftIO $ do
    llr      <- mconcat <$> mapM (Gen2.tryReadShimFile dflags)  (Gen2.linkLibRTS lr)
    lla'     <- mconcat <$> mapM (Gen2.tryReadShimFile dflags)  (Gen2.linkLibA lr)
    llaarch' <- mconcat <$> mapM (Gen2.readShimsArchive dflags) (Gen2.linkLibAArch lr)
    return (llr <> lla' <> llaarch')
  let bs = ext <> BL.toStrict (Gen2.linkOut lr)
               <> T.encodeUtf8 ("\nh$TH.loadedSymbol = " <> symb <> ";\n")
  -- fixme exception handling
  hv <- setSrcSpan (getLoc expr) $ do
    loc <- TH.qLocation
    requestRunner r (TH.RunTH tht bs (Just loc)) >>= \case
      TH.RunTH' bsr -> cvt src_span bsr
      _             -> error
        "ghcjsRunMeta': unexpected response, expected RunTH' message"
  liftIO $ putMVar (thrBase r) (Gen2.linkBase lr)
  return hv

compileExpr :: GhcjsEnv -> GhcjsSettings -> HscEnv -> DynFlags
            -> SrcSpan -> CoreExpr -> TcM (ByteString, Text)
compileExpr js_env js_settings hsc_env dflags src_span ds_expr
  = newUnique >>= \u -> liftIO $ do
      -- liftIO (putStrLn $ "compileExpr:\n" ++ Utils.showIndent ds_expr)
      prep_expr     <- corePrepExpr dflags hsc_env ds_expr
      n             <- modifyMVar (thSplice js_env)
                                  (\n -> let n' = n+1 in pure (n',n'))
      let th_mod   = mod n
          th_binds0 = [bind n u prep_expr]
      (spt_entries, th_binds1) <- sptCreateStaticBinds hsc_env th_mod th_binds0
      let (stg_pgm0, cost_centre_info) =
            coreToStg dflags th_mod th_binds1
      stg_pgm1 <- stg2stg dflags th_mod stg_pgm0
      return (Gen2.generate js_settings
                            dflags
                            (mod n)
                            stg_pgm1
                            spt_entries
                            NoStubs
                            cost_centre_info
             , symb n)
  where
    symb n     = "h$thrunnerZCThRunner" <> T.pack (show n) <> "zithExpr"
    thExpr n u = mkVanillaGlobal (mkExternalName u
                                                 (mod n)
                                                 (mkVarOcc "thExpr")
                                                 src_span)
                                 (exprType ds_expr)
    bind n u e = NonRec (thExpr n u) e
    mod n      = mkModule thrunnerPackage'
                          (mkModuleName $ "ThRunner" ++ show n)

thrunnerPackage :: InstalledUnitId
thrunnerPackage = stringToInstalledUnitId "thrunner"

thrunnerPackage' :: UnitId
thrunnerPackage' = stringToUnitId "thrunner"

getThRunner :: GhcjsEnv -> HscEnv -> DynFlags -> Module -> TcM THRunner
getThRunner js_env hsc_env dflags m = do
  let m' = moduleNameString (moduleName m)
  (r, fin) <- liftIO $ modifyMVar (thRunners js_env) $ \runners ->
    case M.lookup m' (activeRunners runners) of
      Just r  -> return (runners, (r, return ()))
      Nothing -> do
        (r, runners') <- startTHRunner dflags js_env hsc_env runners
        let fin = pure () {- fixme add cleanup again
               do
              th_modfinalizers_var <- fmap tcg_th_modfinalizers
                                           getGblEnv
              writeTcRef th_modfinalizers_var
                         [TH.qRunIO (finishTHModule dflags js_env  m' r)] -}
        return (insertActiveRunner m' r runners', (r, fin))
  fin >> return r

{-
  generate the "virtual" linked object for a hs-boot file
 -}
bootObject :: GhcjsSettings
           -> DynFlags
           -> Module
           -> ModDetails
           -> LinkedObj
bootObject js_settings dflags m details =
  let mn      = moduleName m
      stg_pgm = mapMaybe mk_decl (md_exports details)
      mk_decl (Avail n)
        | Just ty_thing <- lookupNameEnv (md_types details) n
        = mk_decl_ty n ty_thing
      mk_decl _ = Nothing

      mk_decl_ty n (AnId i)                     = mk_decl_id n i
      mk_decl_ty _ (AConLike (RealDataCon _dc)) = Nothing
      mk_decl_ty _ _                            = Nothing
      mk_decl_id _ i = Just
        (StgTopLifted (
         (StgNonRec i (StgRhsClosure noExtFieldSilent
                                     dontCareCCS
                                   -- noBinderInfo
                                   -- []
                                   ReEntrant
                                   []
                                    (StgApp i [])))))
  in ObjLoaded
       (moduleNameString mn ++ " [boot]")
       (Gen2.generate js_settings dflags m stg_pgm [] NoStubs emptyCollectedCCs)

linkTh :: GhcjsEnv
       -> GhcjsSettings        -- settings (contains the base state)
       -> [FilePath]           -- extra js files
       -> DynFlags             -- dynamic flags
       -> [InstalledUnitId]    -- package dependencies
       -> HomePackageTable     -- what to link
       -> Maybe ByteString     -- current module or Nothing to get the initial code + rts
       -> IO Gen2.LinkResult
linkTh env settings js_files dflags pkgs hpt code = do
  (th_deps_pkgs, th_deps)  <- Gen2.thDeps dflags pkgs
  let home_mod_infos = eltsUDFM hpt
      pkgs' | isJust code = L.nub $ pkgs ++ th_deps_pkgs
            | otherwise   = th_deps_pkgs
      is_root   = const True
      linkables = concatMap mk_linkable home_mod_infos
      mk_linkable :: HomeModInfo -> [LinkedObj]
      mk_linkable hm = maybe (generate_linkable hm)
                             convert_linkable
                             (hm_linkable hm)

      convert_linkable :: Linkable -> [LinkedObj]
      convert_linkable = map ObjFile . getOfiles

      generate_linkable :: HomeModInfo -> [LinkedObj]
      generate_linkable hm = [bootObject settings
                                         dflags
                                         (mi_module $ hm_iface hm)
                                         (hm_details hm)]

      getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)

      link      = Gen2.link' dflags'
                             env
                             settings
                             "Template Haskell"
                             []
                             pkgs'
                             obj_files
                             js_files
                             is_root
                             th_deps
      dflags'   = dflags { ways        = WayDebug : ways dflags
                         , thisInstalledUnitId = thrunnerPackage -- thisPackage = thrunnerPackage
                         }
      obj_files = maybe []
                        (\b -> ObjLoaded "<Template Haskell>" b : linkables)
                        code
      packageLibPaths :: InstalledUnitId -> [FilePath]
      packageLibPaths pkg = maybe [] libraryDirs (lookupInstalledPackage dflags pkg)
      cache_key = T.pack $
        (show . L.nub . L.sort . map Gen2.funPackage . S.toList $ th_deps) ++
        show (ways dflags') ++
        show (topDir dflags)
  if isJust code
     then link
     else Gen2.getCached dflags' "template-haskell" cache_key >>= \case
            Just c  -> return (runGet get $ BL.fromStrict c)
            Nothing -> do
              lr <- link
              _ <- Gen2.putCached dflags'
                             "template-haskell"
                             cache_key
                             [topDir dflags </> "ghcjs_boot.completed"]
                             (BL.toStrict . runPut . put $ lr)
              return lr

requestRunner :: THRunner -> TH.Message -> TcM TH.Message
requestRunner runner msg = liftIO (sendToRunner runner 0 msg) >> res
  where
    res = liftIO (readFromRunner runner) >>= \case
            (msg, 0) -> return msg
            (req, n) -> do
              liftIO . sendToRunner runner n =<< handleRunnerReq runner req
              res

finishRunner :: Bool -> THRunner -> IO Int
finishRunner stopRunner runner = do
  sendToRunner runner 0 (TH.FinishTH stopRunner)
  memoryUsage <- readFromRunner runner >>= \case
    (TH.FinishTH' memoryUsage, _) -> do
      when stopRunner $ do
        hClose (thrHandleIn runner) `E.catch` \(_::E.SomeException) -> return ()
        hClose (thrHandleErr runner) `E.catch` \(_::E.SomeException) -> return ()
      return memoryUsage
    _                 -> error
       "finishRunner: unexpected response, expected FinishTH' message"
  return memoryUsage

finishRunnerProcess :: THRunner -> IO ()
finishRunnerProcess runner =
  let ph = thrProcess runner
  in  maybe (void $ terminateProcess ph)
            (\_ -> return ())
        =<< timeout 30000000 (waitForProcess ph)

handleRunnerReq :: THRunner -> TH.Message -> TcM TH.Message
handleRunnerReq runner msg =
  case msg of
    TH.QUserException e     -> term                              >>  error e
    TH.QCompilerException n -> term                              >>  throwCompilerException n runner
    TH.QFail e              -> term                              >>  fail e
    TH.StartRecover         -> startRecover runner               >>  pure TH.StartRecover'
    TH.EndRecover b         -> endRecover b runner               >>  pure TH.EndRecover'
    TH.Report isErr msg     -> TH.qReport isErr msg              >>  pure TH.Report'
    _                       -> getEnv >>= \env -> liftIO $
      runIOEnv env (handleOtherReq msg) `E.catch` \e ->
        addException e >>= \n -> return (TH.QCompilerException' n (show e))
  where
    addException :: E.SomeException -> IO Int
    addException e = modifyMVar (thrExceptions runner) $ \m ->
      let s = IM.size m in return (IM.insert s e m, s)
    handleOtherReq :: TH.Message -> TcM TH.Message
    handleOtherReq msg = case msg of
      TH.NewName n                   -> TH.NewName'                       <$> TH.qNewName n
      TH.LookupName b n              -> TH.LookupName'                    <$> TH.qLookupName b n
      TH.Reify n                     -> TH.Reify'                         <$> TH.qReify n
      TH.ReifyInstances n ts         -> TH.ReifyInstances'                <$> TH.qReifyInstances n ts
      TH.ReifyRoles n                -> TH.ReifyRoles'                    <$> TH.qReifyRoles n
      TH.ReifyAnnotations nn         -> TH.ReifyAnnotations' . map B.pack <$> TH.qReifyAnnotations nn
      TH.ReifyModule m               -> TH.ReifyModule'                   <$> TH.qReifyModule m
      TH.ReifyFixity n               -> TH.ReifyFixity'                   <$> TH.qReifyFixity n
      TH.ReifyConStrictness n        -> TH.ReifyConStrictness'            <$> TH.qReifyConStrictness n
      TH.AddForeignFilePath lang src -> TH.qAddForeignFilePath lang src   >>  pure TH.AddForeignFilePath'
      TH.AddTempFile xs              -> TH.AddTempFile'                   <$> TH.qAddTempFile xs
      TH.AddDependentFile f          -> TH.qAddDependentFile f            >>  pure TH.AddDependentFile'
      TH.AddTopDecls decs            -> TH.qAddTopDecls decs              >>  pure TH.AddTopDecls'
      TH.AddCorePlugin plugin        -> do
        hsc_env <- env_top <$> getEnv
        r <- liftIO $ findHomeModule hsc_env (mkModuleName plugin)
        let err = hang
              (text "addCorePlugin: invalid plugin module "
                 <+> text (show plugin)
              )
              2
              (text "Plugins in the current package can't be specified.")
        case r of
          Found {} -> addErr err
          FoundMultiple {} -> addErr err
          _ -> return ()
        th_coreplugins_var <- tcg_th_coreplugins <$> getGblEnv
        updTcRef th_coreplugins_var (plugin:)
        pure TH.AddCorePlugin'
      TH.IsExtEnabled ext            -> TH.IsExtEnabled'                  <$> TH.qIsExtEnabled ext
      TH.ExtsEnabled                 -> TH.ExtsEnabled'                   <$> TH.qExtsEnabled
      _                              -> term >> error "handleRunnerReq: unexpected request"
    term :: TcM ()
    term = liftIO $ terminateProcess (thrProcess runner)

throwCompilerException :: Int -> THRunner -> TcM a
throwCompilerException n runner = liftIO $ do
  e <- IM.lookup n <$> readMVar (thrExceptions runner)
  case e of
    Just ex -> liftIO (E.throwIO ex)
    Nothing -> error "throwCompilerException: exception id not found"

startRecover :: THRunner -> TcM ()
startRecover (thrRecover -> r) = do
  v <- getErrsVar
  msgs <- readTcRef v
  writeTcRef v emptyMessages
  liftIO (modifyMVar_ r (pure . (msgs:)))

endRecover :: Bool -> THRunner -> TcM ()
endRecover recoveryTaken (thrRecover -> r) = do
  msgs <- liftIO $ modifyMVar r (\(h:t) -> pure (t,h))
  v <- getErrsVar
  if recoveryTaken
     then writeTcRef v msgs
     else updTcRef v (unionMessages msgs)
  where
    unionMessages (wm1, em1) (wm2, em2) = (unionBags wm1 wm2, unionBags em1 em2)

finishTHModule :: DynFlags -> GhcjsEnv -> String -> THRunner -> IO ThModFinalizers
finishTHModule dflags js_env m runner = do
  mr <- finishTHp js_env False m runner
  ns <- readNodeSettings dflags
  modifyMVar_ ( thRunners js_env ) ( pure . deleteActiveRunner m )
  if (fromIntegral mr > nodeKeepAliveMaxMem ns)
    then void $ finishTHp js_env True m runner
    else modifyMVar_ ( thRunners js_env ) ( pure . consIdleRunner runner )
  pure (ThModFinalizers []) -- fixme is this right?

finishTHp :: GhcjsEnv -> Bool -> String -> THRunner -> IO Int
finishTHp _js_env endProcess _m runner = do
  let ph = thrProcess runner
  _ <- readMVar (thrBase runner)
  mu <- finishRunner endProcess runner
  when endProcess $
    maybe (void $ terminateProcess ph)
      (\_ -> return ()) =<< timeout 30000000 (waitForProcess ph)
  return mu

finishTHAll :: GhcjsEnv -> IO ()
finishTHAll js_env = do
  runners <- takeMVar (thRunners js_env)
  forM_ (idleRunners runners ++ M.elems (activeRunners runners))
        (\r -> void (finishRunner True r)
         `E.catch`
         \(_::E.SomeException) -> return ())

sendToRunner :: THRunner -> Int -> TH.Message -> IO ()
sendToRunner runner responseTo msg =
  sendToRunnerRaw runner responseTo (BL.toStrict . runPut . put $ msg)

sendToRunnerRaw :: THRunner -> Int -> ByteString -> IO ()
sendToRunnerRaw runner responseTo bs = do
  let header = BL.toStrict . runPut $ do
        putWord32be (fromIntegral $ B.length bs)
        putWord32be (fromIntegral responseTo)
  B.hPut (thrHandleIn runner) (B16.encode $ header <> bs)
  hFlush (thrHandleIn runner)

readFromRunner :: THRunner -> IO (TH.Message, Int)
readFromRunner runner = do
  let h = thrHandleErr runner
  (len, tgt) <- runGet ((,) <$> getWord32be <*> getWord32be) <$> BL.hGet h 8
  (,fromIntegral tgt) . runGet get <$> BL.hGet h (fromIntegral len)

thSettings :: GhcjsSettings
thSettings = GhcjsSettings False True False False Nothing
                           Nothing Nothing True True True
                           Nothing NoBase
                           Nothing Nothing [] False

startTHRunner :: DynFlags
              -> GhcjsEnv
              -> HscEnv
              -> THRunnerState
              -> IO (THRunner, THRunnerState)
startTHRunner dflags js_env hsc_env runners =
  maybe ((,runners) <$> startTHRunnerProcess dflags js_env hsc_env)
        pure
        (unconsIdleRunner runners)

startTHRunnerProcess :: DynFlags -> GhcjsEnv -> HscEnv -> IO THRunner
startTHRunnerProcess dflags js_env hsc_env = do
  lr <- linkTh js_env thSettings [] dflags [] (hsc_HPT hsc_env) Nothing
  fr <- BL.fromChunks <$> mapM (Gen2.tryReadShimFile dflags) (Gen2.linkLibRTS lr)
  fa <- BL.fromChunks <$> mapM (Gen2.tryReadShimFile dflags) (Gen2.linkLibA lr)
  aa <- BL.fromChunks <$> mapM (Gen2.readShimsArchive dflags) (Gen2.linkLibAArch lr)
  let rtsd = TL.encodeUtf8 Gen2.rtsDeclsText
      rts  = TL.encodeUtf8 $ Gen2.rtsText' dflags (Gen2.dfCgSettings dflags)
  (inp,out,err,pid) <- runNodeInteractive dflags Nothing (topDir dflags </> "thrunner.js")
  mv  <- newMVar (Gen2.linkBase lr)
  emv <- newMVar []
  eev <- newMVar IM.empty
  _ <- forkIO $ catchIOError (forever $ hGetChar out >>= putChar) (\_ -> return ())
  let r = THRunner pid inp err mv emv eev
  sendToRunnerRaw r 0 (BL.toStrict $ rtsd <> fr <> rts <> fa <> aa <> Gen2.linkOut lr)
  return r
