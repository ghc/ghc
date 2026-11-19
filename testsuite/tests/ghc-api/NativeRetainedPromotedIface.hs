module Main where
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import GHC
import GHC.Builtin.WiredIn.Types (intTy, promotedTrueDataCon)
import GHC.CoreToIface (toIfaceType)
import GHC.Data.IOEnv (tryM)
import GHC.Core.Type (mkTyConTy)
import GHC.Iface.Syntax (IfaceExpr(..))
import GHC.Iface.Type
import GHC.IfaceToCore (tcIfaceExpr)
import GHC.Tc.Utils.Monad (initIfaceLoad, initIfaceLcl)
import GHC.Types.Name (nameOccName, nameModule)
import GHC.Unit.Module (IsBootInterface(..))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Utils.Outputable (text)
import System.Environment (getArgs)

main :: IO ()
main = do
  [libdir, source] <- getArgs
  runGhc (Just libdir) $ do
    logger <- getLogger
    flags <- getSessionDynFlags
    (flags', _, _) <- parseDynamicFlags logger flags [noLoc "-fno-code"]
    setSessionDynFlags flags'
    target <- guessTarget source Nothing Nothing
    setTargets [target]
    success <- load LoadAllTargets
    case success of
      Failed -> liftIO (fail "could not load PackedVecLibrary")
      Succeeded -> pure ()
    graph <- getModuleGraph
    summary <- maybe (liftIO (fail "missing PackedVecLibrary")) pure $
      find ((== mkModuleName "PackedVecLibrary") . moduleName . ms_mod)
           (mgModSummaries graph)
    info <- maybe (error "no module") pure =<< getModuleInfo (ms_mod summary)
    let name occ = case find ((== occ) . occNameString . nameOccName) (modInfoExports info) of
          Just n -> n
          Nothing -> error ("missing export " ++ occ)
        con occ = IfaceTyCon (name occ) (mkIfaceTyConInfo IsPromoted IfaceNormalTyCon)
        args = foldr (\a -> IA_Arg a (IfaceForAllTyFlag Required)) IA_Nil
        pack = IfaceTyConApp (con "PackVec") . args
        z = IfaceTyConApp (con "Z") IA_Nil
        s = IfaceTyConApp (con "S") (args [z])
        payload = IfaceTyConApp (con "VNil") (args [toIfaceType intTy])
        wrong = toIfaceType (mkTyConTy promotedTrueDataCon)
        cases =
          [ ("bare", True, pack [])
          , ("universal", True, pack [toIfaceType intTy])
          , ("witness", True, pack [toIfaceType intTy, z])
          , ("valid", True, pack [toIfaceType intTy, z, payload])
          , ("valid suffix", True, IfaceAppTy (pack [toIfaceType intTy]) (args [z, payload]))
          , ("bad erased argument", False, pack [wrong])
          , ("bad witness", False, pack [toIfaceType intTy, wrong, payload])
          , ("wrong payload index", False, pack [toIfaceType intTy, s, payload])
          , ("wrong payload suffix", False, IfaceAppTy (pack [toIfaceType intTy, s]) (args [payload]))
          , ("oversaturated", False, pack [toIfaceType intTy, z, payload, toIfaceType intTy])
          ]
    env <- getSession
    liftIO $ forM_ cases $ \(label, valid, ty) -> do
      decoded <- initIfaceLoad env $
        initIfaceLcl (nameModule (name "PackVec")) (text "probe") NotBoot $
          tryM (tcIfaceExpr (IfaceType ty))
      case (valid, decoded) of
        (True, Right _) -> putStrLn (label ++ ": accepted")
        (False, Left _) -> putStrLn (label ++ ": rejected")
        _ -> fail (label ++ ": unexpected interface validation result")
