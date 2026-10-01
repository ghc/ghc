-- | A GHC plugin that prints the annotations of direct imports of a module.
-- Also adds an annotation to the module itself.
module T26761Plugin where

import Control.Monad (forM_)
import GHC
import GHC.Plugins
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import qualified Data.Map as Map
import Data.Word (Word8)
import System.IO (hFlush, stdout)

plugin :: Plugin
plugin = defaultPlugin { typeCheckResultAction = typecheckPlugin }
  where
    typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
    typecheckPlugin opts summary gblEnv = do
      hscEnv <- getTopEnv
      let home_unit = hsc_home_unit hscEnv
          directImports =
            filter (isHomeModule home_unit) $ Map.keys $ imp_mods $ tcg_imports gblEnv
      annEnv <- liftIO $ prepareAnnotations hscEnv Nothing
      forM_ directImports $ \mod0 -> liftIO $
        case findAnns id annEnv (ModuleTarget mod0) of
          [] -> pure ()
          anns -> do
            putStrLn $ concat
              [ "Found "
              , show (length anns)
              , " annotations for direct import: "
              , moduleNameString (moduleName mod0)
              ]
            -- TODO: Remove #20791
            liftIO $ hFlush stdout

      -- Insert an annotation for the current module itself, so that we can
      -- check that it is visible from other modules.
      let ann = Annotation
                  (ModuleTarget $ tcg_mod gblEnv)
                  (toSerialized id [0])
      return gblEnv { tcg_anns = ann : tcg_anns gblEnv }
