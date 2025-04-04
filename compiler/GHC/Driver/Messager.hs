module GHC.Driver.Messager (Messager, oneShotMsg, batchMsg, batchMultiMsg, showModuleIndex) where

import GHC.Prelude
import GHC.Driver.Env
import GHC.Unit.Module.Graph
import GHC.Iface.Recomp
import GHC.Utils.Logger
import GHC.Utils.Outputable
import GHC.Utils.Error
import GHC.Unit.State

type Messager = HscEnv -> (Int,Int) -> RecompileRequired -> ModuleGraphNode -> IO ()

--------------------------------------------------------------
-- Progress displayers.
--------------------------------------------------------------

oneShotMsg :: Logger -> RecompileRequired -> IO ()
oneShotMsg logger recomp =
    case recomp of
        UpToDate -> compilationProgressMsg logger $ text "compilation IS NOT required"
        NeedsRecompile _ -> return ()

batchMsg :: Messager
batchMsg = batchMsgWith (\_ _ _ _ -> empty)
batchMultiMsg :: Messager
batchMultiMsg = batchMsgWith (\_ _ _ node -> brackets (ppr (mgNodeUnitId node)))

batchMsgWith :: (HscEnv -> (Int, Int) -> RecompileRequired -> ModuleGraphNode -> SDoc) -> Messager
batchMsgWith extra hsc_env_start mod_index recomp node =
      case recomp of
        UpToDate
          | logVerbAtLeast logger 2 -> showMsg (text "Skipping") empty
          | otherwise -> return ()
        NeedsRecompile reason0 -> showMsg (text herald) $ case reason0 of
          MustCompile            -> empty
          (RecompBecause reason) -> text " [" <> pprWithUnitState state (ppr reason) <> text "]"
    where
        herald = case node of
                    LinkNode {} -> "Linking"
                    InstantiationNode {} -> "Instantiating"
                    ModuleNode {} -> "Compiling"
                    UnitNode {} -> "Loading"
        hsc_env = hscSetActiveUnitId (mgNodeUnitId node) hsc_env_start
        dflags = hsc_dflags hsc_env
        logger = hsc_logger hsc_env
        state  = hsc_units hsc_env
        showMsg msg reason =
            compilationProgressMsg logger $
            (showModuleIndex mod_index <>
            msg <+> showModMsg dflags (recompileRequired recomp) node)
                <> extra hsc_env mod_index recomp node
                <> reason

{- **********************************************************************
%*                                                                      *
        Progress Messages: Module i of n
%*                                                                      *
%********************************************************************* -}

showModuleIndex :: (Int, Int) -> SDoc
showModuleIndex (i,n) = text "[" <> pad <> int i <> text " of " <> int n <> text "] "
  where
    -- compute the length of x > 0 in base 10
    len x = ceiling (logBase 10 (fromIntegral x+1) :: Float)
    pad = text (replicate (len n - len i) ' ') -- TODO: use GHC.Utils.Ppr.RStr