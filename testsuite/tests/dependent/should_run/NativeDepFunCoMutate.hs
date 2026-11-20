module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import GHC (getSessionDynFlags, runGhc)
import GHC.Driver.DynFlags (targetProfile)
import GHC.Iface.Binary
import GHC.Iface.Syntax
import GHC.Iface.Type
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Cache (newNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Unit.Module.ModIface
import System.Environment (getArgs)

main :: IO ()
main = do
    [lib, mutation, input, output] <- getArgs
    runGhc (Just lib) $ do
        profile <- targetProfile <$> getSessionDynFlags
        liftIO $ do
            cache <- newNameCache
            original <- readBinIface profile cache CheckHiWay QuietBinIFace input
            modified <- case mapMaybe (changeDecl mutation . snd) (mi_decls original) of
                [_] -> pure (set_mi_decls (map (update mutation) (mi_decls original)) original)
                _ -> fail "expected one unfolding with a non-reflexive DepFunCo"
            writeBinIface profile QuietBinIFace NormalCompression output modified

update mutation (fp, decl) =
    (fp, maybe decl id (changeDecl mutation decl))

changeDecl mutation decl@IfaceId{ifName = name, ifIdInfo = info}
    | occNameString (nameOccName name) == "transport"
    , not (null (mapMaybe (changeInfo mutation) info)) =
        Just decl{ifIdInfo = map (\item -> maybe item id (changeInfo mutation item)) info}
changeDecl _ _ = Nothing

changeInfo mutation (HsUnfold loop (IfCoreUnfold src cache guidance (IfaceCast rhs co))) =
    HsUnfold loop . IfCoreUnfold src cache guidance . IfaceCast rhs <$> changeCo mutation co
changeInfo _ _ = Nothing

changeCo mutation (IfaceSubCo body) = IfaceSubCo <$> changeCo mutation body
changeCo mutation (IfaceFunCo role ma mult arg res) =
    IfaceFunCo role ma mult arg <$> changeCo mutation res
changeCo mutation (IfaceDepFunCo left right binder@(_, name, _) domain body) =
    case domain of
        IfaceMRefl -> IfaceDepFunCo left right binder domain <$> changeCo mutation body
        IfaceMCo co ->
            let bad = case mutation of
                    "role" -> IfaceSubCo co
                    "scope" -> IfaceReflCo (IfaceTermVar name)
                    _ -> error "unknown mutation"
            in Just (IfaceDepFunCo left right binder (IfaceMCo bad) body)
changeCo _ _ = Nothing
