module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import GHC (getSessionDynFlags, runGhc)
import GHC.Driver.DynFlags (targetProfile)
import GHC.Iface.Binary
import GHC.Iface.Syntax
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Cache (newNameCache)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Unit.Module.ModIface
import System.Environment (getArgs)

main :: IO ()
main = do
    [lib, input, output] <- getArgs
    runGhc (Just lib) $ do
        profile <- targetProfile <$> getSessionDynFlags
        liftIO $ do
            cache <- newNameCache
            original <- readBinIface profile cache CheckHiWay QuietBinIFace input
            case mapMaybe (changeDecl . snd) (mi_decls original) of
              [changed] -> writeBinIface profile QuietBinIFace NormalCompression output $
                  set_mi_decls
                    [ (fp, if ifName decl == ifName changed then changed else decl)
                    | (fp, decl) <- mi_decls original ] original
              _ -> fail "expected one legacy constructor proof with one field"

-- | Replace the one legacy constructor proof in the inspectSwap unfolding.
changeDecl :: IfaceDecl -> Maybe IfaceDecl
changeDecl decl@IfaceId{ifName = name, ifIdInfo = info}
  | occNameString (nameOccName name) == "inspectSwap"
  , [_] <- mapMaybe changeInfo info
  = Just decl{ifIdInfo = map (\item -> maybe item id (changeInfo item)) info}
  where
    changeInfo (HsUnfold loop (IfCoreUnfold src cache guidance rhs)) =
        HsUnfold loop . IfCoreUnfold src cache guidance <$> changeExpr rhs
    changeInfo _ = Nothing
changeDecl _ = Nothing

-- | Give the field and its virtual proof the same local name.
changeExpr :: IfaceExpr -> Maybe IfaceExpr
changeExpr (IfaceLam binder rhs) = IfaceLam binder <$> changeExpr rhs
changeExpr (IfaceCase scrut binder alts)
  | [()] <- [() | IfaceAlt IfaceDataAltWithProof{} [_, _] _ <- alts]
  = Just (IfaceCase scrut binder (map duplicate alts))
  where
    duplicate (IfaceAlt tag@IfaceDataAltWithProof{} [_, proof] rhs) =
        IfaceAlt tag [proof, proof] rhs
    duplicate alt = alt
changeExpr _ = Nothing
