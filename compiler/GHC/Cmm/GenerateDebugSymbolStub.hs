{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE Strict #-}

module GHC.Cmm.GenerateDebugSymbolStub
  ( generateDebugSymbolStub,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.List (isSuffixOf)
import Data.Map.Strict qualified as Map
import Data.Maybe
import GHC.Cmm
import GHC.Cmm.CLabel
import GHC.Cmm.Dataflow.Label qualified as H
import GHC.Data.FastString
import GHC.Data.Stream (Stream)
import GHC.Data.Stream qualified as Stream
import GHC.Platform
import GHC.Prelude
import GHC.Types.ForeignStubs
import GHC.Unit.Types
import GHC.Utils.Outputable

generateDebugSymbolStub ::
  (MonadIO m) =>
  Platform ->
  Module ->
  Stream m RawCmmGroup r ->
  Stream m RawCmmGroup (r, CStub)
generateDebugSymbolStub platform this_mod rawcmms0 = do
  (lbls_ref, per_group) <- liftIO $ do
    lbls_ref <- newIORef Map.empty
    let per_group decls = for_ decls per_decl $> decls
        per_decl (CmmData _ (CmmStaticsRaw lbl _)) =
          liftIO
            $ when (externallyVisibleCLabel lbl)
            $ modifyIORef' lbls_ref
            $ Map.insert lbl (data_label_type lbl)
        per_decl (CmmProc h lbl _ _) = case H.mapToList h of
          [] ->
            liftIO
              $ when (externallyVisibleCLabel lbl)
              $ modifyIORef' lbls_ref
              $ Map.insert lbl (proc_label_type lbl)
          hs -> for_ hs $ \(_, CmmStaticsRaw lbl _) ->
            liftIO
              $ when (externallyVisibleCLabel lbl)
              $ modifyIORef' lbls_ref
              $ Map.insert lbl (data_label_type lbl)
        data_label_type lbl
          | "_closure"
              `isSuffixOf` str
              && not
                (str `elem` ["stg_CHARLIKE_closure", "stg_INTLIKE_closure"]) =
              Just ("extern StgClosure ", "")
          | "_str" `isSuffixOf` str =
              Just ("EB_(", ")")
          | str
              `elem` [ "stg_arg_bitmaps",
                       "stg_ap_stack_entries",
                       "stg_stack_save_entries"
                     ] =
              Just ("ERO_(", ")")
          | str
              `elem` [ "no_break_on_exception",
                       "stg_scheduler_loop_epoch",
                       "stg_scheduler_loop_tid"
                     ] =
              Just ("ERW_(", ")")
          | str
              `elem` [ "stg_gc_prim_p_ll_info",
                       "stg_gc_prim_pp_ll_info",
                       "stg_JSVAL_info",
                       "stg_scheduler_loop_info"
                     ] =
              Just ("extern const StgInfoTable ", "")
          | not $ needsCDecl lbl =
              Nothing
          | "_cc" `isSuffixOf` str =
              Just ("extern CostCentre ", "[]")
          | "_ccs" `isSuffixOf` str =
              Just ("extern CostCentreStack ", "[]")
          | "_ipe_buf" `isSuffixOf` str =
              Just ("extern IpeBufferListNode ", "")
          | otherwise =
              Just ("ERW_(", ")")
          where
            str =
              showSDocOneLine defaultSDocContext {sdocStyle = PprCode}
                $ pprCLabel platform lbl
        proc_label_type _ = Just ("EF_(", ")")
    pure (lbls_ref, per_group)
  r <- Stream.mapM per_group rawcmms0
  liftIO $ do
    lbls <- Map.toList <$> readIORef lbls_ref
    let ctor_lbl = mkInitializerStubLabel this_mod $ fsLit "symbolizer"
        entries_lbl =
          mkInitializerStubLabel this_mod $ fsLit "symbolizer_entries"
        ctor_decls =
          vcat
            [ text lbl_type_l
                <> pprCLabel platform lbl
                <> text lbl_type_r
                <> semi
            | (lbl, maybe_lbl_type) <- lbls,
              (lbl_type_l, lbl_type_r) <- maybeToList maybe_lbl_type
            ]
            <> text "static const DebugSymbolEntry "
            <> pprCLabel platform entries_lbl
            <> text "[] = "
            <> braces
              ( hsep
                  $ punctuate
                    comma
                    [ braces
                        $ text ".addr = (void*)&"
                        <> pprCLabel platform lbl
                        <> comma
                        <> text ".sym = "
                        <> doubleQuotes (pprCLabel platform lbl)
                    | (lbl, _) <- lbls
                    ]
              )
            <> semi
        ctor_body =
          text "registerDebugSymbol"
            <> parens
              (pprCLabel platform entries_lbl <> comma <> int (length lbls))
            <> semi
        cstub = case lbls of
          [] -> mempty
          _ -> initializerCStub platform ctor_lbl ctor_decls ctor_body
    pure (r, cstub)
