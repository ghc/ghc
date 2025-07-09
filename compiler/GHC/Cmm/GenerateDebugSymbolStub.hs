module GHC.Cmm.GenerateDebugSymbolStub
  ( generateDebugSymbolStub,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.IORef
import Data.Set qualified as Set
import GHC.Cmm
import GHC.Cmm.CLabel
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
  (data_clbls_ref, func_clbls_ref, per_group) <- liftIO $ do
    data_clbls_ref <- newIORef Set.empty
    func_clbls_ref <- newIORef Set.empty
    let per_group decls = for_ decls per_decl $> decls
        per_decl (CmmData _ (CmmStaticsRaw lbl _)) =
          liftIO $ when (externallyVisibleCLabel lbl) $ modifyIORef' data_clbls_ref $ Set.insert lbl
        per_decl (CmmProc _ lbl _ _) =
          liftIO $ when (externallyVisibleCLabel lbl) $ modifyIORef' func_clbls_ref $ Set.insert lbl
    pure (data_clbls_ref, func_clbls_ref, per_group)
  r <- Stream.mapM per_group rawcmms0
  liftIO $ do
    data_clbls <- Set.toList <$> readIORef data_clbls_ref
    func_clbls <- Set.toList <$> readIORef func_clbls_ref
    let ctor_clbl = mkInitializerStubLabel this_mod $ fsLit "symbolizer"
        ctor_decls =
          vcat
            $ [ text
                  "extern void insertDebugSymbol( void *addr, const char *sym );"
              ]
            ++ [ text "ERW_" <> parens (pprCLabel platform clbl) <> semi
               | clbl <- data_clbls
               ]
            ++ [ text "EF_" <> parens (pprCLabel platform clbl) <> semi
               | clbl <- func_clbls
               ]
        ctor_body =
          vcat
            $ [ text "insertDebugSymbol"
                  <> parens
                    ( text "(void*)"
                        <> pprCLabel platform clbl
                        <> comma
                        <> doubleQuotes (pprCLabel platform clbl)
                    )
                  <> semi
              | clbl <- data_clbls ++ func_clbls
              ]
        cstub = initializerCStub platform ctor_clbl ctor_decls ctor_body
    pure (r, cstub)
