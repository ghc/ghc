module T27121 where

import T27121_aux

updateFileDiagnostics
  :: LanguageContextEnv ()
  -> IO ()
updateFileDiagnostics env = do
  withTrace $ \ _tag ->
    runLspT env $ do
      sendNotification SMethod_TextDocumentPublishDiagnostics
        PublishDiagnosticsParams
