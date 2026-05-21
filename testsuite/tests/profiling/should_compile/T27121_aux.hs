{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}

module T27121_aux
  ( withTrace
  , sendNotification
  , LspT, runLspT
  , SMethod(..)
  , LanguageContextEnv
  , PublishDiagnosticsParams(..)
  )
 where

-- base
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Kind ( Type )
import GHC.TypeLits ( Symbol )

--------------------------------------------------------------------------------

withTrace :: Monad m => ((String -> String -> m ()) -> m a) -> m a
withTrace act
  | myUserTracingEnabled
  = return undefined
  | otherwise = act (\_ _ -> pure ())
{-# NOINLINE withTrace #-}

myUserTracingEnabled :: Bool
myUserTracingEnabled = False
{-# NOINLINE myUserTracingEnabled #-}

type Text = String

newtype LspT config a = LspT {unLspT :: LanguageContextEnv config -> IO a}

instance Functor (LspT config) where
  fmap f (LspT g) = LspT (fmap f . g)

instance Applicative (LspT config) where
  pure = LspT . const . pure
  LspT f <*> LspT a = LspT $ \ env -> f env <*> a env
instance Monad (LspT config) where
  LspT a >>= f = LspT $ \ env -> do
    b <- a env
    unLspT ( f b ) env
instance MonadIO (LspT config) where
  liftIO = LspT . const . liftIO

type role LspT representational nominal

runLspT :: LanguageContextEnv config -> LspT config a -> IO a
runLspT env (LspT f) = f env
{-# INLINE runLspT #-}

data PublishDiagnosticsParams = PublishDiagnosticsParams

data LanguageContextEnv config =
  LanguageContextEnv
    { resSendMessage :: FromServerMessage -> IO () }


sendNotification ::
  forall (m :: Method ServerToClient Notification) f config.
  MonadLsp config f =>
  SServerMethod m ->
  MessageParams m ->
  f ()
sendNotification m params =
  let msg = TNotificationMessage { _method = m, _params = params }
   in case splitServerMethod m of
        IsServerNot -> sendToClient $ fromServerNot msg

type Method :: MessageDirection -> MessageKind -> Type
data Method f t where
  Method_TextDocumentImplementation :: Method ClientToServer Request
  Method_TextDocumentTypeDefinition :: Method ClientToServer Request
  Method_WorkspaceWorkspaceFolders :: Method ServerToClient Request
  Method_WorkspaceConfiguration :: Method ServerToClient Request
  Method_TextDocumentDocumentColor :: Method ClientToServer Request
  Method_TextDocumentColorPresentation :: Method ClientToServer Request
  Method_TextDocumentFoldingRange :: Method ClientToServer Request
  Method_TextDocumentDeclaration :: Method ClientToServer Request
  Method_TextDocumentSelectionRange :: Method ClientToServer Request
  Method_WindowWorkDoneProgressCreate :: Method ServerToClient Request
  Method_TextDocumentPrepareCallHierarchy :: Method ClientToServer Request
  Method_CallHierarchyIncomingCalls :: Method ClientToServer Request
  Method_CallHierarchyOutgoingCalls :: Method ClientToServer Request
  Method_TextDocumentSemanticTokensFull :: Method ClientToServer Request
  Method_TextDocumentSemanticTokensFullDelta :: Method ClientToServer Request
  Method_TextDocumentSemanticTokensRange :: Method ClientToServer Request
  Method_WorkspaceSemanticTokensRefresh :: Method ServerToClient Request
  Method_WindowShowDocument :: Method ServerToClient Request
  Method_TextDocumentLinkedEditingRange :: Method ClientToServer Request
  Method_WorkspaceWillCreateFiles :: Method ClientToServer Request
  Method_WorkspaceWillRenameFiles :: Method ClientToServer Request
  Method_WorkspaceWillDeleteFiles :: Method ClientToServer Request
  Method_TextDocumentMoniker :: Method ClientToServer Request
  Method_TextDocumentPrepareTypeHierarchy :: Method ClientToServer Request
  Method_TypeHierarchySupertypes :: Method ClientToServer Request
  Method_TypeHierarchySubtypes :: Method ClientToServer Request
  Method_TextDocumentInlineValue :: Method ClientToServer Request
  Method_WorkspaceInlineValueRefresh :: Method ServerToClient Request
  Method_TextDocumentInlayHint :: Method ClientToServer Request
  Method_InlayHintResolve :: Method ClientToServer Request
  Method_WorkspaceInlayHintRefresh :: Method ServerToClient Request
  Method_TextDocumentDiagnostic :: Method ClientToServer Request
  Method_WorkspaceDiagnostic :: Method ClientToServer Request
  Method_WorkspaceDiagnosticRefresh :: Method ServerToClient Request
  Method_ClientRegisterCapability :: Method ServerToClient Request
  Method_ClientUnregisterCapability :: Method ServerToClient Request
  Method_Initialize :: Method ClientToServer Request
  Method_Shutdown :: Method ClientToServer Request
  Method_WindowShowMessageRequest :: Method ServerToClient Request
  Method_TextDocumentWillSaveWaitUntil :: Method ClientToServer Request
  Method_TextDocumentCompletion :: Method ClientToServer Request
  Method_CompletionItemResolve :: Method ClientToServer Request
  Method_TextDocumentHover :: Method ClientToServer Request
  Method_TextDocumentSignatureHelp :: Method ClientToServer Request
  Method_TextDocumentDefinition :: Method ClientToServer Request
  Method_TextDocumentReferences :: Method ClientToServer Request
  Method_TextDocumentDocumentHighlight :: Method ClientToServer Request
  Method_TextDocumentDocumentSymbol :: Method ClientToServer Request
  Method_TextDocumentCodeAction :: Method ClientToServer Request
  Method_CodeActionResolve :: Method ClientToServer Request
  Method_WorkspaceSymbol :: Method ClientToServer Request
  Method_WorkspaceSymbolResolve :: Method ClientToServer Request
  Method_TextDocumentCodeLens :: Method ClientToServer Request
  Method_CodeLensResolve :: Method ClientToServer Request
  Method_WorkspaceCodeLensRefresh :: Method ServerToClient Request
  Method_TextDocumentDocumentLink :: Method ClientToServer Request
  Method_DocumentLinkResolve :: Method ClientToServer Request
  Method_TextDocumentFormatting :: Method ClientToServer Request
  Method_TextDocumentRangeFormatting :: Method ClientToServer Request
  Method_TextDocumentOnTypeFormatting :: Method ClientToServer Request
  Method_TextDocumentRename :: Method ClientToServer Request
  Method_TextDocumentPrepareRename :: Method ClientToServer Request
  Method_WorkspaceExecuteCommand :: Method ClientToServer Request
  Method_WorkspaceApplyEdit :: Method ServerToClient Request
  Method_WorkspaceDidChangeWorkspaceFolders :: Method ClientToServer Notification
  Method_WindowWorkDoneProgressCancel :: Method ClientToServer Notification
  Method_WorkspaceDidCreateFiles :: Method ClientToServer Notification
  Method_WorkspaceDidRenameFiles :: Method ClientToServer Notification
  Method_WorkspaceDidDeleteFiles :: Method ClientToServer Notification
  Method_NotebookDocumentDidOpen :: Method ClientToServer Notification
  Method_NotebookDocumentDidChange :: Method ClientToServer Notification
  Method_NotebookDocumentDidSave :: Method ClientToServer Notification
  Method_NotebookDocumentDidClose :: Method ClientToServer Notification
  Method_Initialized :: Method ClientToServer Notification
  Method_Exit :: Method ClientToServer Notification
  Method_WorkspaceDidChangeConfiguration :: Method ClientToServer Notification
  Method_WindowShowMessage :: Method ServerToClient Notification
  Method_WindowLogMessage :: Method ServerToClient Notification
  Method_TelemetryEvent :: Method ServerToClient Notification
  Method_TextDocumentDidOpen :: Method ClientToServer Notification
  Method_TextDocumentDidChange :: Method ClientToServer Notification
  Method_TextDocumentDidClose :: Method ClientToServer Notification
  Method_TextDocumentDidSave :: Method ClientToServer Notification
  Method_TextDocumentWillSave :: Method ClientToServer Notification
  Method_WorkspaceDidChangeWatchedFiles :: Method ClientToServer Notification
  Method_TextDocumentPublishDiagnostics :: Method ServerToClient Notification
  Method_SetTrace :: Method ClientToServer Notification
  Method_LogTrace :: Method ServerToClient Notification
  Method_CancelRequest :: Method f Notification
  Method_Progress :: Method f Notification
  Method_CustomMethod :: Symbol -> Method f t

type SMethod :: forall f t . Method f t -> Type
data SMethod m where
  SMethod_TextDocumentImplementation :: SMethod Method_TextDocumentImplementation
  SMethod_TextDocumentTypeDefinition :: SMethod Method_TextDocumentTypeDefinition
  SMethod_WorkspaceWorkspaceFolders :: SMethod Method_WorkspaceWorkspaceFolders
  SMethod_WorkspaceConfiguration :: SMethod Method_WorkspaceConfiguration
  SMethod_TextDocumentDocumentColor :: SMethod Method_TextDocumentDocumentColor
  SMethod_TextDocumentColorPresentation :: SMethod Method_TextDocumentColorPresentation
  SMethod_TextDocumentFoldingRange :: SMethod Method_TextDocumentFoldingRange
  SMethod_TextDocumentDeclaration :: SMethod Method_TextDocumentDeclaration
  SMethod_TextDocumentSelectionRange :: SMethod Method_TextDocumentSelectionRange
  SMethod_WindowWorkDoneProgressCreate :: SMethod Method_WindowWorkDoneProgressCreate
  SMethod_TextDocumentPrepareCallHierarchy :: SMethod Method_TextDocumentPrepareCallHierarchy
  SMethod_CallHierarchyIncomingCalls :: SMethod Method_CallHierarchyIncomingCalls
  SMethod_CallHierarchyOutgoingCalls :: SMethod Method_CallHierarchyOutgoingCalls
  SMethod_TextDocumentSemanticTokensFull :: SMethod Method_TextDocumentSemanticTokensFull
  SMethod_TextDocumentSemanticTokensFullDelta :: SMethod Method_TextDocumentSemanticTokensFullDelta
  SMethod_TextDocumentSemanticTokensRange :: SMethod Method_TextDocumentSemanticTokensRange
  SMethod_WorkspaceSemanticTokensRefresh :: SMethod Method_WorkspaceSemanticTokensRefresh
  SMethod_WindowShowDocument :: SMethod Method_WindowShowDocument
  SMethod_TextDocumentLinkedEditingRange :: SMethod Method_TextDocumentLinkedEditingRange
  SMethod_WorkspaceWillCreateFiles :: SMethod Method_WorkspaceWillCreateFiles
  SMethod_WorkspaceWillRenameFiles :: SMethod Method_WorkspaceWillRenameFiles
  SMethod_WorkspaceWillDeleteFiles :: SMethod Method_WorkspaceWillDeleteFiles
  SMethod_TextDocumentMoniker :: SMethod Method_TextDocumentMoniker
  SMethod_TextDocumentPrepareTypeHierarchy :: SMethod Method_TextDocumentPrepareTypeHierarchy
  SMethod_TypeHierarchySupertypes :: SMethod Method_TypeHierarchySupertypes
  SMethod_TypeHierarchySubtypes :: SMethod Method_TypeHierarchySubtypes
  SMethod_TextDocumentInlineValue :: SMethod Method_TextDocumentInlineValue
  SMethod_WorkspaceInlineValueRefresh :: SMethod Method_WorkspaceInlineValueRefresh
  SMethod_TextDocumentInlayHint :: SMethod Method_TextDocumentInlayHint
  SMethod_InlayHintResolve :: SMethod Method_InlayHintResolve
  SMethod_WorkspaceInlayHintRefresh :: SMethod Method_WorkspaceInlayHintRefresh
  SMethod_TextDocumentDiagnostic :: SMethod Method_TextDocumentDiagnostic
  SMethod_WorkspaceDiagnostic :: SMethod Method_WorkspaceDiagnostic
  SMethod_WorkspaceDiagnosticRefresh :: SMethod Method_WorkspaceDiagnosticRefresh
  SMethod_ClientRegisterCapability :: SMethod Method_ClientRegisterCapability
  SMethod_ClientUnregisterCapability :: SMethod Method_ClientUnregisterCapability
  SMethod_Initialize :: SMethod Method_Initialize
  SMethod_Shutdown :: SMethod Method_Shutdown
  SMethod_WindowShowMessageRequest :: SMethod Method_WindowShowMessageRequest
  SMethod_TextDocumentWillSaveWaitUntil :: SMethod Method_TextDocumentWillSaveWaitUntil
  SMethod_TextDocumentCompletion :: SMethod Method_TextDocumentCompletion
  SMethod_CompletionItemResolve :: SMethod Method_CompletionItemResolve
  SMethod_TextDocumentHover :: SMethod Method_TextDocumentHover
  SMethod_TextDocumentSignatureHelp :: SMethod Method_TextDocumentSignatureHelp
  SMethod_TextDocumentDefinition :: SMethod Method_TextDocumentDefinition
  SMethod_TextDocumentReferences :: SMethod Method_TextDocumentReferences
  SMethod_TextDocumentDocumentHighlight :: SMethod Method_TextDocumentDocumentHighlight
  SMethod_TextDocumentDocumentSymbol :: SMethod Method_TextDocumentDocumentSymbol
  SMethod_TextDocumentCodeAction :: SMethod Method_TextDocumentCodeAction
  SMethod_CodeActionResolve :: SMethod Method_CodeActionResolve
  SMethod_WorkspaceSymbol :: SMethod Method_WorkspaceSymbol
  SMethod_WorkspaceSymbolResolve :: SMethod Method_WorkspaceSymbolResolve
  SMethod_TextDocumentCodeLens :: SMethod Method_TextDocumentCodeLens
  SMethod_CodeLensResolve :: SMethod Method_CodeLensResolve
  SMethod_WorkspaceCodeLensRefresh :: SMethod Method_WorkspaceCodeLensRefresh
  SMethod_TextDocumentDocumentLink :: SMethod Method_TextDocumentDocumentLink
  SMethod_DocumentLinkResolve :: SMethod Method_DocumentLinkResolve
  SMethod_TextDocumentFormatting :: SMethod Method_TextDocumentFormatting
  SMethod_TextDocumentRangeFormatting :: SMethod Method_TextDocumentRangeFormatting
  SMethod_TextDocumentOnTypeFormatting :: SMethod Method_TextDocumentOnTypeFormatting
  SMethod_TextDocumentRename :: SMethod Method_TextDocumentRename
  SMethod_TextDocumentPrepareRename :: SMethod Method_TextDocumentPrepareRename
  SMethod_WorkspaceExecuteCommand :: SMethod Method_WorkspaceExecuteCommand
  SMethod_WorkspaceApplyEdit :: SMethod Method_WorkspaceApplyEdit
  SMethod_WorkspaceDidChangeWorkspaceFolders :: SMethod Method_WorkspaceDidChangeWorkspaceFolders
  SMethod_WindowWorkDoneProgressCancel :: SMethod Method_WindowWorkDoneProgressCancel
  SMethod_WorkspaceDidCreateFiles :: SMethod Method_WorkspaceDidCreateFiles
  SMethod_WorkspaceDidRenameFiles :: SMethod Method_WorkspaceDidRenameFiles
  SMethod_WorkspaceDidDeleteFiles :: SMethod Method_WorkspaceDidDeleteFiles
  SMethod_NotebookDocumentDidOpen :: SMethod Method_NotebookDocumentDidOpen
  SMethod_NotebookDocumentDidChange :: SMethod Method_NotebookDocumentDidChange
  SMethod_NotebookDocumentDidSave :: SMethod Method_NotebookDocumentDidSave
  SMethod_NotebookDocumentDidClose :: SMethod Method_NotebookDocumentDidClose
  SMethod_Initialized :: SMethod Method_Initialized
  SMethod_Exit :: SMethod Method_Exit
  SMethod_WorkspaceDidChangeConfiguration :: SMethod Method_WorkspaceDidChangeConfiguration
  SMethod_WindowShowMessage :: SMethod Method_WindowShowMessage
  SMethod_WindowLogMessage :: SMethod Method_WindowLogMessage
  SMethod_TelemetryEvent :: SMethod Method_TelemetryEvent
  SMethod_TextDocumentDidOpen :: SMethod Method_TextDocumentDidOpen
  SMethod_TextDocumentDidChange :: SMethod Method_TextDocumentDidChange
  SMethod_TextDocumentDidClose :: SMethod Method_TextDocumentDidClose
  SMethod_TextDocumentDidSave :: SMethod Method_TextDocumentDidSave
  SMethod_TextDocumentWillSave :: SMethod Method_TextDocumentWillSave
  SMethod_WorkspaceDidChangeWatchedFiles :: SMethod Method_WorkspaceDidChangeWatchedFiles
  SMethod_TextDocumentPublishDiagnostics :: SMethod Method_TextDocumentPublishDiagnostics
  SMethod_SetTrace :: SMethod Method_SetTrace
  SMethod_LogTrace :: SMethod Method_LogTrace
  SMethod_CancelRequest :: SMethod Method_CancelRequest
  SMethod_Progress :: SMethod Method_Progress

type SServerMethod (m :: Method ServerToClient t) = SMethod m

data MessageDirection = ServerToClient | ClientToServer

data MessageKind = Notification | Request


type ServerNotOrReq :: forall t. Method ServerToClient t -> Type
data ServerNotOrReq m where
  IsServerNot ::
    ( TMessage m ~ TNotificationMessage m
    ) =>
    ServerNotOrReq (m :: Method ServerToClient Notification)
  IsServerReq ::
    forall (m :: Method ServerToClient Request).
    ( TMessage m ~ TRequestMessage m
    ) =>
    ServerNotOrReq m

type TMessage :: forall f t. Method f t -> Type
type family TMessage m where
  TMessage (Method_CustomMethod s :: Method f t) = ()
  TMessage (m :: Method f Request) = TRequestMessage m
  TMessage (m :: Method f Notification) = TNotificationMessage m


data TNotificationMessage (m :: Method f Notification) = TNotificationMessage
  { _method :: SMethod m
  , _params :: MessageParams m
  }

data TRequestMessage (m :: Method f Request) = TRequestMessage

type MessageParams :: forall f t . Method f t -> Type
type family MessageParams (m ::  Method f t) where
  MessageParams Method_TextDocumentPublishDiagnostics = PublishDiagnosticsParams

class MonadIO m => MonadLsp config m | m -> config where
  getLspEnv :: m (LanguageContextEnv config)

instance MonadLsp config (LspT config) where
  {-# INLINE getLspEnv #-}
  getLspEnv = LspT pure


{-# INLINE splitServerMethod #-}
splitServerMethod :: SServerMethod m -> ServerNotOrReq m
splitServerMethod = \case
  SMethod_TextDocumentPublishDiagnostics -> IsServerNot
  SMethod_WindowShowMessage -> IsServerNot
  SMethod_WindowShowMessageRequest -> IsServerReq
  SMethod_WindowShowDocument -> IsServerReq
  SMethod_WindowLogMessage -> IsServerNot
  SMethod_WindowWorkDoneProgressCreate -> IsServerReq
  SMethod_Progress -> IsServerNot
  SMethod_TelemetryEvent -> IsServerNot
  SMethod_ClientRegisterCapability -> IsServerReq
  SMethod_ClientUnregisterCapability -> IsServerReq
  SMethod_WorkspaceWorkspaceFolders -> IsServerReq
  SMethod_WorkspaceConfiguration -> IsServerReq
  SMethod_WorkspaceApplyEdit -> IsServerReq
  SMethod_LogTrace -> IsServerNot
  SMethod_CancelRequest -> IsServerNot
  SMethod_WorkspaceCodeLensRefresh -> IsServerReq
  SMethod_WorkspaceSemanticTokensRefresh -> IsServerReq
  SMethod_WorkspaceInlineValueRefresh -> IsServerReq
  SMethod_WorkspaceInlayHintRefresh -> IsServerReq
  SMethod_WorkspaceDiagnosticRefresh -> IsServerReq

fromServerNot ::
  forall (m :: Method ServerToClient Notification).
  TMessage m ~ TNotificationMessage m =>
  TNotificationMessage m ->
  FromServerMessage
fromServerNot m@TNotificationMessage{_method = meth} = FromServerMess meth m


data FromServerMessage' a where
  FromServerMess :: forall t (m :: Method ServerToClient t) a. SMethod m -> TMessage m -> FromServerMessage' a
  FromServerRsp :: forall (m :: Method ClientToServer Request) a. a m -> TResponseMessage m -> FromServerMessage' a

type FromServerMessage = FromServerMessage' SMethod

data TResponseMessage (m :: Method f Request) = TResponseMessage

sendToClient :: MonadLsp config m => FromServerMessage -> m ()
sendToClient msg = do
  f <- resSendMessage <$> getLspEnv
  liftIO $ f msg
{-# INLINE sendToClient #-}
