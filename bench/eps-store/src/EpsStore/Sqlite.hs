{-# LANGUAGE OverloadedStrings #-}

-- | A 'DeclStore' in an SQLite database: every declaration is serialised (with
-- GHC's interface-file encoding, including its own name and string tables)
-- into a row.
module EpsStore.Sqlite
  ( newSqliteStore
  ) where

import EpsStore.DeclStore

import GHC.Iface.Binary ( CompressionIFace (..), TraceBinIFace (..)
                        , getWithUserData, putWithUserData )
import GHC.Iface.Syntax ( IfaceDecl )
import GHC.Types.Name.Cache ( NameCache )
import GHC.Utils.Binary ( openBinMem, withBinBuffer, unsafeUnpackBinBuffer )

import Control.Concurrent.MVar
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Database.SQLite3 as Sql
import Database.SQLite3 ( SQLData (..), StepResult (..) )

-- | Open (and create, or empty) the database at the given path. The 'NameCache'
-- must be the one of the session, so that deserialised declarations refer to
-- the same 'GHC.Types.Name.Name's.
newSqliteStore :: NameCache -> FilePath -> IO DeclStore
newSqliteStore nc path = do
  db <- Sql.open (T.pack path)
  -- It is a cache for one session: durability doesn't matter.
  Sql.exec db "PRAGMA journal_mode = OFF; PRAGMA synchronous = OFF;\
              \DROP TABLE IF EXISTS decls;\
              \CREATE TABLE decls (id INTEGER PRIMARY KEY, payload BLOB NOT NULL);"
  ins <- Sql.prepare db "INSERT INTO decls (payload) VALUES (?1)"
  sel <- Sql.prepare db "SELECT payload FROM decls WHERE id = ?1"
  -- One connection, used by one thread at a time.
  lock <- newMVar ()
  let with_db :: IO a -> IO a
      with_db act = withMVar lock (const act)

      put _mod _loc decls = do
        -- Serialise outside the lock: it forces the lazily read parts of the
        -- declarations, which may take a while.
        blobs <- mapM serialise decls
        with_db $ do
          Sql.exec db "BEGIN"
          keys <- forM blobs $ \blob -> do
            Sql.bindBlob ins 1 blob
            _ <- Sql.step ins
            Sql.reset ins
            fromIntegral <$> Sql.lastInsertRowId db
          Sql.exec db "COMMIT"
          pure keys

      get key = do
        blob <- with_db $ do
          Sql.bindInt64 sel 1 (fromIntegral key)
          r <- Sql.step sel
          res <- case r of
            Row  -> Sql.column sel 0
            Done -> pure SQLNull
          Sql.reset sel
          pure res
        case blob of
          SQLBlob bs -> deserialise bs
          _ -> fail ("EpsStore.Sqlite: no declaration with key " ++ show key)

  pure DeclStore
    { ds_name  = "sqlite:" ++ path
    , ds_put   = put
    , ds_get   = get
    , ds_close = with_db $ do
        Sql.finalize ins
        Sql.finalize sel
        Sql.close db
    }
  where
    serialise :: IfaceDecl -> IO BS.ByteString
    serialise decl = do
      bh <- openBinMem (16 * 1024)
      putWithUserData QuietBinIFace NormalCompression bh decl
      withBinBuffer bh (pure . BS.copy)

    deserialise :: BS.ByteString -> IO IfaceDecl
    deserialise bs = do
      bh <- unsafeUnpackBinBuffer bs
      getWithUserData nc bh
