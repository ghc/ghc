module Test20239 where

-- | Leading Haddock Comment
data instance Method PGMigration = MigrationQuery Query
                                 -- ^ Run a query against the database
                                 | MigrationCode (Connection -> IO (Either String ()))
                                 -- ^ Run any arbitrary IO code
