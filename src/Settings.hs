{-# LANGUAGE FlexibleInstances #-}

module Settings (
    IntegerLibrary (..), integerLibrary, integerLibraryName,
    buildHaddock
    ) where

import Base
import Expression

data IntegerLibrary = IntegerGmp | IntegerGmp2 | IntegerSimple

integerLibrary :: IntegerLibrary
integerLibrary = IntegerGmp2

integerLibraryName :: String
integerLibraryName = case integerLibrary of
    IntegerGmp    -> "integer-gmp"
    IntegerGmp2   -> "integer-gmp2"
    IntegerSimple -> "integer-simple"

buildHaddock :: Bool
buildHaddock = True

supportsPackageKey :: Guard
supportsPackageKey = keyYes "supports-package-key"

whenPackageKey :: Guard
whenPackageKey = supportsPackageKey <> notStage Stage0

depSettings :: Settings
depSettings =
    opts ["-hide-all-packages", "-no-user-package-db", "-include-pkg-deps"]
    <>
    stage Stage0 ? opts ["-package-db libraries/bootstrapping.conf"]
    <>
    whenPackageKey ?
        (packageKey "-this-package-key" <> packageDepKeys "-package-key")
    <>
    (Not $ whenPackageKey) ?
        (packageKey "-package-name" <> packageDeps "-package")

--packageArgs :: Stage -> FilePath -> Args
--packageArgs stage pathDist = do
--    usePackageKey <- SupportsPackageKey || stage /= Stage0
--    args [ arg "-hide-all-packages"
--         , arg "-no-user-package-db"
--         , arg "-include-pkg-deps"
--         , when (stage == Stage0) $
--           arg "-package-db libraries/bootstrapping.conf"
--         , if usePackageKey
--           then productArgs ["-this-package-key"] [arg  $ PackageKey pathDist]
--             <> productArgs ["-package-key"     ] [args $ DepKeys    pathDist]
--           else productArgs ["-package-name"    ] [arg  $ PackageKey pathDist]
--             <> productArgs ["-package"         ] [args $ Deps       pathDist]
--         ]
