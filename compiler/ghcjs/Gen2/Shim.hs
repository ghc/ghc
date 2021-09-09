{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{- |
  Shims are non-Haskell dependencies, organized in the
  shims repository. Most of the GHCJS RTS is located there
  too (except the generated parts in "Gen2.Rts" and
  "Gen2.RtsApply").

  example shim yaml:

version:
js:
 - src/edefw.js
 - src/b.js
 - sub:
    - version: [ 0.5.3 , 0.8 )
      js:
        - js1
        - js2
    - version: 0.8.2 ..
      js:
        - js1
        - js2

-}

module Gen2.Shim where

import           DynFlags
import           ToolSettings
import qualified FileCleanup
import Prelude

-- import           Control.Lens hiding ((<.>))
import           Compiler.JMacro.Lens
import           Control.Monad

import           Data.Function ((&))

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Foldable   as F
import qualified Data.List       as L
import           Data.Maybe (catMaybes)
import qualified Data.Set        as S
import qualified Data.Text       as T
import           Data.Text (Text)
import           Data.Aeson
-- import qualified Data.Yaml       as Yaml

import           System.FilePath ((<.>), (</>), isExtensionOf, takeExtensions)
import           System.Directory (doesFileExist, canonicalizePath)

import           Text.Parsec
import           Text.Parsec.Text ()

import           Compiler.Compat
import qualified Compiler.Utils  as Utils

-- import qualified Gen2.Archive    as Ar
import qualified Ar as Ar

type Pkg = Text

data Shim = Shim { versionRange   :: VersionRange
                 , dependencies   :: [FilePath]
                 , subs           :: [Shim]
                 } deriving (Eq, Ord, Show)

data VersionRange = SingleVersion Version
                  | Interval (Maybe Version) (Maybe Version)
             deriving (Eq, Ord, Show)

emptyRange :: VersionRange
emptyRange = Interval Nothing Nothing

inRange :: Version -> Shim -> Bool
inRange ver shim = case versionRange shim of
                     Interval lo hi  -> maybe True (<=ver) lo && maybe True (> ver) hi
                     SingleVersion v -> ver == v

emptyShim :: Shim
emptyShim = Shim emptyRange [] []

instance FromJSON Shim where
  parseJSON (Object v) =
    Shim <$> v .:? "version" .!= (Interval Nothing Nothing)
         <*> v .:? "js"      .!= []
         <*> v .:? "subs"    .!= []
  parseJSON (Array xs) = Shim emptyRange [] <$> (mapM parseJSON $ F.toList xs)
  parseJSON _ = mempty

instance FromJSON VersionRange where
  parseJSON (String t) = maybe mempty pure (parseVersionRange t)
  parseJSON _          = mempty

collectShims :: FilePath                                    -- ^ the base path
             -> [(Text, Version)]                           -- ^ packages being linked
             -> IO ([FilePath], [FilePath])                 -- ^ files to load (with, after) rts
collectShims base pkgs = do
  filesR <- f =<< mapM (collectShim base) (pkgsRts & traverse . _1 %~ T.tail)
  filesA <- f =<< mapM (collectShim base) pkgs
  return (filesR, filter (`notElem`filesR) filesA)
    where
      f = fmap uniq . mapM (canonicalizePath . (base </>)) . concat
      (pkgsRts, _pkgsAfterRts) = L.partition (("@rts" `T.isPrefixOf`).fst) pkgs
      uniq xs = let go (x:xs) s
                      | x `S.notMember` s = x : go xs (S.insert x s)
                      | otherwise         = go xs s
                    go [] _ = []
                in go xs mempty

tryReadShimFile :: DynFlags -> FilePath -> IO B.ByteString
tryReadShimFile dflags file = do
  -- exists <- doesFileExist file
  -- if not exists
  --  then panic ("warning: " <> file <> " does not exist") -- fixme how to raise a proper error here? -- >> return mempty
  --  else do
      let s = toolSettings dflags
          s1 = s { toolSettings_pgm_P =
                      ( fst (toolSettings_pgm_P s)
                      , jsCppPgmOpts (snd $ toolSettings_pgm_P s))
                  , toolSettings_opt_P = jsCppOpts (toolSettings_opt_P s)
                  }
          dflags1 = dflags { toolSettings = s1 }
      if needsCpp file
      then do
        outfile <- FileCleanup.newTempName dflags FileCleanup.TFL_CurrentModule "jspp"
        Utils.doCpp dflags1 True file outfile
        B.readFile outfile
      else B.readFile file

{-
   running the C preprocessor on JS files is a bit fragile
   and breaks in some situations. Therefore we only preprocess
   files with .pp extension, for example lib.js.pp
 -}
needsCpp :: FilePath -> Bool
needsCpp file = "pp" `isExtensionOf` file

-- suppress line numbers and enable extended syntax
jsCppPgmOpts :: [Option] -> [Option]
jsCppPgmOpts opts = Option "-P" : Option "-CC" : filter (/=(Option "-traditional")) opts

jsCppOpts :: [String] -> [String]
jsCppOpts opts = filter (/="-traditional") (removeCabalMacros opts)
  where
    -- options are in reverse order
    removeCabalMacros (x1:x2:xs)
      | x2 == "-include" && "cabal_macros.h" `L.isSuffixOf` x1 =
          removeCabalMacros xs
    removeCabalMacros (x:xs) = x : removeCabalMacros xs
    removeCabalMacros []     = []

readShimsArchive :: DynFlags -> FilePath -> IO B.ByteString
readShimsArchive dflags archive = do
  Ar.Archive entries <- Ar.loadAr archive
  let s       = toolSettings dflags
      s1      = s { toolSettings_pgm_P = 
                        ( fst (toolSettings_pgm_P s)
                        , jsCppPgmOpts (snd $ toolSettings_pgm_P s))
                  , toolSettings_opt_P = jsCppOpts (toolSettings_opt_P s)
                  }
      dflags1 = dflags { toolSettings = s1 }
  srcs' <- forM (filter isJsSourceEntry entries) $ \e ->
    if needsCpp (Ar.filename e)
    then do
      infile  <- FileCleanup.newTempName dflags FileCleanup.TFL_CurrentModule "jspp"
      outfile <- FileCleanup.newTempName dflags FileCleanup.TFL_CurrentModule "jspp"
      B.writeFile infile (Ar.filedata e)
      Utils.doCpp dflags1 True infile outfile
      B.readFile outfile
    else pure (Ar.filedata e)
  return (mconcat $ map (<> "\n") srcs')

isJsSourceEntry :: Ar.ArchiveEntry -> Bool
isJsSourceEntry entry = takeExtensions (Ar.filename entry) `elem`
                        [".js", ".js.pp", ".mjs", ".mjs.pp"]

readShim :: FilePath -> (Pkg, Version) -> IO (Maybe Shim)
readShim base (pkgName, _pkgVer) = do
  checkShimsInstallation base
  let configFile = base </> T.unpack pkgName <.> "json"
  e <- doesFileExist configFile
  if e then do
         cfg <- B.readFile configFile
         case eitherDecodeStrict' cfg of
           Left err -> do
             putStrLn ("invalid shim config: " ++ configFile ++ "\n   " ++ err)
             return Nothing
           Right shim -> return shim
       else return Nothing

collectShim :: FilePath -> (Pkg, Version) -> IO [FilePath]
collectShim base (pkgName, pkgVer) = do
  mbShim <- readShim base (pkgName, pkgVer)
  case mbShim of
    Just shim -> return (foldShim pkgName pkgVer shim)
    Nothing   -> return mempty

checkShimsInstallation :: FilePath -> IO ()
checkShimsInstallation base = do
  e <- doesFileExist (base </> "rts.json")
  unless e (error $ "Shims repository not found in `" ++ base ++ "'.")

foldShim :: Pkg -> Version -> Shim -> [FilePath]
foldShim pkg ver sh
  | inRange ver sh || isEmptyVersion ver =
      dependencies sh ++ concatMap (foldShim pkg ver) (subs sh)
  | otherwise = []

parseVersion :: Text -> Maybe Version
parseVersion tv = either (const Nothing) Just $ parse version "" (T.strip tv)

version :: Stream s m Char => ParsecT s u m Version
version = Version <$> integer `sepBy1` char '.'

integer :: Stream s m Char => ParsecT s u m Integer
integer = read <$> many1 digit

parseVersionRange :: Text -> Maybe VersionRange
parseVersionRange tv = either (const Nothing) Just $
                        parse versionBounds "" (T.strip tv)
  where
    versionBounds = versionRange <|>
                    openRange1 <|>
                    try versionWildcard <|>
                    try openRange2 <|>
                    singleVersion

    -- 1.3.2
    singleVersion = SingleVersion <$> version

    -- 1.3.*
    versionWildcard = wildcardRange <$> (version <* string ".*")

    -- [ 1.1 , 1.4 )
    versionRange = do
      char '[' >> spaces
      start <- version
      spaces >> char ',' >> spaces
      end <- version
      spaces >> void (char ')')
      return (Interval (Just start) (Just end))

    -- .. 1.4
    openRange1 = Interval Nothing . Just <$>
                   (string ".." *> spaces *> version)
    -- 1.2 ..
    openRange2 = (\x -> Interval (Just x) Nothing) <$>
                    (version <* spaces <* string "..")

-- 1.2.* -> (Interval (Just [1,2]) (Just [1,3]))
wildcardRange :: Version -> VersionRange
wildcardRange v@(Version xs)
  | (y:ys) <- reverse xs = Interval (Just v)
                                    (Just . Version . reverse $ (y+1):ys)
  | otherwise = SingleVersion (Version [])

versionRangeToBuildDep :: VersionRange -> Text
versionRangeToBuildDep (SingleVersion ver) = "== " <> showVersion ver
versionRangeToBuildDep (Interval lo hi) = T.intercalate " && " $ catMaybes
                                                    [((">= " <>) . showVersion) <$> lo,
                                                     (("< "  <>) . showVersion) <$> hi]
