module Rules.Compile (compilePackage) where

import Hadrian.BuildPath
import Hadrian.Oracles.TextFile

import Base
import Context as C
import Expression
import Oracles.Flag (platformSupportsSharedLibs)
import Rules.Generate
import Settings
import Target
import Utilities

import qualified Text.Parsec as Parsec

-- * Rules for building objects and Haskell interface files

compilePackage :: [(Resource, Int)] -> Rules ()
compilePackage rs = do
    root <- buildRootRules
    -- We match all file paths that look like:
    --   <root>/...stuffs.../build/...stuffs.../<something>.<suffix>
    --
    -- where:
    --   - the '...stuffs...' bits can be one or more path components,
    --   - the '<suffix>' part is a way prefix (e.g thr_p_, or nothing if
    --     vanilla) followed by an object file extension, without the dot
    --     (o, o-boot, hi, hi-boot),
    --
    -- and parse the information we need (stage, package path, ...) from
    -- the path and figure out the suitable way to produce that object file.
    alternatives $ do
      -- Language is identified by subdirectory under /build.
      -- These are non-haskell files so only have a .o or .<way>_o suffix.
      [ root -/- "**/build/c/**/*." ++ wayPat ++ "o"
        | wayPat <- wayPats] |%> compileNonHsObject rs C

      [ root -/- "**/build/cmm/**/*." ++ wayPat ++ "o"
        | wayPat <- wayPats] |%> compileNonHsObject rs Cmm

      [ root -/- "**/build/s/**/*." ++ wayPat ++ "o"
        | wayPat <- wayPats] |%> compileNonHsObject rs Asm

      [ root -/- "**/build/S/**/*." ++ wayPat ++ "o"
        | wayPat <- wayPats] |%> compileNonHsObject rs Asm

      -- All else is haskell.
      -- These come last as they overlap with the above rules' file patterns.

      -- When building dynamically we depend on the static rule if shared libs
      -- are supported, because it will add the -dynamic-too flag when
      -- compiling to build the dynamic files alongside the static files
      [ root -/- "**/build/hs/**/*.dyn_o", root -/- "**/build/hs/**/*.dyn_hi" ]
        &%> \ [dyn_o, _dyn_hi] -> do
          p <- platformSupportsSharedLibs
          if p
            then need [dyn_o -<.> "o", dyn_o -<.> "hi"]
            else compileHsObjectAndHi rs dyn_o

      forM_ ((,) <$> hsExts <*> wayPats) $ \ ((oExt, hiExt), wayPat) ->
        [ root -/- "**/build/hs/**/*." ++ wayPat ++ oExt
        , root -/- "**/build/hs/**/*." ++ wayPat ++ hiExt ]
          &%> \ [o, _hi] -> compileHsObjectAndHi rs o
  where
    hsExts = [ ("o", "hi")
             , ("o-boot", "hi-boot")
             ]
    wayPats = [ "", "*_" ]

-- * Object file paths types and parsers

{- We are using a non uniform representation that separates
   object files produced from Haskell code and from other
   languages, because the two "groups" have to be parsed
   differently enough that this would complicated the parser
   significantly.

   Indeed, non-Haskell files can only produce .o (or .thr_o, ...)
   files while Haskell modules can produce those as well as
   interface files, both in -boot or non-boot variants.

   Moreover, non-Haskell object files live under:
     <root>/stage<N>/<path/to/pkg>/build/{c,cmm,s}/

   while Haskell object/interface files live under:
     <root>/stage<N>/<path/to/pkg>/build/

   So the kind of object is partially determined by
   whether we're in c/, cmm/ or s/ but also by the
   object file's extension, in the case of a Haskell file.
   This could have been addressed with some knot-tying but
   Parsec's monad doesn't give us a MonadFix instance.

   We therefore stick to treating those two type of object
   files non uniformly.
-}

-- | Non Haskell source languages that we compile to get object files.
data SourceLang = Asm | C | Cmm deriving (Eq, Show)

parseSourceLang :: Parsec.Parsec String () SourceLang
parseSourceLang = Parsec.choice
  [ Parsec.char 'c' *> Parsec.choice
      [ Parsec.string "mm" *> pure Cmm
      , pure C
      ]
  , Parsec.char 's' *> pure Asm
  ]

type Basename = String

parseBasename :: Parsec.Parsec String () Basename
parseBasename = Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.char '.')

-- | > <c|cmm|s>/<file>.<way prefix>_o
data NonHsObject = NonHsObject SourceLang Basename Way
  deriving (Eq, Show)

parseNonHsObject :: Parsec.Parsec String () NonHsObject
parseNonHsObject = do
    lang <- parseSourceLang
    _ <- Parsec.char '/'
    file <- parseBasename
    way <- parseWayPrefix vanilla
    _ <- Parsec.char 'o'
    return (NonHsObject lang file way)

-- | > <o|hi|o-boot|hi-boot>
data SuffixType = O | Hi | OBoot | HiBoot deriving (Eq, Show)

parseSuffixType :: Parsec.Parsec String () SuffixType
parseSuffixType = Parsec.choice
  [ Parsec.char 'o' *> Parsec.choice
      [ Parsec.string "-boot" *> pure OBoot
      , pure O
      ]
  , Parsec.string "hi" *> Parsec.choice
      [ Parsec.string "-boot" *> pure HiBoot
      , pure Hi
      ]
  ]

-- | > <way prefix>_<o|hi|o-boot|hi-boot>
data Extension = Extension Way SuffixType deriving (Eq, Show)

parseExtension :: Parsec.Parsec String () Extension
parseExtension = Extension <$> parseWayPrefix vanilla <*> parseSuffixType

-- | > <file>.<way prefix>_<o|hi|o-boot|hi-boot>
data HsObject = HsObject Basename Extension deriving (Eq, Show)

parseHsObject :: Parsec.Parsec String () HsObject
parseHsObject = do
    file <- parseBasename
    ext <- parseExtension
    return (HsObject file ext)

data Object = Hs HsObject | NonHs NonHsObject deriving (Eq, Show)

parseObject :: Parsec.Parsec String () Object
parseObject = Parsec.choice
    [ NonHs <$> parseNonHsObject
    , Hs    <$> parseHsObject ]

-- * Toplevel parsers

parseBuildObject :: FilePath -> Parsec.Parsec String () (BuildPath Object)
parseBuildObject root = parseBuildPath root parseObject

-- * Getting contexts from objects

objectContext :: BuildPath Object -> Context
objectContext (BuildPath _ stage pkgPath obj) =
    Context stage (unsafeFindPackageByPath pkgPath) way
  where
    way = case obj of
        NonHs (NonHsObject _lang _file w)         -> w
        Hs    (HsObject _file (Extension w _suf)) -> w

-- * Building an object

compileHsObjectAndHi
    :: [(Resource, Int)] -> FilePath -> Action ()
compileHsObjectAndHi rs objpath = do
  root <- buildRoot
  b@(BuildPath _root stage _path _o)
    <- parsePath (parseBuildObject root) "<object file path parser>" objpath
  let ctx = objectContext b
      way = C.way ctx
  ctxPath <- contextPath ctx
  (src, deps) <- lookupDependencies (ctxPath -/- ".dependencies") objpath
  need (src:deps)

  -- The .dependencies file lists indicating inputs. ghc will
  -- generally read more *.hi and *.hi-boot files (direct inputs).
  -- Allow such reads (see https://gitlab.haskell.org/ghc/ghc/wikis/Developing-Hadrian#haskell-object-files-and-hi-inputs)
  -- Note that this may allow too many *.hi and *.hi-boot files, but
  -- calculating the exact set of direct inputs is not feasible.
  trackAllow [ "**/*." ++ hisuf     way
             , "**/*." ++ hibootsuf way
             ]

  buildWithResources rs $ target ctx (Ghc CompileHs stage) [src] [objpath]

compileNonHsObject :: [(Resource, Int)] -> SourceLang -> FilePath -> Action ()
compileNonHsObject rs lang path = do
  root <- buildRoot
  b@(BuildPath _root stage _path _o)
    <- parsePath (parseBuildObject root) "<object file path parser>" path
  let
    ctx = objectContext b
    builder = case lang of
      C -> Ghc CompileCWithGhc
      _ -> Ghc CompileHs
  src <- case lang of
      Asm -> obj2src "S"   (const False)      ctx path
      C   -> obj2src "c"   (const False)      ctx path
      Cmm -> obj2src "cmm" isGeneratedCmmFile ctx path
  need [src]
  needDependencies ctx src (path <.> "d")
  buildWithResources rs $ target ctx (builder stage) [src] [path]

-- * Helpers

-- | Discover dependencies of a given source file by iteratively calling @gcc@
-- in the @-MM -MG@ mode and building generated dependencies if they are missing
-- until reaching a fixed point.
needDependencies :: Context -> FilePath -> FilePath -> Action ()
needDependencies context@Context {..} src depFile = discover
  where
    discover = do
        build $ target context (Cc FindCDependencies stage) [src] [depFile]
        deps <- parseFile depFile
        -- Generated dependencies, if not yet built, will not be found and hence
        -- will be referred to simply by their file names.
        let notFound = filter (\file -> file == takeFileName file) deps
        -- We find the full paths to generated dependencies, so we can request
        -- to build them by calling 'need'.
        todo <- catMaybes <$> mapM (fullPathIfGenerated context) notFound

        if null todo
        then need deps -- The list of dependencies is final, need all
        else do
            need todo  -- Build newly discovered generated dependencies
            discover   -- Continue the discovery process

    parseFile :: FilePath -> Action [String]
    parseFile file = do
        input <- liftIO $ readFile file
        case parseMakefile input of
            [(_file, deps)] -> return deps
            _               -> return []

-- | Find a given 'FilePath' in the list of generated files in the given
-- 'Context' and return its full path.
fullPathIfGenerated :: Context -> FilePath -> Action (Maybe FilePath)
fullPathIfGenerated context file = interpretInContext context $ do
    generated <- generatedDependencies
    return $ find ((== file) . takeFileName) generated

obj2src :: String -> (FilePath -> Bool) -> Context -> FilePath -> Action FilePath
obj2src extension isGenerated context@Context {..} obj
    | isGenerated src = return src
    | otherwise       = (pkgPath package ++) <$> suffix
  where
    src    = obj -<.> extension
    suffix = do
        path <- buildPath context
        return $ fromMaybe ("Cannot determine source for " ++ obj)
               $ stripPrefix (path -/- extension) src
