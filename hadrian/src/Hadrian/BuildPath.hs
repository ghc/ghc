module Hadrian.BuildPath where

import Base

import Data.Functor
import qualified Text.Parsec as Parsec

-- | A path of the form
--
-- > <build root>/stage<N>/<path/to/pkg/from/ghc/root>/build/<something>
--
-- where @something@ describes a library or object file or ... to be built
-- for the given package.
--
-- @a@, which represents that @something@, is instantiated with library-related
-- data types in @Rules.Library@ and with object/interface files related types
-- in @Rules.Compile@.
data BuildPath a = BuildPath
  { _buildPathRoot :: FilePath    -- ^ @<build root>/@
  , _buildPathStage :: Stage      -- ^ @stage<N>/@
  , _buildPathPkgPath :: FilePath -- ^ @<path/to/pkg/from/ghc/root>/build/@
  , _buildPathTarget :: a         -- ^ whatever comes after @build/@
  } deriving (Eq, Show)

-- | Parse a build path under the given build root.
parseBuildPath
    :: FilePath -- ^ build root
    -> Parsec.Parsec String () a -- ^ what to parse after @build/@
    -> Parsec.Parsec String () (BuildPath a)
parseBuildPath root afterBuild = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    pkgpath <- Parsec.manyTill Parsec.anyChar
        (Parsec.try $ Parsec.string "/build/")
    a <- afterBuild
    return (BuildPath root stage pkgpath a)

-- | A path of the form
--
-- > <build root>/stage<N>/lib/<arch>-<os>-ghc-<ghc version>/<something>
--
-- where @something@ describes a library or object file or ... to be registered
-- for the given package. These are files registered into a ghc-pkg database.
--
-- @a@, which represents that @something@, is instantiated with library-related
-- data types in @Rules.Library@ and with object/interface files related types
-- in @Rules.Compile@.
data GhcPkgPath a = GhcPkgPath
   { _ghcpkgPathRoot  :: FilePath -- ^ @<build root>/@
   , _ghcpkgPathStage :: Stage    -- ^ @stage<N>/@
   , _ghcpkgRegPath   :: FilePath -- ^ @lib/<arch>-<os>-ghc-<ghc version>/@
   , _ghcPkgObject    :: a        -- ^ whatever comes after
   } deriving (Eq, Show)

-- | Parse a registered ghc-pkg path under the given build root.
parseGhcPkgPath
    :: FilePath -- ^ build root
    -> Parsec.Parsec String () a -- ^ what to parse after @build/@
    -> Parsec.Parsec String () (GhcPkgPath a)
parseGhcPkgPath root after = do
    _ <- Parsec.string root *> Parsec.optional (Parsec.char '/')
    stage <- parseStage
    _ <- Parsec.char '/'
    regPath <- Parsec.string "lib/"
            <> Parsec.manyTill Parsec.anyChar (Parsec.try $ Parsec.string "/")
    a <- after
    return (GhcPkgPath root stage regPath a)



-- To be kept in sync with Stage.hs's stageString function
-- | Parse @"stageX"@ into a 'Stage'.
parseStage :: Parsec.Parsec String () Stage
parseStage = (Parsec.string "stage" *> Parsec.choice
    [ Parsec.string (show n) $> toEnum n
    | n <- map fromEnum [minBound .. maxBound :: Stage]
    ]) Parsec.<?> "stage string"

-- To be kept in sync with the show instances in 'Way.Type', until we perhaps
-- use some bidirectional parsing/pretty printing approach or library.
-- | Parse a way suffix, returning the argument when no suffix is found (the
-- argument will be vanilla in most cases, but dynamic when we parse the way
-- suffix out of a shared library file name).
parseWaySuffix :: Way -> Parsec.Parsec String () Way
parseWaySuffix w = Parsec.choice
    [ Parsec.char '_' *>
        (wayFromUnits <$> Parsec.sepBy1 parseWayUnit (Parsec.char '_'))
    , pure w
    ] Parsec.<?> "way suffix (e.g _thr_p, or none for vanilla)"

-- | Same as 'parseWaySuffix', but for parsing e.g @thr_p_@
--   instead of @_thr_p@, like 'parseWaySuffix' does.
--
--   This is used to parse paths to object files,
--   in Rules.Compile.
parseWayPrefix :: Way -> Parsec.Parsec String () Way
parseWayPrefix w = Parsec.choice
    [ wayFromUnits <$> Parsec.endBy1 parseWayUnit (Parsec.char '_')
    , pure w
    ] Parsec.<?> "way prefix (e.g thr_p_, or none for vanilla)"

parseWayUnit :: Parsec.Parsec String () WayUnit
parseWayUnit = Parsec.choice
    [ Parsec.string "thr" *> pure Threaded
    , Parsec.char   'd'   *>
      (Parsec.choice [ Parsec.string "ebug" *> pure Debug
                     , Parsec.string "yn"   *> pure Dynamic ])
    , Parsec.char 'p'     *> pure Profiling
    , Parsec.char 'l'     *> pure Logging
    ] Parsec.<?> "way unit (thr, debug, dyn, p, l)"

-- | Parse a @"pkgname-pkgversion"@ string into the package name and the
-- integers that make up the package version.
parsePkgId :: Parsec.Parsec String () (String, [Integer])
parsePkgId = parsePkgId' "" Parsec.<?> "package identifier (<name>-<version>)"
  where
    parsePkgId' currName = do
        s <- Parsec.many1 Parsec.alphaNum
        _ <- Parsec.char '-'
        let newName = if null currName then s else currName ++ "-" ++ s
        Parsec.choice [ (newName,) <$> parsePkgVersion
                      , parsePkgId' newName ]

-- | Parse "."-separated integers that describe a package's version.
parsePkgVersion :: Parsec.Parsec String () [Integer]
parsePkgVersion = fmap reverse (parsePkgVersion' [])
       Parsec.<?> "package version"
  where
    parsePkgVersion' xs = do
        n <- parseNatural
        Parsec.choice
            [ Parsec.try
                  (Parsec.lookAhead (Parsec.char '.' *>
                                        (Parsec.letter <|> Parsec.char '_')
                                    )
                  )
              $> (n:xs)
            , Parsec.char '.' *> parsePkgVersion' (n:xs)
            , pure $ (n:xs) ]

-- | Parse a natural number.
parseNatural :: Parsec.Parsec String () Integer
parseNatural = (read <$> Parsec.many1 Parsec.digit) Parsec.<?> "natural number"

-- | Runs the given parser against the given path, erroring out when the parser
-- fails (because it shouldn't if the code from this module is correct).
parsePath
    :: Parsec.Parsec String () a -- ^ parser to run
    -> String                    -- ^ string describing the input source
    -> FilePath                  -- ^ path to parse
    -> Action a
parsePath p inp path = case Parsec.parse p inp path of
    Left err -> fail $ "Hadrian.BuildPath.parsePath: path="
                    ++ path ++ ", error:\n" ++ show err
    Right a  -> pure a
