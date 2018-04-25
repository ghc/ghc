{-# LANGUAGE DeriveGeneric #-}

--TODO: [code cleanup] plausibly much of this module should be merged with
-- similar functionality in Cabal.
module Distribution.Client.Glob
    ( FilePathGlob(..)
    , FilePathRoot(..)
    , FilePathGlobRel(..)
    , Glob
    , GlobPiece(..)
    , matchFileGlob
    , matchFileGlobRel
    , matchGlob
    , isTrivialFilePathGlob
    , getFilePathRootDirectory
    ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import           Data.List (stripPrefix)
import           Control.Monad (mapM)

import           Distribution.Text
import           Distribution.Compat.ReadP (ReadP, (<++), (+++))
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp

import           System.FilePath
import           System.Directory


-- | A file path specified by globbing
--
data FilePathGlob = FilePathGlob FilePathRoot FilePathGlobRel
  deriving (Eq, Show, Generic)

data FilePathGlobRel
   = GlobDir  !Glob !FilePathGlobRel
   | GlobFile !Glob
   | GlobDirTrailing                -- ^ trailing dir, a glob ending in @/@
  deriving (Eq, Show, Generic)

-- | A single directory or file component of a globbed path
type Glob = [GlobPiece]

-- | A piece of a globbing pattern
data GlobPiece = WildCard
               | Literal String
               | Union [Glob]
  deriving (Eq, Show, Generic)

data FilePathRoot
   = FilePathRelative
   | FilePathRoot FilePath -- ^ e.g. @"/"@, @"c:\"@ or result of 'takeDrive'
   | FilePathHomeDir
  deriving (Eq, Show, Generic)

instance Binary FilePathGlob
instance Binary FilePathRoot
instance Binary FilePathGlobRel
instance Binary GlobPiece


-- | Check if a 'FilePathGlob' doesn't actually make use of any globbing and
-- is in fact equivalent to a non-glob 'FilePath'.
--
-- If it is trivial in this sense then the result is the equivalent constant
-- 'FilePath'. On the other hand if it is not trivial (so could in principle
-- match more than one file) then the result is @Nothing@.
--
isTrivialFilePathGlob :: FilePathGlob -> Maybe FilePath
isTrivialFilePathGlob (FilePathGlob root pathglob) =
    case root of
      FilePathRelative       -> go []      pathglob
      FilePathRoot root'     -> go [root'] pathglob
      FilePathHomeDir        -> Nothing
  where
    go paths (GlobDir  [Literal path] globs) = go (path:paths) globs
    go paths (GlobFile [Literal path]) = Just (joinPath (reverse (path:paths)))
    go paths  GlobDirTrailing          = Just (addTrailingPathSeparator
                                                 (joinPath (reverse paths)))
    go _ _ = Nothing

-- | Get the 'FilePath' corresponding to a 'FilePathRoot'.
--
-- The 'FilePath' argument is required to supply the path for the
-- 'FilePathRelative' case.
--
getFilePathRootDirectory :: FilePathRoot
                         -> FilePath      -- ^ root for relative paths
                         -> IO FilePath
getFilePathRootDirectory  FilePathRelative   root = return root
getFilePathRootDirectory (FilePathRoot root) _    = return root
getFilePathRootDirectory  FilePathHomeDir    _    = getHomeDirectory


------------------------------------------------------------------------------
-- Matching
--

-- | Match a 'FilePathGlob' against the file system, starting from a given
-- root directory for relative paths. The results of relative globs are
-- relative to the given root. Matches for absolute globs are absolute.
--
matchFileGlob :: FilePath -> FilePathGlob -> IO [FilePath]
matchFileGlob relroot (FilePathGlob globroot glob) = do
    root <- getFilePathRootDirectory globroot relroot
    matches <- matchFileGlobRel root glob
    case globroot of
      FilePathRelative -> return matches
      _                -> return (map (root </>) matches)

-- | Match a 'FilePathGlobRel' against the file system, starting from a
-- given root directory. The results are all relative to the given root.
--
matchFileGlobRel :: FilePath -> FilePathGlobRel -> IO [FilePath]
matchFileGlobRel root glob0 = go glob0 ""
  where
    go (GlobFile glob) dir = do
      entries <- getDirectoryContents (root </> dir)
      let files = filter (matchGlob glob) entries
      return (map (dir </>) files)

    go (GlobDir glob globPath) dir = do
      entries <- getDirectoryContents (root </> dir)
      subdirs <- filterM (\subdir -> doesDirectoryExist
                                       (root </> dir </> subdir))
               $ filter (matchGlob glob) entries
      concat <$> mapM (\subdir -> go globPath (dir </> subdir)) subdirs

    go GlobDirTrailing dir = return [dir]


-- | Match a globbing pattern against a file path component
--
matchGlob :: Glob -> String -> Bool
matchGlob = goStart
  where
    -- From the man page, glob(7):
    --   "If a filename starts with a '.', this character must be
    --    matched explicitly."

    go, goStart :: [GlobPiece] -> String -> Bool

    goStart (WildCard:_) ('.':_)  = False
    goStart (Union globs:rest) cs = any (\glob -> goStart (glob ++ rest) cs)
                                        globs
    goStart rest               cs = go rest cs

    go []                 ""    = True
    go (Literal lit:rest) cs
      | Just cs' <- stripPrefix lit cs
                                = go rest cs'
      | otherwise               = False
    go [WildCard]         ""    = True
    go (WildCard:rest)   (c:cs) = go rest (c:cs) || go (WildCard:rest) cs
    go (Union globs:rest)   cs  = any (\glob -> go (glob ++ rest) cs) globs
    go []                (_:_)  = False
    go (_:_)              ""    = False


------------------------------------------------------------------------------
-- Parsing & printing
--

instance Text FilePathGlob where
  disp (FilePathGlob root pathglob) = disp root Disp.<> disp pathglob
  parse =
    parse >>= \root ->
        (FilePathGlob root <$> parse)
    <++ (when (root == FilePathRelative) Parse.pfail >>
         return (FilePathGlob root GlobDirTrailing))

instance Text FilePathRoot where
  disp  FilePathRelative    = Disp.empty
  disp (FilePathRoot root)  = Disp.text root
  disp FilePathHomeDir      = Disp.char '~' Disp.<> Disp.char '/'

  parse =
        (     (Parse.char '/' >> return (FilePathRoot "/"))
          +++ (Parse.char '~' >> Parse.char '/' >> return FilePathHomeDir)
          +++ (do drive <- Parse.satisfy (\c -> (c >= 'a' && c <= 'z')
                                             || (c >= 'A' && c <= 'Z'))
                  _ <- Parse.char ':'
                  _ <- Parse.char '/' +++ Parse.char '\\'
                  return (FilePathRoot (toUpper drive : ":\\")))
        )
    <++ return FilePathRelative

instance Text FilePathGlobRel where
  disp (GlobDir  glob pathglob) = dispGlob glob
                          Disp.<> Disp.char '/'
                          Disp.<> disp pathglob
  disp (GlobFile glob)          = dispGlob glob
  disp  GlobDirTrailing         = Disp.empty

  parse = parsePath
    where
      parsePath :: ReadP r FilePathGlobRel
      parsePath =
        parseGlob >>= \globpieces ->
            asDir globpieces
        <++ asTDir globpieces
        <++ asFile globpieces

      asDir  glob = do dirSep
                       globs <- parsePath
                       return (GlobDir glob globs)
      asTDir glob = do dirSep
                       return (GlobDir glob GlobDirTrailing)
      asFile glob = return (GlobFile glob)

      dirSep = (Parse.char '/' >> return ())
           +++ (do _ <- Parse.char '\\'
                   -- check this isn't an escape code
                   following <- Parse.look
                   case following of
                     (c:_) | isGlobEscapedChar c -> Parse.pfail
                     _                           -> return ())


dispGlob :: Glob -> Disp.Doc
dispGlob = Disp.hcat . map dispPiece
  where
    dispPiece WildCard      = Disp.char '*'
    dispPiece (Literal str) = Disp.text (escape str)
    dispPiece (Union globs) = Disp.braces
                                (Disp.hcat (Disp.punctuate
                                             (Disp.char ',')
                                             (map dispGlob globs)))
    escape []               = []
    escape (c:cs)
      | isGlobEscapedChar c = '\\' : c : escape cs
      | otherwise           =        c : escape cs

parseGlob :: ReadP r Glob
parseGlob = Parse.many1 parsePiece
  where
    parsePiece = literal +++ wildcard +++ union

    wildcard = Parse.char '*' >> return WildCard

    union = Parse.between (Parse.char '{') (Parse.char '}') $
              fmap Union (Parse.sepBy1 parseGlob (Parse.char ','))

    literal = Literal `fmap` litchars1

    litchar = normal +++ escape

    normal  = Parse.satisfy (\c -> not (isGlobEscapedChar c)
                                && c /= '/' && c /= '\\')
    escape  = Parse.char '\\' >> Parse.satisfy isGlobEscapedChar

    litchars1 :: ReadP r [Char]
    litchars1 = liftM2 (:) litchar litchars

    litchars :: ReadP r [Char]
    litchars = litchars1 <++ return []

isGlobEscapedChar :: Char -> Bool
isGlobEscapedChar '*'  = True
isGlobEscapedChar '{'  = True
isGlobEscapedChar '}'  = True
isGlobEscapedChar ','  = True
isGlobEscapedChar _    = False
