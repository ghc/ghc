{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Linters.Common where

-- base
import           Control.Monad
  ( liftM, unless )
import           Data.Function
  ( on )
import           Data.List
  ( groupBy )
import           Data.Maybe
  ( fromMaybe )
import           GHC.IO.Encoding
  ( utf8, setLocaleEncoding, getLocaleEncoding, textEncodingName )

-- deepseq
import           Control.DeepSeq
  ( NFData(rnf), force, ($!!) )

-- process
import System.Process
  ( readProcess )

-- text
import           Data.Text
  ( Text )
import qualified Data.Text as T

--------------------------------------------------------------------------------

data LintMsg = LintMsg !LintLvl !Int !Text !Text
              deriving stock Show

data LintLvl = LintLvlWarn | LintLvlErr
              deriving stock ( Show, Eq, Ord )

type GitRef = Text

type Sh = IO

silently :: a -> a
silently = id

runGit :: FilePath -> Text -> [Text] -> Sh Text
runGit fp t ts = runGitStdin fp t ts ""

-- | Run @git@ operation
runGitStdin :: FilePath -> Text -> [Text] -> Text -> Sh Text
runGitStdin d op args std_in = do
    d' <- return $ T.pack d
    out <- withUtf8 $ silently $ readProcess "git" (map T.unpack ("-C" : d' : op : args)) (T.unpack std_in)
    return (T.pack out)

-- | WARNING: non-reentrant Hack!
withUtf8 :: Sh a -> Sh a
withUtf8 act = do
    oldloc <- getLocaleEncoding
    if (textEncodingName oldloc == textEncodingName utf8)
    then act
    else do
        setLocaleEncoding utf8
        r <- act
        setLocaleEncoding oldloc
        return r

-- | wrapper around @git cat-file commit@
--
-- Returns (commit-header, commit-body)
gitCatCommit :: FilePath -> GitRef -> Sh (Text,Text)
gitCatCommit d ref = do
    tmp <- runGit d "cat-file" ["commit", ref ]
    return (fmap (T.drop 2) $ T.breakOn "\n\n" tmp)

-- | wrapper around @git cat-file commit@
gitCatBlob :: FilePath -> GitRef -> Sh Text
gitCatBlob d ref = do
    tmpl <- liftM tread $ runGit d "cat-file" ["-s", ref] -- workaround shelly adding EOLs
    tmp <- runGit d "cat-file" ["blob", ref]
    return (T.take tmpl tmp)
  where
    tread = read . T.unpack

-- | Wrapper around @git rev-parse --verify@
--
-- Normalise git ref to commit sha1
gitNormCid :: FilePath -> GitRef -> Sh GitRef
gitNormCid d ref = do
    tmp <- runGit d "rev-parse" ["-q", "--verify", ref <> "^{commit}" ]
    return (T.strip tmp)

-- | wrapper around @git branch --contains@
gitBranchesContain :: FilePath -> GitRef -> Sh [Text]
gitBranchesContain d ref = do
    tmp <- liftM T.lines $
           --errExit False $ print_stderr False $
           runGit d "branch" ["--contains", ref, "-r"]

    unless (all (\s -> T.take 2 s `elem` ["  ","* "]) tmp) $
        fail "gitBranchesContain: internal error"

    return $!! map (T.drop 2) tmp

-- | returns @[(path, (url, key))]@
--
-- may throw exception
getModules :: FilePath -> GitRef -> Sh [(Text, (Text, Text))]
getModules d ref = do
    tmp <- runGit d "show" [ref <> ":.gitmodules"]

    res <- liftM T.lines $ runGitStdin d "config" [ "--file", "/dev/stdin", "-l" ] tmp

    let ms  = [ (T.tail key1,(key2, T.tail val))
              | r <- res, "submodule." `T.isPrefixOf` r
              , let (key,val) = T.break (=='=') r
              , let (key',key2) = T.breakOnEnd "." key
              , let (_,key1) = T.break (=='.') (T.init key')
              ]

        ms' = [ (path', (url, k))
              | es@((k,_):_) <- groupBy ((==) `on` fst) ms
              , let props = map snd es
              , let url = fromMaybe (error "getModules1") (lookup "url" props)
              , let path' = fromMaybe (error "getModules2") (lookup "path" props)
              ]

    return $!! ms'


{- |

Possible meanings of the 'Char' value:

 * Added (A),
 * Copied (C),
 * Deleted (D),
 * Modified (M),
 * Renamed (R),
 * have their type (i.e. regular file, symlink, submodule, ...) changed (T),
 * are Unmerged (U),
 * are Unknown (X),
 * or have had their pairing Broken (B).

-}
gitDiffTree :: FilePath -> GitRef -> Sh (Text, [([(GitType, Text, Char)], (GitType, Text), Text)])
gitDiffTree d ref = do
    tmp <- liftM T.lines $ runGit d "diff-tree" ["--root","-c", "-r", ref]
    case tmp of
        cid:deltas -> return $!! (cid, map parseDtLine deltas)
        []         -> return ("", [])

  where
    parseDtLine :: Text -> ([(GitType, Text, Char)], (GitType, Text), Text)
    parseDtLine l
      | sanityCheck = force (zip3 (map cvtMode mode') oid' (T.unpack k),(cvtMode mode,oid),fp)
      | otherwise = error "in parseDtLine"
      where
        sanityCheck = n > 0 && T.length k == n

        n = T.length cols
        (mode',mode:tmp') = splitAt n $ T.split (==' ') l''
        (oid',[oid,k]) = splitAt n tmp'
        [l'',fp] = T.split (=='\t') l'
        (cols,l') = T.span (==':') l

gitDiffTreePatch :: FilePath -> GitRef -> Text -> Sh Text
gitDiffTreePatch d ref fname = runGit d "diff-tree" ["--root", "--cc", "-r", ref, "--", fname]

z40 :: GitRef
z40 = T.pack (replicate 40 '0')

data GitType
    = GitTypeVoid
    | GitTypeRegFile
    | GitTypeExeFile
    | GitTypeTree
    | GitTypeSymLink
    | GitTypeGitLink
    deriving stock (Show,Eq,Ord,Enum)

instance NFData GitType where rnf !_ = ()

cvtMode :: Text -> GitType
cvtMode "000000" = GitTypeVoid
cvtMode "040000" = GitTypeSymLink
cvtMode "100644" = GitTypeRegFile
cvtMode "100755" = GitTypeExeFile
cvtMode "120000" = GitTypeSymLink
cvtMode "160000" = GitTypeGitLink
cvtMode x = error ("cvtMode: " ++ show x)

tshow :: Show a => a -> Text
tshow = T.pack . show
