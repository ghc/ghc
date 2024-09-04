{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module LintCodes.Coverage
  ( getCoveredCodes )
  where

-- base
import Data.Char
  ( isAlphaNum, isDigit, isSpace )
import Data.Maybe
  ( mapMaybe )
import Data.List
  ( dropWhileEnd )

-- bytestring
import qualified Data.ByteString as ByteString
  ( readFile )

-- containers
import Data.Set
  ( Set )
import qualified Data.Set as Set
  ( fromList )

-- directory
import System.Directory
  ( doesDirectoryExist, listDirectory )

-- filepath
import System.FilePath
  ( (</>), normalise, takeExtension )

-- ghc
import GHC.Types.Error
  ( DiagnosticCode(..) )

-- process
import System.Process
  ( readProcess )

-- text
import Data.Text
  ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
  ( decodeUtf8' )

-- transformers
import Control.Monad.Trans.State.Strict
  ( State, runState, get, put )

--------------------------------------------------------------------------------
-- Diagnostic code coverage from testsuite .stdout and .stderr files

-- | Get all diagnostic codes that appear in testsuite .stdout and .stderr
-- files.
getCoveredCodes :: IO (Set DiagnosticCode)
getCoveredCodes =
  do { top <- dropWhileEnd isSpace
          <$> readProcess "git" ["rev-parse", "--show-toplevel"] ""
       -- TODO: would be better to avoid using git entirely.
     ; let testRoot = normalise $ top </> "testsuite" </> "tests"
     ; traverseFilesFrom includeFile diagnosticCodesIn testRoot
     }

-- | Excluded files: we don't look for diagnostic codes in these, as they
-- are not actual diagnostic codes emitted by the compiler.
excludeList :: [ FilePath ]
excludeList = [ "codes.stdout" ]

-- | Which files should we include in the search for diagnostic codes in the
-- output of the testsuite: `.stdout` and `.stderr` files.
includeFile :: FilePath -> Bool
includeFile fn
  =  fn `notElem` excludeList
  && takeExtension fn `elem` [ ".stdout", ".stderr" ]

-- | Collect all diagnostic codes mentioned in the given 'Text'.
diagnosticCodesIn :: Text -> Set DiagnosticCode
diagnosticCodesIn txt =
  Set.fromList $ mapMaybe getCode
               $ concatMap (enclosedBy '[' ']')
               $ Text.lines txt

  where
    getCode :: Text -> Maybe DiagnosticCode
    getCode txt_inside_brackets
      | let (ns, Text.drop 1 -> code) = Text.breakOn "-" txt_inside_brackets
      , not $ Text.null ns
      , not $ Text.null code
      , Text.all isAlphaNum ns
      , Text.all isDigit code
      , let ns' = Text.unpack ns
      = let diag = DiagnosticCode ns' ( read $ Text.unpack code )
        in if ns' `elem` expectedDiagnosticNameSpaces
           then Just diag
           else Nothing
             -- error "lint-codes: unexpected diagnostic code [" ++ show diag ++ "]."
      | otherwise
      = Nothing

-- | Which diagnostic code namespaces are relevant to this test?
expectedDiagnosticNameSpaces :: [String]
expectedDiagnosticNameSpaces = ["GHC"]

-- | Capture pieces of a text enclosed by matching delimiters.
--
-- > enclosedBy '(' ')' "ab(cd(e)f)g(hk)l"
-- > ["cd(e)f", "hk"]
enclosedBy :: Char -> Char -> Text -> [Text]
enclosedBy open close = go . recur
  where
    recur = Text.breakOn (Text.singleton open)
    go (_, Text.drop 1 -> rest)
      | Text.null rest
      = []
      | let ((ok, rest'), n) = ( `runState` 1 ) $ Text.spanM matchingParen rest
      = if n == 0
        then (if Text.null ok then id else (ok:)) $ go $ recur rest'
        else []

    matchingParen :: Char -> State Int Bool
    matchingParen c =
      do { s <- get
         ; if | c == open
              -> do { put (s+1); return True }
              | c == close
              -> do { put (s-1); return (s /= 1) }
              | otherwise
              -> return True }

-- | Recursive traversal from a root directory of all files satisfying
-- the inclusion predicate, collecting up a result according to
-- the parsing function.
traverseFilesFrom :: forall b. Monoid b
                  => ( FilePath -> Bool ) -- ^ inclusion predicate
                  -> ( Text -> b )        -- ^ parsing function
                  -> FilePath             -- ^ directory root
                  -> IO b
traverseFilesFrom include_file parse_contents = go
  where
    go top
     = do { ps <- listDirectory top
          ; (`foldMap` ps) \ p ->
       do { let path = top </> p
          ; is_dir <- doesDirectoryExist path
          ; if is_dir
            then go path
            else if not $ include_file p
            then return mempty
            else
        do { bs <- ByteString.readFile path
           ; return $ case Text.decodeUtf8' bs of
           { Left  _   -> mempty
           ; Right txt -> parse_contents txt
           } } } }
