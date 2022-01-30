{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Notes where

import Data.Either
import Data.Foldable
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Directory (doesFileExist)

data SrcLoc = SrcLoc { fileName :: FilePath
                     , row :: !Int
                     , column :: !Int
                     }
  deriving (Eq, Ord, Show)

showSrcLoc :: SrcLoc -> String
showSrcLoc loc =
    concat [fileName loc, ":", show (row loc), ":", show (column loc), ":"]

newtype NoteName = NoteName T.Text
  deriving (Eq, Ord, Show)

showNoteName :: NoteName -> String
showNoteName (NoteName x) = "Note [" <> T.unpack x <> "]"

data NoteDef = NoteDef { noteDefSrcLoc :: !SrcLoc
                       , noteDefName   :: !NoteName
                       }
  deriving (Eq, Ord, Show)

showNoteDef :: NoteDef -> String
showNoteDef (NoteDef{noteDefSrcLoc=loc, noteDefName=name}) =
    "def    " <> showSrcLoc loc <> "     " <> showNoteName name

data NoteRef = NoteRef { noteRefSrcLoc :: !SrcLoc
                       , noteRefName   :: !NoteName
                       }
  deriving (Eq, Ord, Show)

showNoteRef :: NoteRef -> String
showNoteRef (NoteRef{noteRefSrcLoc=loc, noteRefName=name}) =
    "ref    " <> showSrcLoc loc <> "     " <> showNoteName name

findNotes :: FilePath -> T.Text -> [Either NoteRef NoteDef]
findNotes fname t =
    go 1 (T.lines t)
  where
    go :: Int -> [T.Text] -> [Either NoteRef NoteDef]
    -- Note definitions:
    -- We look for a "Note [" token with a "~~~" rule beneath it.
    go !lineNo (l0 : l1 : ls)
      | hasRule = Right (NoteDef srcLoc name) : go (lineNo+2) ls
      where
        (prefix, rest) = T.breakOn "Note [" l0
        startCol = T.length prefix
        hasRule = T.take 3 (T.drop startCol l1) == "~~~"
        srcLoc = SrcLoc fname lineNo startCol
        name = NoteName $ T.takeWhile (/= ']') $ T.drop 6 rest

    -- Note references:
    -- We look for a "Note [...]", strip away any beginning-of-line
    -- comment symbols, and merge whitespace.
    go lineNo (l0 : ls) =
        [ Left (NoteRef srcLoc (NoteName name))
        | (prefix, rest) <- T.breakOnAll "Note [" l0
        , let startCol = T.length prefix
              srcLoc = SrcLoc fname lineNo startCol
              (name, suffix) = T.breakOn "]" (T.drop 6 rest<>" "<>T.concat (map stripBeginningOfLineComment $ take 1 ls))
        , "]" `T.isPrefixOf` suffix
        ] ++ go (lineNo+1) ls

    go _lineNo [] = []

stripBeginningOfLineComment :: T.Text -> T.Text
stripBeginningOfLineComment = T.pack . go . T.unpack
  where
    -- This implements the following regular expression substitution:
    --
    --    s/$ *[(\-\- )\#( \* )] */ /

    go :: String -> String
    go ('#':rest)     = finish rest
    go ('-':'-':rest) = finish rest
    go (' ':'*':rest) = finish rest
    go ('/':'/':rest) = finish rest
    go (' ':rest)     = go rest
    go xs             = finish xs

    finish :: String -> String
    finish = dropWhile (==' ')

data NoteDb = NoteDb { noteRefs :: M.Map FilePath (S.Set NoteRef)
                     , noteDefs :: M.Map NoteName (S.Set NoteDef)
                     }

instance Monoid NoteDb where
    mempty = NoteDb M.empty M.empty

instance Semigroup NoteDb where
    NoteDb a b <> NoteDb c d =
        NoteDb (M.unionWith (<>) a c) (M.unionWith (<>) b d)

allNoteDefs :: NoteDb -> [NoteDef]
allNoteDefs db =
    [ def
    | defs <- M.elems (noteDefs db)
    , def <- S.toList defs
    ]

allNoteRefs :: NoteDb -> [NoteRef]
allNoteRefs db =
    [ ref
    | (_fname, refs) <- M.toList (noteRefs db)
    , ref <- S.toList refs
    ]

showNoteDb :: NoteDb -> String
showNoteDb db = unlines $
    map showNoteRef (allNoteRefs db)
    ++
    map showNoteDef (allNoteDefs db)

filesNotes :: [FilePath]
           -> IO NoteDb
filesNotes = fmap mconcat . mapM fileNotes

fileNotes :: FilePath -> IO NoteDb
fileNotes fname = do
    is_file <- doesFileExist fname
    if is_file
      then do
        bs <- BS.readFile fname
        return $ case T.decodeUtf8' bs of
          Left _ -> mempty
          Right t ->
              let (refs, defs) = partitionEithers (findNotes fname t)
              in NoteDb
                   { noteRefs = M.singleton fname (S.fromList refs)
                   , noteDefs = M.fromList
                       [ (noteDefName def, S.singleton def)
                       | def <- defs
                       ]
                   }
      else return mempty

brokenNoteRefs :: NoteDb -> [NoteRef]
brokenNoteRefs db =
    [ ref
    | (_fname, refs) <- M.toList (noteRefs db)
    , ref <- S.toList refs
    , Nothing <- pure $ M.lookup (noteRefName ref) (noteDefs db)
    ]

unreferencedNotes :: NoteDb -> S.Set NoteDef
unreferencedNotes db =
    fold $ noteDefs db `M.withoutKeys` referencedNotes
  where
    referencedNotes = S.fromList $ map noteRefName (allNoteRefs db)
