-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.PreProcess.Unlit
-- Copyright   :  ...
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Remove the \"literal\" markups from a Haskell source file, including
-- \"@>@\", \"@\\begin{code}@\", \"@\\end{code}@\", and \"@#@\"

-- This version is interesting because instead of striping comment lines, it
-- turns them into "-- " style comments. This allows using haddock markup
-- in literate scripts without having to use "> --" prefix.

module Distribution.Simple.PreProcess.Unlit (unlit,plain) where

import Prelude ()
import Distribution.Compat.Prelude

import Data.List (mapAccumL)

data Classified = BirdTrack String | Blank String | Ordinary String
                | Line !Int String | CPP String
                | BeginCode | EndCode
                -- output only:
                | Error String | Comment String

-- | No unliteration.
plain :: String -> String -> String
plain _ hs = hs

classify :: String -> Classified
classify ('>':s) = BirdTrack s
classify ('#':s) = case tokens s of
                     (line:file:_) | all isDigit line
                                  && length file >= 2
                                  && head file == '"'
                                  && last file == '"'
                                -> Line (read line) (tail (init file)) -- TODO:eradicateNoParse
                     _          -> CPP s
  where tokens = unfoldr $ \str -> case lex str of
                                   (t@(_:_), str'):_ -> Just (t, str')
                                   _                 -> Nothing
classify ('\\':s)
  | "begin{code}" `isPrefixOf` s = BeginCode
  | "end{code}"   `isPrefixOf` s = EndCode
classify s | all isSpace s       = Blank s
classify s                       = Ordinary s

-- So the weird exception for comment indenting is to make things work with
-- haddock, see classifyAndCheckForBirdTracks below.
unclassify :: Bool -> Classified -> String
unclassify _     (BirdTrack s) = ' ':s
unclassify _     (Blank s)     = s
unclassify _     (Ordinary s)  = s
unclassify _     (Line n file) = "# " ++ show n ++ " " ++ show file
unclassify _     (CPP s)       = '#':s
unclassify True  (Comment "")  = "  --"
unclassify True  (Comment s)   = "  -- " ++ s
unclassify False (Comment "")  = "--"
unclassify False (Comment s)   = "-- " ++ s
unclassify _     _             = internalError

-- | 'unlit' takes a filename (for error reports), and transforms the
--   given string, to eliminate the literate comments from the program text.
unlit :: FilePath -> String -> Either String String
unlit file input =
  let (usesBirdTracks, classified) = classifyAndCheckForBirdTracks
                                   . inlines
                                   $ input
   in either (Left . unlines . map (unclassify usesBirdTracks))
              Right
    . checkErrors
    . reclassify
    $ classified

  where
    -- So haddock requires comments and code to align, since it treats comments
    -- as following the layout rule. This is a pain for us since bird track
    -- style literate code typically gets indented by two since ">" is replaced
    -- by " " and people usually use one additional space of indent ie
    -- "> then the code". On the other hand we cannot just go and indent all
    -- the comments by two since that does not work for latex style literate
    -- code. So the hacky solution we use here is that if we see any bird track
    -- style code then we'll indent all comments by two, otherwise by none.
    -- Of course this will not work for mixed latex/bird track .lhs files but
    -- nobody does that, it's silly and specifically recommended against in the
    -- H98 unlit spec.
    --
    classifyAndCheckForBirdTracks =
      flip mapAccumL False $ \seenBirdTrack line ->
        let classification = classify line
         in (seenBirdTrack || isBirdTrack classification, classification)

    isBirdTrack (BirdTrack _) = True
    isBirdTrack _             = False

    checkErrors ls = case [ e | Error e <- ls ] of
      []          -> Left  ls
      (message:_) -> Right (f ++ ":" ++ show n ++ ": " ++ message)
        where (f, n) = errorPos file 1 ls
    errorPos f n []              = (f, n)
    errorPos f n (Error _:_)     = (f, n)
    errorPos _ _ (Line n' f':ls) = errorPos f' n' ls
    errorPos f n (_         :ls) = errorPos f  (n+1) ls

-- Here we model a state machine, with each state represented by
-- a local function. We only have four states (well, five,
-- if you count the error state), but the rules
-- to transition between then are not so simple.
-- Would it be simpler to have more states?
--
-- Each state represents the type of line that was last read
-- i.e. are we in a comment section, or a latex-code section,
-- or a bird-code section, etc?
reclassify :: [Classified] -> [Classified]
reclassify = blank -- begin in blank state
  where
    latex []               = []
    latex (EndCode    :ls) = Blank "" : comment ls
    latex (BeginCode  :_ ) = [Error "\\begin{code} in code section"]
    latex (BirdTrack l:ls) = Ordinary ('>':l) : latex ls
    latex (          l:ls) = l : latex ls

    blank []               = []
    blank (EndCode    :_ ) = [Error "\\end{code} without \\begin{code}"]
    blank (BeginCode  :ls) = Blank ""    : latex ls
    blank (BirdTrack l:ls) = BirdTrack l : bird ls
    blank (Ordinary  l:ls) = Comment   l : comment ls
    blank (          l:ls) =           l : blank ls

    bird []              = []
    bird (EndCode   :_ ) = [Error "\\end{code} without \\begin{code}"]
    bird (BeginCode :ls) = Blank "" : latex ls
    bird (Blank l   :ls) = Blank l  : blank ls
    bird (Ordinary _:_ ) = [Error "program line before comment line"]
    bird (         l:ls) = l : bird ls

    comment []               = []
    comment (EndCode    :_ ) = [Error "\\end{code} without \\begin{code}"]
    comment (BeginCode  :ls) = Blank "" : latex ls
    comment (CPP l      :ls) = CPP l : comment ls
    comment (BirdTrack _:_ ) = [Error "comment line before program line"]
    -- a blank line and another ordinary line following a comment
    -- will be treated as continuing the comment. Otherwise it's
    -- then end of the comment, with a blank line.
    comment (Blank     l:ls@(Ordinary  _:_)) = Comment l : comment ls
    comment (Blank     l:ls) = Blank l   : blank ls
    comment (Line n f   :ls) = Line n f  : comment ls
    comment (Ordinary  l:ls) = Comment l : comment ls
    comment (Comment   _: _) = internalError
    comment (Error     _: _) = internalError

-- Re-implementation of 'lines', for better efficiency (but decreased laziness).
-- Also, importantly, accepts non-standard DOS and Mac line ending characters.
inlines :: String -> [String]
inlines xs = lines' xs id
  where
  lines' []             acc = [acc []]
  lines' ('\^M':'\n':s) acc = acc [] : lines' s id    -- DOS
  lines' ('\^M':s)      acc = acc [] : lines' s id    -- MacOS
  lines' ('\n':s)       acc = acc [] : lines' s id    -- Unix
  lines' (c:s)          acc = lines' s (acc . (c:))

internalError :: a
internalError = error "unlit: internal error"
