-- | You don't need to import this module to enable bash completion.
--
-- See
-- <http://github.com/pcapriotti/optparse-applicative/wiki/Bash-Completion the wiki>
-- for more information on bash completion.
module Options.Applicative.BashCompletion
  ( bashCompletionParser
  ) where

import Control.Applicative
import Prelude
import Data.Foldable ( asum )
import Data.List ( isPrefixOf )
import Data.Maybe ( fromMaybe, listToMaybe )

import Options.Applicative.Builder
import Options.Applicative.Common
import Options.Applicative.Internal
import Options.Applicative.Types
import Options.Applicative.Help.Pretty
import Options.Applicative.Help.Chunk

-- | Provide basic or rich command completions
data Richness
  = Standard
  -- ^ Add no help descriptions to the completions
  | Enriched Int Int
  -- ^ Include tab separated description for options
  --   and commands when available.
  --   Takes option description length and command
  --   description length.
  deriving (Eq, Ord, Show)

bashCompletionParser :: ParserInfo a -> ParserPrefs -> Parser CompletionResult
bashCompletionParser pinfo pprefs = complParser
  where
    failure opts = CompletionResult
      { execCompletion = \progn -> unlines <$> opts progn }

    complParser = asum
      [ failure <$>
        (  bashCompletionQuery pinfo pprefs
        -- To get rich completions, one just needs the first
        -- command. To customise the lengths, use either of
        -- the `desc-length` options.
        -- zsh commands can go on a single line, so they might
        -- want to be longer.
        <$> ( flag' Enriched (long "bash-completion-enriched" `mappend` internal)
                <*> option auto (long "bash-completion-option-desc-length" `mappend` internal `mappend` value 40)
                <*> option auto (long "bash-completion-command-desc-length" `mappend` internal `mappend` value 40)
          <|> pure Standard
          )
        <*> (many . strOption) (long "bash-completion-word"
                                  `mappend` internal)
        <*> option auto (long "bash-completion-index" `mappend` internal) )
      , failure <$>
          (bashCompletionScript <$>
            strOption (long "bash-completion-script" `mappend` internal))
      , failure <$>
          (fishCompletionScript <$>
            strOption (long "fish-completion-script" `mappend` internal))
      , failure <$>
          (zshCompletionScript <$>
            strOption (long "zsh-completion-script" `mappend` internal))
      ]

bashCompletionQuery :: ParserInfo a -> ParserPrefs -> Richness -> [String] -> Int -> String -> IO [String]
bashCompletionQuery pinfo pprefs richness ws i _ = case runCompletion compl pprefs of
  Just (Left (SomeParser p, a))
    -> list_options a p
  Just (Right c)
    -> run_completer c
  Nothing
    -> return []
  where
    compl = runParserInfo pinfo (drop 1 ws')

    list_options a
      = fmap concat
      . sequence
      . mapParser (opt_completions a)

    --
    -- Prior to 0.14 there was a subtle bug which would
    -- mean that completions from positional arguments
    -- further into the parse would be shown.
    --
    -- We therefore now check to see that
    -- hinfoUnreachableArgs is off before running the
    -- completion for position arguments.
    --
    -- For options and flags, ensure that the user
    -- hasn't disabled them with `--`.
    opt_completions argPolicy reachability opt = case optMain opt of
      OptReader ns _ _
         | argPolicy /= AllPositionals
        -> return . add_opt_help opt $ show_names ns
         | otherwise
        -> return []
      FlagReader ns _
         | argPolicy /= AllPositionals
        -> return . add_opt_help opt $ show_names ns
         | otherwise
        -> return []
      ArgReader rdr
         | argumentIsUnreachable reachability
        -> return []
         | otherwise
        -> run_completer (crCompleter rdr)
      CmdReader _ ns p
         | argumentIsUnreachable reachability
        -> return []
         | otherwise
        -> return . add_cmd_help p $ filter_names ns

    -- When doing enriched completions, add any help specified
    -- to the completion variables (tab separated).
    add_opt_help :: Functor f => Option a -> f String -> f String
    add_opt_help opt = case richness of
      Standard ->
        id
      Enriched len _ ->
        fmap $ \o ->
          let h = unChunk $ optHelp opt
          in  maybe o (\h' -> o ++ "\t" ++ render_line len h') h

    -- When doing enriched completions, add the command description
    -- to the completion variables (tab separated).
    add_cmd_help :: Functor f => (String -> Maybe (ParserInfo a)) -> f String -> f String
    add_cmd_help p = case richness of
      Standard ->
        id
      Enriched _ len ->
        fmap $ \cmd ->
          let h = p cmd >>= unChunk . infoProgDesc
          in  maybe cmd (\h' -> cmd ++ "\t" ++ render_line len h') h

    show_names :: [OptName] -> [String]
    show_names = filter_names . map showOption

    -- We only want to show a single line in the completion results description.
    -- If there was a line break, it would come across as a different completion
    -- possibility.
    render_line :: Int -> Doc -> String
    render_line len doc = case lines (displayS (renderPretty 1 len doc) "") of
      [] -> ""
      [x] -> x
      x : _ -> x ++ "..."

    filter_names :: [String] -> [String]
    filter_names = filter is_completion

    run_completer :: Completer -> IO [String]
    run_completer c = runCompleter c (fromMaybe "" (listToMaybe ws''))

    (ws', ws'') = splitAt i ws

    is_completion :: String -> Bool
    is_completion =
      case ws'' of
        w:_ -> isPrefixOf w
        _ -> const True

bashCompletionScript :: String -> String -> IO [String]
bashCompletionScript prog progn = return
  [ "_" ++ progn ++ "()"
  , "{"
  , "    local CMDLINE"
  , "    local IFS=$'\\n'"
  , "    CMDLINE=(--bash-completion-index $COMP_CWORD)"
  , ""
  , "    for arg in ${COMP_WORDS[@]}; do"
  , "        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)"
  , "    done"
  , ""
  , "    COMPREPLY=( $(" ++ prog ++ " \"${CMDLINE[@]}\") )"
  , "}"
  , ""
  , "complete -o filenames -F _" ++ progn ++ " " ++ progn ]

{-
/Note/: Fish Shell

Derived from Drezil's post in #169.

@
commandline
-c or --cut-at-cursor only print selection up until the current cursor position
-o or --tokenize tokenize the selection and print one string-type token per line
@

We tokenize so that the call to count (and hence --bash-completion-index)
gets the right number use cut-at-curstor to not bother sending anything
after the cursor position, which allows for completion of the middle of
words.

Tab characters separate items from descriptions.
-}
fishCompletionScript :: String -> String -> IO [String]
fishCompletionScript prog progn = return
  [ " function _" ++ progn
  , "    set -l cl (commandline --tokenize --current-process)"
  , "    # Hack around fish issue #3934"
  , "    set -l cn (commandline --tokenize --cut-at-cursor --current-process)"
  , "    set -l cn (count $cn)"
  , "    set -l tmpline --bash-completion-enriched --bash-completion-index $cn"
  , "    for arg in $cl"
  , "      set tmpline $tmpline --bash-completion-word $arg"
  , "    end"
  , "    for opt in (" ++ prog ++ " $tmpline)"
  , "      if test -d $opt"
  , "        echo -E \"$opt/\""
  , "      else"
  , "        echo -E \"$opt\""
  , "      end"
  , "    end"
  , "end"
  , ""
  , "complete --no-files --command " ++ progn ++ " --arguments '(_"  ++ progn ++  ")'"
  ]

zshCompletionScript :: String -> String -> IO [String]
zshCompletionScript prog progn = return
  [ "#compdef " ++ progn
  , ""
  , "local request"
  , "local completions"
  , "local word"
  , "local index=$((CURRENT - 1))"
  , ""
  , "request=(--bash-completion-enriched --bash-completion-index $index)"
  , "for arg in ${words[@]}; do"
  , "  request=(${request[@]} --bash-completion-word $arg)"
  , "done"
  , ""
  , "IFS=$'\\n' completions=($( " ++ prog ++ " \"${request[@]}\" ))"
  , ""
  , "for word in $completions; do"
  , "  local -a parts"
  , ""
  , "  # Split the line at a tab if there is one."
  , "  IFS=$'\\t' parts=($( echo $word ))"
  , ""
  , "  if [[ -n $parts[2] ]]; then"
  , "     if [[ $word[1] == \"-\" ]]; then"
  , "       local desc=(\"$parts[1] ($parts[2])\")"
  , "       compadd -d desc -- $parts[1]"
  , "     else"
  , "       local desc=($(print -f  \"%-019s -- %s\" $parts[1] $parts[2]))"
  , "       compadd -l -d desc -- $parts[1]"
  , "     fi"
  , "  else"
  , "    compadd -f -- $word"
  , "  fi"
  , "done"
  ]
