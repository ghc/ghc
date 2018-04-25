module System.Console.Haskeline.Prefs(
                        Prefs(..),
                        defaultPrefs,
                        readPrefs,
                        CompletionType(..),
                        BellStyle(..),
                        EditMode(..),
                        HistoryDuplicates(..),
                        lookupKeyBinding
                        ) where

import Data.Char(isSpace,toLower)
import Data.List(foldl')
import qualified Data.Map as Map
import System.Console.Haskeline.MonadException(handle,IOException)
import System.Console.Haskeline.Key

{- |
'Prefs' allow the user to customize the terminal-style line-editing interface.  They are
read by default from @~/.haskeline@; to override that behavior, use
'readPrefs' and @runInputTWithPrefs@.

Each line of a @.haskeline@ file defines
one field of the 'Prefs' datatype; field names are case-insensitive and
unparseable lines are ignored.  For example:

> editMode: Vi
> completionType: MenuCompletion
> maxhistorysize: Just 40

-}
data Prefs = Prefs { bellStyle :: !BellStyle,
                     editMode :: !EditMode,
                     maxHistorySize :: !(Maybe Int),
                     historyDuplicates :: HistoryDuplicates,
                     completionType :: !CompletionType,
                     completionPaging :: !Bool, 
                        -- ^ When listing completion alternatives, only display
                        -- one screen of possibilities at a time.
                     completionPromptLimit :: !(Maybe Int),
                        -- ^ If more than this number of completion
                        -- possibilities are found, then ask before listing
                        -- them.
                     listCompletionsImmediately :: !Bool,
                        -- ^ If 'False', completions with multiple possibilities
                        -- will ring the bell and only display them if the user
                        -- presses @TAB@ again.
                     customBindings :: Map.Map Key [Key],
                        -- (termName, keysequence, key)
                     customKeySequences :: [(Maybe String, String,Key)]
                     }
                        deriving Show

data CompletionType = ListCompletion | MenuCompletion
            deriving (Read,Show)


data BellStyle = NoBell | VisualBell | AudibleBell
                    deriving (Show, Read)

data EditMode = Vi | Emacs
                    deriving (Show,Read)

data HistoryDuplicates = AlwaysAdd | IgnoreConsecutive | IgnoreAll
                    deriving (Show,Read)

-- | The default preferences which may be overwritten in the
-- @.haskeline@ file.
defaultPrefs :: Prefs
defaultPrefs = Prefs {bellStyle = AudibleBell,
                      maxHistorySize = Just 100,
                      editMode = Emacs,
                      completionType = ListCompletion,
                      completionPaging = True,
                      completionPromptLimit = Just 100,
                      listCompletionsImmediately = True,
                      historyDuplicates = AlwaysAdd,
                      customBindings = Map.empty,
                      customKeySequences = []
                    }

mkSettor :: Read a => (a -> Prefs -> Prefs) -> String -> Prefs -> Prefs
mkSettor f str = maybe id f (readMaybe str)

readMaybe :: Read a => String -> Maybe a
readMaybe str = case reads str of
                [(x,_)] -> Just x
                _ -> Nothing


settors :: [(String, String -> Prefs -> Prefs)]
settors = [("bellstyle", mkSettor $ \x p -> p {bellStyle = x})
          ,("editmode", mkSettor $ \x p -> p {editMode = x})
          ,("maxhistorysize", mkSettor $ \x p -> p {maxHistorySize = x})
          ,("completiontype", mkSettor $ \x p -> p {completionType = x})
          ,("completionpaging", mkSettor $ \x p -> p {completionPaging = x})
          ,("completionpromptlimit", mkSettor $ \x p -> p {completionPromptLimit = x})
          ,("listcompletionsimmediately", mkSettor $ \x p -> p {listCompletionsImmediately = x})
          ,("historyduplicates", mkSettor $ \x p -> p {historyDuplicates = x})
          ,("bind", addCustomBinding)
          ,("keyseq", addCustomKeySequence)
          ]

addCustomBinding :: String -> Prefs -> Prefs
addCustomBinding str p = case mapM parseKey (words str) of
    Just (k:ks) -> p {customBindings = Map.insert k ks (customBindings p)}
    _ -> p

addCustomKeySequence :: String -> Prefs -> Prefs
addCustomKeySequence str = maybe id addKS maybeParse
    where
        maybeParse :: Maybe (Maybe String, String,Key)
        maybeParse = case words str of
            [cstr,kstr] -> parseWords Nothing cstr kstr
            [term,cstr,kstr] -> parseWords (Just term) cstr kstr
            _ -> Nothing
        parseWords mterm cstr kstr = do
            k <- parseKey kstr
            cs <- readMaybe cstr
            return (mterm,cs,k)
        addKS ks p = p {customKeySequences = ks:customKeySequences p}

lookupKeyBinding :: Key -> Prefs -> [Key]
lookupKeyBinding k = Map.findWithDefault [k] k . customBindings

-- | Read 'Prefs' from a given file.  If there is an error reading the file,
-- the 'defaultPrefs' will be returned.
readPrefs :: FilePath -> IO Prefs
readPrefs file = handle (\(_::IOException) -> return defaultPrefs) $ do
    ls <- fmap lines $ readFile file
    return $! foldl' applyField defaultPrefs ls
  where
    applyField p l = case break (==':') l of
                (name,val)  -> case lookup (map toLower $ trimSpaces name) settors of
                        Nothing -> p
                        Just set -> set (drop 1 val) p  -- drop initial ":", don't crash if val==""
    trimSpaces = dropWhile isSpace . reverse . dropWhile isSpace . reverse
                    
