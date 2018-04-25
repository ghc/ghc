module System.Console.Haskeline.Emacs where

import System.Console.Haskeline.Command
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Key
import System.Console.Haskeline.Command.Completion
import System.Console.Haskeline.Command.History
import System.Console.Haskeline.Command.Undo
import System.Console.Haskeline.Command.KillRing
import System.Console.Haskeline.LineState
import System.Console.Haskeline.InputT

import Data.Char

type InputCmd s t = forall m . MonadException m => Command (InputCmdT m) s t
type InputKeyCmd s t = forall m . MonadException m => KeyCommand (InputCmdT m) s t

emacsCommands :: InputKeyCmd InsertMode (Maybe String)
emacsCommands = choiceCmd [
                    choiceCmd [simpleActions, controlActions] >+> 
                        keyCommand emacsCommands
                    , enders]

enders :: InputKeyCmd InsertMode (Maybe String)
enders = choiceCmd [simpleChar '\n' +> finish, eotKey +> deleteCharOrEOF]
    where
        eotKey = ctrlChar 'd'
        deleteCharOrEOF s
            | s == emptyIM  = return Nothing
            | otherwise = change deleteNext s >>= justDelete
        justDelete = keyChoiceCmd [eotKey +> change deleteNext >|> justDelete
                            , emacsCommands]


simpleActions, controlActions :: InputKeyCmd InsertMode InsertMode
simpleActions = choiceCmd 
            [ simpleKey LeftKey +> change goLeft
            , simpleKey RightKey +> change goRight
            , simpleKey Backspace +> change deletePrev
            , simpleKey Delete +> change deleteNext 
            , changeFromChar insertChar
            , completionCmd (simpleChar '\t')
            , simpleKey UpKey +> historyBack
            , simpleKey DownKey +> historyForward
            , searchHistory
            ] 
            
controlActions = choiceCmd
            [ ctrlChar 'a' +> change moveToStart 
            , ctrlChar 'e' +> change moveToEnd
            , ctrlChar 'b' +> change goLeft
            , ctrlChar 'f' +> change goRight
            , ctrlChar 'l' +> clearScreenCmd
            , metaChar 'f' +> change wordRight
            , metaChar 'b' +> change wordLeft
            , ctrlKey (simpleKey LeftKey) +> change wordLeft
            , ctrlKey (simpleKey RightKey) +> change wordRight
            , metaChar 'c' +> change (modifyWord capitalize)
            , metaChar 'l' +> change (modifyWord (mapBaseChars toLower))
            , metaChar 'u' +> change (modifyWord (mapBaseChars toUpper))
            , ctrlChar '_' +> commandUndo
            , ctrlChar 'x' +> try (ctrlChar 'u' +> commandUndo)
            , ctrlChar 't' +> change transposeChars
            , ctrlChar 'p' +> historyBack
            , ctrlChar 'n' +> historyForward
            , metaChar '<' +> historyStart
            , metaChar '>' +> historyEnd
            , simpleKey Home +> change moveToStart
            , simpleKey End +> change moveToEnd
            , choiceCmd
                [ ctrlChar 'w' +> killFromHelper (SimpleMove bigWordLeft)
                , metaKey (simpleKey Backspace) +> killFromHelper (SimpleMove wordLeft)
                , metaChar 'd' +> killFromHelper (SimpleMove wordRight)
                , ctrlChar 'k' +> killFromHelper (SimpleMove moveToEnd)
                , simpleKey KillLine +> killFromHelper (SimpleMove moveToStart)
                ]
            , ctrlChar 'y' +> rotatePaste
            ]

rotatePaste :: InputCmd InsertMode InsertMode
rotatePaste im = get >>= loop
  where
    loop kr = case peek kr of
                    Nothing -> return im
                    Just s -> setState (insertGraphemes s im)
                            >>= try (metaChar 'y' +> \_ -> loop (rotate kr))


wordRight, wordLeft, bigWordLeft :: InsertMode -> InsertMode
wordRight = goRightUntil (atStart (not . isAlphaNum))
wordLeft = goLeftUntil (atStart isAlphaNum)
bigWordLeft = goLeftUntil (atStart (not . isSpace))

modifyWord :: ([Grapheme] -> [Grapheme]) -> InsertMode -> InsertMode
modifyWord f im = IMode (reverse (f ys1) ++ xs) ys2
    where
        IMode xs ys = skipRight (not . isAlphaNum) im
        (ys1,ys2) = span (isAlphaNum . baseChar) ys

capitalize :: [Grapheme] -> [Grapheme]
capitalize [] = []
capitalize (c:cs) = modifyBaseChar toUpper c : cs
