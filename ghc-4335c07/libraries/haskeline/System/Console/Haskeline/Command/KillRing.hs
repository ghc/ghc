module System.Console.Haskeline.Command.KillRing where

import System.Console.Haskeline.LineState
import System.Console.Haskeline.Command
import System.Console.Haskeline.Monads
import System.Console.Haskeline.Command.Undo
import Control.Monad
import Data.IORef

-- standard trick for a purely functional queue:
data Stack a = Stack [a] [a]
                deriving Show

emptyStack :: Stack a
emptyStack = Stack [] []

peek :: Stack a -> Maybe a
peek (Stack [] []) = Nothing
peek (Stack (x:_) _) = Just x
peek (Stack [] ys) = peek (Stack (reverse ys) [])

rotate :: Stack a -> Stack a
rotate s@(Stack [] []) = s
rotate (Stack (x:xs) ys) = Stack xs (x:ys)
rotate (Stack [] ys) = rotate (Stack (reverse ys) [])

push :: a -> Stack a -> Stack a
push x (Stack xs ys) = Stack (x:xs) ys

type KillRing = Stack [Grapheme]

runKillRing :: MonadIO m => ReaderT (IORef KillRing) m a -> m a
runKillRing act = do
    ringRef <- liftIO $ newIORef emptyStack
    runReaderT act ringRef


pasteCommand :: (Save s, MonadState KillRing m, MonadState Undo m)
            => ([Grapheme] -> s -> s) -> Command m (ArgMode s) s
pasteCommand use = \s -> do
    ms <- liftM peek get
    case ms of
        Nothing -> return $ argState s
        Just p -> do
            modify $ saveToUndo $ argState s
            setState $ applyArg (use p) s

deleteFromDiff' :: InsertMode -> InsertMode -> ([Grapheme],InsertMode)
deleteFromDiff' (IMode xs1 ys1) (IMode xs2 ys2)
    | posChange >= 0 = (take posChange ys1, IMode xs1 ys2)
    | otherwise = (take (negate posChange) ys2 ,IMode xs2 ys1)
  where
    posChange = length xs2 - length xs1

killFromHelper :: (MonadState KillRing m, MonadState Undo m,
                        Save s, Save t)
                => KillHelper -> Command m s t
killFromHelper helper = saveForUndo >|> \oldS -> do
    let (gs,newIM) = applyHelper helper (save oldS)
    modify (push gs)
    setState (restore newIM)

killFromArgHelper :: (MonadState KillRing m, MonadState Undo m, Save s, Save t)
                => KillHelper -> Command m (ArgMode s) t
killFromArgHelper helper = saveForUndo >|> \oldS -> do
    let (gs,newIM) = applyArgHelper helper (fmap save oldS)
    modify (push gs)
    setState (restore newIM)

copyFromArgHelper :: (MonadState KillRing m, Save s)
                => KillHelper -> Command m (ArgMode s) s
copyFromArgHelper helper = \oldS -> do
    let (gs,_) = applyArgHelper helper (fmap save oldS)
    modify (push gs)
    setState (argState oldS)


data KillHelper = SimpleMove (InsertMode -> InsertMode)
                 | GenericKill (InsertMode -> ([Grapheme],InsertMode))
        -- a generic kill gives more flexibility, but isn't repeatable.
        -- for example: dd,cc, %

killAll :: KillHelper
killAll = GenericKill $ \(IMode xs ys) -> (reverse xs ++ ys, emptyIM)

applyHelper :: KillHelper -> InsertMode -> ([Grapheme],InsertMode)
applyHelper (SimpleMove move) im = deleteFromDiff' im (move im)
applyHelper (GenericKill act) im = act im

applyArgHelper :: KillHelper -> ArgMode InsertMode -> ([Grapheme],InsertMode)
applyArgHelper (SimpleMove move) im = deleteFromDiff' (argState im) (applyArg move im)
applyArgHelper (GenericKill act) im = act (argState im)
