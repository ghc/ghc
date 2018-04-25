module System.Console.Haskeline.Command(
                        -- * Commands
                        Effect(..),
                        KeyMap(..), 
                        CmdM(..),
                        Command,
                        KeyCommand,
                        KeyConsumed(..),
                        withoutConsuming,
                        keyCommand,
                        (>|>),
                        (>+>),
                        try,
                        effect,
                        clearScreenCmd,
                        finish,
                        failCmd,
                        simpleCommand,
                        charCommand,
                        setState,
                        change,
                        changeFromChar,
                        (+>),
                        useChar,
                        choiceCmd,
                        keyChoiceCmd,
                        keyChoiceCmdM,
                        doBefore
                        ) where

import Data.Char(isPrint)
import Control.Applicative(Applicative(..))
import Control.Monad(ap, mplus, liftM)
import Control.Monad.Trans.Class
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Key

data Effect = LineChange (Prefix -> LineChars)
              | PrintLines [String]
              | ClearScreen
              | RingBell

lineChange :: LineState s => s -> Effect
lineChange = LineChange . flip lineChars

data KeyMap a = KeyMap {lookupKM :: Key -> Maybe (KeyConsumed a)}

data KeyConsumed a = NotConsumed a | Consumed a

instance Functor KeyMap where
    fmap f km = KeyMap $ fmap (fmap f) . lookupKM km

instance Functor KeyConsumed where
    fmap f (NotConsumed x) = NotConsumed (f x)
    fmap f (Consumed x) = Consumed (f x)


data CmdM m a   = GetKey (KeyMap (CmdM m a))
                | DoEffect Effect (CmdM m a)
                | CmdM (m (CmdM m a))
                | Result a

type Command m s t = s -> CmdM m t

instance Monad m => Functor (CmdM m) where
    fmap = liftM

instance Monad m => Applicative (CmdM m) where
    pure  = Result
    (<*>) = ap

instance Monad m => Monad (CmdM m) where
    return = pure

    GetKey km >>= g = GetKey $ fmap (>>= g) km
    DoEffect e f >>= g = DoEffect e (f >>= g)
    CmdM f >>= g = CmdM $ liftM (>>= g) f
    Result x >>= g = g x

type KeyCommand m s t = KeyMap (Command m s t)

instance MonadTrans CmdM where
    lift m = CmdM $ do
        x <- m
        return $ Result x

keyCommand :: KeyCommand m s t -> Command m s t
keyCommand km = \s -> GetKey $ fmap ($ s) km

useKey :: Key -> a -> KeyMap a
useKey k x = KeyMap $ \k' -> if k==k' then Just (Consumed x) else Nothing

-- TODO: could just be a monadic action that returns a Char.
useChar :: (Char -> Command m s t) -> KeyCommand m s t
useChar act = KeyMap $ \k -> case k of
                    Key m (KeyChar c) | isPrint c && m==noModifier
                        -> Just $ Consumed (act c)
                    _ -> Nothing

withoutConsuming :: Command m s t -> KeyCommand m s t
withoutConsuming = KeyMap . const . Just . NotConsumed

choiceCmd :: [KeyMap a] -> KeyMap a
choiceCmd = foldl orKM nullKM
    where
        nullKM = KeyMap $ const Nothing
        orKM (KeyMap f) (KeyMap g) = KeyMap $ \k -> f k `mplus` g k

keyChoiceCmd :: [KeyCommand m s t] -> Command m s t
keyChoiceCmd = keyCommand . choiceCmd

keyChoiceCmdM :: [KeyMap (CmdM m a)] -> CmdM m a
keyChoiceCmdM = GetKey . choiceCmd

infixr 6 >|>
(>|>) :: Monad m => Command m s t -> Command m t u -> Command m s u
f >|> g = \x -> f x >>= g

infixr 6 >+>
(>+>) :: Monad m => KeyCommand m s t -> Command m t u -> KeyCommand m s u
km >+> g = fmap (>|> g) km

-- attempt to run the command (predicated on getting a valid key); but if it fails, just keep
-- going.
try :: Monad m => KeyCommand m s s -> Command m s s
try f = keyChoiceCmd [f,withoutConsuming return]

infixr 6 +>
(+>) :: Key -> a -> KeyMap a
(+>) = useKey

finish :: (Monad m, Result s) => Command m s (Maybe String)
finish = return . Just . toResult

failCmd :: Monad m => Command m s (Maybe a)
failCmd _ = return Nothing

effect :: Effect -> CmdM m ()
effect e = DoEffect e $ Result ()

clearScreenCmd :: Command m s s
clearScreenCmd = DoEffect ClearScreen . Result

simpleCommand :: (LineState s, Monad m) => (s -> m (Either Effect s))
        -> Command m s s
simpleCommand f = \s -> do
    et <- lift (f s)
    case et of
        Left e -> effect e >> return s
        Right t -> setState t

charCommand :: (LineState s, Monad m) => (Char -> s -> m (Either Effect s))
                    -> KeyCommand m s s
charCommand f = useChar $ simpleCommand . f

setState :: (Monad m, LineState s) => Command m s s
setState s = effect (lineChange s) >> return s

change :: (LineState t, Monad m) => (s -> t) -> Command m s t
change = (setState .)

changeFromChar :: (LineState t, Monad m) => (Char -> s -> t) -> KeyCommand m s t
changeFromChar f = useChar $ change . f

doBefore :: Monad m => Command m s t -> KeyCommand m t u -> KeyCommand m s u
doBefore cmd = fmap (cmd >|>)
