module System.Console.Haskeline.Backend.Terminfo(
                            Draw(),
                            runTerminfoDraw
                            )
                             where

import System.Console.Terminfo
import Control.Applicative
import Control.Monad
import Data.List(foldl')
import System.IO
import qualified Control.Exception as Exception
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.IntMap as Map

import System.Console.Haskeline.Monads as Monads
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Term
import System.Console.Haskeline.Backend.Posix
import System.Console.Haskeline.Backend.WCWidth
import System.Console.Haskeline.Key

import qualified Control.Monad.Trans.Writer as Writer

----------------------------------------------------------------
-- Low-level terminal output

-- | Keep track of all of the output capabilities we can use.
-- 
-- We'll be frequently using the (automatic) 'Monoid' instance for 
-- @Actions -> TermOutput@.
data Actions = Actions {leftA, rightA, upA :: Int -> TermOutput,
                        clearToLineEnd :: TermOutput,
                        nl, cr :: TermOutput,
                        bellAudible,bellVisual :: TermOutput,
                        clearAllA :: LinesAffected -> TermOutput,
                        wrapLine :: TermOutput}

getActions :: Capability Actions
getActions = do
    -- This capability is not strictly necessary, but is very widely supported
    -- and assuming it makes for a much simpler implementation of printText.
    autoRightMargin >>= guard

    leftA' <- moveLeft
    rightA' <- moveRight
    upA' <- moveUp
    clearToLineEnd' <- clearEOL
    clearAll' <- clearScreen
    nl' <- newline
    cr' <- carriageReturn
    -- Don't require the bell capabilities
    bellAudible' <- bell `mplus` return mempty
    bellVisual' <- visualBell `mplus` return mempty
    wrapLine' <- getWrapLine (leftA' 1)
    return Actions{leftA = leftA', rightA = rightA',upA = upA',
                clearToLineEnd = clearToLineEnd', nl = nl',cr = cr',
                bellAudible = bellAudible', bellVisual = bellVisual',
                clearAllA = clearAll',
                 wrapLine = wrapLine'}

-- If the wraparound glitch is in effect, force a wrap by printing a space.
-- Otherwise, it'll wrap automatically.
getWrapLine :: TermOutput -> Capability TermOutput
getWrapLine left1 = (do
    wraparoundGlitch >>= guard
    return (termText " " <#> left1)
    ) `mplus` return mempty

----------------------------------------------------------------
-- The Draw monad

-- denote in modular arithmetic;
-- in particular, 0 <= termCol < width
data TermPos = TermPos {termRow,termCol :: !Int}
    deriving Show

initTermPos :: TermPos
initTermPos = TermPos {termRow = 0, termCol = 0}

data TermRows = TermRows {
                    rowLengths :: !(Map.IntMap Int),
                    -- ^ The length of each nonempty row
                    lastRow :: !Int
                    -- ^ The last nonempty row, or zero if the entire line
                    -- is empty.  Note that when the cursor wraps to the first
                    -- column of the next line, termRow > lastRow.
                         }
    deriving Show

initTermRows :: TermRows
initTermRows = TermRows {rowLengths = Map.empty, lastRow=0}

setRow :: Int -> Int -> TermRows -> TermRows
setRow r len rs = TermRows {rowLengths = Map.insert r len (rowLengths rs),
                            lastRow=r}

lookupCells :: TermRows -> Int -> Int
lookupCells (TermRows rc _) r = Map.findWithDefault 0 r rc

newtype Draw m a = Draw {unDraw :: (ReaderT Actions
                                    (ReaderT Terminal
                                    (StateT TermRows
                                    (StateT TermPos
                                    (PosixT m))))) a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadException,
              MonadReader Actions, MonadReader Terminal, MonadState TermPos,
              MonadState TermRows, MonadReader Handles)

instance MonadTrans Draw where
    lift = Draw . lift . lift . lift . lift . lift

evalDraw :: forall m . (MonadReader Layout m, CommandMonad m) => Terminal -> Actions -> EvalTerm (PosixT m)
evalDraw term actions = EvalTerm eval liftE
  where
    liftE = Draw . lift . lift . lift . lift
    eval = evalStateT' initTermPos
                            . evalStateT' initTermRows
                            . runReaderT' term
                            . runReaderT' actions
                            . unDraw 
 

runTerminfoDraw :: Handles -> MaybeT IO RunTerm
runTerminfoDraw h = do
    mterm <- liftIO $ Exception.try setupTermFromEnv
    case mterm of
        Left (_::SetupTermError) -> mzero
        Right term -> do
            actions <- MaybeT $ return $ getCapability term getActions
            liftIO $ posixRunTerm h (posixLayouts h ++ [tinfoLayout term])
                (terminfoKeys term)
                (wrapKeypad (ehOut h) term)
                (evalDraw term actions)

-- If the keypad on/off capabilities are defined, wrap the computation with them.
wrapKeypad :: MonadException m => Handle -> Terminal -> m a -> m a
wrapKeypad h term f = (maybeOutput keypadOn >> f)
                            `finally` maybeOutput keypadOff
  where
    maybeOutput = liftIO . hRunTermOutput h term .
                            fromMaybe mempty . getCapability term

tinfoLayout :: Terminal -> IO (Maybe Layout)
tinfoLayout term = return $ getCapability term $ do
                        c <- termColumns
                        r <- termLines
                        return Layout {height=r,width=c}

terminfoKeys :: Terminal -> [(String,Key)]
terminfoKeys term = mapMaybe getSequence keyCapabilities
    where
        getSequence (cap,x) = do
                            keys <- getCapability term cap
                            return (keys,x)
        keyCapabilities =
                [(keyLeft,      simpleKey LeftKey)
                ,(keyRight,      simpleKey RightKey)
                ,(keyUp,         simpleKey UpKey)
                ,(keyDown,       simpleKey DownKey)
                ,(keyBackspace,  simpleKey Backspace)
                ,(keyDeleteChar, simpleKey Delete)
                ,(keyHome,       simpleKey Home)
                ,(keyEnd,        simpleKey End)
                ,(keyPageDown,   simpleKey PageDown)
                ,(keyPageUp,     simpleKey PageUp)
                ,(keyEnter,      simpleKey $ KeyChar '\n')
                ]

    

----------------------------------------------------------------
-- Terminal output actions
--
-- We combine all of the drawing commands into one big TermAction,
-- via a writer monad, and then output them all at once.
-- This prevents flicker, i.e., the cursor appearing briefly
-- in an intermediate position.

type TermAction = Actions -> TermOutput

type ActionT = Writer.WriterT TermAction

type ActionM a = forall m . (MonadReader Layout m, MonadIO m) => ActionT (Draw m) a

runActionT :: MonadIO m => ActionT (Draw m) a -> Draw m a
runActionT m = do
    (x,action) <- Writer.runWriterT m
    toutput <- asks action
    term <- ask
    ttyh <- liftM ehOut ask
    liftIO $ hRunTermOutput ttyh term toutput
    return x

output :: TermAction -> ActionM ()
output t = Writer.tell t  -- NB: explicit argument enables build with ghc-6.12.3
                          -- (Probably related to the monomorphism restriction;
                          -- see GHC ticket #1749).

outputText :: String -> ActionM ()
outputText = output . const . termText

left,right,up :: Int -> TermAction
left = flip leftA
right = flip rightA
up = flip upA

clearAll :: LinesAffected -> TermAction
clearAll = flip clearAllA

mreplicate :: Monoid m => Int -> m -> m
mreplicate n m
    | n <= 0    = mempty
    | otherwise = m `mappend` mreplicate (n-1) m

-- We don't need to bother encoding the spaces.
spaces :: Int -> TermAction
spaces 0 = mempty
spaces 1 = const $ termText " " -- share when possible
spaces n = const $ termText $ replicate n ' '


changePos :: TermPos -> TermPos -> TermAction
changePos TermPos {termRow=r1, termCol=c1} TermPos {termRow=r2, termCol=c2}
    | r1 == r2 = if c1 < c2 then right (c2-c1) else left (c1-c2)
    | r1 > r2 = cr <#> up (r1-r2) <#> right c2
    | otherwise = cr <#> mreplicate (r2-r1) nl <#> right c2

moveToPos :: TermPos -> ActionM ()
moveToPos p = do
    oldP <- get
    put p
    output $ changePos oldP p

moveRelative :: Int -> ActionM ()
moveRelative n = liftM3 (advancePos n) ask get get
                    >>= moveToPos

-- Note that these move by a certain number of cells, not graphemes.
changeRight, changeLeft :: Int -> ActionM ()
changeRight n   | n <= 0 = return ()
                | otherwise = moveRelative n
changeLeft n    | n <= 0 = return ()
                | otherwise = moveRelative (negate n)


-- TODO: this could be more efficient by only checking intermediate rows.
-- TODO: this is worth handling with QuickCheck.
advancePos :: Int -> Layout -> TermRows -> TermPos -> TermPos
advancePos k Layout {width=w} rs p = indexToPos $ k + posIndex
  where
    posIndex = termCol p + sum' (map (lookupCells rs)
                                            [0..termRow p-1])
    indexToPos n = loopFindRow 0 n
    loopFindRow r m = r `seq` m `seq` let
        thisRowSize = lookupCells rs r
        in if m < thisRowSize
                || (m == thisRowSize && m < w)
                || thisRowSize <= 0 -- This shouldn't happen in practice,
                                    -- but double-check to prevent an infinite loop
                then TermPos {termRow=r, termCol=m}
                else loopFindRow (r+1) (m-thisRowSize)

sum' :: [Int] -> Int
sum' = foldl' (+) 0

----------------------------------------------------------------
-- Text printing actions

printText :: [Grapheme] -> ActionM ()
printText [] = return ()
printText gs = do
    -- First, get the monadic parameters:
    w <- asks width
    TermPos {termRow=r, termCol=c} <- get
    -- Now, split off as much as will fit on the rest of this row:
    let (thisLine,rest,thisWidth) = splitAtWidth (w-c) gs
    let lineWidth = c + thisWidth
    -- Finally, actually print out the relevant text.
    outputText (graphemesToString thisLine)
    modify $ setRow r lineWidth
    if null rest && lineWidth < w
        then  -- everything fits on one line without wrapping
            put TermPos {termRow=r, termCol=lineWidth}
        else do -- Must wrap to the next line
            put TermPos {termRow=r+1,termCol=0}
            output $ if lineWidth == w then wrapLine else spaces (w-lineWidth)
            printText rest

----------------------------------------------------------------
-- High-level Term implementation

drawLineDiffT :: LineChars -> LineChars -> ActionM ()
drawLineDiffT (xs1,ys1) (xs2,ys2) = case matchInit xs1 xs2 of
    ([],[])     | ys1 == ys2            -> return ()
    (xs1',[])   | xs1' ++ ys1 == ys2    -> changeLeft (gsWidth xs1')
    ([],xs2')   | ys1 == xs2' ++ ys2    -> changeRight (gsWidth xs2')
    (xs1',xs2')                         -> do
        oldRS <- get
        changeLeft (gsWidth xs1')
        printText xs2'
        p <- get
        printText ys2
        clearDeadText oldRS
        moveToPos p

-- The number of nonempty lines after the current row position.
getLinesLeft :: ActionM Int
getLinesLeft = do
    p <- get
    rc <- get
    return $ max 0 (lastRow rc - termRow p)

clearDeadText :: TermRows -> ActionM ()
clearDeadText oldRS = do
    TermPos {termRow = r, termCol = c} <- get
    let extraRows = lastRow oldRS - r
    if extraRows < 0
            || (extraRows == 0 && lookupCells oldRS r <= c)
        then return ()
        else do
            modify $ setRow r c
            when (extraRows /= 0)
                $ put TermPos {termRow = r + extraRows, termCol=0}
            output $ clearToLineEnd <#> mreplicate extraRows (nl <#> clearToLineEnd)

clearLayoutT :: ActionM ()
clearLayoutT = do
    h <- asks height
    output (clearAll h)
    put initTermPos

moveToNextLineT :: ActionM ()
moveToNextLineT = do
    lleft <- getLinesLeft
    output $ mreplicate (lleft+1) nl
    put initTermPos
    put initTermRows

repositionT :: Layout -> LineChars -> ActionM ()
repositionT _ s = do
    oldPos <- get
    l <- getLinesLeft
    output $ cr <#> mreplicate l nl
            <#> mreplicate (l + termRow oldPos) (clearToLineEnd <#> up 1)
    put initTermPos
    put initTermRows
    drawLineDiffT ([],[]) s

instance (MonadException m, MonadReader Layout m) => Term (Draw m) where
    drawLineDiff xs ys = runActionT $ drawLineDiffT xs ys
    reposition layout lc = runActionT $ repositionT layout lc
    
    printLines = mapM_ $ \line -> runActionT $ do
                                    outputText line
                                    output nl
    clearLayout = runActionT clearLayoutT
    moveToNextLine _ = runActionT moveToNextLineT
    ringBell True = runActionT $ output bellAudible
    ringBell False = runActionT $ output bellVisual
