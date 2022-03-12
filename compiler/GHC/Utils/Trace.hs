-- | Tracing utilities
module GHC.Utils.Trace
  ( pprTrace
  , pprTraceM
  , pprTraceDebug
  , pprTraceIt
  , pprTraceWith
  , pprSTrace
  , pprTraceException
  , warnPprTrace
  , pprTraceUserWarning
  , trace
  )
where

import GHC.Prelude
import GHC.Utils.Outputable
import GHC.Utils.Exception
import GHC.Utils.Panic
import GHC.Utils.GlobalVars
import GHC.Utils.Constants
import GHC.Stack

import Debug.Trace (trace)
import Control.Monad.IO.Class

-- | If debug output is on, show some 'SDoc' on the screen
pprTrace :: String -> SDoc -> a -> a
pprTrace str doc x
  | unsafeHasNoDebugOutput = x
  | otherwise              = pprDebugAndThen defaultSDocContext trace (text str) doc x

pprTraceM :: Applicative f => String -> SDoc -> f ()
pprTraceM str doc = pprTrace str doc (pure ())

pprTraceDebug :: String -> SDoc -> a -> a
pprTraceDebug str doc x
   | debugIsOn && unsafeHasPprDebug = pprTrace str doc x
   | otherwise                      = x

-- | @pprTraceWith desc f x@ is equivalent to @pprTrace desc (f x) x@.
-- This allows you to print details from the returned value as well as from
-- ambient variables.
pprTraceWith :: String -> (a -> SDoc) -> a -> a
pprTraceWith desc f x = pprTrace desc (f x) x

-- | @pprTraceIt desc x@ is equivalent to @pprTrace desc (ppr x) x@
pprTraceIt :: Outputable a => String -> a -> a
pprTraceIt desc x = pprTraceWith desc ppr x

-- | @pprTraceException desc x action@ runs action, printing a message
-- if it throws an exception.
pprTraceException :: ExceptionMonad m => String -> SDoc -> m a -> m a
pprTraceException heading doc =
    handleGhcException $ \exc -> liftIO $ do
        putStrLn $ renderWithContext defaultSDocContext
                 $ withPprStyle defaultDumpStyle
                 $ sep [text heading, nest 2 doc]
        throwGhcExceptionIO exc

-- | If debug output is on, show some 'SDoc' on the screen along
-- with a call stack when available.
pprSTrace :: HasCallStack => SDoc -> a -> a
pprSTrace doc = pprTrace "" (doc $$ traceCallStackDoc)

-- | Just warn about an assertion failure, recording the given file and line number.
warnPprTrace :: HasCallStack => Bool -> String -> SDoc -> a -> a
warnPprTrace _     _s _    x | not debugIsOn     = x
warnPprTrace _     _s _msg x | unsafeHasNoDebugOutput = x
warnPprTrace False _s _msg x = x
warnPprTrace True   s  msg x
  = pprDebugAndThen defaultSDocContext trace (text "WARNING:")
                    (text s $$ msg $$ withFrozenCallStack traceCallStackDoc )
                    x

-- | For when we want to show the user a non-fatal WARNING so that they can
-- report a GHC bug, but don't want to panic.
pprTraceUserWarning :: HasCallStack => SDoc -> a -> a
pprTraceUserWarning msg x
  | unsafeHasNoDebugOutput = x
  | otherwise = pprDebugAndThen defaultSDocContext trace (text "WARNING:")
                    (msg $$ withFrozenCallStack traceCallStackDoc )
                    x

traceCallStackDoc :: HasCallStack => SDoc
traceCallStackDoc =
    hang (text "Call stack:")
       4 (vcat $ map text $ lines (prettyCallStack callStack))
