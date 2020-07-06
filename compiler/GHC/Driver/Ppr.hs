-- | Printing related functions that depend on session state (DynFlags)
module GHC.Driver.Ppr
   ( showSDoc
   , showSDocUnsafe
   , showSDocForUser
   , showSDocDebug
   , showSDocDump
   , showPpr
   , showPprUnsafe
   , pprDebugAndThen
   , printForUser
   , printForC
   -- ** Trace
   , warnPprTrace
   , pprTrace
   , pprTraceWithFlags
   , pprTraceM
   , pprTraceDebug
   , pprTraceIt
   , pprSTrace
   , pprTraceException
   )
where

import GHC.Prelude

import {-# SOURCE #-} GHC.Driver.Session

import GHC.Utils.Exception
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.GlobalVars
import GHC.Utils.Ppr       ( Mode(..) )
import {-# SOURCE #-} GHC.Unit.State

import System.IO ( Handle )
import Control.Monad.IO.Class

-- | Show a SDoc as a String with the default user style
showSDoc :: DynFlags -> SDoc -> String
showSDoc dflags sdoc = renderWithContext (initSDocContext dflags defaultUserStyle) sdoc

showSDocUnsafe :: SDoc -> String
showSDocUnsafe sdoc = renderWithContext defaultSDocContext sdoc

showPpr :: Outputable a => DynFlags -> a -> String
showPpr dflags thing = showSDoc dflags (ppr thing)

showPprUnsafe :: Outputable a => a -> String
showPprUnsafe a = renderWithContext defaultSDocContext (ppr a)

-- | Allows caller to specify the PrintUnqualified to use
showSDocForUser :: DynFlags -> PrintUnqualified -> SDoc -> String
showSDocForUser dflags unqual doc = renderWithContext (initSDocContext dflags sty) doc'
   where
      sty        = mkUserStyle unqual AllTheWay
      unit_state = unitState dflags
      doc'       = pprWithUnitState unit_state doc

showSDocDump :: SDocContext -> SDoc -> String
showSDocDump ctx d = renderWithContext ctx (withPprStyle defaultDumpStyle d)

showSDocDebug :: DynFlags -> SDoc -> String
showSDocDebug dflags d = renderWithContext ctx d
   where
      ctx = (initSDocContext dflags defaultDumpStyle)
               { sdocPprDebug = True
               }

printForUser :: DynFlags -> Handle -> PrintUnqualified -> Depth -> SDoc -> IO ()
printForUser dflags handle unqual depth doc
  = printSDocLn ctx PageMode handle doc
    where ctx = initSDocContext dflags (mkUserStyle unqual depth)

-- | Like 'printSDocLn' but specialized with 'LeftMode' and
-- @'PprCode' 'CStyle'@.  This is typically used to output C-- code.
printForC :: DynFlags -> Handle -> SDoc -> IO ()
printForC dflags handle doc =
  printSDocLn ctx LeftMode handle doc
  where ctx = initSDocContext dflags (PprCode CStyle)

pprDebugAndThen :: SDocContext -> (String -> a) -> SDoc -> SDoc -> a
pprDebugAndThen ctx cont heading pretty_msg
 = cont (showSDocDump ctx doc)
 where
     doc = sep [heading, nest 2 pretty_msg]

-- | If debug output is on, show some 'SDoc' on the screen
pprTraceWithFlags :: DynFlags -> String -> SDoc -> a -> a
pprTraceWithFlags dflags str doc x
  | hasNoDebugOutput dflags = x
  | otherwise               = pprDebugAndThen (initSDocContext dflags defaultDumpStyle)
                                              trace (text str) doc x

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
        putStrLn $ showSDocDump defaultSDocContext (sep [text heading, nest 2 doc])
        throwGhcExceptionIO exc

-- | If debug output is on, show some 'SDoc' on the screen along
-- with a call stack when available.
pprSTrace :: HasCallStack => SDoc -> a -> a
pprSTrace doc = pprTrace "" (doc $$ callStackDoc)

warnPprTrace :: HasCallStack => Bool -> String -> Int -> SDoc -> a -> a
-- ^ Just warn about an assertion failure, recording the given file and line number.
-- Should typically be accessed with the WARN macros
warnPprTrace _     _     _     _    x | not debugIsOn     = x
warnPprTrace _     _file _line _msg x
   | unsafeHasNoDebugOutput = x
warnPprTrace False _file _line _msg x = x
warnPprTrace True   file  line  msg x
  = pprDebugAndThen defaultSDocContext trace heading
                    (msg $$ callStackDoc )
                    x
  where
    heading = hsep [text "WARNING: file", text file <> comma, text "line", int line]
