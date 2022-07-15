module TestUtils
  ( readTestHie
  , render
  , text
  , DynFlags
  , module GHC.Iface.Ext.Types
  , module GHC.Iface.Ext.Utils
  ) where

import System.Environment
import Data.Tree
import GHC.Types.Name.Cache
import GHC.Types.SrcLoc
import GHC.Types.Unique.Supply
import GHC.Types.Name
import GHC.Utils.Outputable                 ( Outputable, renderWithContext, ppr, defaultUserStyle, text)
import GHC.Iface.Ext.Binary
import GHC.Iface.Ext.Types
import GHC.Iface.Ext.Utils
 
import GHC.Driver.Session
import GHC.SysTools

makeNc :: IO NameCache
makeNc = initNameCache 'z' []
 
dynFlagsForPrinting :: String -> IO DynFlags
dynFlagsForPrinting libdir = do
  systemSettings <- initSysTools libdir
  return $ defaultDynFlags systemSettings (LlvmConfig [] [])

readTestHie :: FilePath -> IO (DynFlags, HieFile)
readTestHie fp = do
  libdir:_ <- getArgs
  df <- dynFlagsForPrinting libdir
  nc <- makeNc
  hfr <- readHieFile nc fp
  pure (df, hie_file_result hfr)

render :: Outputable a => DynFlags -> a -> String
render df = renderWithContext (initSDocContext df defaultUserStyle) . ppr
