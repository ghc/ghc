-----------------------------------------------------------------------------
--
-- GHCi's :ctags and :etags commands
--
-- (c) The GHC Team 2005-2007
--
-----------------------------------------------------------------------------

module GhciTags (createCTagsFileCmd, createETagsFileCmd) where

import GHC
import GhciMonad
import Outputable
import Util

-- ToDo: figure out whether we need these, and put something appropriate
-- into the GHC API instead
import Name (nameOccName)
import OccName (pprOccName)
import MonadUtils

import Data.Maybe
import Panic
import Data.List
import Control.Monad
import System.IO
import System.IO.Error as IO

-----------------------------------------------------------------------------
-- create tags file for currently loaded modules.

createETagsFileCmd, createCTagsFileCmd :: String -> GHCi ()

createCTagsFileCmd ""   = ghciCreateTagsFile CTags "tags"
createCTagsFileCmd file = ghciCreateTagsFile CTags file

createETagsFileCmd ""    = ghciCreateTagsFile ETags "TAGS"
createETagsFileCmd file  = ghciCreateTagsFile ETags file

data TagsKind = ETags | CTags

ghciCreateTagsFile :: TagsKind -> FilePath -> GHCi ()
ghciCreateTagsFile kind file = do
  createTagsFile kind file

-- ToDo: 
-- 	- remove restriction that all modules must be interpreted
--	  (problem: we don't know source locations for entities unless
--	  we compiled the module.
--
--	- extract createTagsFile so it can be used from the command-line
--	  (probably need to fix first problem before this is useful).
--
createTagsFile :: TagsKind -> FilePath -> GHCi ()
createTagsFile tagskind tagFile = do
  graph <- GHC.getModuleGraph
  let ms = map GHC.ms_mod graph
      tagModule m = do 
        is_interpreted <- GHC.moduleIsInterpreted m
        -- should we just skip these?
        when (not is_interpreted) $
          ghcError (CmdLineError ("module '" 
                                ++ GHC.moduleNameString (GHC.moduleName m)
                                ++ "' is not interpreted"))
        mbModInfo <- GHC.getModuleInfo m
        unqual <-
          case mbModInfo of
             Just minf -> do
                mb_print_unqual <- GHC.mkPrintUnqualifiedForModule minf
                return (fromMaybe GHC.alwaysQualify mb_print_unqual)
             Nothing ->
                return GHC.alwaysQualify
        case mbModInfo of 
          Just modInfo -> return $! listTags unqual modInfo 
          _            -> return []

  mtags <- mapM tagModule ms
  either_res <- liftIO $ collateAndWriteTags tagskind tagFile $ concat mtags
  case either_res of
    Left e  -> liftIO $ hPutStrLn stderr $ ioeGetErrorString e
    Right _ -> return ()

listTags :: PrintUnqualified -> GHC.ModuleInfo -> [TagInfo]
listTags unqual modInfo =
	   [ tagInfo unqual name loc 
           | name <- GHC.modInfoExports modInfo
           , let loc = srcSpanStart (nameSrcSpan name)
           , isGoodSrcLoc loc
           ]

type TagInfo = (String -- tag name
               ,String -- file name
               ,Int    -- line number
               ,Int    -- column number
               )

-- get tag info, for later translation into Vim or Emacs style
tagInfo :: PrintUnqualified -> Name -> SrcLoc -> TagInfo
tagInfo unqual name loc
    = ( showSDocForUser unqual $ pprOccName (nameOccName name)
      , showSDocForUser unqual $ ftext (srcLocFile loc)
      , srcLocLine loc
      , srcLocCol loc
      )

collateAndWriteTags :: TagsKind -> FilePath -> [TagInfo] -> IO (Either IOError ())
collateAndWriteTags CTags file tagInfos = do -- ctags style, Vim et al
  let tags = unlines $ sortLe (<=) $ nub $ map showTag tagInfos
  IO.try (writeFile file tags)
collateAndWriteTags ETags file tagInfos = do -- etags style, Emacs/XEmacs
  let byFile op (_,f1,_,_) (_,f2,_,_) = f1 `op` f2
      groups = groupBy (byFile (==)) $ sortLe (byFile (<=)) tagInfos
  tagGroups <- mapM tagFileGroup groups 
  IO.try (writeFile file $ concat tagGroups)
  where
    tagFileGroup [] = ghcError (CmdLineError "empty tag file group??")
    tagFileGroup group@((_,fileName,_,_):_) = do
      file <- readFile fileName -- need to get additional info from sources..
      let byLine (_,_,l1,_) (_,_,l2,_) = l1 <= l2
          sortedGroup = sortLe byLine group
          tags = unlines $ perFile sortedGroup 1 0 $ lines file
      return $ "\x0c\n" ++ fileName ++ "," ++ show (length tags) ++ "\n" ++ tags
    perFile (tagInfo@(_tag, _file, lNo, _colNo):tags) count pos lines@(line:lines')
     | lNo >  count = perFile (tagInfo:tags) (count+1) (pos+length line) lines'
     | lNo == count = showETag tagInfo line pos : perFile tags count pos lines
    perFile _ _ _ _ = []

-- simple ctags format, for Vim et al
showTag :: TagInfo -> String
showTag (tag, file, lineNo, _colNo)
    =  tag ++ "\t" ++ file ++ "\t" ++ show lineNo

-- etags format, for Emacs/XEmacs
showETag :: TagInfo -> String -> Int -> String
showETag (tag, _file, lineNo, colNo) line charPos
    =  take colNo line ++ tag
    ++ "\x7f" ++ tag
    ++ "\x01" ++ show lineNo
    ++ "," ++ show charPos

