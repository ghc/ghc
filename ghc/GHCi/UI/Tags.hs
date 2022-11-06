-----------------------------------------------------------------------------
--
-- GHCi's :ctags and :etags commands
--
-- (c) The GHC Team 2005-2007
--
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module GHCi.UI.Tags (
  createCTagsWithLineNumbersCmd,
  createCTagsWithRegExesCmd,
  createETagsFileCmd
) where

import GHC.Utils.Exception
import GHC
import GHCi.UI.Monad

-- ToDo: figure out whether we need these, and put something appropriate
-- into the GHC API instead
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Core.ConLike
import GHC.Utils.Monad
import GHC.Data.FastString

import Control.Monad
import Data.Function
import Data.List (sort, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ord
import GHC.Driver.Phases
import GHC.Utils.Panic
import Prelude
import System.Directory
import System.IO
import System.IO.Error

-----------------------------------------------------------------------------
-- create tags file for currently loaded modules.

createCTagsWithLineNumbersCmd, createCTagsWithRegExesCmd,
  createETagsFileCmd :: String -> GHCi ()

createCTagsWithLineNumbersCmd ""   =
  ghciCreateTagsFile CTagsWithLineNumbers "tags"
createCTagsWithLineNumbersCmd file =
  ghciCreateTagsFile CTagsWithLineNumbers file

createCTagsWithRegExesCmd ""   =
  ghciCreateTagsFile CTagsWithRegExes "tags"
createCTagsWithRegExesCmd file =
  ghciCreateTagsFile CTagsWithRegExes file

createETagsFileCmd ""    = ghciCreateTagsFile ETags "TAGS"
createETagsFileCmd file  = ghciCreateTagsFile ETags file

data TagsKind = ETags | CTagsWithLineNumbers | CTagsWithRegExes

ghciCreateTagsFile :: TagsKind -> FilePath -> GHCi ()
ghciCreateTagsFile kind file = do
  liftIO $ putStrLn "Tags generation from GHCi will be deprecated in GHC 9.8"
  liftIO $ putStrLn "Use the method described in https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/GHCi/Tags"
  createTagsFile kind file

-- ToDo:
--      - remove restriction that all modules must be interpreted
--        (problem: we don't know source locations for entities unless
--        we compiled the module.
--
--      - extract createTagsFile so it can be used from the command-line
--        (probably need to fix first problem before this is useful).
--
createTagsFile :: TagsKind -> FilePath -> GHCi ()
createTagsFile tagskind tagsFile = do
  graph <- GHC.getModuleGraph
  mtags <- mapM listModuleTags (map GHC.ms_mod $ GHC.mgModSummaries graph)
  either_res <- liftIO $ collateAndWriteTags tagskind tagsFile $ concat mtags
  case either_res of
    Left e  -> liftIO $ hPutStrLn stderr $ ioeGetErrorString e
    Right _ -> return ()


listModuleTags :: GHC.Module -> GHCi [TagInfo]
listModuleTags m = do
  is_interpreted <- GHC.moduleIsInterpreted m
  -- should we just skip these?
  when (not is_interpreted) $
    let mName = GHC.moduleNameString (GHC.moduleName m) in
    throwGhcException (CmdLineError ("module '" ++ mName ++ "' is not interpreted"))
  mbModInfo <- GHC.getModuleInfo m
  case mbModInfo of
    Nothing -> return []
    Just mInfo -> do
       let names = fromMaybe [] $ GHC.modInfoTopLevelScope mInfo
       let localNames = filter ((m==) . nameModule) names
       mbTyThings <- mapM GHC.lookupName localNames
       return $! [ tagInfo exported kind name realLoc
                     | tyThing <- catMaybes mbTyThings
                     , let name = getName tyThing
                     , let exported = GHC.modInfoIsExportedName mInfo name
                     , let kind = tyThing2TagKind tyThing
                     , let loc = srcSpanStart (nameSrcSpan name)
                     , RealSrcLoc realLoc _ <- [loc]
                     ]

  where
    tyThing2TagKind (AnId _)                 = 'v'
    tyThing2TagKind (AConLike RealDataCon{}) = 'd'
    tyThing2TagKind (AConLike PatSynCon{})   = 'p'
    tyThing2TagKind (ATyCon _)               = 't'
    tyThing2TagKind (ACoAxiom _)             = 'x'


data TagInfo = TagInfo
  { tagExported :: Bool -- is tag exported
  , tagKind :: Char   -- tag kind
  , tagName :: String -- tag name
  , tagFile :: String -- file name
  , tagLine :: Int    -- line number
  , tagCol :: Int     -- column number
  , tagSrcInfo :: Maybe (String,Integer)  -- source code line and char offset
  }


-- get tag info, for later translation into Vim or Emacs style
tagInfo :: Bool -> Char -> Name -> RealSrcLoc
        -> TagInfo
tagInfo exported kind name loc
    = TagInfo exported kind
        (occNameString $ nameOccName name)
        (unpackFS (srcLocFile loc))
        (srcLocLine loc) (srcLocCol loc) Nothing

-- throw an exception when someone tries to overwrite existing source file (fix for #10989)
writeTagsSafely :: FilePath -> String -> IO ()
writeTagsSafely file str = do
    dfe <- doesFileExist file
    if dfe && isSourceFilename file
        then throwGhcException (CmdLineError (file ++ " is existing source file. " ++
             "Please specify another file name to store tags data"))
        else writeFile file str

collateAndWriteTags :: TagsKind -> FilePath -> [TagInfo] -> IO (Either IOError ())
-- ctags style with the Ex expression being just the line number, Vim et al
collateAndWriteTags CTagsWithLineNumbers file tagInfos = do
  let tags = unlines $ sort $ map showCTag tagInfos
  tryIO (writeTagsSafely file tags)

-- ctags style with the Ex expression being a regex searching the line, Vim et al
collateAndWriteTags CTagsWithRegExes file tagInfos = do -- ctags style, Vim et al
  tagInfoGroups <- makeTagGroupsWithSrcInfo tagInfos
  let tags = unlines $ sort $ map showCTag $ concat tagInfoGroups
  tryIO (writeTagsSafely file tags)

collateAndWriteTags ETags file tagInfos = do -- etags style, Emacs/XEmacs
  tagInfoGroups <- makeTagGroupsWithSrcInfo $ filter tagExported tagInfos
  let tagGroups = map processGroup tagInfoGroups
  tryIO (writeTagsSafely file $ concat tagGroups)

  where
    processGroup [] = throwGhcException (CmdLineError "empty tag file group??")
    processGroup group@(tagInfo:_) =
      let tags = unlines $ map showETag group in
      "\x0c\n" ++ tagFile tagInfo ++ "," ++ show (length tags) ++ "\n" ++ tags


makeTagGroupsWithSrcInfo :: [TagInfo] -> IO [[TagInfo]]
makeTagGroupsWithSrcInfo tagInfos = do
  let groups = NE.groupAllWith tagFile tagInfos
  mapM addTagSrcInfo groups

  where
    addTagSrcInfo group@(tagInfo NE.:| _) = do
      file <- readFile $ tagFile tagInfo
      let sortedGroup = sortOn tagLine (NE.toList group)
      return $ perFile sortedGroup 1 0 $ lines file

    perFile allTags@(tag:tags) cnt pos allLs@(l:ls)
     | tagLine tag > cnt =
         perFile allTags (cnt+1) (pos+fromIntegral(length l)) ls
     | tagLine tag == cnt =
         tag{ tagSrcInfo = Just(l,pos) } : perFile tags cnt pos allLs
    perFile _ _ _ _ = []


-- ctags format, for Vim et al
showCTag :: TagInfo -> String
showCTag ti =
  tagName ti ++ "\t" ++ tagFile ti ++ "\t" ++ tagCmd ++ ";\"\t" ++
    tagKind ti : ( if tagExported ti then "" else "\tfile:" )

  where
    tagCmd =
      case tagSrcInfo ti of
        Nothing -> show $ tagLine ti
        Just (srcLine,_) -> "/^"++ foldr escapeSlashes [] srcLine ++"$/"

      where
        escapeSlashes '/' r = '\\' : '/' : r
        escapeSlashes '\\' r = '\\' : '\\' : r
        escapeSlashes c r = c : r


-- etags format, for Emacs/XEmacs
showETag :: TagInfo -> String
showETag TagInfo{ tagName = tag, tagLine = lineNo, tagCol = colNo,
                  tagSrcInfo = Just (srcLine,charPos) }
    =  take (colNo - 1) srcLine ++ tag
    ++ "\x7f" ++ tag
    ++ "\x01" ++ show lineNo
    ++ "," ++ show charPos
showETag _ = throwGhcException (CmdLineError "missing source file info in showETag")
