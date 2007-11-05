{-# OPTIONS_GHC -XCPP #-}
module Main where

import GHC
import BasicTypes
import Digraph  ( flattenSCCs )
import DriverPhases ( isHaskellSrcFilename )
import HscTypes (msHsFilePath)
import Name
import Outputable
import ErrUtils ( printBagOfErrors )
import DynFlags(GhcMode, defaultDynFlags)
import SrcLoc
import Bag
import Util ( handle, handleDyn )
import FastString

import Control.Monad
import System.Environment
import System.Console.GetOpt
import System.Exit
import Data.Char
import System.IO
import Data.List as List
import Data.Maybe

-- search for definitions of things 
-- we do this by parsing the source and grabbing top-level definitions

-- We generate both CTAGS and ETAGS format tags files
-- The former is for use in most sensible editors, while EMACS uses ETAGS

----------------------------------
---- CENTRAL DATA TYPES ----------

type FileName = String
type ThingName = String -- name of a defined entity in a Haskell program

-- A definition we have found (we know its containing module, name, and location)
data FoundThing = FoundThing ModuleName ThingName SrcLoc

-- Data we have obtained from a file (list of things we found)
data FileData = FileData FileName [FoundThing]
--- invariant (not checked): every found thing has a source location in that file?


------------------------------
-------- MAIN PROGRAM --------

main :: IO ()
main = do
        progName <- getProgName
        let usageString =
              "Usage: " ++ progName ++ " [OPTION...] [-- GHC OPTION... --] [files...]"
	args <- getArgs
        let (ghcArgs, ourArgs, unbalanced) = splitArgs args
	let (flags, filenames, errs) = getOpt Permute options ourArgs
        let (hsfiles, otherfiles) = List.partition isHaskellSrcFilename filenames
        let ghc_topdir = case [ d | FlagTopDir d <- flags ] of
                                [] -> ""
                                (x:_) -> x
        mapM_ (\n -> putStr $ "Warning: ignoring non-Haskellish file " ++ n ++ "\n")
              otherfiles
	if unbalanced || errs /= [] || elem FlagHelp flags || hsfiles == []
         then do
           putStr $ unlines errs 
	   putStr $ usageInfo usageString options
	   exitWith (ExitFailure 1)
         else return ()

        let modes = getMode flags
        let openFileMode = if elem FlagAppend flags
		     then AppendMode
		     else WriteMode
        ctags_hdl <-  if CTags `elem` modes
                           then Just `liftM` openFile "tags" openFileMode
                           else return Nothing
        etags_hdl <- if ETags `elem` modes
                           then Just `liftM` openFile "TAGS" openFileMode
                           else return Nothing

        GHC.defaultErrorHandler defaultDynFlags $ do
          session <- newSession (Just ghc_topdir)
          flags <- getSessionDynFlags session
          (pflags, _) <- parseDynamicFlags flags{ verbosity=1 } ghcArgs
          let flags = pflags { hscTarget = HscNothing } -- don't generate anything
          GHC.defaultCleanupHandler flags $ do
  
          setSessionDynFlags session flags
          targetsAtOneGo session hsfiles (ctags_hdl,etags_hdl)

----------------------------------------------
----------  ARGUMENT PROCESSING --------------

data Flag
   = FlagETags
   | FlagCTags
   | FlagBoth
   | FlagAppend
   | FlagHelp
   | FlagTopDir FilePath
  deriving (Ord, Eq, Show)
  -- ^Represents options passed to the program

data Mode = ETags | CTags deriving Eq

getMode :: [Flag] -> [Mode]
getMode fs = go (concatMap modeLike fs)
 where go []     = [ETags,CTags]
       go [x]    = [x]
       go more   = nub more

       modeLike FlagETags = [ETags]
       modeLike FlagCTags = [CTags]
       modeLike FlagBoth  = [ETags,CTags]
       modeLike _         = []

splitArgs :: [String] -> ([String], [String], Bool)
-- ^Pull out arguments between -- for GHC
splitArgs args = split [] [] False args
    where split ghc' tags' unbal ("--" : args) = split tags' ghc' (not unbal) args
          split ghc' tags' unbal (arg : args) = split ghc' (arg:tags') unbal args
          split ghc' tags' unbal [] = (reverse ghc', reverse tags', unbal)

options :: [OptDescr Flag]
-- supports getopt
options = [ Option "" ["topdir"] 
            (ReqArg FlagTopDir "DIR") "root of GHC installation (optional)"
          , Option "c" ["ctags"]
	    (NoArg FlagCTags) "generate CTAGS file (ctags)"
	  , Option "e" ["etags"]
	    (NoArg FlagETags) "generate ETAGS file (etags)"
	  , Option "b" ["both"]
	    (NoArg FlagBoth) ("generate both CTAGS and ETAGS")
	  , Option "a" ["append"]
	    (NoArg FlagAppend) ("append to existing CTAGS and/or ETAGS file(s)")
	  , Option "h" ["help"] (NoArg FlagHelp) "This help"
	  ]


----------------------------------------------------------------
--- LOADING HASKELL SOURCE
--- (these bits actually run the compiler and produce abstract syntax)

safeLoad :: Session -> LoadHowMuch -> IO SuccessFlag
-- like GHC.load, but does not stop process on exception
safeLoad session mode = do
  dflags <- getSessionDynFlags session
  handle (\exception -> return Failed ) $
    handleDyn (\dyn -> do printBagOfErrors dflags (unitBag dyn)
	  		  return Failed) $ load session mode


targetsAtOneGo :: Session -> [FileName] -> (Maybe Handle, Maybe Handle) -> IO ()
-- load a list of targets
targetsAtOneGo session hsfiles handles = do
  targets <- mapM (\f -> guessTarget f Nothing) hsfiles
  setTargets session targets
  putStrLn $ "Load it all:"
  flag <- load session LoadAllTargets
  when (failed flag) $ exitWith (ExitFailure 1)
  modgraph <- getModuleGraph session
  let mods = flattenSCCs $ topSortModuleGraph False modgraph Nothing
  graphData session mods handles

  where targetInfo [hs] = "target " ++ hs
        targetInfo hss  = show (length hss) ++ " targets at one go"

fileTarget :: FileName -> Target
fileTarget filename = Target (TargetFile filename Nothing) Nothing

---------------------------------------------------------------
----- CRAWLING ABSTRACT SYNTAX TO SNAFFLE THE DEFINITIONS -----

graphData :: Session -> ModuleGraph -> (Maybe Handle, Maybe Handle) -> IO ()
graphData session graph handles = do
    mapM_ foundthings graph
    where foundthings ms =
              let filename = msHsFilePath ms
                  modname = moduleName $ ms_mod ms
              in  do mod <- checkModule session modname False
                     let fd = maybe (FileData filename []) id $ do
                                m <- mod
                                s <- renamedSource m
                                return $ fileData filename modname s
                     writeTagsData handles fd

fileData :: FileName -> ModuleName -> RenamedSource -> FileData
fileData filename modname (group, _imports, _lie, _doc, _haddock) =
    -- lie is related to type checking and so is irrelevant
    -- imports contains import declarations and no definitions
    -- doc and haddock seem haddock-related; let's hope to ignore them
    FileData filename (boundValues modname group)

boundValues :: ModuleName -> HsGroup Name -> [FoundThing]    
-- ^Finds all the top-level definitions in a module
boundValues mod group =
  let vals = case hs_valds group of
               ValBindsOut nest sigs ->
                   [ x | (_rec, binds) <- nest, bind <- bagToList binds,
                              x <- boundThings mod bind ]
      tys = concat $ map tyBound (hs_tyclds group)
            where tyBound ltcd = case unLoc ltcd of
                                   ForeignType { tcdLName = n } -> [found n]
                                   TyData { tcdLName = tycon, tcdCons = cons } ->
                                       dataNames tycon cons
                                   TySynonym { tcdLName = n } -> [found n]
                                   ClassDecl {	tcdLName = n } -> [found n]
      fors = concat $ map forBound (hs_fords group)
             where forBound lford = case unLoc lford of
                                      ForeignImport n _ _ -> [found n]
                                      ForeignExport { } -> []
  in vals ++ tys ++ fors
  where dataNames tycon cons = found tycon : map conName cons
        conName td = found $ con_name $ unLoc td
        found = foundOfLName mod

startOfLocated :: Located a -> SrcLoc
startOfLocated lHs = srcSpanStart $ getLoc lHs

foundOfLName :: ModuleName -> Located Name -> FoundThing
foundOfLName mod id = FoundThing mod (getOccString $ unLoc id) (startOfLocated id)

boundThings :: ModuleName -> LHsBind Name -> [FoundThing]
boundThings modname lbinding = 
  case unLoc lbinding of
    FunBind { fun_id = id } -> [thing id]
    PatBind { pat_lhs = lhs } -> patThings lhs []
    VarBind { var_id = id } -> [FoundThing modname (getOccString id) (startOfLocated lbinding)]
    AbsBinds { } -> [] -- nothing interesting in a type abstraction
  where thing = foundOfLName modname
        patThings lpat tl =
          let loc = startOfLocated lpat
              lid id = FoundThing modname (getOccString id) loc
          in case unLoc lpat of
               WildPat _ -> tl
               VarPat name -> lid name : tl
               VarPatOut name _ -> lid name : tl -- XXX need help here
               LazyPat p -> patThings p tl
               AsPat id p -> patThings p (thing id : tl)
               ParPat p -> patThings p tl
               BangPat p -> patThings p tl
               ListPat ps _ -> foldr patThings tl ps
               TuplePat ps _ _ -> foldr patThings tl ps
               PArrPat ps _ -> foldr patThings tl ps
               ConPatIn _ conargs -> conArgs conargs tl
               ConPatOut _ _ _ _ conargs _ -> conArgs conargs tl
               LitPat _ -> tl
#if __GLASGOW_HASKELL__ > 608
               NPat _ _ _ -> tl -- form of literal pattern?
#else
               NPat _ _ _ _ -> tl -- form of literal pattern?
#endif
               NPlusKPat id _ _ _ -> thing id : tl
               TypePat _ -> tl -- XXX need help here
               SigPatIn p _ -> patThings p tl
               SigPatOut p _ -> patThings p tl
        conArgs (PrefixCon ps) tl = foldr patThings tl ps
        conArgs (RecCon (HsRecFields { rec_flds = flds })) tl 
             = foldr (\f tl -> patThings (hsRecFieldArg f) tl) tl flds
        conArgs (InfixCon p1 p2) tl = patThings p1 $ patThings p2 tl


-- stuff for dealing with ctags output format

writeTagsData (mb_ctags_hdl, mb_etags_hdl) fd = do 
  maybe (return ()) (\hdl -> writectagsfile hdl fd) mb_ctags_hdl
  maybe (return ()) (\hdl -> writeetagsfile hdl fd) mb_etags_hdl

writectagsfile :: Handle -> FileData -> IO ()
writectagsfile ctagsfile filedata = do
	let things = getfoundthings filedata
	mapM_ (\x -> hPutStrLn ctagsfile $ dumpthing False x) things
	mapM_ (\x -> hPutStrLn ctagsfile $ dumpthing True  x) things

getfoundthings :: FileData -> [FoundThing]
getfoundthings (FileData filename things) = things

dumpthing :: Bool -> FoundThing -> String
dumpthing showmod (FoundThing modname name loc) =
	fullname ++ "\t" ++ filename ++ "\t" ++ (show $ line + 1)
    where line = srcLocLine loc
          filename = unpackFS $ srcLocFile loc
          fullname = if showmod then moduleNameString modname ++ "." ++ name
                     else name

-- stuff for dealing with etags output format

writeetagsfile :: Handle -> FileData -> IO ()
writeetagsfile etagsfile = hPutStr etagsfile . e_dumpfiledata

e_dumpfiledata :: FileData -> String
e_dumpfiledata (FileData filename things) = 
	"\x0c\n" ++ filename ++ "," ++ (show thingslength) ++ "\n" ++ thingsdump
	where 
		thingsdump = concat $ map e_dumpthing things 
		thingslength = length thingsdump

e_dumpthing :: FoundThing -> String
e_dumpthing (FoundThing modname name loc) =
    tagline name ++ tagline (moduleNameString modname ++ "." ++ name)
    where tagline n = n ++ "\x7f" ++ (show line) ++ "," ++ (show $ line+1) ++ "\n"
          line = srcLocLine loc
