module Main where
import Bag
import Char
import DynFlags(GhcMode, defaultDynFlags)
import FastString
import GHC
import HscTypes (msHsFilePath)
import List
import IO
import Name
import Outputable
import SrcLoc
import System.Environment
import System.Console.GetOpt
import System.Exit


-- search for definitions of things 
-- we do this by parsing the source and grabbing top-level definitions

-- We generate both CTAGS and ETAGS format tags files
-- The former is for use in most sensible editors, while EMACS uses ETAGS

{-
placateGhc :: IO ()
placateGhc = defaultErrorHandler defaultDynFlags $ do
  GHC.init (Just "/usr/local/lib/ghc-6.5")  -- or your build tree!
  s <- newSession mode
-}

main :: IO ()
main = do
        progName <- getProgName
        let usageString =
              "Usage: " ++ progName ++ " [OPTION...] [-- GHC OPTION... --] [files...]"
	args <- getArgs
        let (ghcArgs, ourArgs, unbalanced) = splitArgs args
	let (modes, filenames, errs) = getOpt Permute options ourArgs
	if unbalanced || errs /= [] || elem Help modes || filenames == []
         then do
           putStr $ unlines errs 
	   putStr $ usageInfo usageString options
	   exitWith (ExitFailure 1)
         else return ()
        let mode = getMode (Append `delete` modes)
        let openFileMode = if elem Append modes
			   then AppendMode
			   else WriteMode
        GHC.init (Just "/usr/local/lib/ghc-6.5")
        GHC.defaultErrorHandler defaultDynFlags $ do
          session <- newSession JustTypecheck
          print "created a session"
          flags <- getSessionDynFlags session
          (pflags, _) <- parseDynamicFlags flags ghcArgs
          let flags = pflags { hscTarget = HscNothing }
          GHC.defaultCleanupHandler flags $ do
            flags <- initPackages flags
            setSessionDynFlags session flags
          setTargets session (map fileTarget filenames)
          print "set targets"
          success <- load session LoadAllTargets  --- bring module graph up to date
          filedata <- case success of
                        Failed -> do { putStr "Load failed"; exitWith (ExitFailure 2) }
                        Succeeded -> do
                                     print "loaded all targets"
                                     graph <- getModuleGraph session
                                     print "got modules graph"
                                     graphData session graph
          if mode == BothTags || mode == CTags
           then do 
             ctagsfile <- openFile "tags" openFileMode
             writectagsfile ctagsfile filedata
             hClose ctagsfile
           else return ()
          if mode == BothTags || mode == ETags 
           then do
             etagsfile <- openFile "TAGS" openFileMode
             writeetagsfile etagsfile filedata
             hClose etagsfile
           else return ()

-- | getMode takes a list of modes and extract the mode with the
--   highest precedence.  These are as follows: Both, CTags, ETags
--   The default case is Both.
getMode :: [Mode] -> Mode
getMode [] = BothTags
getMode [x] = x
getMode (x:xs) = max x (getMode xs)


splitArgs :: [String] -> ([String], [String], Bool)
-- pull out arguments between -- for GHC
splitArgs args = split [] [] False args
    where split ghc' tags' unbal ("--" : args) = split tags' ghc' (not unbal) args
          split ghc' tags' unbal (arg : args) = split ghc' (arg:tags') unbal args
          split ghc' tags' unbal [] = (reverse ghc', reverse tags', unbal)

data Mode = ETags | CTags | BothTags | Append | Help deriving (Ord, Eq, Show)

options :: [OptDescr Mode]
options = [ Option "c" ["ctags"]
	    (NoArg CTags) "generate CTAGS file (ctags)"
	  , Option "e" ["etags"]
	    (NoArg ETags) "generate ETAGS file (etags)"
	  , Option "b" ["both"]
	    (NoArg BothTags) ("generate both CTAGS and ETAGS")
	  , Option "a" ["append"]
	    (NoArg Append) ("append to existing CTAGS and/or ETAGS file(s)")
	  , Option "h" ["help"] (NoArg Help) "This help"
	  ]

type FileName = String

type ThingName = String

-- The position of a token or definition
data Pos = Pos 
		FileName 	-- file name
		Int			-- line number 
		Int     	-- token number
		String 		-- string that makes up that line
	deriving Show

srcLocToPos :: SrcLoc -> Pos
srcLocToPos loc =
    Pos (unpackFS $ srcLocFile loc) (srcLocLine loc) (srcLocCol loc) "bogus"

-- A definition we have found
data FoundThing = FoundThing ThingName Pos
	deriving Show

-- Data we have obtained from a file
data FileData = FileData FileName [FoundThing]

data Token = Token String Pos
	deriving Show


-- stuff for dealing with ctags output format

writectagsfile :: Handle -> [FileData] -> IO ()
writectagsfile ctagsfile filedata = do
	let things = concat $ map getfoundthings filedata
	mapM_ (\x -> hPutStrLn ctagsfile $ dumpthing x) things

getfoundthings :: FileData -> [FoundThing]
getfoundthings (FileData filename things) = things

dumpthing :: FoundThing -> String
dumpthing (FoundThing name (Pos filename line _ _)) = 
	name ++ "\t" ++ filename ++ "\t" ++ (show $ line + 1)


-- stuff for dealing with etags output format

writeetagsfile :: Handle -> [FileData] -> IO ()
writeetagsfile etagsfile filedata = do
	mapM_ (\x -> hPutStr etagsfile $ e_dumpfiledata x) filedata

e_dumpfiledata :: FileData -> String
e_dumpfiledata (FileData filename things) = 
	"\x0c\n" ++ filename ++ "," ++ (show thingslength) ++ "\n" ++ thingsdump
	where 
		thingsdump = concat $ map e_dumpthing things 
		thingslength = length thingsdump

e_dumpthing :: FoundThing -> String
e_dumpthing (FoundThing name (Pos filename line token fullline)) =
	---- (concat $ take (token + 1) $ spacedwords fullline) 
        name
	++ "\x7f" ++ (show line) ++ "," ++ (show $ line+1) ++ "\n"
	
	
-- like "words", but keeping the whitespace, and so letting us build
-- accurate prefixes	
	
spacedwords :: String -> [String]
spacedwords [] = []
spacedwords xs = (blanks ++ wordchars):(spacedwords rest2)
	where 
		(blanks,rest) = span Char.isSpace xs
		(wordchars,rest2) = span (\x -> not $ Char.isSpace x) rest
	
	
-- Find the definitions in a file	
	
modsummary :: ModuleGraph -> FileName -> Maybe ModSummary
modsummary graph n = 
  List.find matches graph
  where matches ms = n == msHsFilePath ms

modname :: ModSummary -> ModuleName
modname summary = moduleName $ ms_mod $ summary

fileTarget :: FileName -> Target
fileTarget filename = Target (TargetFile filename Nothing) Nothing

graphData :: Session -> ModuleGraph -> IO [FileData]
graphData session graph =
    mapM foundthings graph
    where foundthings ms =
              let filename = msHsFilePath ms
              in  do mod <- checkModule session (moduleName $ ms_mod ms)
                     return $ maybe (FileData filename []) id $ do
                       m <- mod
                       s <- renamedSource m
                       return $ fileData filename s

fileData :: FileName -> RenamedSource -> FileData
fileData filename (group, imports, lie) =
    -- lie is related to type checking and so is irrelevant
    -- imports contains import declarations and no definitions
    FileData filename (boundValues group)

boundValues :: HsGroup Name -> [FoundThing]    
boundValues group =
  let vals = case hs_valds group of
               ValBindsOut nest sigs ->
                   [ x | (_rec, binds) <- nest, bind <- bagToList binds, x <- boundThings bind ]
      tys = concat $ map tyBound (hs_tyclds group)
            where tyBound ltcd = case unLoc ltcd of
                                   ForeignType { tcdLName = n } -> [foundOfLName n]
                                   TyData { tcdLName = n } -> [foundOfLName n]
                                   TySynonym { tcdLName = n } -> [foundOfLName n]
                                   ClassDecl {	tcdLName = n } -> [foundOfLName n]
      fors = concat $ map forBound (hs_fords group)
             where forBound lford = case unLoc lford of
                                      ForeignImport n _ _ -> [foundOfLName n]
                                      ForeignExport { } -> []
  in vals ++ tys ++ fors

posOfLocated :: Located a -> Pos
posOfLocated lHs = srcLocToPos $ srcSpanStart $ getLoc lHs

foundOfLName :: Located Name -> FoundThing
foundOfLName id = FoundThing (getOccString $ unLoc id) (posOfLocated id)

boundThings :: LHsBind Name -> [FoundThing]
boundThings lbinding = 
  let thing = foundOfLName
  in  case unLoc lbinding of
        FunBind { fun_id = id } -> [thing id]
        PatBind { pat_lhs = lhs } -> panic "Pattern at top level"
        VarBind { var_id = id } -> [FoundThing (getOccString id) (posOfLocated lbinding)]
        AbsBinds { } -> [] -- nothing interesting in a type abstraction
