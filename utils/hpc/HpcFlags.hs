-- (c) 2007 Andy Gill

module HpcFlags where

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import qualified HpcSet as Set
import Data.Char
import Trace.Hpc.Tix

data Flags = Flags 
  { outputFile		:: String
  , includeMods         :: Set.Set String
  , excludeMods         :: Set.Set String
  , hsDirs		:: [String]
  , hpcDirs		:: [String]
  , destDir		:: String

  , perModule 		:: Bool
  , decList 		:: Bool
  , xmlOutput 		:: Bool

  , funTotals           :: Bool
  , altHighlight        :: Bool

  , combineFun          :: CombineFun
  , postInvert		:: Bool
  }

default_flags = Flags
  { outputFile		= "-"
  , includeMods         = Set.empty
  , excludeMods         = Set.empty
  , hpcDirs             = []
  , hsDirs              = []
  , destDir             = "."

  , perModule           = False
  , decList		= False
  , xmlOutput		= False

  , funTotals           = False
  , altHighlight        = False

  , combineFun          = ADD
  , postInvert		= False
  }

-- We do this after reading flags, because the defaults
-- depends on if specific flags we used.

default_final_flags flags = flags 
  { hpcDirs = if null (hpcDirs flags)
    	      then [".hpc"]
	      else hpcDirs flags
  , hsDirs = if null (hsDirs flags)
    	      then ["."]
	      else hsDirs flags
  }

noArg :: String -> String -> (Flags -> Flags) -> OptDescr (Flags -> Flags)
noArg flag detail fn = Option [] [flag] (NoArg $ fn) detail

anArg :: String -> String -> String -> (String -> Flags -> Flags) -> OptDescr (Flags -> Flags)
anArg flag detail argtype fn = Option [] [flag] (ReqArg fn argtype) detail

infoArg :: String -> OptDescr (Flags -> Flags)
infoArg info = Option [] [] (NoArg $ id) info

excludeOpt    = anArg "exclude"    "exclude MODULE and/or PACKAGE" "[PACKAGE:][MODULE]"  $ \ a f -> f { excludeMods = a `Set.insert` excludeMods f }

includeOpt    = anArg "include"    "include MODULE and/or PACKAGE" "[PACKAGE:][MODULE]"  $ \ a f -> f { includeMods = a `Set.insert` includeMods f }
hpcDirOpt     = anArg "hpcdir"     "path to .mix files (default .hpc)" "DIR"
	      	      		   	 		      $ \ a f -> f { hpcDirs = hpcDirs f ++ [a] }
hsDirOpt      = anArg "hsdir"     "path to .hs files (default .)" "DIR"
	      	      		   	 		      $ \ a f -> f { hsDirs = hsDirs f ++ [a] }
destDirOpt    = anArg "destdir"   "path to write output to" "DIR"
	      	      		  	   	 	      $ \ a f -> f { destDir = a }
outputOpt     = anArg "output"    "output FILE" "FILE"        $ \ a f -> f { outputFile = a }
-- markup

perModuleOpt  = noArg "per-module" "show module level detail" $ \ f -> f { perModule = True }
decListOpt    = noArg "dec-list"   "show unused decls"	      $ \ f -> f { decList = True }
xmlOutputOpt  = noArg "xml-output" "show output in XML"       $ \ f -> f { xmlOutput = True }  
funTotalsOpt  = noArg "fun-entry-count" "show top-level function entry counts"      
							      $ \ f -> f { funTotals = True }  
altHighlightOpt  
	      = noArg "highlight-covered" "highlight covered code, rather that code gaps"
							      $ \ f -> f { altHighlight = True }  

combineFunOpt = anArg "combine" 
	      	      "combine .tix files with join function, default = ADD" "FUNCTION"
	      $ \ a f -> case reads (map toUpper a) of
	      	          [(c,"")] -> f { combineFun = c }
			  _ -> error $ "no such combine function : " ++ a
combineFunOptInfo = infoArg 
		  $ "FUNCTION = " ++ foldr1 (\ a b -> a ++ " | " ++ b) (map fst combineFuns)

postInvertOpt = noArg "post-invert" "invert output; ticked becomes unticked, unticked becomes ticked"
							      $ \ f -> f { funTotals = True }  
-------------------------------------------------------------------------------

command_usage plugin = 
  putStrLn $
				       "Usage: hpc " ++ (name plugin) ++ " " ++ 
				        (usage plugin) ++
				        if null (options plugin)
				        then ""
  	                                else usageInfo "\n\nOptions:\n" (options plugin)

-------------------------------------------------------------------------------

data Plugin = Plugin { name           :: String
     	             , usage          :: String
		     , options        :: [OptDescr (Flags -> Flags)]
		     , summary        :: String
		     , implementation :: Flags -> [String] -> IO ()
		     , init_flags     :: Flags
		     , final_flags    :: Flags -> Flags
		     }

------------------------------------------------------------------------------

-- filterModules takes a list of candidate modules, 
-- and 
--  * excludes the excluded modules
--  * includes the rest if there are no explicity included modules
--  * otherwise, accepts just the included modules.

allowModule :: Flags -> String -> Bool
allowModule flags full_mod 
      | full_mod `Set.member` excludeMods flags = False
      | pkg_name `Set.member` excludeMods flags = False
      | mod_name `Set.member` excludeMods flags = False
      | Set.null (includeMods flags)            = True
      | full_mod `Set.member` includeMods flags = True
      | pkg_name `Set.member` includeMods flags = True
      | mod_name `Set.member` includeMods flags = True
      | otherwise	 	       	        = False
  where
      -- pkg name always ends with '/', main 
	  (pkg_name,mod_name) = 
			case span (/= ':') full_mod of
		     (p,':':m) -> (p ++ ":",m)
		     (m,[])    -> (":",m)
		     _         -> error "impossible case in allowModule" 

filterTix :: Flags -> Tix -> Tix
filterTix flags (Tix tixs) =
     Tix $ filter (allowModule flags . tixModuleName) tixs

------------------------------------------------------------------------------
-- HpcCombine specifics 

data CombineFun = ADD | DIFF | SUB | ZERO
     deriving (Eq,Show, Read, Enum)

combineFuns = [ (show comb,comb) 
	      | comb <- [ADD .. ZERO]
	      ]
