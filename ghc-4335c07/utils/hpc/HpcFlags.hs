-- (c) 2007 Andy Gill

module HpcFlags where

import System.Console.GetOpt
import qualified Data.Set as Set
import Data.Char
import Trace.Hpc.Tix
import Trace.Hpc.Mix
import System.Exit
import System.FilePath

data Flags = Flags
  { outputFile          :: String
  , includeMods         :: Set.Set String
  , excludeMods         :: Set.Set String
  , hpcDirs             :: [String]
  , srcDirs             :: [String]
  , destDir             :: String

  , perModule           :: Bool
  , decList             :: Bool
  , xmlOutput           :: Bool

  , funTotals           :: Bool
  , altHighlight        :: Bool

  , combineFun          :: CombineFun   -- tick-wise combine
  , postFun             :: PostFun      --
  , mergeModule         :: MergeFun     -- module-wise merge

  , verbosity           :: Verbosity
  }

default_flags :: Flags
default_flags = Flags
  { outputFile          = "-"
  , includeMods         = Set.empty
  , excludeMods         = Set.empty
  , hpcDirs             = [".hpc"]
  , srcDirs             = []
  , destDir             = "."

  , perModule           = False
  , decList             = False
  , xmlOutput           = False

  , funTotals           = False
  , altHighlight        = False

  , combineFun          = ADD
  , postFun             = ID
  , mergeModule         = INTERSECTION

  , verbosity           = Normal
  }


data Verbosity = Silent | Normal | Verbose
  deriving (Eq, Ord)

verbosityFromString :: String -> Verbosity
verbosityFromString "0" = Silent
verbosityFromString "1" = Normal
verbosityFromString "2" = Verbose
verbosityFromString v   = error $ "unknown verbosity: " ++ v


-- We do this after reading flags, because the defaults
-- depends on if specific flags we used.

default_final_flags :: Flags -> Flags
default_final_flags flags = flags
  { srcDirs = if null (srcDirs flags)
              then ["."]
              else srcDirs flags
  }

type FlagOptSeq = [OptDescr (Flags -> Flags)] -> [OptDescr (Flags -> Flags)]

noArg :: String -> String -> (Flags -> Flags) -> FlagOptSeq
noArg flag detail fn = (:) $ Option [] [flag] (NoArg $ fn) detail

anArg :: String -> String -> String -> (String -> Flags -> Flags) -> FlagOptSeq
anArg flag detail argtype fn = (:) $ Option [] [flag] (ReqArg fn argtype) detail

infoArg :: String -> FlagOptSeq
infoArg info = (:) $ Option [] [] (NoArg $ id) info

excludeOpt, includeOpt, hpcDirOpt, resetHpcDirsOpt, srcDirOpt,
    destDirOpt, outputOpt, verbosityOpt,
    perModuleOpt, decListOpt, xmlOutputOpt, funTotalsOpt,
    altHighlightOpt, combineFunOpt, combineFunOptInfo, mapFunOpt,
    mapFunOptInfo, unionModuleOpt :: FlagOptSeq
excludeOpt      = anArg "exclude"    "exclude MODULE and/or PACKAGE" "[PACKAGE:][MODULE]"
                $ \ a f -> f { excludeMods = a `Set.insert` excludeMods f }

includeOpt      = anArg "include"    "include MODULE and/or PACKAGE" "[PACKAGE:][MODULE]"
                $ \ a f -> f { includeMods = a `Set.insert` includeMods f }

hpcDirOpt       = anArg "hpcdir"     "append sub-directory that contains .mix files" "DIR"
                   (\ a f -> f { hpcDirs = hpcDirs f ++ [a] })
                .  infoArg "default .hpc [rarely used]"

resetHpcDirsOpt = noArg "reset-hpcdirs" "empty the list of hpcdir's"
                   (\ f -> f { hpcDirs = [] })
                .  infoArg "[rarely used]"

srcDirOpt       = anArg "srcdir"     "path to source directory of .hs files" "DIR"
                  (\ a f -> f { srcDirs = srcDirs f ++ [a] })
                . infoArg "multi-use of srcdir possible"

destDirOpt      = anArg "destdir"   "path to write output to" "DIR"
                $ \ a f -> f { destDir = a }


outputOpt     = anArg "output"    "output FILE" "FILE"        $ \ a f -> f { outputFile = a }

verbosityOpt  = anArg "verbosity" "verbosity level, 0-2" "[0-2]"
                (\ a f -> f { verbosity  = verbosityFromString a })
              . infoArg "default 1"

-- markup

perModuleOpt  = noArg "per-module" "show module level detail" $ \ f -> f { perModule = True }
decListOpt    = noArg "decl-list"  "show unused decls"        $ \ f -> f { decList = True }
xmlOutputOpt  = noArg "xml-output" "show output in XML"       $ \ f -> f { xmlOutput = True }
funTotalsOpt  = noArg "fun-entry-count" "show top-level function entry counts"
                                                              $ \ f -> f { funTotals = True }
altHighlightOpt
              = noArg "highlight-covered" "highlight covered code, rather that code gaps"
                                                              $ \ f -> f { altHighlight = True }

combineFunOpt = anArg "function"
                      "combine .tix files with join function, default = ADD" "FUNCTION"
              $ \ a f -> case reads (map toUpper a) of
                          [(c,"")] -> f { combineFun = c }
                          _ -> error $ "no such combine function : " ++ a
combineFunOptInfo = infoArg
                  $ "FUNCTION = " ++ foldr1 (\ a b -> a ++ " | " ++ b) (map fst foldFuns)

mapFunOpt = anArg "function"
                      "apply function to .tix files, default = ID" "FUNCTION"
              $ \ a f -> case reads (map toUpper a) of
                          [(c,"")] -> f { postFun = c }
                          _ -> error $ "no such combine function : " ++ a
mapFunOptInfo = infoArg
                  $ "FUNCTION = " ++ foldr1 (\ a b -> a ++ " | " ++ b) (map fst postFuns)

unionModuleOpt = noArg "union"
                      "use the union of the module namespace (default is intersection)"
              $ \ f -> f { mergeModule = UNION }


-------------------------------------------------------------------------------

readMixWithFlags :: Flags -> Either String TixModule -> IO Mix
readMixWithFlags flags modu = readMix [ dir </> hpcDir
                                      | dir <- srcDirs flags
                                      , hpcDir <- hpcDirs flags
                                      ] modu

-------------------------------------------------------------------------------

command_usage :: Plugin -> IO ()
command_usage plugin =
  putStrLn $
                                       "Usage: hpc " ++ (name plugin) ++ " " ++
                                        (usage plugin) ++
                                        "\n" ++ summary plugin ++ "\n" ++
                                        if null (options plugin [])
                                        then ""
                                        else usageInfo "\n\nOptions:\n" (options plugin [])

hpcError :: Plugin -> String -> IO a
hpcError plugin msg = do
   putStrLn $ "Error: " ++ msg
   command_usage plugin
   exitFailure

-------------------------------------------------------------------------------

data Plugin = Plugin { name           :: String
                     , usage          :: String
                     , options        :: FlagOptSeq
                     , summary        :: String
                     , implementation :: Flags -> [String] -> IO ()
                     , init_flags     :: Flags
                     , final_flags    :: Flags -> Flags
                     }

------------------------------------------------------------------------------

-- filterModules takes a list of candidate modules,
-- and
--  * excludes the excluded modules
--  * includes the rest if there are no explicitly included modules
--  * otherwise, accepts just the included modules.

allowModule :: Flags -> String -> Bool
allowModule flags full_mod
      | full_mod' `Set.member` excludeMods flags = False
      | pkg_name  `Set.member` excludeMods flags = False
      | mod_name  `Set.member` excludeMods flags = False
      | Set.null (includeMods flags)             = True
      | full_mod' `Set.member` includeMods flags = True
      | pkg_name  `Set.member` includeMods flags = True
      | mod_name  `Set.member` includeMods flags = True
      | otherwise                                = False
  where
          full_mod' = pkg_name ++ mod_name
      -- pkg name always ends with '/', main
          (pkg_name,mod_name) =
                        case span (/= '/') full_mod of
                     (p,'/':m) -> (p ++ ":",m)
                     (m,[])    -> (":",m)
                     _         -> error "impossible case in allowModule"

filterTix :: Flags -> Tix -> Tix
filterTix flags (Tix tixs) =
     Tix $ filter (allowModule flags . tixModuleName) tixs



------------------------------------------------------------------------------
-- HpcCombine specifics

data CombineFun = ADD | DIFF | SUB
     deriving (Eq,Show, Read, Enum)

theCombineFun :: CombineFun -> Integer -> Integer -> Integer
theCombineFun fn = case fn of
            ADD  -> \ l r -> l + r
            SUB  -> \ l r -> max 0 (l - r)
            DIFF -> \ g b -> if g > 0 then 0 else min 1 b

foldFuns :: [ (String,CombineFun) ]
foldFuns = [ (show comb,comb)
           | comb <- [ADD .. SUB]
           ]

data PostFun = ID | INV | ZERO
     deriving (Eq,Show, Read, Enum)

thePostFun :: PostFun -> Integer -> Integer
thePostFun ID   x = x
thePostFun INV  0 = 1
thePostFun INV  _ = 0
thePostFun ZERO _ = 0

postFuns :: [(String, PostFun)]
postFuns = [ (show pos,pos)
             | pos <- [ID .. ZERO]
           ]


data MergeFun = INTERSECTION | UNION
     deriving (Eq,Show, Read, Enum)

theMergeFun :: (Ord a) => MergeFun -> Set.Set a -> Set.Set a -> Set.Set a
theMergeFun INTERSECTION = Set.intersection
theMergeFun UNION        = Set.union

mergeFuns :: [(String, MergeFun)]
mergeFuns = [ (show pos,pos)
             | pos <- [INTERSECTION,UNION]
           ]

