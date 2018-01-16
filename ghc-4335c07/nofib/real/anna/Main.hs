 
-- ==========================================================--
-- === Main module                                Main.hs ===--
-- ==========================================================--

module Main where
import BaseDefs
import Utils
import MyUtils
import Parser2
import PrettyPrint
import LambdaLift5
import TypeCheck5
import EtaAbstract
import StrictAn6
import ReadTable

import System.Environment
import Data.Char(isDigit)

-- ==========================================================--
--
maBaseTypes :: TcTypeEnv

maBaseTypes 
   = [ 
      ("_not", Scheme [] (TArr tcBool tcBool)),
      ("_+",   Scheme [] (TArr tcInt (TArr tcInt tcInt))),
      ("_-",   Scheme [] (TArr tcInt (TArr tcInt tcInt))),
      ("_*",   Scheme [] (TArr tcInt (TArr tcInt tcInt))),
      ("_/",   Scheme [] (TArr tcInt (TArr tcInt tcInt))),
      ("_%",   Scheme [] (TArr tcInt (TArr tcInt tcInt))),

      ("_<",   Scheme [] (TArr tcInt (TArr tcInt tcBool))),
      ("_<=",  Scheme [] (TArr tcInt (TArr tcInt tcBool))),
      ("_==",  Scheme [] (TArr tcInt (TArr tcInt tcBool))),
      ("_~=",  Scheme [] (TArr tcInt (TArr tcInt tcBool))),
      ("_>=",  Scheme [] (TArr tcInt (TArr tcInt tcBool))),
      ("_>",   Scheme [] (TArr tcInt (TArr tcInt tcBool))),

      ("_|",   Scheme [] (TArr tcBool (TArr tcBool tcBool))),
      ("_&",   Scheme [] (TArr tcBool (TArr tcBool tcBool))),
      ("_#",   Scheme [] (TArr tcBool (TArr tcBool tcBool)))
       -- *** parallel or *** ---
     ] 


-- ==========================================================--
--
maBaseAnns :: AList Naam (HExpr Naam)

maBaseAnns 
   = [ 
      ("_not",   strictUnaryFunc ),
      ("_+",     strictBinaryFunc ),
      ("_-",     strictBinaryFunc ),
      ("_*",     strictBinaryFunc ),
      ("_/",     strictBinaryFunc ),
      ("_%",     strictBinaryFunc ),
      ("_<",     strictBinaryFunc ),
      ("_<=",    strictBinaryFunc ),
      ("_==",    strictBinaryFunc ),
      ("_~=",    strictBinaryFunc ),
      ("_>=",    strictBinaryFunc ),
      ("_>",     strictBinaryFunc ),
      ("_|",     strictBinaryFunc ),
      ("_&",     strictBinaryFunc ),
      ("_#",     nonLambdaDefinableFunc ),
      ("False",  HPoint One),
      ("True",   HPoint One)
     ]
     where
	strictUnaryFunc 
           = HPoint (Rep (RepTwo 
                      (Min1Max0 1 [MkFrel [One]]
                                  [MkFrel [Zero]])))
        strictBinaryFunc 
           = HPoint (Rep (RepTwo
                      (Min1Max0 2 [MkFrel [One, One]]
                                  [MkFrel [Zero, One], MkFrel [One, Zero]])))
        nonLambdaDefinableFunc
           = HPoint (Rep (RepTwo
                      (Min1Max0 2 [MkFrel [Zero, One], MkFrel [One, Zero]]
                                  [MkFrel [Zero, Zero]])))


-- ==========================================================--
--
maKludgeFlags :: [Flag] -> [Flag]

maKludgeFlags flags
   = if     DryRun `elem` flags
     then   bdDryRunSettings ++ flags ++ bdDefaultSettings
     else                       flags ++ bdDefaultSettings
     

-- ==========================================================--
--
maStrictAn :: AList Domain Int -> [Flag] -> [Char] -> [Char]

maStrictAn table flagsInit fileName
   = "\nJules's Strictness Analyser, version 0.400" ++
     "\nCopyright (c) Julian Seward 1992" ++
     (let n = length table in
      mySeq n ("\nRead " ++ show n ++ " lattice sizes.\n")) ++
     "\n\n=============" ++
     "\n=== Input ===" ++
     "\n=============\n" ++
     (ppPrintParsed prog) ++
     "\n\n\n=============" ++
     "\n=== Types ===" ++
     "\n=============\n" ++
     prettyTypes ++ 
     "\n\n" ++
     strictAnResults ++ "\n"
     where
         flags = maKludgeFlags flagsInit
         -- call the strictness analyser if required
         strictAnResults
            = if Typecheck `notElem` flags
              then
               saMain 
                 (eaEtaAbstract typedTree) darAug fullEnvAug pseudoParams 
                 maBaseAnns tdsAug flags table
              else ""

         -- call the parser (never returns if cannot parse)
	 (dar, (tds, expr)) = paParse fileName

         (progAfterLL, pseudoParams) 
            = llMain builtInNames expr doPretty
         builtInNames = map first maBaseAnns
         prog = (tds, progAfterLL)
         doPretty = NoPretty `notElem` flags

         -- call the typechecker, fish out the resulting components
         (prettyTypes, typedTree, fullEnv) 
            = f (tcCheck maBaseTypes ([1],[0]) prog)
         f (words, (Fail m)) 
            = panic "maStrictAn: Typecheck failed -- cannot proceed."
	 f (words, Ok (rootTree, fullEnv)) 
            = (words, rootTree, fullEnv)

         -- augment type definitions to cover built-in type bool
         tdsAug = [("bool", [], [("True", []), ("False", [])])] ++ tds
         darAug = [(False, ["bool"])] ++ dar

         -- augment type environment to include built-in types
         fullEnvAug = fullEnv ++ map2nd deScheme maBaseTypes
         deScheme (Scheme _ texpr) = texpr

-- ==========================================================--
--
--main :: [Response] -> [Request]

main :: IO ()

main = do
    raw_args <- getArgs
    let cmd_line_args = maGetFlags raw_args
    tableStr <- readFile ("runtime_files/anna_table")
    file_contents <- getContents
    let table = rtReadTable tableStr
    putStr (maStrictAn table cmd_line_args file_contents)


-- ==========================================================--
--
maGetFlags :: [String] -> [Flag]

maGetFlags [] = []
maGetFlags ("-fTypecheck"  :fs) = Typecheck    : maGetFlags fs
maGetFlags ("-fSimp"       :fs) = Simp         : maGetFlags fs
maGetFlags ("-fNoCaseOpt"  :fs) = NoCaseOpt    : maGetFlags fs
maGetFlags ("-fShowHExpr"  :fs) = ShowHExpr    : maGetFlags fs
maGetFlags ("-fNoPretty"   :fs) = NoPretty     : maGetFlags fs
maGetFlags ("-fNoFormat"   :fs) = NoFormat     : maGetFlags fs
maGetFlags ("-fNoBaraki"   :fs) = NoBaraki     : maGetFlags fs
maGetFlags ("-fSimpleInv"  :fs) = SimpleInv    : maGetFlags fs
maGetFlags ("-fForceAll"   :fs) = ForceAll     : maGetFlags fs
maGetFlags ("-fDryRun"     :fs) = DryRun       : maGetFlags fs

maGetFlags
  (('-':'f':'P':'o':'l':'y':'L':'i':'m':f):fs)
    = (PolyLim (paNumval (filter isDigit f))): maGetFlags fs

maGetFlags
  (('-':'f':'L':'o':'w':'e':'r':'L':'i':'m':f):fs)
    = (LowerLim (paNumval (filter isDigit f))): maGetFlags fs

maGetFlags
  (('-':'f':'U':'p':'p':'e':'r':'L':'i':'m':f):fs)
    = (UpperLim (paNumval (filter isDigit f))): maGetFlags fs

maGetFlags
  (('-':'f':'S':'c':'a':'l':'e':'U':'p':f):fs)
    = (ScaleUp (paNumval (filter isDigit f))): maGetFlags fs

maGetFlags (other:_) = myFail ("Unknown flag: " ++ other ++ maUsage )


-- ==========================================================--
--
maUsage :: String

maUsage 
   = concat 
     [ 
       "\n\nUsage:   Anna400 [lmlflags -] [flags] < corefile",
       "\n",
       "\nAllowable flags are:",
       "\n   -fTypecheck   don't do strictness analysis",
       "\n   -fSimp        simplify abstract expressions",
       "\n   -fNoCaseOpt   don't do case-of-case optimisation",
       "\n   -fShowHExpr   show abstract expressions",
       "\n   -fNoPretty    don't clean up after lambda lifting",
       "\n   -fNoFormat    don't prettily format first-order output",
       "\n   -fNoBaraki    don't use Baraki generalisation",
       "\n   -fSimpleInv   use mindless inverses",
       "\n   -fForceAll    force all thunks before analysis",
       "\n   -fDryRun      trial run so as to check lattice table is ok",
       "\n   -fPolyLimN    set generalisation limit to `N'     (default 10000)",
       "\n   -fLowerLimN   set lower lattice threshold to `N'  (default 0)",
       "\n   -fUpperLimN   set upper lattice threshold to `N'  (default 1000000)",
       "\n   -fScaleUpN    set scaleup ratio to N/10           (default 20)",
       "\nDefault settings are opposite to those listed.\n"
     ]


-- ==========================================================--
-- === end                                        Main.hs ===--
-- ==========================================================--
