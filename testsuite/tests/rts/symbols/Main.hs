import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import Data.List (isSuffixOf, stripPrefix)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

main :: IO ()
main = do
   args <- getArgs
   case args of
      ["dump"] -> dumpDeclaredSymbols
      ["diff", objType, declaredSymbolsFile, observedSymbolsFile] -> do
         parseObserved <- case objType of
            "elf"     -> return elfSymbols
            "windows" -> return importLibSymbols
            _         -> fail ("Unknown object type: " ++ objType)
         diffStr <- diff
            <$> (readCodeOrData <$$> linePairs <$> readFile declaredSymbolsFile)
            <*> (parseObserved <$> readFile observedSymbolsFile)
         -- writeFile "<ghc root>/testsuite/tests/rts/symbols/rts-check-symbols.stdout"
         putStrLn diffStr
      _ -> fail ("Unexpected arguments: " ++ show args)
   where
      (<$$>) = fmap . fmap

foreign import ccall "dumpDeclaredSymbols"
   dumpDeclaredSymbols :: IO ()

data CodeOrData = Code | Data
   deriving (Show, Eq)

-- Compare observedSymbols (symbols observed in the DSO) to declaredSymbols
-- (symbols declared in RtsSymbols.c)
diff :: M.Map String CodeOrData -> M.Map String CodeOrData -> String
diff declaredSymbols observedSymbols = unlines $
      [ "Symbols declared in RtsSymbols.c but not observed in the DSO:" ]
   ++ [ "Declared> " ++ k ++ " " ++ show v | (k, v) <- M.toList diffInDeclaredSymbols ]
   ++ [ "" ]
   ++ [ "Symbols observed in the DSO but not declared in RtsSymbols.c:" ]
   ++ [ "Observed> " ++ k ++ " " ++ show v | (k, v) <- M.toList diffInObservedSymbols ]
   ++ [ "" ]
   ++ [ "Symbols observed in the DSO and declared in RtsSymbols.c but with conflicting type:" ]
   ++ [ "WrongType> " ++ k ++ " (declared, observed): (" ++ show declared ++ ", " ++ show observed ++ ")"
         | (k, (declared, observed)) <- M.toList diffOnType ]
   where
      -- Undeclared observed symbols
      diffInObservedSymbols :: M.Map String CodeOrData
      diffInObservedSymbols = observedSymbols M.\\ declaredSymbols

      -- Declared unobserved symbols
      diffInDeclaredSymbols :: M.Map String CodeOrData
      diffInDeclaredSymbols = declaredSymbols M.\\ observedSymbols

      -- Declared observed symbols with conflicting type
      diffOnType :: M.Map String (CodeOrData, CodeOrData)
      diffOnType = M.filter (\(a, b) -> a /= b) $ M.intersectionWith (,) declaredSymbols observedSymbols

linePairs :: String -> M.Map String String
linePairs = M.fromList . map (\str -> let [a,b] = words str in (a,b)) . filter (/= "") . lines

readCodeOrData :: String -> CodeOrData
readCodeOrData s = case s of
   "code" -> Code
   "data" -> Data
   _ -> error ("Unknown symbol type: " ++ show s)

-- | Parse the output of @readelf -W --dyn-syms@ of an ELF shared object.
--
-- Symbol lines look like:
--
-- >   Num:    Value          Size Type    Bind   Vis      Ndx Name
-- >     2: 0000000000000000     0 FUNC    GLOBAL DEFAULT  UND tcsetattr@GLIBC_2.2.5 (2)
-- >  1149: 000000000006d020   190 OBJECT  GLOBAL DEFAULT   12 stg_AP_info
--
-- Note that Vis may be followed by extra annotations on some platforms (e.g.
-- @[<localentry>: 8]@ on ppc64le), so Ndx and Name are taken from the end of
-- the line.
elfSymbols :: String -> M.Map String CodeOrData
elfSymbols = M.fromList . mapMaybe parseLine . lines
   where
      parseLine l = case words l of
         (num : _value : _size : ty : bind : rest)
            | ":" `isSuffixOf` num
            , name : ndx : _ <- reverse (dropVersionIndex rest)
            , bind == "GLOBAL"
            , ndx /= "UND"  -- Imports
            -> (,) name <$> typeToCodeOrData ty
         _ -> Nothing

      -- Drop the trailing version index e.g. "(2)"
      dropVersionIndex ws = case reverse ws of
         ('(' : _) : ws' -> reverse ws'
         _ -> ws

      -- Returns Nothing if the symbol should be ignored
      typeToCodeOrData :: String -> Maybe CodeOrData
      typeToCodeOrData ty = case ty of
         "FILE"   -> Nothing
         "NOTYPE" -> Nothing
         "FUNC"   -> Just Code
         "OBJECT" -> Just Data
         _ -> error ("Unknown symbol type: " ++ show ty)

-- | Parse the output of @nm -P@ into (name, type letter) pairs. Lines that are
-- not symbols (e.g. the archive member headers printed for import libraries)
-- are ignored.
parseNm :: String -> [(String, Char)]
parseNm = mapMaybe parseLine . lines
   where
      parseLine l = case words l of
         (name : [ty] : _) -> Just (name, ty)
         _ -> Nothing

-- | Interpret the symbols of a windows import library. Every export @foo@ has
-- an import address table entry @__imp_foo@, but only code exports also have
-- a @foo@ thunk.
importLibSymbols :: String -> M.Map String CodeOrData
importLibSymbols nmOutput = M.fromList
   [ (name, if name `S.member` defined then Code else Data)
   | name <- mapMaybe (stripPrefix "__imp_") (S.toList defined)
   ]
   where
      defined :: S.Set String
      defined = S.fromList [ name | (name, ty) <- parseNm nmOutput, ty /= 'U' ]
