{-# LANGUAGE TupleSections #-}

-- Generates CmmSwitchTest.hs

import qualified Data.Set as S
import Data.Int
import Data.Word
import Data.List
import System.Environment

output :: Integer -> Integer
output n = n`div`2 + 42

def :: Integer
def = 1337

data Bits = X32 | X64
    deriving Eq
type Spec = (String, Bool, [Integer], Bits)

primtyp True = "Int#"
primtyp False = "Word#"

con True = "I#"
con False = "W#"

hash True = "#"
hash False = "##"

primLit s v = show v ++ hash s

genSwitch :: Spec -> String
genSwitch (name, signed, values, _) = unlines $
  [ "{-# NOINLINE " ++ name ++ " #-}" ] ++
  [ name ++ " :: " ++ primtyp signed ++ " -> " ++ primtyp signed ] ++
  [ name ++ " " ++ primLit signed v ++ " = " ++ primLit signed (output v)
  | v <- values] ++
  [ name ++ " _ = " ++ primLit signed def ]

genCheck :: Spec -> String
genCheck (name, signed, values, bits) = unlines $
  [ checkName name ++ " :: IO ()"
  , checkName name ++ " = forM_\n    [ " ++ pairs ++ "] $ \\(" ++ con signed ++ " i,o) -> do"
  , "   let r = " ++ con signed ++ " (" ++ name ++ " i)"
  , "   unless (r == o) $ putStrLn $ \"ERR: " ++ name ++ " (\" ++ show (" ++ con signed ++ " i)++ \") is \" ++ show r ++ \" and not \" ++ show o ++\".\""
  ]
  where
    f x | x `S.member` range = output x
        | otherwise          = def
    range = S.fromList values
    minS = if bits == X32 then minS32 else minS64
    maxS = if bits == X32 then maxS32 else maxS64
    maxU = if bits == X32 then maxU32 else maxU64
    checkValues = S.toList $ S.fromList $
        [ v' | v <- values, v' <- [v-1,v,v+1],
               if signed then v' >= minS && v' <= maxS else v' >= minU && v' <= maxU ]
    pairs = intercalate "\n    , " ["(" ++ show v ++ "," ++ show (f v) ++ ")" | v <- checkValues ]

checkName :: String -> String
checkName f = f ++ "_check"

genMain :: [Spec] -> String
genMain specs = unlines $ "main = do" : [ "    " ++ checkName n | (n,_,_,_) <- specs ]

genMod :: [Spec] -> String
genMod specs = unlines $
    "-- This file is generated from CmmSwitchTestGen! @generated" :
    "{-# LANGUAGE MagicHash, NegativeLiterals #-}" :
    "import Control.Monad (unless, forM_)" :
    "import GHC.Exts" :
    map genSwitch specs ++
    map genCheck specs ++
    [ genMain specs ]

main = do
    args <- getArgs
    bits <- parse args
    putStrLn $
        genMod $ zipWith (\n (s,v) -> (n,s,v,bits)) names $ signedChecks bits ++ unsignedChecks bits

parse :: [String] -> IO Bits  -- Use IO to avoid lazy parsing
parse ["-32"] = return X32
parse ["-64"] = return X64
parse _ = error "Please, supply -32 or -64 option"

signedChecks :: Bits -> [(Bool, [Integer])]
signedChecks bits = map (True,)
    [ [1..10]
    , [0..10]
    , [1..3]
    , [1..4]
    , [1..5]
    , [-1..10]
    , [-10..10]
    , [-20.. -10]++[0..10]
    , [-20.. -10]++[1..10]
    , [minS,0,maxS]
    , [maxS-10 .. maxS]
    , [minS..minS+10]++[maxS-10 .. maxS]
    ]
    where
        minS = if bits == X32 then minS32 else minS64
        maxS = if bits == X32 then maxS32 else maxS64

minU, maxU32, maxU64, minS32, minS64, maxS32, maxS64 :: Integer
minU = 0
maxU32 = fromIntegral (maxBound :: Word32)
maxU64 = fromIntegral (maxBound :: Word64)

minS32 = fromIntegral (minBound :: Int32)
minS64 = fromIntegral (minBound :: Int64)
maxS32 = fromIntegral (maxBound :: Int32)
maxS64 = fromIntegral (maxBound :: Int64)


unsignedChecks :: Bits -> [(Bool, [Integer])]
unsignedChecks bits = map (False,)
    [ [0..10]
    , [1..10]
    , [0]
    , [0..1]
    , [0..2]
    , [0..3]
    , [0..4]
    , [1]
    , [1..2]
    , [1..3]
    , [1..4]
    , [1..5]
    , [minU,maxU]
    , [maxU-10 .. maxU]
    , [minU..minU+10]++[maxU-10 .. maxU]
    ]
    where
        maxU = if bits == X32 then maxU32 else maxU64

names :: [String]
names = [ c1:c2:[] | c1 <- ['a'..'z'], c2 <- ['a'..'z']]
