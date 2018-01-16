{-# OPTIONS -fglasgow-exts #-}
-- ^ unboxed strings
--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--
import BenchUtils

import Data.ByteString (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as L

import Data.List
import Data.Char
import Data.Word
import Data.Int

import System.IO
import Control.Monad
import Text.Printf

--
-- temporarily broken
--
main :: IO ()
main = do
    -- initialise
    force (fps,fps') >> force (lps,lps')

    printf "# Size of test data: %dk\n" ((floor $ (fromIntegral (B.length fps)) / 1024) :: Int)
    printf "#Byte\t Lazy\n"

    run 5 ((fps,fps'),(lps,lps')) tests

------------------------------------------------------------------------

tests =
    [ ("++",
        [F ({-# SCC "append"       #-}      app  (uncurry B.append))
        ,F ({-# SCC "lazy append"  #-}      app  (uncurry L.append))
    ])
    , ("concat",
        [F ({-# SCC "concat"       #-}      app   B.concat)
        ,F ({-# SCC "lazy concat"  #-}      app   L.concat)
    ])
    , ("length",
        [F ({-# SCC "length"       #-}      app   B.length)
        ,F ({-# SCC "lazy length"  #-}      app   L.length)
    ])

{-
    , ("compare",
        [F ({-# SCC "compare"      #-}      app2 compare) :: needs type annotation
        ,F ({-# SCC "lazy compare" #-}      app2 compare) ])
-}

    , ("index",
        [F ({-# SCC "index"        #-}      app$  flip B.index 260000)
        ,F ({-# SCC "lazy index"   #-}      app$  flip L.index 260000)
    ])
    , ("map",
        [F ({-# SCC "map"          #-}      app$  B.map (+1))
        ,F ({-# SCC "lazy map"     #-}      app$  L.map (+1))
    ])
    , ("filter",
        [F ({-# SCC "filter"       #-}      app$  B.filter (/=101))
        ,F ({-# SCC "lazy filter"  #-}      app$  L.filter (/=101))
    ])
--  , ("map'",
--      [F ({-# SCC "map"          #-}      app$  B.map (*2))
--      ,F ({-# SCC "map"          #-}      app$  B.map' (*1))
--  ])
--  , ("filter'",
--      [F ({-# SCC "filter"       #-}      app$  B.filter  (/=121))
--      ,F ({-# SCC "filter'"      #-}      app$  B.filter' (/=121))
--  ])
--  , ("filterNotByte",
--      [F ({-# SCC "filterNotByte"      #-}app$  B.filterNotByte 101)
--      ,F ({-# SCC "lazy filterNotByte" #-}app$  L.filterNotByte 101)
--  ])
--  , ("filterByte",
--      [F ({-# SCC "filterByte"       #-}  app$  B.filterByte 103)
---     ,F ({-# SCC "lazy filterByte"  #-}  app$  L.filterByte 103)
--  ])
--  , ("findIndexOrEnd",
--      [F ({-# SCC "findIndexOrEnd"   #-}  app$  B.findIndexOrEnd (==126))
--  ])
    , ("findIndex",
        [F ({-# SCC "findIndex"      #-}    app$  B.findIndex (==126))
        ,F ({-# SCC "lazy findIndex" #-}    app$  L.findIndex (==126))
    ])
    , ("find",
        [F ({-# SCC "find"          #-}     app$  B.find (==126))
        ,F ({-# SCC "lazy find"     #-}     app$  L.find (==126))
    ])
    , ("foldl",
        [F ({-# SCC "fold"          #-}     app$  B.foldl (\a w -> a+1::Int) 0)
        ,F ({-# SCC "lazy fold"     #-}     app$  L.foldl (\a w -> a+1::Int) 0)
    ])
    , ("foldl'",
        [F ({-# SCC "fold"          #-}     app$  B.foldl' (\a w -> a+1::Int) 0)
        ,F ({-# SCC "lazy fold"     #-}     app$  L.foldl' (\a w -> a+1::Int) 0)
    ])
    , ("take",
        [F ({-# SCC "take"          #-}     app $ B.take 100000)
        ,F ({-# SCC "lazy take"     #-}     app $ L.take 100000)
    ])
    , ("drop",
        [F ({-# SCC "drop"          #-}     app $ B.drop 100000)
        ,F ({-# SCC "lazy drop"     #-}     app $ L.drop 100000)
    ])
    , ("takeWhile",
        [F ({-# SCC "takeWhile"     #-}     app $ B.takeWhile (/=122))
        ,F ({-# SCC "lazy takeWhile" #-}    app $ L.takeWhile (==122))
    ])
    , ("dropWhile",
        [F ({-# SCC "dropWhile"     #-}     app $ B.dropWhile (/=122))
        ,F ({-# SCC "lazy dropWhile" #-}    app $ L.dropWhile (/=122))
    ])
    , ("span",
        [F ({-# SCC "span"          #-}     app $ B.span (/=122))
        ,F ({-# SCC "lazy span"     #-}     app $ L.span (/=122))
    ])
    , ("break",
        [F ({-# SCC "break"         #-}     app $ B.break (==122))
        ,F ({-# SCC "lazy break"    #-}     app $ L.break (==122))
    ])
    , ("split",
        [F ({-# SCC "split"         #-}     app $ B.split 0x0a)
        ,F ({-# SCC "lazy split"    #-}     app $ L.split 0x0a)
    ])
--  , ("breakByte",
--      [F ({-# SCC "breakChar"     #-}     app $ B.breakByte 122)
--      ,F ({-# SCC "lazy breakChar" #-}    app $ L.breakByte 122)
--  ])
--  , ("spanByte",
--      [F ({-# SCC "spanChar"      #-}     app $ B.spanByte 122)
--      ,F ({-# SCC "lazy spanChar" #-}     app $ L.spanByte 122)
--  ])
    , ("reverse",
        [F ({-# SCC "reverse"       #-}     app B.reverse)
        ,F ({-# SCC "lazy reverse"  #-}     app L.reverse)
    ])
    , ("cons",
        [F ({-# SCC "cons"          #-}     app $ B.cons 120)
        ,F ({-# SCC "lazy cons"     #-}     app $ L.cons 120)
    ])
    , ("snoc",
        [F ({-# SCC "snoc"          #-}     app $ flip B.snoc 120)
        ,F ({-# SCC "lazy snoc"     #-}     app $ flip L.snoc 120)
    ])
    , ("empty",
        [F ({-# SCC "empty"         #-}     const B.empty)
        ,F ({-# SCC "lazy empty"    #-}     const L.empty)
    ])
    , ("head",
        [F ({-# SCC "head"          #-}     app B.head)
        ,F ({-# SCC "lazy head"     #-}     app L.head)
    ])
    , ("tail",
        [F ({-# SCC "tail"          #-}     app B.tail)
        ,F ({-# SCC "lazy tail"     #-}     app L.tail)
    ])
    , ("last",
        [F ({-# SCC "last"          #-}     app B.last)
        ,F ({-# SCC "lazy last"     #-}     app L.last)
    ])
    , ("init",
        [F ({-# SCC "init"          #-}     app B.init)
        ,F ({-# SCC "lazy init"     #-}     app L.init)
    ])
    , ("count",
        [F ({-# SCC "count"         #-}     app $ B.count 10)
        ,F ({-# SCC "lazy count"    #-}     app $ L.count 10)
    ])
    , ("isPrefixOf",
        [F ({-# SCC "isPrefixOf" #-}        app $ B.isPrefixOf
                (C.pack "The Project Gutenberg eBook"))
        ,F ({-# SCC "lazy isPrefixOf" #-}   app $ L.isPrefixOf
                (L.pack [84,104,101,32,80,114,111,106,101
                           ,99,116,32,71,117,116,101,110,98
                           ,101,114,103,32,101,66,111,111,107]))
    ])
    , ("join",
        [F ({-# SCC "join"          #-}     app $ B.intercalate (B.pack [1,2,3]))
        ,F ({-# SCC "lazy join"     #-}     app $ L.intercalate (L.pack [1,2,3]))
    ])
--  , ("joinWithByte",
--      [F ({-# SCC "joinWithByte"  #-}     app $ uncurry (B.joinWithByte 32))
--      ,F ({-# SCC "lazy joinWithByte" #-} app $ uncurry (L.joinWithByte 32))
--  ])
    , ("any",
        [F ({-# SCC "any"           #-}     app $ B.any (==120))
        ,F ({-# SCC "lazy any"      #-}     app $ L.any (==120))
    ])
    , ("all",
        [F ({-# SCC "all"           #-}     app $ B.all (==120))
        ,F ({-# SCC "lazy all"      #-}     app $ L.all (==120))
    ])
    , ("maximum",
        [F ({-# SCC "maximum"       #-}     app B.maximum)
        ,F ({-# SCC "lazy maximum"  #-}     app L.maximum)
    ])
    , ("minimum",
        [F ({-# SCC "minimum"       #-}     app B.minimum)
        ,F ({-# SCC "lazy minimum"  #-}     app L.minimum)
    ])
    , ("elem",
        [F ({-# SCC "elem"          #-}     app $ B.elem 122)
        ,F ({-# SCC "lazy elem"     #-}     app $ L.elem 122)
    ])
    , ("notElem",
        [F ({-# SCC "notElem"       #-}     app $ B.notElem 122)
        ,F ({-# SCC "lazy notElem"  #-}     app $ L.notElem 122)
    ])
    , ("elemIndex",
        [F ({-# SCC "elemIndex"     #-}     app $ B.elemIndex 122)
        ,F ({-# SCC "lazy elemIndex" #-}    app $ L.elemIndex 122)
    ])
    , ("findIndices",
        [F ({-# SCC "findIndicies"  #-}     app $ B.findIndices (==122))
        ,F ({-# SCC "lazy findIndices" #-}  app $ L.findIndices (==122))
    ])
    , ("elemIndices",
        [F ({-# SCC "elemIndicies"  #-}     app $ B.elemIndices 122)
        ,F ({-# SCC "lazy elemIndices" #-}  app $ L.elemIndices 122)
    ])
    , ("splitAt",
        [F ({-# SCC "splitAt"       #-}     app $ B.splitAt 10000)
        ,F ({-# SCC "lazy splitAt"  #-}     app $ L.splitAt 10000)
    ])
    , ("splitWith",
        [F ({-# SCC "splitWith"     #-}     app $ B.splitWith (==122))
        ,F ({-# SCC "lazy splitWith" #-}    app $ L.splitWith (==122))
    ])
    , ("replicate",
        [F ({-# SCC "replicate"     #-}     const $ B.replicate 10000000 120)
        ,F ({-# SCC "lazy replicate" #-}    const $ L.replicate 10000000 120)
    ])
    , ("group",
        [F ({-# SCC "group"         #-}     app B.group)
        ,F ({-# SCC "lazy group"    #-}     app L.group)
    ])
    , ("groupBy",
        [F ({-# SCC "groupBy"       #-}     app $ B.groupBy (==))
        ,F ({-# SCC "lazy groupBy"  #-}     app $ L.groupBy (==))
    ])
    , ("inits",
        [F ({-# SCC "inits"         #-}     app B.inits)
    ])
    , ("tails",
        [F ({-# SCC "tails"         #-}     app B.tails)
    ])
--  , ("transpose",[F ({-# SCC "transpose" #-}B.transpose [fps,fps'])])

------------------------------------------------------------------------
--
-- Char8 or ByteString only

    , ("intersperse",
        [F ({-# SCC "intersperse"   #-}     app $ B.intersperse 120 )
    ])
    , ("sort",
        [F ({-# SCC "sort"          #-}     app B.sort)
    ])
--  , ("lineIndices",
--      [F ({-# SCC "lineIndicies"  #-}     app C.lineIndices)
--  ])
    , ("elemIndexEnd",
        [F ({-# SCC "elemIndexEnd"  #-}     app $ B.elemIndexEnd 122)
    ])
--  , ("breakSpace",
--      [F ({-# SCC "breakSpace"    #-}     app C.breakSpace)
--  ])
--  , ("dropSpace",
--      [F ({-# SCC "dropSpace"     #-}     app C.dropSpace)
--  ])
--  , ("dropSpaceEnd",
--      [F ({-# SCC "dropSpaceEnd"  #-}     app C.dropSpaceEnd)
--  ])

--  , ("zip",[F ({-# SCC "zip" #-} B.zip fps fps)])

    , ("zipWith'",
        [F ({-# SCC "zipWith'"      #-}     app (uncurry (B.zipWith (+))))
    ])
    , ("isInfixOf",
        [F ({-# SCC "isSubstringOf" #-}     app $ B.isInfixOf     (C.pack "email news"))
    ])
    , ("isSuffixOf",
        [F ({-# SCC "isSuffixOf"    #-}     app $ B.isSuffixOf (C.pack "new eBooks"))
    ])
    , ("spanEnd",
        [F ({-# SCC "spanEnd"       #-}     app $ B.spanEnd (/=122))
    ])
    , ("lines",
        [F ({-# SCC "lines"         #-}     app C.lines)
    ])
    , ("unlines",
        [F ({-# SCC "unlines"       #-}     app C.unlines)
    ])
    , ("words",
        [F ({-# SCC "words"         #-}     app C.words)
    ])
    , ("unwords",
        [F ({-# SCC "unwords"       #-}     app C.unwords)
    ])

 ]

------------------------------------------------------------------------

fst1        f ((x,_),_) = f x
snd1        f (_,(x,_)) = f x
fst2list    f ((x,y),_) = f [x,y]
snd2list    f (_,(x,y)) = f [x,y]
fst2        f (x,_)     = f x
snd2        f (_,y)     = f y

type Input = ((B.ByteString,B.ByteString),(L.ByteString,L.ByteString))

class (Eq a, Ord a) => Ap a where app :: (a -> b) -> Input -> b

instance Ap B.ByteString                   where app = fst1
instance Ap L.ByteString                   where app = snd1
instance Ap [B.ByteString]                 where app = fst2list
instance Ap [L.ByteString]                 where app = snd2list
instance Ap (B.ByteString, B.ByteString)   where app = fst2
instance Ap (L.ByteString, L.ByteString)   where app = snd2

app2 :: Ap (a, b) => (a -> b -> c) -> Input -> c
app2 = app . uncurry
