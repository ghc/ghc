
module Main (main) where

import Control.Monad
import System.IO
import System.Random

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    let q = fold $ zip [1..] (take 200 [500.0,400.0..])
    print q
    putStrLn "Before atMost"
    let (xs,q') = atMost 0.5 q -- this causes seqfault with -O2
    print xs
    print q'
    putStrLn "After atMost"

fold :: [(Key, Prio)] -> PSQ
fold []         = Void
fold ((u,r):xs) = insert u r $ fold xs

data Elem = E
    { _key   :: Key
    , prio   :: Prio
    } deriving (Eq, Show)

type Prio = Double
type Key = Int

data PSQ = Void
           | Winner Elem Tree
           deriving (Eq, Show)

singleton :: Key -> Prio -> PSQ
singleton k p = Winner (E k p) Start

insert :: Key -> Prio -> PSQ -> PSQ
insert k p q = case q of
    Void -> singleton k p
    Winner e t -> Winner (E k p) (Fork e Start t)

atMost :: Prio -> PSQ -> ([Elem], PSQ)
atMost pt q = case q of
    (Winner e _)
        | prio e > pt -> ([], q)
    Void              -> ([], Void)
    Winner e Start    -> ([e], Void)
    Winner e (Fork e' tl tr) ->
        let (sequ, q')   = atMost pt (Winner e' tl)
            (sequ', q'') = atMost pt (Winner e tr)
        in (sequ ++ sequ', q' `play` q'')

data Tree = Start
          | Fork Elem Tree Tree
    deriving (Eq, Show)

lloser :: Key -> Prio -> Tree -> Tree -> Tree
lloser k p tl tr = Fork (E k p) tl tr

play :: PSQ -> PSQ -> PSQ
Void `play` t' = t'
t `play` Void  = t
Winner e@(E k p) t `play` Winner e'@(E k' p') t'
    | p <= p'   = Winner e  (lloser k' p' t t')
    | otherwise = Winner e' (lloser k  p  t t')

