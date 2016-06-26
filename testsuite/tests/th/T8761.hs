{-# LANGUAGE TemplateHaskell, RankNTypes, GADTs, PatternSynonyms #-}

module T8761 where

{- Testsuite for pattern synonyms as implemented by ticket #8761 -}

import Control.Monad
import Language.Haskell.TH
import System.IO

data Ex         where MkEx       :: forall a. a -> Ex
data ExProv     where MkExProv   :: forall a. (Show a) => a -> ExProv
data UnivProv a where MkUnivProv :: forall a. (Show a) => a -> UnivProv a

{- Test manual construction and pretty printing of pattern synonyms -}
do
  [qx1,qy1,qz1] <- mapM (\i -> newName $ "x" ++ show i) [1,2,3]
  let nm1       = mkName "Q1"
      prefixPat = patSynD nm1 (prefixPatSyn [qx1,qy1,qz1]) unidir
        (tupP [tupP [varP qx1, varP qy1], listP [varP qz1], wildP, wildP])

  [qx2,qy2] <- mapM (\i -> newName $ "x" ++ show i) [1,2]
  let nm2      = mkName "Q2"
      infixPat = patSynD nm2 (infixPatSyn qx2 qy2) implBidir
        (tupP [tupP [varP qx2, varP qy2]])

  let nm3           = mkName "Q3"
      [qx3,qy3,qz3] = map mkName ["qx3", "qy3", "qz3"]
      patP          = tupP [tupP [varP qx3, varP qy3], listP [varP qz3]]
      patE          = tupE [tupE [varE qx3, varE qy3], listE [varE qz3]]
      cls           = clause [varP qx3, varP qy3, varP qz3] (normalB patE) []
      recordPat     = patSynD nm3 (recordPatSyn [qx3,qy3,qz3])
                        (explBidir [cls]) patP

  pats <- sequence [prefixPat, infixPat, recordPat]
  -- pretty print the pattern synonyms:
  mapM_ (runIO . hPutStrLn stderr . pprint) pats
  -- splice in the pattern synonyms
  return pats

{- Test prefix pattern synonyms -}
[d|
 pattern P1 x y z <- ((x,y), [z], _, _)   -- unidirectional pattern
 pattern P2 x y z =  ((x,y), [z])         -- implicit bidirectional pattern
 pattern P3 x y z <- ((x,y), [z]) where   -- explicit bidirectional pattern
   P3 x y z = ((x,y), [z]) |]

{- Test infix pattern synonyms -}
[d|
 pattern x :*: y <- ((x,_), [y])
 pattern x :+: y =  (x,y)
 pattern x :~: y <- (x,y) where
   x :~: y = (x,y) |]

{- Test record pattern synonyms -}
[d|
 pattern R1 {x1, y1} <- ((x1,_), [y1])
 getX1 = x1 ((1, 2), [3]) -- should yield 1
 getY1 = y1 ((1, 2), [3]) -- should yield 3
 pattern R2 {x2, y2} =  (x2, [y2])
 pattern R3 {x3, y3} <- (x3, [y3]) where
   R3 x y = (x, [y]) |]

--x1 = "no, no, no"
--y1 = "no, no, no"

getX1' = x1 ((1, 2), [3]) -- should yield 1
getY1' = y1 ((1, 2), [3]) -- should yield 3

{- Test splicing unidirectional pattern synonyms with different types -}
[d|
 pattern P :: Bool
 pattern P <- True

 pattern Pe :: () => forall a. a -> Ex
 pattern Pe x <- MkEx x

 pattern Pu :: forall a. a -> a
 pattern Pu x <-  x

 pattern Pue :: forall a. () => forall b. a -> b -> (a, Ex)
 pattern Pue x y <- (x, MkEx y)

 pattern Pur :: forall a. (Num a, Eq a) => a -> [a]
 pattern Pur x <- [x, 1]

 pattern Purp :: forall a b. (Num a, Eq a) =>
                 Show b => a -> b -> ([a], UnivProv b)
 pattern Purp x y <- ([x, 1], MkUnivProv y)

 pattern Pure :: forall a. (Num a, Eq a) => forall b. a -> b -> ([a], Ex)
 pattern Pure x y <- ([x, 1], MkEx y)

 pattern Purep :: forall a. (Num a, Eq a) =>
                 forall b. Show b => a -> b -> ([a], ExProv)
 pattern Purep x y <- ([x, 1], MkExProv y)

 pattern Pep :: () => forall a. Show a => a -> ExProv
 pattern Pep x <- MkExProv x

 pattern Pup :: forall a. () => Show a => a -> UnivProv a
 pattern Pup x <- MkUnivProv x

 pattern Puep :: forall a. () => forall b. (Show b) => a -> b -> (ExProv, a)
 pattern Puep x y <- (MkExProv y, x) |]

{- Test reification of different pattern synonyms and their types -}
do
  infos <- mapM reify [ 'P, 'Pe, 'Pu, 'Pue, 'Pur, 'Purp
                      , 'Pure, 'Purep, 'Pep, 'Pup, 'Puep ]
  mapM_ (runIO . hPutStrLn stderr . pprint) infos
    -- NB. use stderr rather than stdout, because GHC does not
    -- guarantee to flush stdout after TH code.  In particular when
    -- the output is going to a file, and we're using GHC with the
    -- runtime linker or with -fexternal-interpreter, stdout will not
    -- get flushed.
  [d| theAnswerIs = 42 |]
