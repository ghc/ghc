import Prelude hiding (lookup)
import Data.Foldable (toList)
import GHC.Event.IntTable
import GHC.Primitive.Array (Array)
import qualified GHC.Primitive.Array as PM

main :: IO ()
main = do
  t0 <- new 4 :: IO (IntTable Char)
  lookup 5 t0 >>= demand PM.emptyArray
  t1 <- insertCons 17 'x' t0 >>= demandPrev PM.emptyArray
  t2 <- insertCons 23 'm' t1 >>= demandPrev PM.emptyArray
  t3 <- insertCons 19 'a' t2 >>= demandPrev PM.emptyArray
  t4 <- insertCons 47 'z' t3 >>= demandPrev PM.emptyArray
  t5 <- insertCons 12 'a' t4 >>= demandPrev PM.emptyArray
  t6 <- insertCons 15 'y' t5 >>= demandPrev PM.emptyArray
  t7 <- insertCons 20 'g' t6 >>= demandPrev PM.emptyArray
  t8 <- insertCons 11 'n' t7 >>= demandPrev PM.emptyArray
  t9 <- insertCons 47 'b' t8 >>= demandPrev (PM.singletonArray 'z')
  lookup 47 t9 >>= demand (PM.arrayFromList ['b','z'])
  t10 <- updateWith (\_ -> PM.emptyArray) 15 t9
    >>= demandOldNew (PM.singletonArray 'y') PM.emptyArray
  return ()

demand :: Array Char -> Array Char -> IO ()
demand expected actual = if toList expected == toList actual
  then pure ()
  else fail $ concat
    [ "demand: expected "
    , show (toList expected)
    , " but got "
    , show (toList actual)
    ]

demandPrev :: Array Char -> (x,Array Char) -> IO x
demandPrev expected (x,actual) = if toList expected == toList actual
  then pure x
  else fail $ concat
    [ "demandPrev: expected "
    , show (toList expected)
    , " but got "
    , show (toList actual)
    ]

demandOldNew :: Array Char -> Array Char -> (x,Array Char,Array Char) -> IO x
demandOldNew expOld expNew (x,actOld,actNew) =
  if toList expOld == toList actOld && toList expNew == toList actNew
    then pure x
    else fail $ concat
      [ "demandPrev: expected old "
      , show (toList expOld)
      , " and new "
      , show (toList expNew)
      , " but got old "
      , show (toList actOld)
      , " and new "
      , show (toList actNew)
      ]

