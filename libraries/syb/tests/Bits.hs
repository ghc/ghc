{-# OPTIONS -fglasgow-exts #-}

module Bits (tests) where

{-

This test exercices some oldies of generic programming, namely
encoding terms as bit streams and decoding these bit streams in turn
to obtain terms again. (This sort of function might actually be useful
for serialisation and sending companies and other terms over the
internet.)

Here is how it works.

A constuctor is encoded as a bit stream. To this end, we encode the
index of the constructor as a binary number of a fixed length taking
into account the maximum index for the type at hand. (Similarly, we
could view the list of constructors as a binary tree, and then encode
a constructor as the path to the constructor in this tree.) If there
is just a single constructor, as for newtypes, for example, then the
computed bit stream is empty.

Otherwise we just recurse into subterms.

Well, we need to handle basic datatypes in a special way. We observe
such basic datatypes by testing the maximum index to be 0 for the
datatype at hand. An efficient encoding should be tuned per basic
datatype. The following solution is generic, but it wastes space.
That is, we turn the basic value into a string relying on the general
Data API. This string can now be encoded by first converting it into a
list of bit streams at the term level, which can then be easily
encoded as a single bit stream (because lists and bits can be
encoded).

-}

import Test.Tasty.HUnit

import Data.Generics
import Data.Char
import Data.Maybe
import Control.Applicative (Alternative(..), Applicative(..))
import Control.Monad
import CompanyDatatypes



-----------------------------------------------------------------------------



-- | We need bits and bit streams.
data Bit = Zero | One deriving (Show, Eq, Typeable, Data)
type Bin = [Bit]



-----------------------------------------------------------------------------



-- Compute length of bit stream for a natural
lengthNat :: Int -> Int
lengthNat x = ceiling (logBase 2 (fromIntegral (x + 1)))


-- Encode a natural as a bit stream
varNat2bin :: Int -> Bin
varNat2bin 0 = []
varNat2bin x =
  ( ( if even x then Zero else One )
  : varNat2bin (x `div` 2)
  )


-- Encode a natural as a bit stream of fixed length
fixedNat2bin :: Int -> Int -> Bin
fixedNat2bin 0 0 = []
fixedNat2bin p x | p>0 =
  ( ( if even x then Zero else One )
  : fixedNat2bin (p - 1) (x `div` 2)
  )


-- Decode a natural
bin2nat :: Bin -> Int
bin2nat []          = 0
bin2nat (Zero : bs) = 2 * (bin2nat bs)
bin2nat (One  : bs) = 2 * (bin2nat bs) + 1



-----------------------------------------------------------------------------



-- | Generically map terms to bit streams
showBin :: Data t => t -> Bin

showBin t
  = if isAlgType myDataType
      then con2bin ++ concat (gmapQ showBin t)
      else showBin base

 where

  -- The datatype for introspection
  myDataType = dataTypeOf t

  -- Obtain the maximum index for the type at hand
  max :: Int
  max = maxConstrIndex myDataType

  -- Obtain the index for the constructor at hand
  idx :: Int
  idx = constrIndex (toConstr t)

  -- Map basic values to strings, then to lists of bit streams
  base = map (varNat2bin . ord) (showConstr (toConstr t))

  -- Map constructors to bit streams of fixed length
  con2bin = fixedNat2bin (lengthNat (max - 1)) (idx - 1)


-----------------------------------------------------------------------------



-- | A monad on bit streams
data ReadB a = ReadB (Bin -> (Maybe a, Bin))
unReadB (ReadB f) = f

instance Functor ReadB where
  fmap  = liftM

instance Applicative ReadB where
  pure  = return
  (<*>) = ap

instance Alternative ReadB where
  (<|>) = mplus
  empty = mzero

-- It's a monad.
instance Monad ReadB where
  return a = ReadB (\bs -> (Just a, bs))
  (ReadB c) >>= f = ReadB (\bs -> case c bs of
                             (Just a, bs')  -> unReadB (f a) bs'
                             (Nothing, bs') -> (Nothing, bs')
                          )


-- It's a bit monad with 0 and +.
instance MonadPlus ReadB where
  mzero = ReadB (\bs -> (Nothing, bs))
  (ReadB f) `mplus` (ReadB g) = ReadB (\bs -> case f bs of
                                         (Just a, bs') -> (Just a, bs')
                                         (Nothing, _)  -> g bs
                                      )


-- Read a few bits
readB :: Int -> ReadB Bin
readB x = ReadB (\bs -> if length bs >= x
                          then (Just (take x bs), drop x bs)
                          else (Nothing, bs)
                )



-----------------------------------------------------------------------------



-- | Generically map bit streams to terms
readBin :: Data t => ReadB t
readBin = result
 where

  -- The worker, which we also use as type argument
  result = if isAlgType myDataType

             then do bin <- readB (lengthNat (max - 1))
                     fromConstrM readBin (bin2con bin)

             else do str <- readBin
                     con <- str2con (map (chr . bin2nat) str)
                     return (fromConstr con)

  -- Determine result type
  myDataType = dataTypeOf (getArg result)
     where
      getArg :: ReadB a -> a
      getArg = undefined

  -- Obtain the maximum index for the type at hand
  max :: Int
  max = maxConstrIndex myDataType

  -- Convert a bit stream into a constructor
  bin2con :: Bin -> Constr
  bin2con bin = indexConstr myDataType ((bin2nat bin) + 1)

  -- Convert string to constructor; could fail
  str2con :: String -> ReadB Constr
  str2con = maybe mzero return
                . readConstr myDataType



-----------------------------------------------------------------------------



tests = (   showBin True
        , ( showBin [True]
        , ( showBin (1::Int)
        , ( showBin "1"
        , ( showBin genCom
        , ( geq genCom genCom'
        )))))) @=? output
 where
  genCom' = fromJust (fst (unReadB readBin (showBin genCom))) :: Company

output = ([One],([One,One,Zero],([One,One,One,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,Zero],([One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero],([One,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,Zero,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,Zero,One,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,Zero,One,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,Zero,Zero,One,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,Zero,One,Zero,One,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,One,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,Zero,One,Zero,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,One,One,Zero,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,One,One,One,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,One,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,Zero,One,One,One,One,Zero,One,One,One,One,One,One,One,One,Zero,One,Zero,One,One,Zero,Zero,Zero,One,One,One,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,One,One,Zero,One,One,One,One,One,One,One,Zero,One,One,Zero,One,One,Zero,One,Zero,One,Zero,One,Zero,One,One,One,One,Zero,Zero,Zero,Zero],True)))))
