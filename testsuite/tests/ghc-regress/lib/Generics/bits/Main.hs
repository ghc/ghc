{-# OPTIONS -fglasgow-exts #-}

{-
 
This test exercices some oldies of generic programming, namely
encoding terms as bit streams and decoding these bit streams in turn
to obtain terms again. (This sort of function might actually be useful
for serialisation and sending companies and other terms over the
internet.)

Here is how it works.

A constuctor is encoded as a bit stream. To this end, we view the list
of all constructors as a right-associative binary tree, and we encode
a given constructor as the path that leads to it in this tree. If
there is just a single constructor, as for newtypes, for example, then
the computed bit stream is empty.

Otherwise we just recurse into subterms.

Well, we need to handle basic datatypes in a special way. We observe
them by seeing that the datatype for the term at hand does not offer
any constructors. Then, we turn the value into a string, which can now
be encoded by first converting it into a list of bit streams.

 
-}



module Main where
import Data.Generics
import Data.Char
import Maybe
import Monad
import CompanyDatatypes



-----------------------------------------------------------------------------



-- | We need bits and streams
data Bit = Zero | One deriving (Show, Typeable, Data)
type Bin = [Bit]



-----------------------------------------------------------------------------



-- | Generically map terms to bit streams
showBin :: Data t => t -> Bin

showBin t
  = if null cons 
      then showBin (map (int2bin . ord) (conString (toConstr t)))
      else con2bin cons (toConstr t) ++ concat (gmapQ showBin t)

 where
 
  -- Retrieve the list of all constructors
  cons :: [Constr]
  cons = dataTypeCons (dataTypeOf t)

  -- Encode a constructor
  con2bin :: [Constr] -> Constr -> Bin
  con2bin [x]    y | x == y     = []
  con2bin (x:_)  y | x == y     = [Zero]
  con2bin (x:xs) y              = One:con2bin xs y

-- Encode an integer as bit stream
int2bin :: Int -> Bin
int2bin 0          = []
int2bin x | even x = (Zero : int2bin (x `div` 2))
int2bin x | odd  x = (One  : int2bin (x `div` 2))



-----------------------------------------------------------------------------



-- | A reader monad on bit streams with failure
data ReadB a = ReadB (Bin -> (Maybe a, Bin))
unReadB (ReadB f) = f

instance Monad ReadB where
  return a = ReadB (\bs -> (Just a, bs))
  (ReadB c) >>= f = ReadB (\bs -> case c bs of
                             (Just a, bs')  -> unReadB (f a) bs'
                             (Nothing, bs') -> (Nothing, bs')
                          )

instance MonadPlus ReadB where
  mzero = ReadB (\bs -> (Nothing, bs))
  (ReadB f) `mplus` (ReadB g) = ReadB (\bs -> case f bs of
                                         (Just a, bs') -> (Just a, bs')
                                         (Nothing, _)  -> g bs
                                      )



-----------------------------------------------------------------------------



-- | Generically map bit streams to terms
readBin :: Data t => ReadB t
readBin = result
 where

  -- The worker, which we also use as type argument
  result = if null cons

             then do chars <- readBin
                     con   <- str2con (map (chr . bin2int) chars)
                     return (fromConstr con)

             else do con <- bin2con cons
                     gunfoldM con readBin


  -- Get the datatype for the type at hand
  myDataTypeOf :: Data a => ReadB a -> DataType
  myDataTypeOf (_::ReadB a) = dataTypeOf (undefined::a)

  -- Retrieve the list of all constructors
  cons :: [Constr]
  cons = dataTypeCons (myDataTypeOf result)

  -- Construct a constructor from a string
  str2con :: String -> ReadB Constr
  str2con = maybe mzero return
                . stringCon (myDataTypeOf result)

  -- Decode a constructor
  bin2con :: [Constr] -> ReadB Constr
  bin2con cs
    = ReadB ( \bs -> case (bs,cs) of 
                       (bs'     , [c]   ) -> (Just c, bs')
                       (Zero:bs', c:_   ) -> (Just c, bs')
                       (One :bs', _:cs' ) -> unReadB (bin2con cs') bs'
                       (bs'     , _     ) -> (Nothing, bs') 
            )

-- Decode an integer
bin2int :: Bin -> Int
bin2int []          = 0
bin2int (Zero : bs) = 2 * (bin2int bs)
bin2int (One  : bs) = 2 * (bin2int bs) + 1



-----------------------------------------------------------------------------



main = print $ ( showBin genCom
               , geq genCom genCom' 
               )
 where
  genCom' = fromJust (fst (unReadB readBin (showBin genCom))) :: Company