{-# OPTIONS -O2 -XBangPatterns #-}

-- The thing to look for here is that the implementation
-- of 'length' does not allocate in the inner loop
--
-- See Trac #3116

module T3116 where

import Foreign

data SByteString
   = BS  {-# UNPACK #-} !(ForeignPtr Word8)  -- payload
         {-# UNPACK #-} !Int                 -- offset
         {-# UNPACK #-} !Int                 -- length

data ByteString
    =  Empty
    |  Chunk {-# UNPACK #-} !SByteString ByteString

bnull :: ByteString -> Bool
bnull Empty = True
bnull _     = False

btail :: ByteString -> ByteString
btail Empty = error "empty tail"
btail (Chunk (BS fp s 1) cs) = cs
btail (Chunk (BS fp s l) cs) = Chunk (BS fp (s+1) (l-1)) cs

length :: ByteString -> Int
length = go 0
  where
    go !n bs  | bnull bs   = n
              | otherwise  = go (n+1) (btail bs)
