#if defined(__YALE_HASKELL__)
-- Native.hs -- native data conversions and I/O
--
-- author :  Sandra Loosemore
-- date   :  07 Jun 1994
--
--
-- Unlike in the original hbc version of this library, a Byte is a completely
-- abstract data type and not a character.  You can't read and write Bytes
-- to ordinary text files; you must use the operations defined here on
-- Native files.
-- It's guaranteed to be more efficient to read and write objects directly
-- to a file than to do the conversion to a Byte stream and read/write
-- the Byte stream.
#endif

module Native(
       Native(..), Bytes(..),
       shortIntToBytes, bytesToShortInt,
       longIntToBytes, bytesToLongInt, 
       showB, readB
#if __HASKELL1__ < 3
       , Maybe..
#endif
#if defined(__YALE_HASKELL__)
       , openInputByteFile, openOutputByteFile, closeByteFile
       , readBFile, readBytesFromByteFile
       , shortIntToByteFile, bytesToShortIntIO
       , ByteFile
       , Byte
#endif       
    ) where

#if __HASKELL1__ < 3
import  {-flummox mkdependHS-}
	Maybe
#endif

#if defined(__YALE_HASKELL__)
import NativePrims

-- these data types are completely opaque on the Haskell side.

data Byte = Byte
data ByteFile = ByteFile
type Bytes = [Byte]

instance Text(Byte) where
 showsPrec _ _ = showString "Byte"

instance Text(ByteFile) where
 showsPrec _ _ = showString "ByteFile"

-- Byte file primitives

openInputByteFile	:: String -> IO (ByteFile)
openOutputByteFile	:: String -> IO (ByteFile)
closeByteFile		:: ByteFile -> IO ()

openInputByteFile	= primOpenInputByteFile
openOutputByteFile	= primOpenOutputByteFile
closeByteFile		= primCloseByteFile
#endif {- YALE-}

#if defined(__GLASGOW_HASKELL__)
import ByteOps -- partain
type Bytes = [Char]
#endif

#if defined(__HBC__)
import LMLbyteops
type Bytes = [Char]
#endif

-- Here are the basic operations defined on the class.

class Native a where

    -- these are primitives
    showBytes     :: a -> Bytes -> Bytes	        -- convert to bytes
    readBytes     :: Bytes -> Maybe (a, Bytes)	        -- get an item and the rest
#if defined(__YALE_HASKELL__)
    showByteFile  :: a -> ByteFile -> IO ()
    readByteFile  :: ByteFile -> IO a
#endif

    -- these are derived
    listShowBytes :: [a] -> Bytes -> Bytes	        -- convert a list to bytes
    listReadBytes :: Int -> Bytes -> Maybe ([a], Bytes) -- get n items and the rest
#if defined(__YALE_HASKELL__)
    listShowByteFile :: [a] -> ByteFile -> IO ()
    listReadByteFile :: Int -> ByteFile -> IO [a]
#endif

    -- here are defaults for the derived methods.
  
    listShowBytes []     bs = bs
    listShowBytes (x:xs) bs = showBytes x (listShowBytes xs bs)

    listReadBytes 0 bs = Just ([], bs)
    listReadBytes n bs = 
	case readBytes bs of
	Nothing -> Nothing
	Just (x,bs') ->
		case listReadBytes (n-1) bs' of
		Nothing -> Nothing
		Just (xs,bs'') -> Just (x:xs, bs'')

#if defined(__YALE_HASKELL__)
    listShowByteFile l f =
      foldr (\ head tail -> (showByteFile head f) >> tail)
	    (return ())
	    l

    listReadByteFile 0 f =
      return []
    listReadByteFile n f =
      readByteFile f     	    	>>= \ h ->
      listReadByteFile (n - 1) f	>>= \ t ->
      return (h:t)
#endif

#if ! defined(__YALE_HASKELL__)
-- Some utilities that Yale doesn't use
hasNElems :: Int -> [a] -> Bool
hasNElems 0 _      = True
hasNElems 1 (_:_)  = True		-- speedup
hasNElems 2 (_:_:_)  = True		-- speedup
hasNElems 3 (_:_:_:_)  = True		-- speedup
hasNElems 4 (_:_:_:_:_)  = True		-- speedup
hasNElems _ []     = False
hasNElems n (_:xs) = hasNElems (n-1) xs

lenLong   = length (longToBytes   0 [])
lenInt    = length (intToBytes    0 [])
lenShort  = length (shortToBytes  0 [])
lenFloat  = length (floatToBytes  0 [])
lenDouble = length (doubleToBytes 0 [])
#endif

-- Basic instances, defined as primitives

instance Native Char where
#if defined(__YALE_HASKELL__)
    showBytes		= primCharShowBytes
    readBytes		= primCharReadBytes
    showByteFile	= primCharShowByteFile
    readByteFile	= primCharReadByteFile
#else
    showBytes	c bs = c:bs
    readBytes [] = Nothing
    readBytes (c:cs) = Just (c,cs)
    listReadBytes n bs = f n bs []
	where f 0 bs cs = Just (reverse cs, bs)
	      f _ [] _  = Nothing
	      f n (b:bs) cs = f (n-1::Int) bs (b:cs)
#endif

instance Native Int where
#if defined(__YALE_HASKELL__)
    showBytes		= primIntShowBytes
    readBytes		= primIntReadBytes
    showByteFile	= primIntShowByteFile
    readByteFile	= primIntReadByteFile
#else
    showBytes i bs = intToBytes i bs
    readBytes bs = if hasNElems lenInt bs then Just (bytesToInt bs) else Nothing
#endif

instance Native Float where
#if defined(__YALE_HASKELL__)
    showBytes		= primFloatShowBytes
    readBytes		= primFloatReadBytes
    showByteFile	= primFloatShowByteFile
    readByteFile	= primFloatReadByteFile
#else
    showBytes i bs = floatToBytes i bs
    readBytes bs = if hasNElems lenFloat bs then Just (bytesToFloat bs) else Nothing
#endif

instance Native Double where
#if defined(__YALE_HASKELL__)
    showBytes		= primDoubleShowBytes
    readBytes		= primDoubleReadBytes
    showByteFile	= primDoubleShowByteFile
    readByteFile	= primDoubleReadByteFile
#else
    showBytes i bs = doubleToBytes i bs
    readBytes bs = if hasNElems lenDouble bs then Just (bytesToDouble bs) else Nothing
#endif

instance Native Bool where
#if defined(__YALE_HASKELL__)
    showBytes		= primBoolShowBytes
    readBytes		= primBoolReadBytes
    showByteFile	= primBoolShowByteFile
    readByteFile	= primBoolReadByteFile
#else
    showBytes b bs = if b then '\x01':bs else '\x00':bs
    readBytes [] = Nothing
    readBytes (c:cs) = Just(c/='\x00', cs)
#endif

#if defined(__YALE_HASKELL__)
-- Byte instances, so you can write Bytes to a ByteFile

instance Native Byte where
    showBytes		= (:)
    readBytes l =
      case l of
	[]  -> Nothing
	h:t -> Just(h,t)
    showByteFile		= primByteShowByteFile
    readByteFile		= primByteReadByteFile
#endif

-- A pair is stored as two consecutive items.
instance (Native a, Native b) => Native (a,b) where
    showBytes (a,b) = showBytes a . showBytes b
    readBytes bs = readBytes bs  `thenMaybe` \(a,bs') -> 
                   readBytes bs' `thenMaybe` \(b,bs'') ->
                   Just ((a,b), bs'')
#if defined(__YALE_HASKELL__)
    showByteFile (a,b) f = (showByteFile a f) >> (showByteFile b f)

    readByteFile f =
      readByteFile f	    >>= \ a ->
      readByteFile f	    >>= \ b ->
      return (a,b)
#endif

-- A triple is stored as three consectutive items.
instance (Native a, Native b, Native c) => Native (a,b,c) where
    showBytes (a,b,c) = showBytes a . showBytes b . showBytes c
    readBytes bs = readBytes bs  `thenMaybe` \(a,bs') -> 
                   readBytes bs' `thenMaybe` \(b,bs'') ->
                   readBytes bs'' `thenMaybe` \(c,bs''') ->
                   Just ((a,b,c), bs''')
#if defined(__YALE_HASKELL__)
    showByteFile (a,b,c) f =
      (showByteFile a f) >>
      (showByteFile b f) >>
      (showByteFile c f)

    readByteFile f =
      readByteFile f	>>= \ a ->
      readByteFile f	>>= \ b ->
      readByteFile f	>>= \ c ->
      return (a,b,c)
#endif

-- A list is stored with an Int with the number of items followed by the items.
instance (Native a) => Native [a] where
    showBytes xs bs = showBytes (length xs) (f xs) where f [] = bs
                                                         f (x:xs) = showBytes x (f xs)
    readBytes bs = readBytes bs `thenMaybe` \(n,bs') ->
                   listReadBytes n bs' `thenMaybe` \(xs, bs'') ->
                   Just (xs, bs'')
#if defined(__YALE_HASKELL__)
    showByteFile l f = (showByteFile (length l) f) >> (listShowByteFile l f)
    readByteFile f = readByteFile f >>= \ n -> listReadByteFile n f
#endif

-- A Maybe is stored as a Boolean possibly followed by a value
instance (Native a) => Native (Maybe a) where
#if !defined(__YALE_HASKELL__)
    showBytes Nothing = ('\x00' :)
    showBytes (Just x) = ('\x01' :) . showBytes x
    readBytes ('\x00':bs) = Just (Nothing, bs)
    readBytes ('\x01':bs) = readBytes bs `thenMaybe` \(a,bs') ->
                            Just (Just a, bs')
    readBytes _ = Nothing
#else
    showBytes (Just a) = showBytes True . showBytes a
    showBytes Nothing  = showBytes False
    readBytes bs =
	readBytes bs		`thenMaybe` \ (isJust, bs') ->
	if isJust then
		readBytes bs'	`thenMaybe` \ (a, bs'') ->
		Just (Just a, bs'')
	else
		Just (Nothing, bs')

    showByteFile (Just a) f = showByteFile True f >> showByteFile a f
    showByteFile Nothing  f = showByteFile False f
    readByteFile f = 
	readByteFile f		>>= \ isJust ->
	if isJust then
		readByteFile f	>>= \ a ->
		return (Just a)
	else
		return Nothing
#endif

instance (Native a, Ix a, Native b) => Native (Array a b) where
    showBytes a = showBytes (bounds a) . showBytes (elems a)
    readBytes bs = readBytes bs `thenMaybe` \(b, bs')->
                   readBytes bs' `thenMaybe` \(xs, bs'')->
		   Just (listArray b xs, bs'')

shortIntToBytes :: Int   -> Bytes -> Bytes
bytesToShortInt :: Bytes -> Maybe (Int, Bytes)
longIntToBytes  :: Int   -> Bytes -> Bytes
bytesToLongInt  :: Bytes -> Maybe (Int, Bytes)
#if defined(__YALE_HASKELL__)
shortIntToByteFile	:: Int -> ByteFile -> IO ()
bytesToShortIntIO       :: ByteFile -> IO Int
#endif

#if defined(__YALE_HASKELL__)
-- These functions are like the primIntxx but use a "short" rather than
-- "int" representation.
shortIntToBytes		= primShortShowBytes
bytesToShortInt 	= primShortReadBytes
shortIntToByteFile	= primShortShowByteFile
bytesToShortIntIO 	= primShortReadByteFile

#else {-! YALE-}

shortIntToBytes s bs = shortToBytes s bs

bytesToShortInt bs = if hasNElems lenShort bs then Just (bytesToShort bs) else Nothing

longIntToBytes s bs = longToBytes s bs

bytesToLongInt bs = if hasNElems lenLong bs then Just (bytesToLong bs) else Nothing

#endif {-! YALE-}

showB :: (Native a) => a -> Bytes
showB x = showBytes x []

readB :: (Native a) => Bytes -> a
readB bs = 
	case readBytes bs of
	Just (x,[]) -> x
	Just (_,_)  -> error "Native.readB data too long"
        Nothing     -> error "Native.readB data too short"

#if defined(__YALE_HASKELL__)
readBFile :: String -> IO(Bytes)
readBFile name =
  openInputByteFile name >>= \ f ->
  readBytesFromByteFile f

readBytesFromByteFile :: ByteFile -> IO(Bytes)
readBytesFromByteFile f =
  try
    (primByteReadByteFile f  >>= \ h -> 
     readBytesFromByteFile f >>= \ t ->
     return (h:t))
    onEOF
 where
   onEOF EOF = closeByteFile f >> return []
   onEOF err = closeByteFile f >> failwith err
#endif
