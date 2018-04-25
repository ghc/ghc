{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Data.Bits (Bits(..))
import Data.Int (Int32)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCString, withCString, withCStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (Storable(..))
import System.IO (IOMode(..), hGetContents, withFile)

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

foreign import WINDOWS_CCONV "D3DCompile" c_d3dCompile
 :: Ptr () -> Word32 -> CString ->
    Ptr D3DShaderMacro -> Ptr ID3DInclude ->
    CString -> CString -> D3DCompileFlag -> D3DCompileEffectFlag ->
    Ptr (Ptr ID3DBlob) -> Ptr (Ptr ID3DBlob) -> IO HRESULT

maybePoke :: (Storable a) => Maybe a -> (Ptr a -> IO b) -> IO b
maybePoke Nothing proc = proc nullPtr
maybePoke (Just m) proc = alloca $ \ptr -> do
  poke ptr m
  proc ptr

maybeWithCString :: Maybe String -> (CString -> IO a) -> IO a
maybeWithCString Nothing proc = proc nullPtr
maybeWithCString (Just m) proc = withCString m proc

type HRESULT = LONG
data ID3DBlob = ID3DBlob
data ID3DInclude = ID3DInclue
type LONG = Int32

data D3DShaderMacro = D3DShaderMacro
  { _name :: String
  , _definition :: String }

instance Storable D3DShaderMacro where
  sizeOf _ = 8
  alignment _ = 8
  peek ptr = do
    n <- peekByteOff ptr 0
    d <- peekByteOff ptr 4
    n' <- peekCString n
    d' <- peekCString d
    return $ D3DShaderMacro n' d'
  poke ptr (D3DShaderMacro n d) = do
    withCString n $ \n' -> withCString d $ \d' -> do
      pokeByteOff ptr 0 n'
      pokeByteOff ptr 4 d'

type D3DCompileFlag = Word32
type D3DCompileEffectFlag = Word32

d3dCompileEnableStrictness :: D3DCompileFlag
d3dCompileEnableStrictness = shift 1 11

d3dCompile
  :: String -> Maybe String ->
     Maybe D3DShaderMacro -> Ptr ID3DInclude ->
     Maybe String -> String ->
     [D3DCompileFlag] -> [D3DCompileEffectFlag] ->
     IO (Either (HRESULT, Ptr ID3DBlob) (Ptr ID3DBlob))
d3dCompile source sourceName defines pInclude entryPoint target compileFlags effectFlags = do
  withCStringLen source $ \(csource, len) -> withCString target $ \pTarget ->
    maybeWithCString sourceName $ \pSourceName -> maybePoke defines $ \pDefines ->
      maybeWithCString entryPoint $ \pEntryPoint -> alloca $ \ppCode -> alloca $ \ppErrorMsgs -> do
        let sFlag = foldl (.|.) 0 compileFlags
        let eFlag = foldl (.|.) 0 effectFlags
        putStrLn "Before d3dCompile"
        hr <- c_d3dCompile
                (castPtr csource)
                (fromIntegral len)
                pSourceName
                pDefines
                pInclude
                pEntryPoint
                pTarget
                sFlag
                eFlag
                ppCode
                ppErrorMsgs
        putStrLn "After d3dCompile"
        if hr < 0
        then do
          pErrorMsgs <- peek ppErrorMsgs
          return $ Left (hr, pErrorMsgs)
        else do
          pCode <- peek ppCode
          return $ Right pCode

d3dCompileFromFile
  :: String -> Maybe String ->
     Maybe D3DShaderMacro -> Ptr ID3DInclude ->
     Maybe String -> String ->
     [D3DCompileFlag] -> [D3DCompileEffectFlag] ->
     IO (Either (HRESULT, Ptr ID3DBlob) (Ptr ID3DBlob))
d3dCompileFromFile fileName sourceName defines pInclude entryPoint target compileFlags effectFlags =
  withFile fileName ReadMode $ \handle -> do
    contents <- hGetContents handle
    d3dCompile contents sourceName defines pInclude entryPoint target compileFlags effectFlags

main :: IO ()
main = do
  _vb <- compileShaderFromFile "Triangle.fx" "VS" "vs_4_0"
  return ()

compileShaderFromFile :: String -> String -> String -> IO (Ptr ID3DBlob)
compileShaderFromFile fileName entryPoint shaderModel = do
  Right res <- d3dCompileFromFile
      fileName
      Nothing
      Nothing
      nullPtr
      (Just entryPoint)
      shaderModel
      [d3dCompileEnableStrictness]
      []
  return res
