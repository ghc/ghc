{-# OPTIONS_GHC -fno-implicit-prelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Dotnet
-- Copyright   :  (c) sof, 2003
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Primitive operations and types for doing .NET interop
-- 
-----------------------------------------------------------------------------
-- #hide
module GHC.Dotnet 
	( Object
	, unmarshalObject
	, marshalObject
	, unmarshalString
	, marshalString
	, checkResult
	) where

import GHC.Prim
import GHC.Base
import GHC.IO
import GHC.IOBase
import GHC.Ptr
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String

data Object a 
  = Object Addr#

checkResult :: (State# RealWorld -> (# State# RealWorld, a, Addr# #))
	    -> IO a
checkResult fun = IO $ \ st -> 
  case fun st of
    (# st1, res, err #) 
      | err `eqAddr#` nullAddr# -> (# st1, res #)
      | otherwise               -> throw (IOException (raiseError err)) st1
  
-- ToDo: attach finaliser.
unmarshalObject :: Addr# -> Object a
unmarshalObject x = Object x

marshalObject :: Object a -> (Addr# -> IO b) -> IO b
marshalObject (Object x) cont = cont x

-- dotnet interop support passing and returning
-- strings.
marshalString :: String 
	      -> (Addr# -> IO a)
	      -> IO a
marshalString str cont = withCString str (\ (Ptr x) -> cont x)

-- char** received back from a .NET interop layer.
unmarshalString :: Addr# -> String
unmarshalString p = unsafePerformIO $ do
   let ptr = Ptr p
   str <- peekCString ptr
   free ptr
   return str


-- room for improvement..
raiseError :: Addr# -> IOError
raiseError p = userError (".NET error: " ++ unmarshalString p)
