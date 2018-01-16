{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
-- | Import all modules from prelude that should be safe
module Main where

import Numeric
import Prelude

import Control.Applicative
import Control.Arrow
import Control.Category

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Concurrent.QSemN

import Control.Exception
import Control.Exception.Base

import Control.Monad
import Control.Monad.Fix
import Control.Monad.Instances
import Control.Monad.Zip

import Control.Monad.ST
import Control.Monad.ST.Lazy
import Control.Monad.ST.Strict

import Data.Bifunctor
import Data.Bits
import Data.Bool
import Data.Char
-- import Data.Coerce
import Data.Complex
import Data.Data
import Data.Dynamic
import Data.Either
import Data.Eq
import Data.Fixed
import Data.Foldable
import Data.Function
import Data.Functor.Identity
import Data.Functor
import Data.IORef
import Data.Int
import Data.Ix
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Proxy
import Data.Ratio
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Typeable
import Data.Unique
import Data.Version
import Data.Word

import Data.STRef
import Data.STRef.Lazy
import Data.STRef.Strict

import Data.Type.Bool
-- import Data.Type.Coercion
-- import Data.Type.Equality

-- import Debug.Trace

import Foreign

import Foreign.Concurrent
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable

import Foreign.C
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types

import Foreign.Marshal
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Marshal.Error
import Foreign.Marshal.Pool
import Foreign.Marshal.Utils

import System.CPUTime
import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO
import System.IO.Error
import System.Info
import System.Mem
import System.Mem.StableName
import System.Mem.Weak
import System.Posix.Internals
import System.Posix.Types
import System.Timeout

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import Text.Printf
import Text.Read
import Text.Read.Lex
import Text.Show
import Text.Show.Functions

import Type.Reflection

-- import Unsafe.Coerce

f :: Int
f = 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

