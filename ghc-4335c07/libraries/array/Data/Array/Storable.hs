-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Array.Storable
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable (uses Data.Array.MArray)
--
-- A storable array is an IO-mutable array which stores its
-- contents in a contiguous memory block living in the C
-- heap. Elements are stored according to the class 'Storable'.
-- You can obtain the pointer to the array contents to manipulate
-- elements from languages like C.
--
-- It is similar to 'Data.Array.IO.IOUArray' but slower.
-- Its advantage is that it's compatible with C.
--
-----------------------------------------------------------------------------

module Data.Array.Storable (
    -- * Arrays of 'Storable' things.
    StorableArray, -- data StorableArray index element
                   --  + index type must be in class Ix
                   --  + element type must be in class Storable

    -- * Overloaded mutable array interface
    -- | Module "Data.Array.MArray" provides the interface of storable arrays.
    -- They are instances of class 'MArray' (with the 'IO' monad).
    module Data.Array.MArray,

    -- * Accessing the pointer to the array contents
    withStorableArray,  -- :: StorableArray i e -> (Ptr e -> IO a) -> IO a

    touchStorableArray, -- :: StorableArray i e -> IO ()
  ) where

import Data.Array.MArray
import Data.Array.Storable.Internals
