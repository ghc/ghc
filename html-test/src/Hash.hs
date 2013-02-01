{- |
  Implementation of fixed-size hash tables, with a type 
  class for constructing hash values for structured types.
-}
module Hash (
  -- * The @HashTable@ type
  HashTable,

  -- ** Operations on @HashTable@s
  new, insert, lookup,

  -- * The @Hash@ class
  Hash(..),
 ) where

import Data.Array
import Prelude hiding (lookup)

-- | A hash table with keys of type @key@ and values of type @val@.
-- The type @key@ should be an instance of 'Eq'.
data HashTable key val = HashTable Int (Array Int [(key,val)])

-- | Builds a new hash table with a given size
new :: (Eq key, Hash key) => Int -> IO (HashTable key val)
new = undefined

-- | Inserts a new element into the hash table
insert :: (Eq key, Hash key) => key -> val -> IO ()
insert = undefined

-- | Looks up a key in the hash table, returns @'Just' val@ if the key
-- was found, or 'Nothing' otherwise.
lookup 	:: Hash key => key -> IO (Maybe val)
lookup = undefined

-- | A class of types which can be hashed.
class Hash a where
   -- | hashes the value of type @a@ into an 'Int'
   hash :: a -> Int

instance Hash Int where
   hash = id

instance Hash Float where
   hash = trunc

instance (Hash a, Hash b) => Hash (a,b) where
   hash (a,b) = hash a `xor` hash b

trunc = undefined
xor = undefined
