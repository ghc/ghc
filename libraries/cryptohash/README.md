:warning: this library is now deprecated in favor of `cryptonite`, which is
a superset of this, and also have more functionality. For new projects,
you should *not* use this library anymore, and preferably you should
convert old projects to `cryptonite` too.

:warning: the SHA3 implementation available in Crypto.Hash.SHA3 is
not SHA3 as standardized by NIST, but Keccak as submitted for the SHA3 contest.
A matching implementation is available as Keccak in `cryptonite`, although I would
recommend not to use unless you happens to really really need your digest value
to be compatible. On the other hand, the centralized `Crypto.Hash` export
a proper SHA3 implementation (as standardized by NIST)

CryptoHash
==========

`hs-cryptohash` provides many different secure digest algorithms, also
called cryptographic hash functions or, simply, cryptohashes. The package
exports common hash functions, as well as some more exotic ones, and
provides a single API for them all.

The general performance is comparable to the most optimised implementations
available.

Here is the complete list of supported algorithms:

* MD2, MD4, MD5 
* RIPEMD160
* SHA1
* SHA-2 family: 224, 256, 384, 512 and the newer 512t
* SHA-3 (aka Keccak)
* Skein: 256, 512
* Tiger
* Whirlpool

You can easily import any hash with the following:

    import qualified Crypto.Hash.<HASH> as <Hash>

We recommend using `import qualified` because the APIs are similar and
many of the modules reuse the same names. However, if you are ony using
one module, there is no need to qualify the names.

Incremental API
---------------

it's based on 4 different functions, similar to the lowlevel operations
of a typical hash:

* init: create a new hash context
* update: update non-destructively a new hash context with a strict bytestring
* updates: same as update, except that it takes a list of strict bytestring
* finalize: finalize the context and returns a digest bytestring.

all those operations are completely pure, and instead of changing the
context as usual in others language, it re-allocates a new context each time.

One Pass API
------------

The one pass API use the incremental API under the hood, but expose
common operations to create digests out of a bytestring and lazy bytestring.

* hash: create a digest (init+update+finalize) from a strict bytestring
* hashlazy: create a digest (init+update+finalize) from a lazy bytestring

More Type safety
----------------

A more type safe API is also available from Crypto.Hash. The API provides
all the supported hashes in the same namespace, through unified functions.

It introduces 2 new types, the Context type and the Digest type.
Both those types are parametrized with the HashAlgorithm used.

The API is very similar to each single hash module, except the types are
slightly different.

    import Crypto.Hash

    -- use the incremental API to hash the byte [1,2,3] with SHA1
    -- and print the hexadecimal digest.
    example1 = do
        let ctx = hashInit
            ctx' = hashUpdates ctx [ Data.ByteString.pack [1,2,3] ]
            dgt  = hashFinalize ctx' :: Digest SHA1
        putStrLn $ show dgt

    -- use the one-pass API to hash the byte 1,2,3 with SHA3_512
    -- and print the hexadecimal digest.
    example2 = do
        let dgt  = hash (Data.ByteString.pack [1,2,3]) :: Digest SHA3_512
        putStrLn $ show dgt

Performance
-----------

Cryptohash uses C implementations to provide maximum performance.
see the cbits directory for more information
