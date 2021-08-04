See also https://pvp.haskell.org/faq

## Version 1.3.2.0

 * Add `Hashable (Fixed a)` for `base <4.7` versions.
 * Add documentation:
   - `hashable` is not a stable hash
   - `hashWithSalt` may return negative values
   - there is `time-compat` with `Hashable` instances for `time` types.
 * Add `random-initial-seed` flag causing the initial seed
   to be randomized on each start of an executable using `hashable`.

## Version 1.3.1.0

 * Add `Hashable1` instances to `semigroups` types.

 * Use `ghc-bignum` with GHC-9.0

 * Use FNV-1 constants.

 * Make `hashable-examples` a test-suite

## Version 1.3.0.0

 * Semantic change of `Hashable Arg` instance to *not* hash the second
   argument of `Arg` in order to be consistent with `Eq Arg` (#171)

 * Semantic change of `Hashable Float` and `Hashable Double` instances
   to hash `-0.0` and `0.0` to the same value (#173)

 * Add `Hashable` instance for `Fingerprint` (#156)

 * Add new `Data.Hashable.Generic` module providing the default
   implementations `genericHashWithSalt` and `genericLiftHashWithSalt`
   together with other Generics support helpers (#148, #178)

 * Bump minimum version requirement of `base` to `base-4.5` (i.e. GHC >= 7.4)

----

## Version 1.2.7.0

 * Add `Hashable` and `Hashable1` instances for `Complex`

 * Fix undefined behavior in `hashable_fn_hash()` implementation
   due to signed integer overflow (#152)

 * Mark `Data.Hashable.Lifted` as `Trustworthy` (re SafeHaskell)

 * Support GHC 8.4

## Version 1.2.6.1

 * Use typeRepFingerprint from Type.Reflection.Unsafe

 * Bump minimum version of base to 4.4.

## Version 1.2.6.0

 * Add support for type-indexed `Typeable`.

 * Rework the `Generic` hashable for sums.

## Version 1.2.5.0

  * Add `Hashable1` and `Hashable2`

  * Add instances for: `Eq1`, `Ord1`, `Show1`, `Ptr`, `FunPtr`, `IntPtr`, `WordPtr`

  * Add `Hashed` type for caching the `hash` function result.

## Version 1.2.4.0

 * Add instances for: Unique, Version, Fixed, NonEmpty, Min, Max, Arg,
   First, Last, WrappedMonoid, Option

 * Support GHC 8.0

## Version 1.2.3.3

 * Support integer-simple.

## Version 1.2.3.2

 * Add support for GHC 7.10 typeRepFingerprint

## Version 1.2.3.1

 * Added support for random 1.1.*.

## Version 1.2.3.0

 * Silence integer literal overflow warning

 * Add support for GHC 7.10 `integer-gmp2` & `Natural`

 * Add instance for Data.Void

 * Make the SSE .cabal flags manual

 * Add an upper bound on bytestring

## Version 1.2.2.0

 * Add instances for `Data.ByteString.Short`

 * Use a 32-bit default salt on 32-bit archs.

## Version 1.2.1.0

 * Revert instances to their 1.1 implementations to regain the
   performance we had then.

 * Remove use of random salt altogether. Without using SipHash the
   benefit is unclear (i.e. collision attacks still work) and the
   complexity is no longer worth it.

 * Documentation improvements.

## Version 1.2.0.10

 * Fix for GHC 7.0.

## Version 1.2.0.9

 * Stop using SipHash. The current implementation still has segfault
   causing bugs that we won't be able to fix soon.

 * Stop using Wang hash. It degrades performance of fixed-size integer
   hashing too much.

## Version 1.2.0.8

 * Fix linking issue when SSE was disabled.

 * Hash small signed Integers correctly.

## Version 1.2.0.7

 * Add flags to control usage of SSE.

## Version 1.2.0.6

 * Fix another segfault caused by SSE2 code.

## Version 1.2.0.5

 * More portability fixes.

 * Force stack alignment to 16 bytes everywhere. Fixes a segfault.

 * Fix bug where code relied on rewrite rules firing for correctness.

## Version 1.2.0.4

 * Update docs to match code.

 * Work around bug in GHCi runtime linker, which never call static
   initializers.

## Version 1.2.0.3

 * Make building of SSE 4.1 code conditional, as it doesn't work on all
   platforms.

 * Use a fixed salt, but allow random salting. Random salting by
   default broke people's code.

## Version 1.2.0.2

 * Work around ghci linker bug on Windows.

## Version 1.2.0.1

 * Fix performance bug in SSE implementation of SipHash.

 * Fix segfault due to incorrect stack alignment on Windows.

## Version 1.2.0.0

 * Switch string hashing from FNV-1 to SipHash, in an effort to
   prevent collision attacks.

 * Switch fixed-size integer hashing to Wang hash.

 * The default salt now switched on every program run, in an effort to
   prevent collision attacks.

 * Move hash method out of Hashable type class.

 * Add support for generic instance deriving.

 * Add instance for Ordering.

----

## Version 1.1.2.5

 * Bug fix for bytestring < 0.10.0.

## Version 1.1.2.4

 * Switch string hashing from Bernstein to FNV-1

 * Faster instance for Integer.

 * Update dependency on base, ghc-prim

 * Now works with GHC 7.6.

## Version 1.1.2.3

 * Add instance for TypeRep.

 * Update dependency on test-framework.

## Version 1.1.2.2

 * Bug fix for GHC 7.4

## Version 1.1.2.1

 * Update dependency on test-framework.

 * Improve documentation of combine.

## Version 1.1.2.0

 * Fix hash collision issues for lists and tuples when using a
   user-specified salt.

 * Add instances for `Integer`, `Ratio`, `Float`, `Double`, and `StableName`.

 * Improved instances for tuples and lists.

## Version 1.1.1.0

 * Add `hashWithSalt`, which allows the user to create different hash
   values for the same input by providing different seeds. This is
   useful for application like Cuckoo hashing which need a family of
   hash functions.

 * Fix a bug in the `Hashable` instance for `Int64`/`Word64` on 32-bit
   platforms.

 * Improved resilience to leading zero in the input being hashed.

## Version 1.1.0.0

 * Add instance for: strict and lazy Texts, ThreadId

 * Add hashPtrWithSalt and hashByteArrayWithSalt.

 * Faster ByteArray# hashing.

 * Fix a signedness bug that affected ByteString.

 * Fix ByteString hashing to work correctly on both 32 and 64-bit
   platforms.

## Version 1.0.1.1

 * Fix bug in Hashable instance for lazy ByteStrings where differences
   in the internal structure of the ByteString could cause different
   hash values for ByteStrings that are equal according to ==.

## Version 1.0.1.0

 * Add two helpers for creating Hashable instances: hashPtr and
   hashByteArray.

----

## Version 1.0.0

 * Separate Hashable class to its own package from hashmap 1.0.0.3.
