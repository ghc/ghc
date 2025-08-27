{-# LANGUAGE Safe #-}

{-|
    This module bridges between Haskell handles and underlying operating-system
    features.
-}
module System.IO.OS
(
    -- * Obtaining file descriptors and Windows handles
    withFileDescriptorReadingBiased,
    withFileDescriptorWritingBiased,
    withWindowsHandleReadingBiased,
    withWindowsHandleWritingBiased,
    withFileDescriptorReadingBiasedRaw,
    withFileDescriptorWritingBiasedRaw,
    withWindowsHandleReadingBiasedRaw,
    withWindowsHandleWritingBiasedRaw

    -- ** Caveats
    -- $with-ref-caveats
)
where

import GHC.Internal.System.IO.OS
       (
           withFileDescriptorReadingBiased,
           withFileDescriptorWritingBiased,
           withWindowsHandleReadingBiased,
           withWindowsHandleWritingBiased,
           withFileDescriptorReadingBiasedRaw,
           withFileDescriptorWritingBiasedRaw,
           withWindowsHandleReadingBiasedRaw,
           withWindowsHandleWritingBiasedRaw
       )

-- ** Caveats

{-$with-ref-caveats
    #with-ref-caveats#There are the following caveats regarding the above
    operations:

      * Flushing of buffers can fail if the given handle is readable but not
        seekable.

      * If one of these operations is performed as part of an action executed by
        'System.IO.Unsafe.unsafePerformIO',
        'System.IO.Unsafe.unsafeInterleaveIO', or one of their “dupable”
        variants and the user-provided action receives an asychnchronous
        exception and does not catch it, then the following happens:

          - Before the overall computation is suspended, the blocking of handle
            operations is removed.

          - When the computation is later resumed due to another evaluation
            attempt, the blocking of handle operations is reinstantiated, the
            Haskell-managed buffers are flushed again, and the user-provided
            action is run from the beginning.

        Repeating the previously executed part of the user-provided action
        cannot be avoided apparently. See the @[async]@ note in the source code
        of "GHC.Internal.IO.Handle.Internals" for further explanation.
-}
