module GHC.Event.Windows.Clock (
    Clock,
    Seconds,
    getTime,
    getClock,

    -- * Specific implementations
    queryPerformanceCounter,
    getTickCount64
) where

import GHC.Internal.Event.Windows.Clock
