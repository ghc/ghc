-- |
-- Module      :  GHC.RTS.Flags.Experimental
-- Copyright   :  (c) The University of Glasgow, 1994-2000
-- License     :  see libraries/ghc-experimental/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- /The API of this module is unstable and is coupled to GHC's internals./ As
-- such if you depend on it, you should expect to follow GHC's releases. This
-- API could change without warning.
--
-- Descriptions of flags can be seen in
-- <https://www.haskell.org/ghc/docs/latest/html/users_guide/runtime_control.html GHC User's Guide>,
-- or by running RTS help message using @+RTS --help@.
--
--

module GHC.RTS.Flags.Experimental
  ( RtsTime
  , RTSFlags (..)
  , GiveGCStats (..)
  , GCFlags (..)
  , ConcFlags (..)
  , MiscFlags (..)
  , IoManagerFlag (..)
  , DebugFlags (..)
  , DoCostCentres (..)
  , CCFlags (..)
  , DoHeapProfile (..)
  , ProfFlags (..)
  , DoTrace (..)
  , TraceFlags (..)
  , TickyFlags (..)
  , ParFlags (..)
  , HpcFlags (..)
  , {-# DEPRECATED "import GHC.IO.SubSystem (IoSubSystem (..))" #-}
    IoSubSystem (..)
  , getRTSFlags
  , getGCFlags
  , getConcFlags
  , getMiscFlags
  , getDebugFlags
  , getCCFlags
  , getProfFlags
  , getTraceFlags
  , getTickyFlags
  , getParFlags
  , getHpcFlags
  ) where

import GHC.Internal.RTS.Flags
import GHC.Internal.IO.SubSystem (IoSubSystem(..))
