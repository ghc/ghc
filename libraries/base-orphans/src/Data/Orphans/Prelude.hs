{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
#endif

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE NoStarIsType #-}
#endif

{-# OPTIONS_GHC -fno-warn-deprecations #-}

{-|
Exports modules that Data.Orphans needs. Because Data.Orphans uses several modules
that only need to be in scope for certain versions of GHC, exporting all of the modules
separately eliminates the need to use CPP pragmas for GHC-version-specific imports.
This makes it much easier to be -Wall-compliant.

Note that this module does not export any modules that could introduce name clashes.
-}
module Data.Orphans.Prelude
#if MIN_VERSION_base(4,15,0)
    () where
#else
    ( module OrphansPrelude
    , realPart
    , imagPart
    , Typeable1
    , Typeable2
    , Typeable3
    , Typeable4
    , Typeable5
    , Typeable6
    , Typeable7
    ) where

import Control.Applicative as OrphansPrelude
import Control.Arrow as OrphansPrelude hiding (first, loop, second)
import Control.Category as OrphansPrelude hiding ((.), id)
import Control.Concurrent.QSem as OrphansPrelude
import Control.Monad as OrphansPrelude hiding (mapM, sequence)
import Control.Monad.Fix as OrphansPrelude

import Data.Bits as OrphansPrelude
import Data.Char as OrphansPrelude
import Data.Complex as OrphansPrelude (Complex(..))
import Data.Data as OrphansPrelude (Data(..), Constr, DataType, mkConstr, mkDataType)
import Data.Fixed as OrphansPrelude
import Data.Int as OrphansPrelude
import Data.List as OrphansPrelude (genericLength)
import Data.Monoid as OrphansPrelude
  hiding ( Any(..)
# if MIN_VERSION_base(4,5,0)
         , (<>)
# endif
         )
import Data.String as OrphansPrelude (IsString)
import Data.Typeable as OrphansPrelude (Typeable)
import Data.Version as OrphansPrelude
import Data.Word as OrphansPrelude

import Foreign.C.Error as OrphansPrelude
import Foreign.C.Types as OrphansPrelude
import Foreign.Marshal.Pool as OrphansPrelude
import Foreign.Ptr as OrphansPrelude
import Foreign.Storable as OrphansPrelude

import GHC.Arr as OrphansPrelude (Ix(..))
import GHC.Base as OrphansPrelude
import GHC.Conc as OrphansPrelude
import GHC.Desugar as OrphansPrelude (AnnotationWrapper)
import GHC.ForeignPtr as OrphansPrelude
import GHC.IO.Buffer as OrphansPrelude
import GHC.IO.BufferedIO as OrphansPrelude (BufferedIO)
import GHC.IO.Device as OrphansPrelude (IODevice, IODeviceType(..), RawIO)
import GHC.IO.Encoding as OrphansPrelude
import GHC.IO.Handle as OrphansPrelude
import GHC.IO.Handle.Types as OrphansPrelude
import GHC.Real as OrphansPrelude (Ratio(..), (%))
import GHC.ST as OrphansPrelude

import Numeric as OrphansPrelude (showHex)

import System.Console.GetOpt as OrphansPrelude
import System.IO as OrphansPrelude
import System.Posix.Internals as OrphansPrelude
import System.Posix.Types as OrphansPrelude

import Text.Printf as OrphansPrelude

import Unsafe.Coerce as OrphansPrelude (unsafeCoerce)

# if defined(mingw32_HOST_OS)
import GHC.IO.Encoding.CodePage.Table as OrphansPrelude
# endif

# if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip as OrphansPrelude
import GHC.Fingerprint as OrphansPrelude
import GHC.IO.Encoding.Failure as OrphansPrelude

#  if !defined(mingw32_HOST_OS) && !defined(__GHCJS__)
import GHC.Event as OrphansPrelude
#  endif
# endif

# if MIN_VERSION_base(4,5,0)
import GHC.Stack as OrphansPrelude
import GHC.Stats as OrphansPrelude
# endif

# if MIN_VERSION_base(4,6,0)
import Data.Ord as OrphansPrelude (Down(..))
import GHC.GHCi as OrphansPrelude
import GHC.TypeLits as OrphansPrelude hiding (type (*))
# else
import GHC.Exts as OrphansPrelude (Down(..))
# endif

# if MIN_VERSION_base(4,6,0) && !(MIN_VERSION_base(4,8,2))
import GHC.IP as OrphansPrelude
# endif

# if MIN_VERSION_base(4,7,0)
import Data.Proxy as OrphansPrelude
import Data.Type.Coercion as OrphansPrelude (Coercion, TestCoercion)
import Data.Type.Equality as OrphansPrelude ((:~:)(..), TestEquality(..))
import GHC.Exts as OrphansPrelude (IsList(..))
import Text.Read.Lex as OrphansPrelude (Number)
# else
import Control.Concurrent.SampleVar as OrphansPrelude
# endif

# if MIN_VERSION_base(4,8,0)
import Data.Bifunctor as OrphansPrelude
import Data.Functor.Identity as OrphansPrelude
#endif

# if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes as OrphansPrelude
import Data.Functor.Compose as OrphansPrelude
import Data.Semigroup as OrphansPrelude (stimesMonoid)
# endif

# if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,12,0))
import Data.List.NonEmpty as OrphansPrelude (NonEmpty(..))
import Data.Semigroup as OrphansPrelude (Semigroup(..))
# endif

# if !(MIN_VERSION_base(4,11,0))
import Data.Typeable ( Typeable1, Typeable2, Typeable3, Typeable4
                     , Typeable5, Typeable6, Typeable7 )
# endif

# if MIN_VERSION_base(4,4,0)
realPart, imagPart :: Complex a -> a
# else
realPart, imagPart :: RealFloat a => Complex a -> a
# endif
realPart (x :+ _) = x
imagPart (_ :+ y) = y

# if MIN_VERSION_base(4,11,0)
type Typeable1 (a :: Type -> Type)                                                 = Typeable a
type Typeable2 (a :: Type -> Type -> Type)                                         = Typeable a
type Typeable3 (a :: Type -> Type -> Type -> Type)                                 = Typeable a
type Typeable4 (a :: Type -> Type -> Type -> Type -> Type)                         = Typeable a
type Typeable5 (a :: Type -> Type -> Type -> Type -> Type -> Type)                 = Typeable a
type Typeable6 (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type)         = Typeable a
type Typeable7 (a :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type) = Typeable a
# endif
#endif
