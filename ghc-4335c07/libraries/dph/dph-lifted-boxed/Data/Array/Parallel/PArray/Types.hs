{-# LANGUAGE CPP #-}
#include "fusion-phases.h"

-- | Defines the extra types we use when representing algebraic data in
--   parallel arrays. We don't store values of user defined algebraic type
--   directly in PArrays. Instead, we convert these to a generic representation
--   and store that representation.
--
--   Conversion to and from the generic representation is handled by the
--   methods of the PA class defined in "Data.Array.Parallel.PArray.PRepr".
--
---  For further information see:
--     "Instant Generics: Fast and Easy", Chakravarty, Ditu and Keller, 2009
-- 
module Data.Array.Parallel.PArray.Types 
        ( -- * The Void type
          Void
        , void
        , fromVoid

        -- * Generic sums
        , Sum2(..), tagOfSum2
        , Sum3(..), tagOfSum3

        -- * The Wrap type
        , Wrap (..))
where
import Data.Array.Parallel.Base (Tag)
import Data.Array.Parallel.Pretty


-- Void -----------------------------------------------------------------------
-- | The `Void` type is used when representing enumerations. 
-- 
--   A type like Bool is represented as @Sum2 Void Void@, meaning that we only
--   only care about the tag of the data constructor and not its argumnent.
-- 
data Void


-- | A 'value' with the void type. Used as a placholder like `undefined`.
--   Forcing this yields `error`. 
void    :: Void
void     = error $ unlines
         [ "Data.Array.Parallel.PArray.Types.void"
         , "  With the DPH generic array representation, values of type void"
         , "  should never be forced. Something has gone badly wrong." ]


-- | Coerce a `Void` to a different type. Used as a placeholder like `undefined`.
--   Forcing the result yields `error`.
fromVoid :: a
fromVoid = error $ unlines
         [ "Data.Array.Parallel.PArray.Types.fromVoid"
         , "  With the DPH generic array representation, values of type void"
         , "  should never be forced. Something has gone badly wrong." ]


-- Sum2 -----------------------------------------------------------------------
-- | Sum types used for the generic representation of algebraic data.
data Sum2 a b
        = Alt2_1 a | Alt2_2 b

tagOfSum2 :: Sum2 a b -> Tag
tagOfSum2 ss
 = case ss of
        Alt2_1 _        -> 0
        Alt2_2 _        -> 1
{-# INLINE tagOfSum2 #-}


instance (PprPhysical a, PprPhysical b)
        => PprPhysical (Sum2 a b) where
 pprp ss
  = case ss of
        Alt2_1 x        -> text "Alt2_1" <+> pprp x
        Alt2_2 y        -> text "Alt2_2" <+> pprp y



-- Sum3 -----------------------------------------------------------------------
data Sum3 a b c
        = Alt3_1 a | Alt3_2 b | Alt3_3 c

tagOfSum3 :: Sum3 a b c -> Tag
tagOfSum3 ss
 = case ss of
        Alt3_1 _        -> 0
        Alt3_2 _        -> 1
        Alt3_3 _        -> 2
{-# INLINE tagOfSum3 #-}


-- Wrap -----------------------------------------------------------------------
-- | When converting a data type to its generic representation, we use
--   `Wrap` to help us convert only one layer at a time. For example:
--
--   @
--   data Foo a = Foo Int a
--
--   instance PA a => PA (Foo a) where
--    type PRepr (Foo a) = (Int, Wrap a)  -- define how (Foo a) is represented
--   @
--
--   Here we've converted the @Foo@ data constructor to a pair, and Int
--   is its own representation type. We have PData/PR instances for pairs and
--   Ints, so we can work with arrays of these types. However, we can't just
--   use (Int, a) as the representation of (Foo a) because 'a' might
--   be user defined and we won't have PData/PR instances for it.
--
--   Instead, we wrap the second element with the Wrap constructor, which tells
--   us that if we want to process this element we still need to convert it
--   to the generic representation (and back). This last part is done by
--   the PR instance of Wrap, who's methods are defined by calls to the *PD 
--   functions from "Data.Array.Parallel.PArray.PRepr".
--
newtype Wrap a = Wrap { unWrap :: a }



