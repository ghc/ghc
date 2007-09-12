
module StackPlacements
  ( SlotSet, allStackSlots  -- the infinite set of stack slots
  , SlotClass(..), slotClassBits, stackSlot32, stackSlot64, stackSlot128
  , allSlotClasses
  , getStackSlot, extendSlotSet, deleteFromSlotSet, elemSlotSet, chooseSlot
  , StackPlacement(..)
  )
where

import Maybes
import Outputable
import Unique

import Prelude hiding (pi)
import Data.List

{- 

The goal here is to provide placements on the stack that will allow,
for example, two 32-bit words to spill to a slot previously used by a
64-bit floating-point value.  I use a simple buddy-system allocator
that splits large slots in half as needed; this will work fine until
the day when somebody wants to spill an 80-bit Intel floating-point
register into the Intel standard 96-bit stack slot.

-}

data SlotClass = SlotClass32 | SlotClass64 | SlotClass128
  deriving (Eq)

instance Uniquable SlotClass where
    getUnique = getUnique . slotClassBits

instance Outputable SlotClass where
    ppr cls = text "class of" <+> int (slotClassBits cls) <> text "-bit stack slots"

slotClassBits :: SlotClass -> Int
slotClassBits SlotClass32 = 32
slotClassBits SlotClass64 = 64
slotClassBits SlotClass128 = 128

data StackPlacement = FullSlot SlotClass Int
               | YoungHalf StackPlacement
               | OldHalf StackPlacement
  deriving (Eq)

data OneSize = OneSize { full_slots :: [StackPlacement], fragments :: [StackPlacement] }
  -- ^ Always used for slots that have been previously used

data SlotSet = SlotSet { s32, s64, s128 :: OneSize, next_unused :: Int }

allStackSlots :: SlotSet
allStackSlots = SlotSet empty empty empty 0
    where empty = OneSize [] []


psize :: StackPlacement -> Int
psize (FullSlot cls _) = slotClassBits cls
psize (YoungHalf p) = psize p `div` 2
psize (OldHalf   p) = psize p `div` 2




-- | Get a slot no matter what
get32, get64, get128 :: SlotSet -> (StackPlacement, SlotSet)

-- | Get a previously used slot if one exists
getu32, getu64, getu128 :: SlotSet -> Maybe (StackPlacement, SlotSet)

-- | Only supported slot classes

stackSlot32, stackSlot64, stackSlot128 :: SlotClass
stackSlot32  = SlotClass32
stackSlot64  = SlotClass64
stackSlot128 = SlotClass128

allSlotClasses :: [SlotClass]
allSlotClasses = [stackSlot32, stackSlot64, stackSlot128]

-- | Get a fresh slot, never before used
getFull :: SlotClass -> SlotSet -> (StackPlacement, SlotSet)

infixr 4 |||

(|||) :: (SlotSet -> Maybe (StackPlacement, SlotSet)) ->
         (SlotSet ->       (StackPlacement, SlotSet)) ->
         (SlotSet ->       (StackPlacement, SlotSet))
      
f1 ||| f2 = \slots -> f1 slots `orElse`   f2 slots

getFull cls slots = (FullSlot cls n, slots { next_unused = n + 1 })
    where n = next_unused slots

get32  = getu32  ||| (fmap split64  . getu64)  ||| getFull stackSlot32
get64  = getu64  ||| (fmap split128 . getu128) ||| getFull stackSlot64
get128 = getu128 ||| getFull stackSlot128

type SizeGetter = SlotSet -> OneSize
type SizeSetter = OneSize -> SlotSet -> SlotSet

upd32, upd64, upd128 :: SizeSetter
upd32  this_size slots = slots { s32  = this_size }
upd64  this_size slots = slots { s64  = this_size }
upd128 this_size slots = slots { s128 = this_size }

with_size :: Int -> (SizeGetter -> SizeSetter -> a) -> a
with_size  32 = with_32
with_size  64 = with_64
with_size 128 = with_128
with_size _   = panic "non-standard slot size -- error in size computation?"

with_32, with_64, with_128 :: (SizeGetter -> SizeSetter -> a) -> a
with_32  f = f s32  upd32
with_64  f = f s64  upd64
with_128 f = f s128 upd128

getu32  = with_32  getUsed
getu64  = with_64  getUsed
getu128 = with_128 getUsed

getUsed :: SizeGetter -> SizeSetter -> SlotSet -> Maybe (StackPlacement, SlotSet)
getUsed get set slots = 
    let this_size = get slots in
    case full_slots this_size of
      p : ps -> Just (p, set (this_size { full_slots = ps }) slots)
      [] -> case fragments this_size of
              p : ps -> Just (p, set (this_size { fragments = ps }) slots)
              [] -> Nothing

-- | When splitting, allocate the old half first in case it makes the
-- stack smaller at a call site.
split64, split128 :: (StackPlacement, SlotSet) -> (StackPlacement, SlotSet)
split64  (p, slots) = (OldHalf p, slots { s32 = cons_frag (YoungHalf p) (s32 slots) })
split128 (p, slots) = (OldHalf p, slots { s64 = cons_frag (YoungHalf p) (s64 slots) })

cons_frag :: StackPlacement -> OneSize -> OneSize
cons_frag p this_size = this_size { fragments = p : fragments this_size }


----------------------------
instance Outputable StackPlacement where
  ppr (FullSlot cls n) = int (slotClassBits cls) <> text "-bit slot " <> int n
  ppr (YoungHalf p) = text "young half of" <+> ppr p
  ppr (OldHalf   p) = text "old half of"   <+> ppr p

instance Outputable SlotSet where
  ppr slots = fsep $ punctuate comma
              (pprSlots (s32 slots) ++ pprSlots (s64 slots) ++ pprSlots (s128 slots) ++
               [text "and slots numbered" <+> int (next_unused slots)
                         <+> text "and up"])
   where pprSlots (OneSize w fs) = map ppr w ++ map ppr fs

{-
instance ColorSet SlotSet SlotClass StackPlacement where
  emptyColorSet = panic "The set of stack slots is never empty"
  deleteFromColorSet = deleteFromSlotSet
  extendColorSet slots (cls, p@(FullSlot {})) =
      with_size (slotClassBits cls) add_full p (pi slots)
  extendColorSet slots (cls, p) = with_size (slotClassBits cls) add_frag p (pi slots)
  chooseColor        = chooseSlot
-}

deleteFromSlotSet :: StackPlacement -> SlotSet -> SlotSet
deleteFromSlotSet p@(FullSlot {}) slots = with_size (psize p) remove_full p (pi slots)
deleteFromSlotSet p               slots = with_size (psize p) remove_frag p (pi slots)

extendSlotSet :: SlotSet -> StackPlacement -> SlotSet
extendSlotSet slots p@(FullSlot {}) = with_size (psize p) add_full p (pi slots)
extendSlotSet slots p               = with_size (psize p) add_frag p (pi slots)

elemSlotSet :: StackPlacement -> SlotSet -> Bool
elemSlotSet p@(FullSlot {}) slots = with_size (psize p) elem_full p slots
elemSlotSet p               slots = with_size (psize p) elem_frag p slots

remove_full, remove_frag, add_full, add_frag
    :: SizeGetter -> SizeSetter -> StackPlacement -> SlotSet -> SlotSet

remove_full get set p slots = set p' slots
    where this_size = get slots
          p' = this_size { full_slots = delete p $ full_slots this_size }

remove_frag get set p slots = set p' slots
    where this_size = get slots
          p' = this_size { full_slots = delete p $ full_slots this_size }

add_full get set p slots = set p' slots
    where this_size = get slots
          p' = this_size { full_slots = add p $ full_slots this_size }

add_frag get set p slots = set p' slots
    where this_size = get slots
          p' = this_size { full_slots = add p $ full_slots this_size }

add :: Eq a => a -> [a] -> [a]
add x xs = if notElem x xs then x : xs else xs

elem_full, elem_frag :: SizeGetter -> SizeSetter -> StackPlacement -> SlotSet -> Bool
elem_full get _set p slots = elem p (full_slots $ get slots)
elem_frag get _set p slots = elem p (fragments  $ get slots)




getStackSlot :: SlotClass -> SlotSet -> (StackPlacement, SlotSet)
getStackSlot cls slots =
  case cls of
    SlotClass32  -> get32  (pi slots)
    SlotClass64  -> get64  (pi slots)
    SlotClass128 -> get128 (pi slots)
 

chooseSlot :: SlotClass -> [StackPlacement] -> SlotSet -> Maybe (StackPlacement, SlotSet)
chooseSlot cls prefs slots =
  case filter (flip elemSlotSet slots) prefs of
    placement : _ -> Just (placement, deleteFromSlotSet placement (pi slots))
    [] -> Just (getStackSlot cls slots)

check_invariant :: Bool
check_invariant = True

pi :: SlotSet -> SlotSet
pi = if check_invariant then panic_on_invariant_violation else id

panic_on_invariant_violation :: SlotSet -> SlotSet
panic_on_invariant_violation slots =
    check 32 (s32 slots) $ check 64 (s64 slots) $ check 128 (s128 slots) $ slots
  where n = next_unused slots
        check bits this_size = (check_full bits $ full_slots this_size) .
                               (check_frag bits $ fragments  this_size)
        check_full _ [] = id
        check_full bits (FullSlot cls k : ps) =
            if slotClassBits cls /= bits then panic "slot in bin of wrong size"
            else if k >= n then panic "slot number is unreasonably fresh"
                 else check_full bits ps
        check_full _ _ = panic "a fragment is in a bin reserved for full slots"
        check_frag _ [] = id
        check_frag _ (FullSlot {} : _) =
            panic "a full slot is in a bin reserved for fragments"
        check_frag bits (p : ps) =
            if bits /= psize p then panic "slot in bin of wrong size"
            else if pnumber p >= n then panic "slot number is unreasonably fresh"
                 else check_frag bits ps
        pnumber (FullSlot _ k) = k
        pnumber (YoungHalf p) = pnumber p
        pnumber (OldHalf p)   = pnumber p

