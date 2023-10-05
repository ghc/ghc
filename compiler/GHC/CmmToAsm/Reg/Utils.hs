module GHC.CmmToAsm.Reg.Utils
    ( toRegMap, toVRegMap )
where

{- Note [UniqFM and the register allocator]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Before UniqFM had a key type the register allocator
   wasn't picky about key types, using VirtualReg, Reg
   and Unique at various use sites for the same map.

   This is safe.
   * The Unique values come from registers at various
     points where we lose a reference to the original
     register value, but the unique is still valid.

   * VirtualReg is a subset of the registers in Reg's type.
     Making a value of VirtualReg into a Reg in fact doesn't
     change its unique. This is because Reg consists of virtual
     regs and real regs, whose unique values do not overlap.

   * Since the code was written in the assumption that keys are
     not typed it's hard to reverse this assumption now. So we get
     some gnarly but correct code where we often pass around Uniques
     and switch between using Uniques, VirtualReg and RealReg as keys
     of the same map. These issues were always there. But with the
     now-typed keys they become visible. It's a classic case of not all
     correct programs type checking.

   We reduce some of the burden by providing a way to cast

        UniqFM VirtualReg a

   to

        UniqFM Reg a

    in this module. This is safe as Reg is the sum of VirtualReg and
    RealReg. With each kind of register keeping the same unique when
    treated as Reg.

   TODO: If you take offense to this I encourage you to refactor this
   code. I'm sure we can do with less casting of keys and direct use
   of uniques. It might also be reasonable to just use a IntMap directly
   instead of dealing with UniqFM at all.


-}
import GHC.Types.Unique.FM
import GHC.Platform.Reg

-- These should hopefully be zero cost.

toRegMap :: UniqFM VirtualReg elt -> UniqFM Reg elt
toRegMap = unsafeCastUFMKey

toVRegMap :: UniqFM Reg elt -> UniqFM VirtualReg elt
toVRegMap = unsafeCastUFMKey

