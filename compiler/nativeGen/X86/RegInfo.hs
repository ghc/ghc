{-# LANGUAGE CPP #-}
module X86.RegInfo (
        mkVirtualReg,
        regDotColor
)

where

#include "nativeGen/NCG.h"
#include "HsVersions.h"

import GhcPrelude

import Format
import Reg

import Outputable
import Platform
import Unique

import UniqFM
import X86.Regs


mkVirtualReg :: Unique -> Format -> VirtualReg
mkVirtualReg u format
   = case format of
        FF32    -> VirtualRegSSE u
        FF64    -> VirtualRegSSE u
        FF80    -> VirtualRegD   u
        _other  -> VirtualRegI   u

regDotColor :: Platform -> RealReg -> SDoc
regDotColor platform reg
 = case (lookupUFM (regColors platform) reg) of
        Just str -> text str
        _        -> panic "Register not assigned a color"

regColors :: Platform -> UniqFM [Char]
regColors platform = listToUFM (normalRegColors platform ++ fpRegColors platform)

normalRegColors :: Platform -> [(Reg,String)]
normalRegColors platform =
    zip (map regSingle [0..lastint platform]) colors
  where
    -- 16 colors - enough for amd64 gp regs
    colors = ["#800000","#ff0000","#808000","#ffff00","#008000"
             ,"#00ff00","#008080","#00ffff","#000080","#0000ff"
             ,"#800080","#ff00ff","#87005f","#875f00","#87af00"
             ,"#ff00af"]

fpRegColors :: Platform -> [(Reg,String)]
fpRegColors platform =
        [ (fake0, "red")
        , (fake1, "red")
        , (fake2, "red")
        , (fake3, "red")
        , (fake4, "red")
        , (fake5, "red") ]

        ++ zip (map regSingle [firstxmm..lastxmm platform]) greys
  where
    -- 16 shades of grey, enough for the currently supported
    -- SSE extensions.
    greys = ["#0e0e0e","#1c1c1c","#2a2a2a","#383838","#464646"
            ,"#545454","#626262","#707070","#7e7e7e","#8c8c8c"
            ,"#9a9a9a","#a8a8a8","#b6b6b6","#c4c4c4","#d2d2d2"
            ,"#e0e0e0"]



--     32 shades of grey - use for avx 512 if we ever need it
--     greys = ["#070707","#0e0e0e","#151515","#1c1c1c"
--             ,"#232323","#2a2a2a","#313131","#383838","#3f3f3f"
--             ,"#464646","#4d4d4d","#545454","#5b5b5b","#626262"
--             ,"#696969","#707070","#777777","#7e7e7e","#858585"
--             ,"#8c8c8c","#939393","#9a9a9a","#a1a1a1","#a8a8a8"
--             ,"#afafaf","#b6b6b6","#bdbdbd","#c4c4c4","#cbcbcb"
--             ,"#d2d2d2","#d9d9d9","#e0e0e0"]


