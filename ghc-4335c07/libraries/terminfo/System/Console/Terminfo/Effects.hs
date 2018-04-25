{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 703
{-# LANGUAGE Safe #-}
#endif
-- |
-- Maintainer  : judah.jacobson@gmail.com
-- Stability   : experimental
-- Portability : portable (FFI)
module System.Console.Terminfo.Effects(
                    -- * Bell alerts
                    bell,visualBell,
                    -- * Text attributes
                    Attributes(..),
                    defaultAttributes,
                    withAttributes,
                    setAttributes,
                    allAttributesOff,
                    -- ** Mode wrappers
                    withStandout,
                    withUnderline,
                    withBold,
                    -- ** Low-level capabilities
                    enterStandoutMode,
                    exitStandoutMode,
                    enterUnderlineMode,
                    exitUnderlineMode,
                    reverseOn,
                    blinkOn,
                    boldOn,
                    dimOn,
                    invisibleOn,
                    protectedOn
                    ) where

import System.Console.Terminfo.Base
import Control.Monad

wrapWith :: TermStr s => Capability s -> Capability s -> Capability (s -> s)
wrapWith start end = do
    s <- start
    e <- end
    return (\t -> s <#> t <#> e)

-- | Turns on standout mode before outputting the given
-- text, and then turns it off.
withStandout :: TermStr s => Capability (s -> s)
withStandout = wrapWith enterStandoutMode exitStandoutMode

-- | Turns on underline mode before outputting the given
-- text, and then turns it off.
withUnderline :: TermStr s => Capability (s -> s)
withUnderline = wrapWith enterUnderlineMode exitUnderlineMode

-- | Turns on bold mode before outputting the given text, and then turns
-- all attributes off.
withBold :: TermStr s => Capability (s -> s)
withBold = wrapWith boldOn allAttributesOff

enterStandoutMode :: TermStr s => Capability s
enterStandoutMode = tiGetOutput1 "smso"

exitStandoutMode :: TermStr s => Capability s
exitStandoutMode = tiGetOutput1 "rmso"

enterUnderlineMode :: TermStr s => Capability s
enterUnderlineMode = tiGetOutput1 "smul"

exitUnderlineMode :: TermStr s => Capability s
exitUnderlineMode = tiGetOutput1 "rmul"

reverseOn :: TermStr s => Capability s
reverseOn = tiGetOutput1 "rev"

blinkOn:: TermStr s => Capability s
blinkOn = tiGetOutput1 "blink"

boldOn :: TermStr s => Capability s
boldOn = tiGetOutput1 "bold"

dimOn :: TermStr s => Capability s
dimOn = tiGetOutput1 "dim"

invisibleOn :: TermStr s => Capability s
invisibleOn = tiGetOutput1 "invis"

protectedOn :: TermStr s => Capability s
protectedOn = tiGetOutput1 "prot"

-- | Turns off all text attributes.  This capability will always succeed, but it has
-- no effect in terminals which do not support text attributes.
allAttributesOff :: TermStr s => Capability s
allAttributesOff = tiGetOutput1 "sgr0" `mplus` return mempty

data Attributes = Attributes {
                    standoutAttr,
                    underlineAttr,
                    reverseAttr,
                    blinkAttr,
                    dimAttr,
                    boldAttr,
                    invisibleAttr,
                    protectedAttr :: Bool
                -- NB: I'm not including the "alternate character set." 
                }

-- | Sets the attributes on or off before outputting the given text,
-- and then turns them all off.  This capability will always succeed; properties
-- which cannot be set in the current terminal will be ignored.
withAttributes :: TermStr s => Capability (Attributes -> s -> s)
withAttributes = do
    set <- setAttributes
    off <- allAttributesOff
    return $ \attrs to -> set attrs <#> to <#> off

-- | Sets the attributes on or off.  This capability will always succeed;
-- properties which cannot be set in the current terminal will be ignored.
setAttributes :: TermStr s => Capability (Attributes -> s)
setAttributes = usingSGR0 `mplus` manualSets
    where
        usingSGR0 = do
            sgr <- tiGetOutput1 "sgr"
            return $ \a -> let mkAttr f = if f a then 1 else 0 :: Int
                           in sgr (mkAttr standoutAttr)
                                  (mkAttr underlineAttr)
                                  (mkAttr reverseAttr)
                                  (mkAttr blinkAttr)
                                  (mkAttr dimAttr)
                                  (mkAttr boldAttr)
                                  (mkAttr invisibleAttr)
                                  (mkAttr protectedAttr)
                                  (0::Int) -- for alt. character sets
        attrCap :: TermStr s => (Attributes -> Bool) -> Capability s 
                    -> Capability (Attributes -> s)
        attrCap f cap = do {to <- cap; return $ \a -> if f a then to else mempty}
                        `mplus` return (const mempty)
        manualSets = do
            cs <- sequence [attrCap standoutAttr enterStandoutMode
                            , attrCap underlineAttr enterUnderlineMode
                            , attrCap reverseAttr reverseOn
                            , attrCap blinkAttr blinkOn
                            , attrCap boldAttr boldOn
                            , attrCap dimAttr dimOn
                            , attrCap invisibleAttr invisibleOn
                            , attrCap protectedAttr protectedOn
                            ]
            return $ \a -> mconcat $ map ($ a) cs

                                     

-- | These attributes have all properties turned off.
defaultAttributes :: Attributes
defaultAttributes = Attributes False False False False False False False False

-- | Sound the audible bell.
bell :: TermStr s => Capability s
bell = tiGetOutput1 "bel"

-- | Present a visual alert using the @flash@ capability.
visualBell :: Capability TermOutput
visualBell = tiGetOutput1 "flash"
