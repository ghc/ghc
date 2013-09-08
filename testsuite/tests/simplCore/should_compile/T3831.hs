{-# LANGUAGE ScopedTypeVariables, FlexibleInstances  #-}

-- This test has a deep nest of join points, which led to 
-- an exponential blow-up in SpecConstr

module T3831(setAttributes)  where

import Data.Monoid
import Control.Applicative (Applicative(..), Alternative(empty, (<|>)))
import Control.Monad

class (Monoid s, OutputCap s) => TermStr s

class OutputCap f where
    outputCap :: ([Int] -> String) -> [Int] -> f
    outputCap = error "urk"

instance OutputCap [Char] where
instance (Enum p, OutputCap f) => OutputCap (p -> f) where


instance Functor Capability where
    fmap = liftM

instance Applicative Capability where
    pure = return
    (<*>) = ap

instance Monad Capability where
    return = Capability . const . return . Just
    Capability f >>= g = Capability $ \t -> do
        mx <- f t
        case mx of
            Nothing -> return Nothing
            Just x -> let Capability g' = g x in g' t

instance Alternative Capability where
    empty = mzero
    (<|>) = mplus

instance MonadPlus Capability where
    mzero = Capability (const $ return Nothing)
    Capability f `mplus` Capability g = Capability $ \t -> do
        mx <- f t
        case mx of
            Nothing -> g t
            _ -> return mx

newtype Capability a = Capability (() -> IO (Maybe a))

tiGetOutput1 :: forall f . OutputCap f => String -> Capability f
{-# NOINLINE tiGetOutput1 #-}
tiGetOutput1 _ = return (outputCap (const "") [])

enterStandoutMode :: TermStr s => Capability s
enterStandoutMode = tiGetOutput1 "smso"

enterUnderlineMode :: TermStr s => Capability s
enterUnderlineMode = tiGetOutput1 "smul"

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

data Attributes = Attributes {
                    standoutAttr,
                    underlineAttr,
                    reverseAttr,
                    blinkAttr,
                    dimAttr,
                    boldAttr,
                    invisibleAttr,
                    protectedAttr :: Bool
                }

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
                                  (0::Int)
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

