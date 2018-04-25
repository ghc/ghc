{-# LANGUAGE TypeFamilies, PartialTypeSignatures #-}

-- In Trac #12033 this was called HsakellBug.hs

module T12033 where
tripleStoreToRuleSet :: v -> v
tripleStoreToRuleSet getAtom
 = makeTuple getAtom
 where
   makeRule v = makeExpression v
   makeTuple v = makeExpression v
   makeExpression :: _
   makeExpression v = makeTuple getAtom
