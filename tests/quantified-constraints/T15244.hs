{-# LANGUAGE QuantifiedConstraints, TypeFamilies #-}

module T15244 where
class (forall t . Eq (c t)) => Blah c

-- Unquantified works
foo :: (Eq (a t), Eq (b t), a ~ b) => a t -> b t -> Bool
foo a b = a == b
-- Works

-- Two quantified instances fail with double ambiguity check errors
bar :: (forall t . Eq (a t), forall t . Eq (b t), a ~ b) => a t -> b t -> Bool
bar a b = a == b
-- Minimal.hs:11:8: error:
--     • Could not deduce (Eq (b t1))
--       from the context: (forall t1. Eq (a t1), forall t1. Eq (b t1),
--                          a ~ b)
--         bound by the type signature for:
--                    bar :: forall (a :: * -> *) (b :: * -> *) t.
--                           (forall t1. Eq (a t1), forall t1. Eq (b t1), a ~ b) =>
--                           a t -> b t -> Bool
--         at Minimal.hs:11:8-78
--     • In the ambiguity check for ‘bar’
--       To defer the ambiguity check to use sites, enable AllowAmbiguousTypes
--       In the type signature:
--         bar :: (forall t. Eq (a t), forall t. Eq (b t), a ~ b) =>
--                a t -> b t -> Bool
--    |
-- 11 | bar :: (forall t . Eq (a t), forall t . Eq (b t), a ~ b) => a t -> b t -> Bool
--    |
-- [And then another copy of the same error]

-- Two copies from superclass instances fail
baz :: (Blah a, Blah b, a ~ b) => a t -> b t -> Bool
baz a b = a == b
-- Minimal.hs:34:11: error:
--     • Could not deduce (Eq (b t)) arising from a use of ‘==’
--       from the context: (Blah a, Blah b, a ~ b)
--         bound by the type signature for:
--                    baz :: forall (a :: * -> *) (b :: * -> *) t.
--                           (Blah a, Blah b, a ~ b) =>
--                           a t -> b t -> Bool
--         at Minimal.hs:33:1-52
--     • In the expression: a == b
--       In an equation for ‘baz’: baz a b = a == b
--    |
-- 34 | baz a b = a == b
--    |

-- Two copies from superclass from same declaration also fail
mugga :: (Blah a, Blah a) => a t -> a t -> Bool
mugga a b = a == b
--     • Could not deduce (Eq (a t)) arising from a use of ‘==’
--       from the context: (Blah a, Blah a)
--         bound by the type signature for:
--                    mugga :: forall (a :: * -> *) t.
--                             (Blah a, Blah a) =>
--                             a t -> a t -> Bool
--         at Minimal.hs:50:1-47
--     • In the expression: a == b
--       In an equation for ‘mugga’: mugga a b = a == b
--    |
-- 51 | mugga a b = a == b
--    |

-- One copy works
quux :: (Blah a, a ~ b) => a t -> b t -> Bool
quux a b = a == b
-- Works
