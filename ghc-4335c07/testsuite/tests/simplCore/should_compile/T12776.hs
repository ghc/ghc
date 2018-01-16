{-# OPTIONS_GHC -O2 #-}

-- This made the simplifier loop doing infinite inlining
-- in GHC 8.0

module T12776(distinct_degree_factorization_i) where

import Prelude ((==), Eq);

data Slist a = Nil_s | Cons_s a (Slist a) deriving Eq;

map_s :: (a -> b) -> Slist a -> Slist b;
map_s f (Cons_s x21 x22) = Cons_s (f x21) (map_s f x22);

normalize_poly_i :: (Eq a) => (a -> a) -> Slist a -> Slist a;
normalize_poly_i ops xs = if ops (lead_coeff_i xs) == lead_coeff_i xs then Nil_s else map_s ops xs;

mod_poly_one_main_i :: (Eq a) => (a -> a) -> Slist a -> Slist a;
mod_poly_one_main_i ops d =
  if d == d then d else mod_poly_one_main_i ops (normalize_poly_i ops d);

last_s :: (Eq a) => Slist a -> a;
last_s (Cons_s x xs) = (if xs == Nil_s then x else last_s xs);

mod_field_poly_i :: (Eq a) => (a -> a) -> Slist a -> Slist a;
mod_field_poly_i ops cf =
  (if cf == cf then cf else
     mod_poly_one_main_i ops (map_s (\_ -> ops (last_s cf)) cf));

lead_coeff_i :: Eq a => Slist a -> a;
lead_coeff_i pp = (case pp of {
                        Cons_s _ _ -> last_s pp;
                      });

dist_degree_factorize_main_i :: Eq a => (a -> a) -> Slist a -> [Slist a] -> [Slist a];
dist_degree_factorize_main_i ff_ops w res =
  if w == w then res
           else dist_degree_factorize_main_i ff_ops (mod_field_poly_i ff_ops w)
                   [normalize_poly_i ff_ops (mod_field_poly_i ff_ops w)];

distinct_degree_factorization_i :: Eq a => (a -> a) -> [Slist a];
distinct_degree_factorization_i ff_ops = dist_degree_factorize_main_i ff_ops Nil_s []
