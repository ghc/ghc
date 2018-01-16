module DPH.Testsuite.Preproc ( testcases, (<@) )
where

import Language.Haskell.TH
import Data.List
import Control.Monad (liftM)

-- | Property declarations (no body)
data Prop = Prop { propName   :: Name   -- e.g. prop_foo, prop_bar, baz
                 , propTyvars :: [Name] -- type variables in type decalarations
                 , propType   :: Type   -- type of proprty declaration
                 }

-- | Property instantiations
data Inst = Inst { instName   :: Name           -- e.g. foo, bar, baz (w/out prop_)
                 , instSubsts :: [(Name, Type)] -- available type variable substutions
                 , instExp    :: Exp            -- the actual body
                 }

-- Utility function to bring the prefix into Quotation monad.
(<@) :: String -> Q Type -> Q (String, Type)
pfx <@ qty = liftM ((,) pfx) qty


-- | Mapping from type variable prefixes to types it may be substituted for.
--   E.g. 
-- @
-- [ ("",    [GHC.Types.Bool, GHC.Types.Int])
-- , ("num", [GHC.Types.Int])
-- , ...
-- ]
-- @
--
type Domain = [(String, [Type])]

-- | Entry point for individual test files.
--
--   It generates a single @main@ function which, when run, will
--   quickcheck all of the properties defined in the given testfile.
--
--   Each test file contains a splice expression of the form
-- @
-- $(testcases [ ""    <@ [t| ( Int, Bool ) |]
--             , "num" <@ [t| ( Int,      ) |]
--             , "ord" <@ [t| ( Int, Bool ) |]
--             ,  ...
--             ]
--   [d|
--     prop_foo = ...
--     prop_bar = ...
--   |])
-- @
--
--   The first argument defines the domain of types for which each
--   property would be tested. It is a list of pairs of:
--     * type variables prefixes such as "num" and "ord", and
--     * actual Types belonging to type class of similar name 
--       AND supported by DPH prim libs
--
--   The second argument is a list of Testable property declarations.
--
--   Note that the testing framework is not yet flexible enough to walk over
--   type declarations and picking the types within encountered type classes.
--   Therefore, the type variables themselves must be of the form "num" or
--   similar, narrowing the domain over which the property will be tested.
--
testcases :: [Q (String, Type)] -> Q [Dec] -> Q [Dec]
testcases qdom qdecs =
  do
    dom <- liftM domain $ sequence qdom
    decs <- qdecs
    let props = embed . generate dom $ properties decs
    --  props = [mkTest "foo" <foo_exp>, mkTest "bar" <bar_exp>, ...]
        rn    = AppE (VarE (mkName "runTests"))
                     props
    --  rn    = runTests props
        main  = ValD (VarP (mkName "main"))
                     (NormalB rn) []
    --  main  = runTests [mkTest "foo" <foo_exp>, mkTest "bar" <bar_exp>, ...]
    return (decs ++ [main])

domain :: [(String, Type)] -> Domain
domain ps = sortBy cmpPfx
          . zip (map fst ps)
          . map types
          $ map snd ps
  where
    cmpPfx (s,_) (s',_) = length s' `compare` length s

types :: Type -> [Type]
types ty = case unAppT ty of
             (TupleT _ : tys) -> tys
             _                -> [ty]
  where
    unAppT (AppT t u) = unAppT t ++ [u]
    unAppT t          = [t]

-- | Returns property id by removing "prop_" prefix (if present)
instid :: Inst -> String
instid inst = name inst ++ env inst
  where
    name (Inst { instName = nm }) =
      let s = nameBase nm
      in
      if "prop_" `isPrefixOf` s then drop 5 s else s

    env (Inst { instSubsts = substs })
      | null substs = ""
      | otherwise   = let ss = [nameBase tv ++ " = " ++ pprint ty
                                | (tv, ty) <- substs]
                      in "[" ++ head ss ++ concatMap (", " ++) (tail ss) ++ "]"

properties :: [Dec] -> [Prop]
properties decs = [mkProp nm ty | SigD nm ty <- decs]
  where
    mkProp nm (ForallT vars _ ty) = Prop nm (names vars) ty
    mkProp nm ty                  = Prop nm []   ty
    
names :: [TyVarBndr] -> [Name]
names tvs = map name tvs
  where
    name (PlainTV  n  )        = n
    name (KindedTV n _)        = n

-- | Create a list of mkTest expressions out of Inst expressions.
--
--   E.g. given a list of Insts
-- @
-- [
--   {instName = "prop_foo", instSubsts = ..., instExp = <foo_exp>}
--   {instName = "prop_bar", instSubsts = ..., instExp = <bar_exp>}
--   {instName = "baz",      instSubsts = ..., instExp = <baz_exp>}
-- ]
-- @
--   The resulting expression has the form
-- @
-- [mkTest "foo" <foo_exp>, mkTest "bar" <bar_exp>, mkTest "baz" <baz_exp>]
-- @
--   Note the absence of "prop_" prefixes
--
embed :: [Inst] -> Exp
embed insts = ListE [((VarE $ mkName "mkTest")    `AppE`
                     (LitE . StringL $ instid i)) `AppE`
                     instExp i
                    | i <- insts ]

-- | Instantiate properties for all allowed type combinations
generate :: Domain -> [Prop] -> [Inst]
generate dom = concatMap gen
  where
    gen prop@(Prop { propName   = name
                   , propTyvars = []
                   , propType   = ty }) =
          [Inst name [] (VarE name `SigE` ty)]

    gen prop@(Prop { propName   = name
                   , propTyvars = tvs
                   , propType   = ty }) =
          [Inst name env (VarE name `SigE` subst env ty)
           | env <- combinations tvs dom]

-- | Recursively lookup concrete substitutions for type variables
--   in the environment.
subst :: [(Name, Type)] -> Type -> Type
subst env (VarT nm)  = case lookup nm env of
                         Just ty -> ty
subst env (AppT t u) = AppT (subst env t) (subst env u)
subst env t          = t

-- | Produce all possible combinations of substitutions for a given
--   set of type variables under a given domain.
combinations :: [Name] -> [(String, [Type])] -> [[(Name, Type)]]
combinations []     _   = [[]]
combinations (n:ns) dom = [(n,t) : ps | t <- ts, ps <- combinations ns dom]
  where
    s  = nameBase n
    ts = snd . fromJust $ find ((`isPrefixOf` s) . fst) dom
    fromJust (Just x) = x
    fromJust Nothing  = error $ "No types to select from for type variable `"
                        ++ s ++ "'\n      "
                        ++ "At least add `\"\" <@ [t| (Int) |]' to the domain."
                        ++ s ++ " \n      "
                        ++ "See documentation for function `testcases'."

