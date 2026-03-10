{-# LANGUAGE ViewPatterns #-}

module GHC.Core.Opt.Split
  ( splitCompUnit
  , checkNameClashes
  ) where

import GHC.Prelude hiding ( head, init, last )

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Opt.OccurAnal (occurAnalyseCompUnit)
import GHC.Core.Stats (coreBindsSize)

import GHC.Data.Graph.Directed (SCC(..), Node(..), stronglyConnCompFromEdgedVerticesUniq)
import GHC.Data.Maybe (orElse)

import GHC.Types.Unique.Set
import GHC.Types.Name (Name, isExternalName, nameModule)
import GHC.Types.Name.Set (NameSet, isEmptyNameSet)
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Var

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Unit.Module (Module)

import Data.List (foldl', sortOn)
import Data.Ord (Down(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet

{- Note [Splitting core programs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
splitCompUnit splits a single compilation unit into multiple.
To do so we:

Initially we run OccAnal on the compilation unit. I don't think it's strictly neccessary
but it zaps fragile unfoldings which speeds up and gets rid of glomming.
We will try getting rid of this later.

We split imported rules into those which only concern imported rules and those mentioning
local binders (called unit_rules).

Rules mentioning local binders introduce edges between any local binders they mention.

Next we build the graph nodes from binders:
* Fully nodes have one key, the first Id the binder defines. Their node data is the binder itself.
* Pseudo nodes for every id the binder defines (tail $ bindersOf bind) with no data.

Now we introduce edges:
  * For binders from the first id to all other defined ids and those it mentions as fv
      let key = (head bindersOf bind)
      let edges = map (\x -> (key, x)) bindersOf bind ++ map (\x -> (key, x)) (bindFreeVars bind)
  * For rules if they mention local binders introduce edges between any local binders they mention.

* All edges computed so far are directional. So we take all edges and also add their reversed version.

* After this we split the graph into independent components.

* As the last step we assign each unit rule to a unit from which it mentions variables

Note [Goal of core splitting]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The goal of core splitting is to determine multiple independent fragmanets (units) within
the current module. This means after core splitting there most be no way for any
binder in one build unit to reference a binder in another unit.

If there is *any* connection between two local binders we have to put both those
binders into the same unit.

Sources of units are:

Unfoldings: A unfolding creates a dependency between it's binder and all id's that
are mentioned by it's unfolding. This is similarly true for DFuns.

RULES: The function the RULE matches on has dependencies with all binders it mentions
on both the LHS and RHS.

We have to build a transitive closure of those dependencies. Concretely if some
binder foo depends on bar through a unfolding/RULE then we have to also analyze bar.

This is because of import loops. Consider for example:

```
A:fa1 = B.foo
A:fa2

B:foo = ... C.baz ...

C:baz = ... {-# SOURCE #-} A.fa2
```

Here despite `B.foo` being imported it can bring the local binder `fa.2` into
scope. So we must ensure `fa1` and fa2 end up in the same compilation unit.

But for now I think I will just disable splitting if there is a boot module.

-}

data DepGraphNode
  = BindNode
  { depNodeKey  :: !Var
  , depNodeBind :: CoreBind
  }
  | PseudoNode
  { depNodeKey :: !Var }

type Edge = (Var, Var)

-- | Is the given variable defined in the given module.
varFromModule :: Module -> Var -> Bool
varFromModule _ var = isLocalId var

maybeRuleEdges :: Module -> CoreRule -> Maybe [Edge]
maybeRuleEdges this_module rule =
  case local_fvs of
    []  -> Nothing
    [_] -> Just []
    _   -> Just (zip local_fvs (drop 1 local_fvs))
  where
    local_fvs = filter (varFromModule this_module) (nonDetEltsUniqSet (ruleFreeVars rule))

bindNode :: VarSet -> CoreBind -> ([DepGraphNode], [Edge])
bindNode local_top_bndrs bind =
  case bindersOf bind of
    []       -> ([], [])
    key:rest ->
      let split_fvs = bindSplitFreeVars local_top_bndrs bind
          intern_edges = map (\v -> (key, v)) rest
          ext_edges = map (\v -> (key, v)) (nonDetEltsUniqSet split_fvs)
          node = BindNode key bind
          pseudo_nodes = map PseudoNode rest
      in (node : pseudo_nodes, intern_edges ++ ext_edges)

bindSplitFreeVars :: VarSet -> CoreBind -> VarSet
bindSplitFreeVars local_top_bndrs bind =
  (bindMentionedVars bind `unionVarSet` bindBndrInfoVars bind) `intersectVarSet` local_top_bndrs

bindMentionedVars :: CoreBind -> VarSet
bindMentionedVars (NonRec _ rhs) = exprSomeFreeVars (const True) rhs
bindMentionedVars (Rec prs)      = exprsSomeFreeVars (const True) (map snd prs)

bindBndrInfoVars :: CoreBind -> VarSet
bindBndrInfoVars bind =
  mkVarSet $
    concatMap (dVarSetElems . bndrRuleAndUnfoldingVarsDSet) (bindersOf bind)

type Edges = IdEnv [Var]

-- Split core binders takes directed edges treating them as undirected by adding the reverse edge internally.
splitCoreBinders :: [DepGraphNode] -> [Edge] -> [(VarSet, [CoreBind])]
splitCoreBinders nodes edges =
  [ (mkVarSet (concatMap bindersOf binds), binds)
  | comp_nodes <- map scc_payloads (stronglyConnCompFromEdgedVerticesUniq (map mk_graph_node nodes))
  , let binds = [ b | BindNode { depNodeBind = b } <- comp_nodes ]
  ]
  where
    key_set = mkVarSet (map depNodeKey nodes)
    undirected_edges = foldr add_edge_pair emptyVarEnv edges

    add_edge :: Edge -> Edges -> Edges
    add_edge (src, dst) env = extendVarEnv_C (++) env src [dst]

    add_edge_pair :: Edge -> Edges -> Edges
    add_edge_pair (src, dst) env = add_edge (src, dst) (add_edge (dst, src) env)

    mk_graph_node node
      = DigraphNode
          { node_payload = node
          , node_key = varUnique key
          , node_dependencies =
              [ varUnique dst
              | dst <- lookupVarEnv undirected_edges key `orElse` []
              , elemVarSet dst key_set
              ]
          }
      where
        key = depNodeKey node

    scc_payloads (AcyclicSCC p) = [p]
    scc_payloads (CyclicSCC ps) = ps

assignLocalRules
  :: [CoreRule]
  -> [(VarSet, [CoreBind])]
  -> ([(VarSet, [CoreBind], [CoreRule])], [CoreRule])
assignLocalRules unit_rules binder_components =
  (components_with_rules, rules_without_component)
  where
    (component_rule_map, rules_without_component)
      = foldr assign_rule (IntMap.empty, []) unit_rules

    assign_rule rule (rule_map, no_comp_rules)
      = case IntMap.keys comp_hits of
          [i] -> (IntMap.insertWith (++) i [rule] rule_map, no_comp_rules)
          []  -> (rule_map, rule : no_comp_rules)
          is  -> pprPanic "splitCompUnit"
                 ( text "Rule free vars span multiple components"
                $$ text "rule:" <+> ppr rule
                $$ text "components:" <+> ppr is
                $$ text "rule_fvs:" <+> pprVarsWithModule (nonDetEltsUniqSet (ruleFreeVars rule))
                $$ vcat [ text "component" <+> int i <> colon <+> pprVarsWithModule hits
                        | (i, hits) <- IntMap.toAscList comp_hits ] )
      where
        comp_hits = ruleComponentHits rule

    ruleComponentHits rule =
      foldr add_fv_hit IntMap.empty (nonDetEltsUniqSet (ruleFreeVars rule))

    add_fv_hit v hits =
      case lookupVarEnv binder_component_map v of
        Just i  -> IntMap.insertWith (++) i [v] hits
        Nothing -> hits

    components_with_rules =
      [ (bndrs, binds, IntMap.findWithDefault [] i component_rule_map)
      | ((bndrs, binds), i) <- zip binder_components [0 :: Int ..]
      ]

    binder_component_map =
      foldr add_component emptyVarEnv (zip binder_components [0 :: Int ..])

    add_component ((bndrs, _), i) env =
      foldr (\v env' -> extendVarEnv env' v i) env (nonDetEltsUniqSet bndrs)

pprVarsWithModule :: [Var] -> SDoc
pprVarsWithModule vars = braces (fsep (punctuate comma (map pprVarWithModule vars)))

pprVarWithModule :: Var -> SDoc
pprVarWithModule v
  | isExternalName n = ppr v <+> parens (ppr (nameModule n))
  | otherwise        = ppr v
  where
    n = varName v

-- After optimizations a rule might no longer reference binders from this module.
-- In these cases we return them here and then add them to mg_rules.
splitCompUnit :: Int -> Module -> NameSet -> [CoreRule] -> CoreCompUnit -> ([CoreCompUnit], [CoreRule])
splitCompUnit n_threads this_module boot_exported imp_rules unit
  | not boot_exported_is_empty = single_comp_unit
  | otherwise
  = let comp_units = combineCompUnits max_units (map mk_comp_unit components_with_rules)
        result = (comp_units, rules_for_imps ++ rules_without_component)
    in -- pprTrace "CoreSplitTrace" (pprSplitTrace comp_units) $
       checkNameClashes comp_units `seq`
       result
  where
    CoreCompUnit occ_binds unit_rules =
      occurAnalyseCompUnit this_module (const True) (const True) imp_rules unit

    top_level_bndrs = bindersOfBinds occ_binds
    checked_bndrs =
      assertPpr (all isLocalVar top_level_bndrs)
        ( text "splitCompUnit: non-local top-level binder(s)"
       $$ ppr top_level_bndrs )
      top_level_bndrs

    local_top_bndrs = mkVarSet checked_bndrs

    bind_node_info = checked_bndrs `seq` map (bindNode local_top_bndrs) occ_binds
    bind_nodes = concatMap fst bind_node_info
    bind_edges = concatMap snd bind_node_info

    rule_edge_pairs = [ (r, maybeRuleEdges this_module r) | r <- unit_rules ]
    rule_edges = concat [ es | (_, Just es) <- rule_edge_pairs ]
    rules_for_imps = [ r | (r, Nothing) <- rule_edge_pairs ]
    unit_rules_local = [ r | (r, Just _) <- rule_edge_pairs ]

    all_edges = bind_edges ++ rule_edges
    binder_components = splitCoreBinders bind_nodes all_edges
    (components_with_rules, rules_without_component) =
      assignLocalRules unit_rules_local binder_components

    mk_comp_unit (_, binds, rules) = CoreCompUnit binds rules

    boot_exported_is_empty = isEmptyNameSet boot_exported
    max_units = max 1 (2 * n_threads)

    single_comp_unit = ([unit], [])

combineCompUnits :: Int -> [CoreCompUnit] -> [CoreCompUnit]
combineCompUnits max_units units
  | length units <= max_units = units
  | otherwise = map finishBucket (IntMap.elems final_buckets)
  where
    initial_buckets =
      IntMap.fromDistinctAscList
        [ (i, Bucket 0 [] [])
        | i <- [0 .. max_units - 1]
        ]

    final_buckets =
      foldl' assignUnit initial_buckets sorted_units

    sorted_units =
      sortOn (Down . unitSize . snd) (zip [0 :: Int ..] units)

    assignUnit buckets (_, unit) =
      IntMap.adjust (addUnit unit) target_bucket buckets
      where
        target_bucket = smallestBucket buckets

    smallestBucket buckets =
      case IntMap.toAscList buckets of
        [] -> panic "combineCompUnits.smallestBucket: empty bucket map"
        b : bs -> fst (foldl' choose_bucket b bs)

    choose_bucket best@(i1, b1) candidate@(i2, b2)
      | (bucketSize b2, i2) < (bucketSize b1, i1) = candidate
      | otherwise                                  = best

    finishBucket (Bucket _ binds_acc rules_acc) =
      CoreCompUnit (reverse binds_acc) (reverse rules_acc)

    unitSize = coreBindsSize . coreCompUnitBinds

data Bucket = Bucket
  { bucketSize  :: !Int
  , bucketBinds :: [CoreBind]
  , bucketRules :: [CoreRule]
  }

addUnit :: CoreCompUnit -> Bucket -> Bucket
addUnit (CoreCompUnit binds rules) (Bucket sz binds_acc rules_acc) =
  Bucket
    { bucketSize = sz + coreBindsSize binds
    , bucketBinds = foldl' (flip (:)) binds_acc binds
    , bucketRules = foldl' (flip (:)) rules_acc rules
    }

checkNameClashes :: [CoreCompUnit] -> ()
checkNameClashes comp_units
  | null dup_bndrs = ()
  | otherwise
  = pprPanic "checkNameClashes"
      ( text "Duplicate top-level binders across split compilation units"
     $$ ppr dup_bndrs )
  where
    all_bndrs = concatMap (bindersOfBinds . coreCompUnitBinds) comp_units

    dup_bndrs :: [Var]
    dup_bndrs = go emptyVarSet all_bndrs

    go _    [] = []
    go seen (b:bs)
      | b `elemVarSet` seen = b : go seen bs
      | otherwise           = go (extendVarSet seen b) bs

pprSplitTrace :: [CoreCompUnit] -> SDoc
pprSplitTrace comp_units =
  text (show (length comp_units))
  <+> text "Unit; CoreSizes:"
  <+> pprIntList sizes
  <> semi
  <+> text "RelativeSize:"
  <+> pprPercentList rel_sizes
  where
    sizes = map (coreBindsSize . coreCompUnitBinds) comp_units
    total_size = sum sizes
    rel_sizes
      | total_size == 0 = replicate (length sizes) 0
      | otherwise       = map (\sz -> (100 * sz) `div` total_size) sizes

pprIntList :: [Int] -> SDoc
pprIntList xs = brackets (hcat (punctuate comma (map int xs)))

pprPercentList :: [Int] -> SDoc
pprPercentList xs = brackets (hcat (punctuate comma [ int x <> char '%' | x <- xs ]))
