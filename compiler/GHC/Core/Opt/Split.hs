{-# LANGUAGE ViewPatterns #-}

module GHC.Core.Opt.Split
  ( splitCompUnit
  , checkNameClashes
  ) where

import GHC.Prelude hiding ( head, init, last )

import GHC.Core
import GHC.Core.FVs
import GHC.Core.Opt.OccurAnal (occurAnalyseCompUnit)

import GHC.Data.Graph.Directed (SCC(..), Node(..), stronglyConnCompFromEdgedVerticesUniq)
import GHC.Data.Maybe (orElse)

import GHC.Types.Unique.Set
import GHC.Types.Name (isExternalName, nameModule)
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Var

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Unit.Module (Module)

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

bindNode :: CoreBind -> ([DepGraphNode], [Edge])
bindNode bind =
  case bindersOf bind of
    []       -> ([], [])
    key:rest ->
      let intern_edges = map (\v -> (key, v)) rest
          ext_edges = map (\v -> (key, v)) (nonDetEltsUniqSet (bindFreeVars bind))
          node = BindNode key bind
          pseudo_nodes = map PseudoNode rest
      in (node : pseudo_nodes, intern_edges ++ ext_edges)

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
    undirected_edges = foldr add_edge emptyVarEnv (edges ++ map reverse_edge edges)

    add_edge :: Edge -> Edges -> Edges
    add_edge (src, dst) env = extendVarEnv_C (++) env src [dst]

    reverse_edge :: Edge -> Edge
    reverse_edge (src, dst) = (dst, src)

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
      = case rule_comp_indices rule of
          [i] -> (IntMap.insertWith (++) i [rule] rule_map, no_comp_rules)
          []  -> (rule_map, rule : no_comp_rules)
          is  -> pprPanic "splitCompUnit"
                 ( text "Rule free vars span multiple components"
                $$ text "rule:" <+> ppr rule
                $$ text "components:" <+> ppr is
                $$ text "rule_fvs:" <+> pprVarsWithModule (nonDetEltsUniqSet (ruleFreeVars rule))
                $$ vcat [ text "component" <+> int i <> colon <+> ppr hits
                        | (i, hits) <- comp_hits rule ] )

    rule_comp_indices rule
      = IntSet.toList $ IntSet.fromList
          [ i
          | ((bndrs, _), i) <- zip binder_components [0..]
          , not (isEmptyVarSet (ruleFreeVars rule `intersectVarSet` bndrs))
          ]

    comp_hits rule =
      [ (i, ruleFreeVars rule `intersectVarSet` bndrs)
      | ((bndrs, _), i) <- zip binder_components [0..]
      , not (isEmptyVarSet (ruleFreeVars rule `intersectVarSet` bndrs))
      ]

    components_with_rules =
      [ (bndrs, binds, IntMap.findWithDefault [] i component_rule_map)
      | ((bndrs, binds), i) <- zip binder_components [0..]
      ]

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
splitCompUnit :: Module -> [CoreRule] -> CoreCompUnit -> ([CoreCompUnit], [CoreRule])
splitCompUnit this_module imp_rules unit
  = let comp_units = map mk_comp_unit components_with_rules
    in checkNameClashes comp_units `seq`
       (comp_units, rules_for_imps ++ rules_without_component)
  where
    CoreCompUnit occ_binds unit_rules =
      occurAnalyseCompUnit this_module (const True) (const True) imp_rules unit

    top_level_bndrs = bindersOfBinds occ_binds
    checked_bndrs =
      assertPpr (all isLocalVar top_level_bndrs)
        ( text "splitCompUnit: non-local top-level binder(s)"
       $$ ppr top_level_bndrs )
      top_level_bndrs

    (bind_nodes, bind_edges)
      = checked_bndrs `seq`
        foldr (\b (ns, es) -> let (ns', es') = bindNode b in (ns' ++ ns, es' ++ es))
              ([], [])
              occ_binds

    rule_edge_pairs = [ (r, maybeRuleEdges this_module r) | r <- unit_rules ]
    rule_edges = concat [ es | (_, Just es) <- rule_edge_pairs ]
    rules_for_imps = [ r | (r, Nothing) <- rule_edge_pairs ]
    unit_rules_local = [ r | (r, Just _) <- rule_edge_pairs ]

    all_edges = bind_edges ++ rule_edges
    binder_components = splitCoreBinders bind_nodes all_edges
    (components_with_rules, rules_without_component) =
      assignLocalRules unit_rules_local binder_components

    mk_comp_unit (_, binds, rules) = CoreCompUnit binds rules

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
