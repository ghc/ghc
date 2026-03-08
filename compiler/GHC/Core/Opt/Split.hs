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
import GHC.Types.Id (isDFunId, realIdUnfolding)
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.Var
import GHC.Types.Name.Env

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Unit.Module (Module)

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (find)

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
  close_over_imported_unfoldings (bindMentionedVars bind `unionVarSet` bindBndrInfoVars bind)
  where
    local_name_env :: NameEnv Var
    local_name_env = mkNameEnv [ (varName v, v) | v <- nonDetEltsUniqSet local_top_bndrs ]

    close_over_imported_unfoldings fvs = go emptyVarSet fvs

    go !seen !fvs =
      case pick_new_import (fvs `minusVarSet` seen) of
        Nothing -> fvs
        Just v  ->
          let unfolding_fvs = localizeLocalRefs (unfoldingRefs v)
              local_unfolding_fvs = unfolding_fvs `intersectVarSet` local_top_bndrs
          in go (extendVarSet seen v) (fvs `unionVarSet` local_unfolding_fvs `unionVarSet` unfolding_fvs)

    pick_new_import vars =
      find pickable (nonDetEltsUniqSet vars)

    pickable v = isId v && isDFunId v && not (v `elemVarSet` local_top_bndrs)

    unfoldingRefs v =
      case realIdUnfolding v of
        BootUnfolding -> emptyVarSet
        unf ->
          case maybeUnfoldingTemplate unf of
            Just rhs -> exprSomeFreeVars (const True) rhs
            Nothing  -> emptyVarSet

    localizeLocalRefs :: VarSet -> VarSet
    localizeLocalRefs vars = mkVarSet (map localizeVar (nonDetEltsUniqSet vars))

    localizeVar :: Var -> Var
    localizeVar v =
      case lookupNameEnv local_name_env (varName v) of
        Just local_v -> local_v
        Nothing      -> v

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

    (bind_nodes, bind_edges)
      = checked_bndrs `seq`
        foldr (\b (ns, es) -> let (ns', es') = bindNode local_top_bndrs b in (ns' ++ ns, es' ++ es))
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
