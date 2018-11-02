-- | Graph Coloring.
--      This is a generic graph coloring library, abstracted over the type of
--      the node keys, nodes and colors.
--

module GraphColor (
        module GraphBase,
        module GraphOps,
        module GraphPpr,
        colorGraph
)

where

import GhcPrelude

import GraphBase
import GraphOps
import GraphPpr

import Unique
import UniqFM
import UniqSet
import Outputable

import Data.Maybe
import Data.List


-- | Try to color a graph with this set of colors.
--      Uses Chaitin's algorithm to color the graph.
--      The graph is scanned for nodes which are deamed 'trivially colorable'. These nodes
--      are pushed onto a stack and removed from the graph.
--      Once this process is complete the graph can be colored by removing nodes from
--      the stack (ie in reverse order) and assigning them colors different to their neighbors.
--
colorGraph
        :: ( Uniquable  k, Uniquable cls,  Uniquable  color
           , Eq cls, Ord k
           , Outputable k, Outputable cls, Outputable color)
        => Bool                         -- ^ whether to do iterative coalescing
        -> Int                          -- ^ how many times we've tried to color this graph so far.
        -> UniqFM (UniqSet color)       -- ^ map of (node class -> set of colors available for this class).
        -> Triv   k cls color           -- ^ fn to decide whether a node is trivially colorable.
        -> (Graph k cls color -> k)     -- ^ fn to choose a node to potentially leave uncolored if nothing is trivially colorable.
        -> Graph  k cls color           -- ^ the graph to color.

        -> ( Graph k cls color          -- the colored graph.
           , UniqSet k                  -- the set of nodes that we couldn't find a color for.
           , UniqFM  k )                -- map of regs (r1 -> r2) that were coalesced
                                        --       r1 should be replaced by r2 in the source

colorGraph iterative spinCount colors triv spill graph0
 = let
        -- If we're not doing iterative coalescing then do an aggressive coalescing first time
        --      around and then conservative coalescing for subsequent passes.
        --
        --      Aggressive coalescing is a quick way to get rid of many reg-reg moves. However, if
        --      there is a lot of register pressure and we do it on every round then it can make the
        --      graph less colorable and prevent the algorithm from converging in a sensible number
        --      of cycles.
        --
        (graph_coalesced, kksCoalesce1)
         = if iterative
                then (graph0, [])
                else if spinCount == 0
                        then coalesceGraph True  triv graph0
                        else coalesceGraph False triv graph0

        -- run the scanner to slurp out all the trivially colorable nodes
        --      (and do coalescing if iterative coalescing is enabled)
        (ksTriv, ksProblems, kksCoalesce2)
                = colorScan iterative triv spill graph_coalesced

        -- If iterative coalescing is enabled, the scanner will coalesce the graph as does its business.
        --      We need to apply all the coalescences found by the scanner to the original
        --      graph before doing assignColors.
        --
        --      Because we've got the whole, non-pruned graph here we turn on aggressive coalecing
        --      to force all the (conservative) coalescences found during scanning.
        --
        (graph_scan_coalesced, _)
                = mapAccumL (coalesceNodes True triv) graph_coalesced kksCoalesce2

        -- color the trivially colorable nodes
        --      during scanning, keys of triv nodes were added to the front of the list as they were found
        --      this colors them in the reverse order, as required by the algorithm.
        (graph_triv, ksNoTriv)
                = assignColors colors graph_scan_coalesced ksTriv

        -- try and color the problem nodes
        --      problem nodes are the ones that were left uncolored because they weren't triv.
        --      theres a change we can color them here anyway.
        (graph_prob, ksNoColor)
                = assignColors colors graph_triv ksProblems

        -- if the trivially colorable nodes didn't color then something is probably wrong
        --      with the provided triv function.
        --
   in   if not $ null ksNoTriv
         then   pprPanic "colorGraph: trivially colorable nodes didn't color!" -- empty
                        (  empty
                        $$ text "ksTriv    = " <> ppr ksTriv
                        $$ text "ksNoTriv  = " <> ppr ksNoTriv
                        $$ text "colors    = " <> ppr colors
                        $$ empty
                        $$ dotGraph (\_ -> text "white") triv graph_triv)

         else   ( graph_prob
                , mkUniqSet ksNoColor   -- the nodes that didn't color (spills)
                , if iterative
                        then (listToUFM kksCoalesce2)
                        else (listToUFM kksCoalesce1))


-- | Scan through the conflict graph separating out trivially colorable and
--      potentially uncolorable (problem) nodes.
--
--      Checking whether a node is trivially colorable or not is a reasonably expensive operation,
--      so after a triv node is found and removed from the graph it's no good to return to the 'start'
--      of the graph and recheck a bunch of nodes that will probably still be non-trivially colorable.
--
--      To ward against this, during each pass through the graph we collect up a list of triv nodes
--      that were found, and only remove them once we've finished the pass. The more nodes we can delete
--      at once the more likely it is that nodes we've already checked will become trivially colorable
--      for the next pass.
--
--      TODO:   add work lists to finding triv nodes is easier.
--              If we've just scanned the graph, and removed triv nodes, then the only
--              nodes that we need to rescan are the ones we've removed edges from.

colorScan
        :: ( Uniquable k, Uniquable cls, Uniquable color
           , Ord k,       Eq cls
           , Outputable k, Outputable cls)
        => Bool                         -- ^ whether to do iterative coalescing
        -> Triv k cls color             -- ^ fn to decide whether a node is trivially colorable
        -> (Graph k cls color -> k)     -- ^ fn to choose a node to potentially leave uncolored if nothing is trivially colorable.
        -> Graph k cls color            -- ^ the graph to scan

        -> ([k], [k], [(k, k)])         --  triv colorable nodes, problem nodes, pairs of nodes to coalesce

colorScan iterative triv spill graph
        = colorScan_spin iterative triv spill graph [] [] []

colorScan_spin
        :: ( Uniquable k, Uniquable cls, Uniquable color
           , Ord k,       Eq cls
           , Outputable k, Outputable cls)
        => Bool
        -> Triv k cls color
        -> (Graph k cls color -> k)
        -> Graph k cls color
        -> [k]
        -> [k]
        -> [(k, k)]
        -> ([k], [k], [(k, k)])

colorScan_spin iterative triv spill graph
        ksTriv ksSpill kksCoalesce

        -- if the graph is empty then we're done
        | isNullUFM $ graphMap graph
        = (ksTriv, ksSpill, reverse kksCoalesce)

        -- Simplify:
        --      Look for trivially colorable nodes.
        --      If we can find some then remove them from the graph and go back for more.
        --
        | nsTrivFound@(_:_)
                <-  scanGraph   (\node -> triv  (nodeClass node) (nodeConflicts node) (nodeExclusions node)

                                  -- for iterative coalescing we only want non-move related
                                  --    nodes here
                                  && (not iterative || isEmptyUniqSet (nodeCoalesce node)))
                        $ graph

        , ksTrivFound   <- map nodeId nsTrivFound
        , graph2        <- foldr (\k g -> let Just g' = delNode k g
                                          in  g')
                                graph ksTrivFound

        = colorScan_spin iterative triv spill graph2
                (ksTrivFound ++ ksTriv)
                ksSpill
                kksCoalesce

        -- Coalesce:
        --      If we're doing iterative coalescing and no triv nodes are available
        --      then it's time for a coalescing pass.
        | iterative
        = case coalesceGraph False triv graph of

                -- we were able to coalesce something
                --      go back to Simplify and see if this frees up more nodes to be trivially colorable.
                (graph2, kksCoalesceFound@(_:_))
                 -> colorScan_spin iterative triv spill graph2
                        ksTriv ksSpill (reverse kksCoalesceFound ++ kksCoalesce)

                -- Freeze:
                -- nothing could be coalesced (or was triv),
                --      time to choose a node to freeze and give up on ever coalescing it.
                (graph2, [])
                 -> case freezeOneInGraph graph2 of

                        -- we were able to freeze something
                        --      hopefully this will free up something for Simplify
                        (graph3, True)
                         -> colorScan_spin iterative triv spill graph3
                                ksTriv ksSpill kksCoalesce

                        -- we couldn't find something to freeze either
                        --      time for a spill
                        (graph3, False)
                         -> colorScan_spill iterative triv spill graph3
                                ksTriv ksSpill kksCoalesce

        -- spill time
        | otherwise
        = colorScan_spill iterative triv spill graph
                ksTriv ksSpill kksCoalesce


-- Select:
-- we couldn't find any triv nodes or things to freeze or coalesce,
--      and the graph isn't empty yet.. We'll have to choose a spill
--      candidate and leave it uncolored.
--
colorScan_spill
        :: ( Uniquable k, Uniquable cls, Uniquable color
           , Ord k,       Eq cls
           , Outputable k, Outputable cls)
        => Bool
        -> Triv k cls color
        -> (Graph k cls color -> k)
        -> Graph k cls color
        -> [k]
        -> [k]
        -> [(k, k)]
        -> ([k], [k], [(k, k)])

colorScan_spill iterative triv spill graph
        ksTriv ksSpill kksCoalesce

 = let  kSpill          = spill graph
        Just graph'     = delNode kSpill graph
   in   colorScan_spin iterative triv spill graph'
                ksTriv (kSpill : ksSpill) kksCoalesce


-- | Try to assign a color to all these nodes.

assignColors
        :: ( Uniquable k, Uniquable cls, Uniquable color
           , Outputable cls)
        => UniqFM (UniqSet color)       -- ^ map of (node class -> set of colors available for this class).
        -> Graph k cls color            -- ^ the graph
        -> [k]                          -- ^ nodes to assign a color to.
        -> ( Graph k cls color          -- the colored graph
           , [k])                       -- the nodes that didn't color.

assignColors colors graph ks
        = assignColors' colors graph [] ks

 where  assignColors' _ graph prob []
                = (graph, prob)

        assignColors' colors graph prob (k:ks)
         = case assignColor colors k graph of

                -- couldn't color this node
                Nothing         -> assignColors' colors graph (k : prob) ks

                -- this node colored ok, so do the rest
                Just graph'     -> assignColors' colors graph' prob ks


        assignColor colors u graph
                | Just c        <- selectColor colors graph u
                = Just (setColor u c graph)

                | otherwise
                = Nothing



-- | Select a color for a certain node
--      taking into account preferences, neighbors and exclusions.
--      returns Nothing if no color can be assigned to this node.
--
selectColor
        :: ( Uniquable k, Uniquable cls, Uniquable color
           , Outputable cls)
        => UniqFM (UniqSet color)       -- ^ map of (node class -> set of colors available for this class).
        -> Graph k cls color            -- ^ the graph
        -> k                            -- ^ key of the node to select a color for.
        -> Maybe color

selectColor colors graph u
 = let  -- lookup the node
        Just node       = lookupNode graph u

        -- lookup the available colors for the class of this node.
        colors_avail
         = case lookupUFM colors (nodeClass node) of
                Nothing -> pprPanic "selectColor: no colors available for class " (ppr (nodeClass node))
                Just cs -> cs

        -- find colors we can't use because they're already being used
        --      by a node that conflicts with this one.
        Just nsConflicts
                        = sequence
                        $ map (lookupNode graph)
                        $ nonDetEltsUniqSet
                        $ nodeConflicts node
                        -- See Note [Unique Determinism and code generation]

        colors_conflict = mkUniqSet
                        $ catMaybes
                        $ map nodeColor nsConflicts

        -- the prefs of our neighbors
        colors_neighbor_prefs
                        = mkUniqSet
                        $ concat $ map nodePreference nsConflicts

        -- colors that are still valid for us
        colors_ok_ex    = minusUniqSet colors_avail (nodeExclusions node)
        colors_ok       = minusUniqSet colors_ok_ex colors_conflict

        -- the colors that we prefer, and are still ok
        colors_ok_pref  = intersectUniqSets
                                (mkUniqSet $ nodePreference node) colors_ok

        -- the colors that we could choose while being nice to our neighbors
        colors_ok_nice  = minusUniqSet
                                colors_ok colors_neighbor_prefs

        -- the best of all possible worlds..
        colors_ok_pref_nice
                        = intersectUniqSets
                                colors_ok_nice colors_ok_pref

        -- make the decision
        chooseColor

                -- everyone is happy, yay!
                | not $ isEmptyUniqSet colors_ok_pref_nice
                , c : _         <- filter (\x -> elementOfUniqSet x colors_ok_pref_nice)
                                        (nodePreference node)
                = Just c

                -- we've got one of our preferences
                | not $ isEmptyUniqSet colors_ok_pref
                , c : _         <- filter (\x -> elementOfUniqSet x colors_ok_pref)
                                        (nodePreference node)
                = Just c

                -- it wasn't a preference, but it was still ok
                | not $ isEmptyUniqSet colors_ok
                , c : _         <- nonDetEltsUniqSet colors_ok
                -- See Note [Unique Determinism and code generation]
                = Just c

                -- no colors were available for us this time.
                --      looks like we're going around the loop again..
                | otherwise
                = Nothing

   in   chooseColor



