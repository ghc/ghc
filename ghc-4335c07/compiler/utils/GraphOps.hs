-- | Basic operations on graphs.
--

module GraphOps (
        addNode,        delNode,        getNode,       lookupNode,     modNode,
        size,
        union,
        addConflict,    delConflict,    addConflicts,
        addCoalesce,    delCoalesce,
        addExclusion,   addExclusions,
        addPreference,
        coalesceNodes,  coalesceGraph,
        freezeNode,     freezeOneInGraph, freezeAllInGraph,
        scanGraph,
        setColor,
        validateGraph,
        slurpNodeConflictCount
)
where

import GhcPrelude

import GraphBase

import Outputable
import Unique
import UniqSet
import UniqFM

import Data.List        hiding (union)
import Data.Maybe

-- | Lookup a node from the graph.
lookupNode
        :: Uniquable k
        => Graph k cls color
        -> k -> Maybe (Node  k cls color)

lookupNode graph k
        = lookupUFM (graphMap graph) k


-- | Get a node from the graph, throwing an error if it's not there
getNode
        :: Uniquable k
        => Graph k cls color
        -> k -> Node k cls color

getNode graph k
 = case lookupUFM (graphMap graph) k of
        Just node       -> node
        Nothing         -> panic "ColorOps.getNode: not found"


-- | Add a node to the graph, linking up its edges
addNode :: Uniquable k
        => k -> Node k cls color
        -> Graph k cls color -> Graph k cls color

addNode k node graph
 = let
        -- add back conflict edges from other nodes to this one
        map_conflict =
          nonDetFoldUniqSet
            -- It's OK to use nonDetFoldUFM here because the
            -- operation is commutative
            (adjustUFM_C (\n -> n { nodeConflicts =
                                      addOneToUniqSet (nodeConflicts n) k}))
            (graphMap graph)
            (nodeConflicts node)

        -- add back coalesce edges from other nodes to this one
        map_coalesce =
          nonDetFoldUniqSet
            -- It's OK to use nonDetFoldUFM here because the
            -- operation is commutative
            (adjustUFM_C (\n -> n { nodeCoalesce =
                                      addOneToUniqSet (nodeCoalesce n) k}))
            map_conflict
            (nodeCoalesce node)

  in    graph
        { graphMap      = addToUFM map_coalesce k node}


-- | Delete a node and all its edges from the graph.
delNode :: (Uniquable k)
        => k -> Graph k cls color -> Maybe (Graph k cls color)

delNode k graph
        | Just node     <- lookupNode graph k
        = let   -- delete conflict edges from other nodes to this one.
                graph1  = foldl' (\g k1 -> let Just g' = delConflict k1 k g in g') graph
                        $ nonDetEltsUniqSet (nodeConflicts node)

                -- delete coalesce edge from other nodes to this one.
                graph2  = foldl' (\g k1 -> let Just g' = delCoalesce k1 k g in g') graph1
                        $ nonDetEltsUniqSet (nodeCoalesce node)
                        -- See Note [Unique Determinism and code generation]

                -- delete the node
                graph3  = graphMapModify (\fm -> delFromUFM fm k) graph2

          in    Just graph3

        | otherwise
        = Nothing


-- | Modify a node in the graph.
--      returns Nothing if the node isn't present.
--
modNode :: Uniquable k
        => (Node k cls color -> Node k cls color)
        -> k -> Graph k cls color -> Maybe (Graph k cls color)

modNode f k graph
 = case lookupNode graph k of
        Just Node{}
         -> Just
         $  graphMapModify
                 (\fm   -> let  Just node       = lookupUFM fm k
                                node'           = f node
                           in   addToUFM fm k node')
                graph

        Nothing -> Nothing


-- | Get the size of the graph, O(n)
size    :: Graph k cls color -> Int

size graph
        = sizeUFM $ graphMap graph


-- | Union two graphs together.
union   :: Graph k cls color -> Graph k cls color -> Graph k cls color

union   graph1 graph2
        = Graph
        { graphMap              = plusUFM (graphMap graph1) (graphMap graph2) }


-- | Add a conflict between nodes to the graph, creating the nodes required.
--      Conflicts are virtual regs which need to be colored differently.
addConflict
        :: Uniquable k
        => (k, cls) -> (k, cls)
        -> Graph k cls color -> Graph k cls color

addConflict (u1, c1) (u2, c2)
 = let  addNeighbor u c u'
                = adjustWithDefaultUFM
                        (\node -> node { nodeConflicts = addOneToUniqSet (nodeConflicts node) u' })
                        (newNode u c)  { nodeConflicts = unitUniqSet u' }
                        u

   in   graphMapModify
        ( addNeighbor u1 c1 u2
        . addNeighbor u2 c2 u1)


-- | Delete a conflict edge. k1 -> k2
--      returns Nothing if the node isn't in the graph
delConflict
        :: Uniquable k
        => k -> k
        -> Graph k cls color -> Maybe (Graph k cls color)

delConflict k1 k2
        = modNode
                (\node -> node { nodeConflicts = delOneFromUniqSet (nodeConflicts node) k2 })
                k1


-- | Add some conflicts to the graph, creating nodes if required.
--      All the nodes in the set are taken to conflict with each other.
addConflicts
        :: Uniquable k
        => UniqSet k -> (k -> cls)
        -> Graph k cls color -> Graph k cls color

addConflicts conflicts getClass

        -- just a single node, but no conflicts, create the node anyway.
        | (u : [])      <- nonDetEltsUniqSet conflicts
        = graphMapModify
        $ adjustWithDefaultUFM
                id
                (newNode u (getClass u))
                u

        | otherwise
        = graphMapModify
        $ \fm -> foldl' (\g u  -> addConflictSet1 u getClass conflicts g) fm
                $ nonDetEltsUniqSet conflicts
                -- See Note [Unique Determinism and code generation]


addConflictSet1 :: Uniquable k
                => k -> (k -> cls) -> UniqSet k
                -> UniqFM (Node k cls color)
                -> UniqFM (Node k cls color)
addConflictSet1 u getClass set
 = case delOneFromUniqSet set u of
    set' -> adjustWithDefaultUFM
                (\node -> node                  { nodeConflicts = unionUniqSets set' (nodeConflicts node) } )
                (newNode u (getClass u))        { nodeConflicts = set' }
                u


-- | Add an exclusion to the graph, creating nodes if required.
--      These are extra colors that the node cannot use.
addExclusion
        :: (Uniquable k, Uniquable color)
        => k -> (k -> cls) -> color
        -> Graph k cls color -> Graph k cls color

addExclusion u getClass color
        = graphMapModify
        $ adjustWithDefaultUFM
                (\node -> node                  { nodeExclusions = addOneToUniqSet (nodeExclusions node) color })
                (newNode u (getClass u))        { nodeExclusions = unitUniqSet color }
                u

addExclusions
        :: (Uniquable k, Uniquable color)
        => k -> (k -> cls) -> [color]
        -> Graph k cls color -> Graph k cls color

addExclusions u getClass colors graph
        = foldr (addExclusion u getClass) graph colors


-- | Add a coalescence edge to the graph, creating nodes if requried.
--      It is considered adventageous to assign the same color to nodes in a coalesence.
addCoalesce
        :: Uniquable k
        => (k, cls) -> (k, cls)
        -> Graph k cls color -> Graph k cls color

addCoalesce (u1, c1) (u2, c2)
 = let  addCoalesce u c u'
         =      adjustWithDefaultUFM
                        (\node -> node { nodeCoalesce = addOneToUniqSet (nodeCoalesce node) u' })
                        (newNode u c)  { nodeCoalesce = unitUniqSet u' }
                        u

   in   graphMapModify
        ( addCoalesce u1 c1 u2
        . addCoalesce u2 c2 u1)


-- | Delete a coalescence edge (k1 -> k2) from the graph.
delCoalesce
        :: Uniquable k
        => k -> k
        -> Graph k cls color    -> Maybe (Graph k cls color)

delCoalesce k1 k2
        = modNode (\node -> node { nodeCoalesce = delOneFromUniqSet (nodeCoalesce node) k2 })
                k1


-- | Add a color preference to the graph, creating nodes if required.
--      The most recently added preference is the most prefered.
--      The algorithm tries to assign a node it's prefered color if possible.
--
addPreference
        :: Uniquable k
        => (k, cls) -> color
        -> Graph k cls color -> Graph k cls color

addPreference (u, c) color
        = graphMapModify
        $ adjustWithDefaultUFM
                (\node -> node { nodePreference = color : (nodePreference node) })
                (newNode u c)  { nodePreference = [color] }
                u


-- | Do aggressive coalescing on this graph.
--      returns the new graph and the list of pairs of nodes that got coalesced together.
--      for each pair, the resulting node will have the least key and be second in the pair.
--
coalesceGraph
        :: (Uniquable k, Ord k, Eq cls, Outputable k)
        => Bool                 -- ^ If True, coalesce nodes even if this might make the graph
                                --      less colorable (aggressive coalescing)
        -> Triv k cls color
        -> Graph k cls color
        -> ( Graph k cls color
           , [(k, k)])          -- pairs of nodes that were coalesced, in the order that the
                                --      coalescing was applied.

coalesceGraph aggressive triv graph
        = coalesceGraph' aggressive triv graph []

coalesceGraph'
        :: (Uniquable k, Ord k, Eq cls, Outputable k)
        => Bool
        -> Triv k cls color
        -> Graph k cls color
        -> [(k, k)]
        -> ( Graph k cls color
           , [(k, k)])
coalesceGraph' aggressive triv graph kkPairsAcc
 = let
        -- find all the nodes that have coalescence edges
        cNodes  = filter (\node -> not $ isEmptyUniqSet (nodeCoalesce node))
                $ nonDetEltsUFM $ graphMap graph
                -- See Note [Unique Determinism and code generation]

        -- build a list of pairs of keys for node's we'll try and coalesce
        --      every pair of nodes will appear twice in this list
        --      ie [(k1, k2), (k2, k1) ... ]
        --      This is ok, GrapOps.coalesceNodes handles this and it's convenient for
        --      build a list of what nodes get coalesced together for later on.
        --
        cList   = [ (nodeId node1, k2)
                        | node1 <- cNodes
                        , k2    <- nonDetEltsUniqSet $ nodeCoalesce node1 ]
                        -- See Note [Unique Determinism and code generation]

        -- do the coalescing, returning the new graph and a list of pairs of keys
        --      that got coalesced together.
        (graph', mPairs)
                = mapAccumL (coalesceNodes aggressive triv) graph cList

        -- keep running until there are no more coalesces can be found
   in   case catMaybes mPairs of
         []     -> (graph', reverse kkPairsAcc)
         pairs  -> coalesceGraph' aggressive triv graph' (reverse pairs ++ kkPairsAcc)


-- | Coalesce this pair of nodes unconditionally \/ aggressively.
--      The resulting node is the one with the least key.
--
--      returns: Just    the pair of keys if the nodes were coalesced
--                       the second element of the pair being the least one
--
--               Nothing if either of the nodes weren't in the graph

coalesceNodes
        :: (Uniquable k, Ord k, Eq cls)
        => Bool                 -- ^ If True, coalesce nodes even if this might make the graph
                                --      less colorable (aggressive coalescing)
        -> Triv  k cls color
        -> Graph k cls color
        -> (k, k)               -- ^ keys of the nodes to be coalesced
        -> (Graph k cls color, Maybe (k, k))

coalesceNodes aggressive triv graph (k1, k2)
        | (kMin, kMax)  <- if k1 < k2
                                then (k1, k2)
                                else (k2, k1)

        -- the nodes being coalesced must be in the graph
        , Just nMin     <- lookupNode graph kMin
        , Just nMax     <- lookupNode graph kMax

        -- can't coalesce conflicting modes
        , not $ elementOfUniqSet kMin (nodeConflicts nMax)
        , not $ elementOfUniqSet kMax (nodeConflicts nMin)

        -- can't coalesce the same node
        , nodeId nMin /= nodeId nMax

        = coalesceNodes_merge aggressive triv graph kMin kMax nMin nMax

        -- don't do the coalescing after all
        | otherwise
        = (graph, Nothing)

coalesceNodes_merge
        :: (Uniquable k, Eq cls)
        => Bool
        -> Triv  k cls color
        -> Graph k cls color
        -> k -> k
        -> Node k cls color
        -> Node k cls color
        -> (Graph k cls color, Maybe (k, k))

coalesceNodes_merge aggressive triv graph kMin kMax nMin nMax

        -- sanity checks
        | nodeClass nMin /= nodeClass nMax
        = error "GraphOps.coalesceNodes: can't coalesce nodes of different classes."

        | not (isNothing (nodeColor nMin) && isNothing (nodeColor nMax))
        = error "GraphOps.coalesceNodes: can't coalesce colored nodes."

        ---
        | otherwise
        = let
                -- the new node gets all the edges from its two components
                node    =
                 Node   { nodeId                = kMin
                        , nodeClass             = nodeClass nMin
                        , nodeColor             = Nothing

                        -- nodes don't conflict with themselves..
                        , nodeConflicts
                                = (unionUniqSets (nodeConflicts nMin) (nodeConflicts nMax))
                                        `delOneFromUniqSet` kMin
                                        `delOneFromUniqSet` kMax

                        , nodeExclusions        = unionUniqSets (nodeExclusions nMin) (nodeExclusions nMax)
                        , nodePreference        = nodePreference nMin ++ nodePreference nMax

                        -- nodes don't coalesce with themselves..
                        , nodeCoalesce
                                = (unionUniqSets (nodeCoalesce nMin) (nodeCoalesce nMax))
                                        `delOneFromUniqSet` kMin
                                        `delOneFromUniqSet` kMax
                        }

          in    coalesceNodes_check aggressive triv graph kMin kMax node

coalesceNodes_check
        :: Uniquable k
        => Bool
        -> Triv  k cls color
        -> Graph k cls color
        -> k -> k
        -> Node k cls color
        -> (Graph k cls color, Maybe (k, k))

coalesceNodes_check aggressive triv graph kMin kMax node

        -- Unless we're coalescing aggressively, if the result node is not trivially
        --      colorable then don't do the coalescing.
        | not aggressive
        , not $ triv (nodeClass node) (nodeConflicts node) (nodeExclusions node)
        = (graph, Nothing)

        | otherwise
        = let -- delete the old nodes from the graph and add the new one
                Just graph1     = delNode kMax graph
                Just graph2     = delNode kMin graph1
                graph3          = addNode kMin node graph2

          in    (graph3, Just (kMax, kMin))


-- | Freeze a node
--      This is for the iterative coalescer.
--      By freezing a node we give up on ever coalescing it.
--      Move all its coalesce edges into the frozen set - and update
--      back edges from other nodes.
--
freezeNode
        :: Uniquable k
        => k                    -- ^ key of the node to freeze
        -> Graph k cls color    -- ^ the graph
        -> Graph k cls color    -- ^ graph with that node frozen

freezeNode k
  = graphMapModify
  $ \fm ->
    let -- freeze all the edges in the node to be frozen
        Just node = lookupUFM fm k
        node'   = node
                { nodeCoalesce          = emptyUniqSet }

        fm1     = addToUFM fm k node'

        -- update back edges pointing to this node
        freezeEdge k node
         = if elementOfUniqSet k (nodeCoalesce node)
                then node { nodeCoalesce = delOneFromUniqSet (nodeCoalesce node) k }
                else node       -- panic "GraphOps.freezeNode: edge to freeze wasn't in the coalesce set"
                                -- If the edge isn't actually in the coelesce set then just ignore it.

        fm2     = nonDetFoldUniqSet (adjustUFM_C (freezeEdge k)) fm1
                    -- It's OK to use nonDetFoldUFM here because the operation
                    -- is commutative
                        $ nodeCoalesce node

    in  fm2


-- | Freeze one node in the graph
--      This if for the iterative coalescer.
--      Look for a move related node of low degree and freeze it.
--
--      We probably don't need to scan the whole graph looking for the node of absolute
--      lowest degree. Just sample the first few and choose the one with the lowest
--      degree out of those. Also, we don't make any distinction between conflicts of different
--      classes.. this is just a heuristic, after all.
--
--      IDEA:   freezing a node might free it up for Simplify.. would be good to check for triv
--              right here, and add it to a worklist if known triv\/non-move nodes.
--
freezeOneInGraph
        :: (Uniquable k)
        => Graph k cls color
        -> ( Graph k cls color          -- the new graph
           , Bool )                     -- whether we found a node to freeze

freezeOneInGraph graph
 = let  compareNodeDegree n1 n2
                = compare (sizeUniqSet $ nodeConflicts n1) (sizeUniqSet $ nodeConflicts n2)

        candidates
                = sortBy compareNodeDegree
                $ take 5        -- 5 isn't special, it's just a small number.
                $ scanGraph (\node -> not $ isEmptyUniqSet (nodeCoalesce node)) graph

   in   case candidates of

         -- there wasn't anything available to freeze
         []     -> (graph, False)

         -- we found something to freeze
         (n : _)
          -> ( freezeNode (nodeId n) graph
             , True)


-- | Freeze all the nodes in the graph
--      for debugging the iterative allocator.
--
freezeAllInGraph
        :: (Uniquable k)
        => Graph k cls color
        -> Graph k cls color

freezeAllInGraph graph
        = foldr freezeNode graph
                $ map nodeId
                $ nonDetEltsUFM $ graphMap graph
                -- See Note [Unique Determinism and code generation]


-- | Find all the nodes in the graph that meet some criteria
--
scanGraph
        :: (Node k cls color -> Bool)
        -> Graph k cls color
        -> [Node k cls color]

scanGraph match graph
        = filter match $ nonDetEltsUFM $ graphMap graph
          -- See Note [Unique Determinism and code generation]


-- | validate the internal structure of a graph
--      all its edges should point to valid nodes
--      If they don't then throw an error
--
validateGraph
        :: (Uniquable k, Outputable k, Eq color)
        => SDoc                         -- ^ extra debugging info to display on error
        -> Bool                         -- ^ whether this graph is supposed to be colored.
        -> Graph k cls color            -- ^ graph to validate
        -> Graph k cls color            -- ^ validated graph

validateGraph doc isColored graph

        -- Check that all edges point to valid nodes.
        | edges         <- unionManyUniqSets
                                (  (map nodeConflicts       $ nonDetEltsUFM $ graphMap graph)
                                ++ (map nodeCoalesce        $ nonDetEltsUFM $ graphMap graph))

        , nodes         <- mkUniqSet $ map nodeId $ nonDetEltsUFM $ graphMap graph
        , badEdges      <- minusUniqSet edges nodes
        , not $ isEmptyUniqSet badEdges
        = pprPanic "GraphOps.validateGraph"
                (  text "Graph has edges that point to non-existent nodes"
                $$ text "  bad edges: " <> pprUFM (getUniqSet badEdges) (vcat . map ppr)
                $$ doc )

        -- Check that no conflicting nodes have the same color
        | badNodes      <- filter (not . (checkNode graph))
                        $ nonDetEltsUFM $ graphMap graph
                           -- See Note [Unique Determinism and code generation]
        , not $ null badNodes
        = pprPanic "GraphOps.validateGraph"
                (  text "Node has same color as one of it's conflicts"
                $$ text "  bad nodes: " <> hcat (map (ppr . nodeId) badNodes)
                $$ doc)

        -- If this is supposed to be a colored graph,
        --      check that all nodes have a color.
        | isColored
        , badNodes      <- filter (\n -> isNothing $ nodeColor n)
                        $  nonDetEltsUFM $ graphMap graph
        , not $ null badNodes
        = pprPanic "GraphOps.validateGraph"
                (  text "Supposably colored graph has uncolored nodes."
                $$ text "  uncolored nodes: " <> hcat (map (ppr . nodeId) badNodes)
                $$ doc )


        -- graph looks ok
        | otherwise
        = graph


-- | If this node is colored, check that all the nodes which
--      conflict with it have different colors.
checkNode
        :: (Uniquable k, Eq color)
        => Graph k cls color
        -> Node  k cls color
        -> Bool                 -- ^ True if this node is ok

checkNode graph node
        | Just color            <- nodeColor node
        , Just neighbors        <- sequence $ map (lookupNode graph)
                                $  nonDetEltsUniqSet $ nodeConflicts node
            -- See Note [Unique Determinism and code generation]

        , neighbourColors       <- catMaybes $ map nodeColor neighbors
        , elem color neighbourColors
        = False

        | otherwise
        = True



-- | Slurp out a map of how many nodes had a certain number of conflict neighbours

slurpNodeConflictCount
        :: Graph k cls color
        -> UniqFM (Int, Int)    -- ^ (conflict neighbours, num nodes with that many conflicts)

slurpNodeConflictCount graph
        = addListToUFM_C
                (\(c1, n1) (_, n2) -> (c1, n1 + n2))
                emptyUFM
        $ map   (\node
                  -> let count  = sizeUniqSet $ nodeConflicts node
                     in  (count, (count, 1)))
        $ nonDetEltsUFM
        -- See Note [Unique Determinism and code generation]
        $ graphMap graph


-- | Set the color of a certain node
setColor
        :: Uniquable k
        => k -> color
        -> Graph k cls color -> Graph k cls color

setColor u color
        = graphMapModify
        $ adjustUFM_C
                (\n -> n { nodeColor = Just color })
                u


{-# INLINE adjustWithDefaultUFM #-}
adjustWithDefaultUFM
        :: Uniquable k
        => (a -> a) -> a -> k
        -> UniqFM a -> UniqFM a

adjustWithDefaultUFM f def k map
        = addToUFM_C
                (\old _ -> f old)
                map
                k def

-- Argument order different from UniqFM's adjustUFM
{-# INLINE adjustUFM_C #-}
adjustUFM_C
        :: Uniquable k
        => (a -> a)
        -> k -> UniqFM a -> UniqFM a

adjustUFM_C f k map
 = case lookupUFM map k of
        Nothing -> map
        Just a  -> addToUFM map k (f a)

