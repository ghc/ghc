{-# LANGUAGE ScopedTypeVariables #-}
module MkZipCfg
    ( AGraph, (<*>), catAGraphs
    , freshBlockId
    , emptyAGraph, withFreshLabel, withUnique
    , mkMiddle, mkMiddles, mkLast, mkZTail, mkBranch, mkLabel, mkIfThenElse, mkWhileDo
    , outOfLine
    , emptyGraph, graphOfMiddles, graphOfZTail
    , lgraphOfAGraph, graphOfAGraph, labelAGraph, pprAGraph
    )
where

import BlockId (BlockId(..), emptyBlockEnv, plusBlockEnv)
import ZipCfg

import Outputable
import Unique
import UniqSupply
import Util

import Prelude hiding (zip, unzip, last)

#include "HsVersions.h"

-------------------------------------------------------------------------
--     GENERIC ZIPPER-BASED CONTROL-FLOW GRAPH (CONSTRUCTOR VIEW)      --
-------------------------------------------------------------------------

{-

You can think of an AGraph like this: it is the program built by
composing in sequence three kinds of nodes:
  * Label nodes (e.g. L2:)
  * Middle nodes (e.g. x = y*3)
  * Last nodes (e.g. if b then goto L1 else goto L2)

The constructors mkLabel, mkMiddle, and mkLast build single-node
AGraphs of the indicated type.  The composition operator <*> glues
AGraphs together in sequence (in constant time).

For example:
       x = 0
  L1:  
       x = x+1
       if x<10 then goto L1 else goto L2
  L2:  
       y = y*x
       x = 0

Notice that the AGraph may begin without a label, and may end without
a control transfer.  Control *always* falls through a label and middle
node, and *never* falls through a Last node.

A 'AGraph m l' is simply an abstract version of a 'Graph m l' from
module 'ZipCfg'.  The only difference is that the 'AGraph m l'
supports a constant-time splicing operation, written infix <*>.
That splicing operation, together with the constructor functions in
this module (and with 'labelAGraph'), is the recommended way to build
large graphs.  Each construction or splice has constant cost, and to
turn an AGraph into a Graph requires time linear in the number of
nodes and N log N in the number of basic blocks.

The splicing operation warrants careful explanation.  Like a Graph, an
AGraph is a control-flow graph which begins with a distinguished,
unlabelled sequence of middle nodes called the *entry*.  An unlabelled
graph may also end with a sequence of middle nodes called the *exit*.
The entry may fall straight through to the exit, or it may fall into 
the rest of the graph, which may include arbitrary control flow.

Using ASCII art, here are examples of the two kinds of graph.  On the
left, the entry and exit sequences are labelled A and B, where the
control flow in the middle is labelled X.   On the right, there is no
exit sequence:
                                              
        |                      |              
        | A                    | C            
        |                      |              
       / \                    / \
      /   \                  /   \
     |  X  |                |  Y  |           
      \   /                  \   /            
       \ /                    \_/             
        |                      
        | B                    
        |                      


The AGraph has these properties:

  * A AGraph is opaque; nothing about its structure can be observed.

  * A AGraph may be turned into a LGraph in time linear in the number
    of nodes and O(N log N) in the number of basic blocks.

  * Two AGraphs may be spliced in constant time by writing  g1 <*> g2

There are two rules for splicing, depending on whether the left-hand
graph falls through.  If it does, the rule is as follows:
                                              
        |                      |                          |      
        | A                    | C                        | A    
        |                      |                          |      
       / \                    / \                        / \
      /   \                  /   \                      /   \
     |  X  |      <*>       |  Y  |           =        |  X  |   
      \   /                  \   /                      \   /    
       \ /                    \_/                        \ /     
        |                      |                          |          
        | B                    | D                        | B        
        |                      |                          |          
                                                          |      
                                                          | C
                                                          |      
                                                         / \
                                                        /   \
                                                       |  Y  |   
                                                        \   /    
                                                         \ /     
                                                          |      
                                                          | D    
                                                          |      

And in the case where the left-hand graph does not fall through, the
rule is

                                              
        |                      |                          |      
        | A                    | C                        | A    
        |                      |                          |      
       / \                    / \                        / \
      /   \                  /   \                      /   \
     |  X  |      <*>       |  Y  |           =        |  X  |   
      \   /                  \   /                      \   /    
       \_/                    \_/                        \_/     
                               |                                    
                               | D                        _      
                               |                         / \
                                                        /   \
                                                       |  Y  |   
                                                        \   /    
                                                         \ /     
                                                          |      
                                                          | D    
                                                          |      

In this case C will become unreachable and is lost; when such a graph
is converted into a data structure, the system will bleat about
unreachable code.  Also it must be assumed that there are branches
from somewhere in X to labelled blocks in Y; otherwise Y and D are
unreachable as well.   (However, it may be the case that X branches
into some third AGraph, which in turn branches into D; the
representation is agnostic on this point.)

-}

infixr 3 <*>
(<*>) :: AGraph m l -> AGraph m l -> AGraph m l

catAGraphs :: [AGraph m l] -> AGraph m l

-- | A graph is built up by splicing together graphs each containing a
-- single node (where a label is considered a 'first' node.  The empty
-- graph is a left and right unit for splicing.  All of the AGraph
-- constructors (even complex ones like 'mkIfThenElse', as well as the
-- splicing operation <*>, are constant-time operations.

emptyAGraph :: AGraph m l
mkLabel     :: (LastNode l) => BlockId -> AGraph m l -- graph contains the label
mkMiddle    :: m -> AGraph m l   -- graph contains the node
mkLast      :: (Outputable m, Outputable l, LastNode l) =>
               l       -> AGraph m l              -- graph contains the node

-- | This function provides access to fresh labels without requiring
-- clients to be programmed monadically.
withFreshLabel :: String -> (BlockId -> AGraph m l) -> AGraph m l
withUnique     :: (Unique -> AGraph m l) -> AGraph m l


outOfLine :: (LastNode l, Outputable m, Outputable l)
          => AGraph m l -> AGraph m l
-- ^ The argument is an AGraph that has an 
-- empty entry sequence and no exit sequence.
-- The result is a new AGraph that has an empty entry sequence
-- connected to an empty exit sequence, with the original graph
-- sitting to the side out-of-line.
--
-- Example:  mkMiddle (x = 3)
--           <*> outOfLine (mkLabel L <*> ...stuff...)
--           <*> mkMiddle (y = x)
-- Control will flow directly from x=3 to y=x;
-- the block starting with L is "on the side".
--
-- N.B. algebraically forall g g' : g <*> outOfLine g' == outOfLine g' <*> g



-- below for convenience
mkMiddles :: [m] -> AGraph m l
mkZTail   :: (Outputable m, Outputable l, LastNode l) =>
  ZTail m l -> AGraph m l
mkBranch  :: (Outputable m, Outputable l, LastNode l) =>
  BlockId   -> AGraph m l

-- | For the structured control-flow constructs, a condition is
-- represented as a function that takes as arguments the labels to
-- goto on truth or falsehood.
--
--	mkIfThenElse mk_cond then else
--	= (mk_cond L1 L2) <*> L1: then <*> goto J
--		          <*> L2: else <*> goto J
--	  <*> J:
--
-- where L1, L2, J are fresh

mkIfThenElse :: (Outputable m, Outputable l, LastNode l)
                => (BlockId -> BlockId -> AGraph m l) -- branch condition
                -> AGraph m l   -- code in the 'then' branch
                -> AGraph m l   -- code in the 'else' branch 
                -> AGraph m l   -- resulting if-then-else construct

mkWhileDo    :: (Outputable m, Outputable l, LastNode l)
                => (BlockId -> BlockId -> AGraph m l) -- loop condition
                -> AGraph m l  -- body of the bloop
                -> AGraph m l  -- the final while loop

-- | Converting an abstract graph to a concrete form is expensive: the
-- cost is linear in the number of nodes in the answer, plus N log N
-- in the number of basic blocks.  The conversion is also monadic
-- because it may require the allocation of fresh, unique labels.

graphOfAGraph  :: AGraph m l -> UniqSM (Graph  m l)
lgraphOfAGraph :: AGraph m l -> UniqSM (LGraph m l)
  -- ^ allocate a fresh label for the entry point
labelAGraph    :: BlockId -> AGraph m l -> UniqSM (LGraph m l)
  -- ^ use the given BlockId as the label of the entry point


-- | The functions below build Graphs directly; for convenience, they
-- are included here with the rest of the constructor functions.

emptyGraph     ::              Graph m l
graphOfMiddles :: [m]       -> Graph m l
graphOfZTail   :: ZTail m l -> Graph m l


-- ================================================================
--                          IMPLEMENTATION
-- ================================================================

newtype AGraph m l = AGraph (Graph m l -> UniqSM (Graph m l))
  -- an AGraph is a monadic function from a successor Graph to a new Graph

AGraph f1 <*> AGraph f2 = AGraph f 
    where f g = f2 g >>= f1 -- note right associativity

catAGraphs = foldr (<*>) emptyAGraph

emptyAGraph = AGraph return

graphOfAGraph (AGraph f) = f emptyGraph
emptyGraph = Graph (ZLast LastExit) emptyBlockEnv

labelAGraph id g =
    do Graph tail blocks <- graphOfAGraph g
       return $ LGraph id $ insertBlock (Block id tail) blocks

lgraphOfAGraph g = do id <- freshBlockId "graph entry"
                      labelAGraph id g

-------------------------------------
-- constructors

mkLabel id = AGraph f
    where f (Graph tail blocks) =
            return $ Graph (ZLast (mkBranchNode id))
                           (insertBlock (Block id tail) blocks)

mkBranch target = mkLast $ mkBranchNode target

mkMiddle m = AGraph f
    where f (Graph tail blocks) = return $ Graph (ZTail m tail) blocks

mkMiddles ms = AGraph f
    where f (Graph tail blocks) = return $ Graph (foldr ZTail tail ms) blocks

graphOfMiddles ms = Graph (foldr ZTail (ZLast LastExit) ms) emptyBlockEnv
graphOfZTail   t  = Graph t emptyBlockEnv


mkLast l = AGraph f
    where f (Graph tail blocks) =
            do note_this_code_becomes_unreachable "mkLast" (ppr l <+> ppr blocks) tail
               return $ Graph (ZLast (LastOther l)) blocks

mkZTail tail = AGraph f
    where f (Graph utail blocks) =
            do note_this_code_becomes_unreachable "mkZTail" (ppr tail) utail
               return $ Graph tail blocks

withFreshLabel name ofId = AGraph f
  where f g = do id <- freshBlockId name
                 let AGraph f' = ofId id
                 f' g

withUnique ofU = AGraph f
  where f g = do u <- getUniqueM
                 let AGraph f' = ofU u
                 f' g

outOfLine (AGraph f) = AGraph f'
    where f' (Graph tail' blocks') =
            do Graph emptyEntrance blocks <- f emptyGraph
               note_this_code_becomes_unreachable "outOfLine" (ppr tail') emptyEntrance
               return $ Graph tail' (blocks `plusBlockEnv` blocks')

mkIfThenElse cbranch tbranch fbranch = 
    withFreshLabel "end of if"     $ \endif ->
    withFreshLabel "start of then" $ \tid ->
    withFreshLabel "start of else" $ \fid ->
        cbranch tid fid <*>
        mkLabel tid <*> tbranch <*> mkBranch endif <*>
        mkLabel fid <*> fbranch <*>
        mkLabel endif

mkWhileDo cbranch body = 
  withFreshLabel "loop test" $ \test ->
  withFreshLabel "loop head" $ \head ->
  withFreshLabel "end while" $ \endwhile ->
     -- Forrest Baskett's while-loop layout
     mkBranch test <*> mkLabel head <*> body
                   <*> mkLabel test <*> cbranch head endwhile
                   <*> mkLabel endwhile

-- | Bleat if the insertion of a last node will create unreachable code
note_this_code_becomes_unreachable ::
    (Monad m, LastNode l, Outputable middle, Outputable l) =>
       String -> SDoc -> ZTail middle l -> m ()

note_this_code_becomes_unreachable str old = if debugIsOn then u else \_ -> return ()
    where u (ZLast LastExit)                       = return ()
          u (ZLast (LastOther l)) | isBranchNode l = return ()
                                    -- Note [Branch follows branch]
          u tail = fail ("unreachable code in " ++ str ++ ": " ++
                         (showSDoc ((ppr tail) <+> old)))

-- | The string argument to 'freshBlockId' was originally helpful in debugging
-- the Quick C-- compiler, so I have kept it here even though at present it is
-- thrown away at this spot---there's no reason a BlockId couldn't one day carry
-- a string.  

freshBlockId :: MonadUnique m => String -> m BlockId
freshBlockId _s = getUniqueM >>= return . BlockId

-------------------------------------
-- Debugging

pprAGraph :: (Outputable m, LastNode l, Outputable l) => AGraph m l -> UniqSM SDoc
pprAGraph g = graphOfAGraph g >>= return . ppr

{-
Note [Branch follows branch]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Why do we say it's ok for a Branch to follow a Branch?
Because the standard constructor mkLabel-- has fall-through
semantics. So if you do a mkLabel, you finish the current block,
giving it a label, and start a new one that branches to that label.
Emitting a Branch at this point is fine: 
       goto L1; L2: ...stuff... 
-}


