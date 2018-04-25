{-
// This is adapted from a benchmark written by John Ellis and Pete Kovac
// of Post Communications.
// It was modified by Hans Boehm of Silicon Graphics.
//
// 	This is no substitute for real applications.  No actual application
//	is likely to behave in exactly this way.  However, this benchmark was
//	designed to be more representative of real applications than other
//	Java GC benchmarks of which we are aware.
//	It attempts to model those properties of allocation requests that
//	are important to current GC techniques.
//	It is designed to be used either to obtain a single overall performance
//	number, or to give a more detailed estimate of how collector
//	performance varies with object lifetimes.  It prints the time
//	required to allocate and collect balanced binary trees of various
//	sizes.  Smaller trees result in shorter object lifetimes.  Each cycle
//	allocates roughly the same amount of memory.
//	Two data structures are kept around during the entire process, so
//	that the measured performance is representative of applications
//	that maintain some live in-memory data.  One of these is a tree
//	containing many pointers.  The other is a large array containing
//	double precision floating point numbers.  Both should be of comparable
//	size.
//
//	The results are only really meaningful together with a specification
//	of how much memory was used.  It is possible to trade memory for
//	better time performance.  This benchmark should be run in a 32 MB
//	heap, though we don't currently know how to enforce that uniformly.
//
//	Unlike the original Ellis and Kovac benchmark, we do not attempt
// 	measure pause times.  This facility should eventually be added back
//	in.  There are several reasons for omitting it for now.  The original
//	implementation depended on assumptions about the thread scheduler
//	that don't hold uniformly.  The results really measure both the
//	scheduler and GC.  Pause time measurements tend to not fit well with
//	current benchmark suites.  As far as we know, none of the current
//	commercial Java implementations seriously attempt to minimize GC pause
//	times.
//
//	Known deficiencies:
//		- No way to check on memory use
//		- No cyclic data structures
//		- No attempt to measure variation with object size
//		- Results are sensitive to locking cost, but we dont
//		  check for proper locking

class Node {
	Node left, right;
	int i, j;
	Node(Node l, Node r) { left = l; right = r; }
	Node() { }
}

public class GCBench {

	public static final int kStretchTreeDepth    = 18;	// about 16Mb
	public static final int kLongLivedTreeDepth  = 16;  // about 4Mb
	public static final int kArraySize  = 500000;  // about 4Mb
	public static final int kMinTreeDepth = 4;
	public static final int kMaxTreeDepth = 16;

	// Nodes used by a tree of a given size
	static int TreeSize(int i) {
	    	return ((1 << (i + 1)) - 1);
	}

	// Number of iterations to use for a given tree depth
	static int NumIters(int i) {
                return 2 * TreeSize(kStretchTreeDepth) / TreeSize(i);
        }

	// Build tree top down, assigning to older objects. 
	static void Populate(int iDepth, Node thisNode) {
		if (iDepth<=0) {
			return;
		} else {
			iDepth--;
			thisNode.left  = new Node();
			thisNode.right = new Node();
			Populate (iDepth, thisNode.left);
			Populate (iDepth, thisNode.right);
		}
	}

	// Build tree bottom-up
	static Node MakeTree(int iDepth) {
		if (iDepth<=0) {
			return new Node();
		} else {
			return new Node(MakeTree(iDepth-1),
					MakeTree(iDepth-1));
		}
	}

	static void PrintDiagnostics() {
		long lFreeMemory = Runtime.getRuntime().freeMemory();
		long lTotalMemory = Runtime.getRuntime().totalMemory();

		System.out.print(" Total memory available="
				 + lTotalMemory + " bytes");
		System.out.println("  Free memory=" + lFreeMemory + " bytes");
	}

	static void TimeConstruction(int depth) {
		Node    root;
		long    tStart, tFinish;
		int 	iNumIters = NumIters(depth);
		Node	tempTree;

		System.out.println("Creating " + iNumIters +
				   " trees of depth " + depth);
		tStart = System.currentTimeMillis();
		for (int i = 0; i < iNumIters; ++i) {
			tempTree = new Node();
			Populate(depth, tempTree);
			tempTree = null;
		}
		tFinish = System.currentTimeMillis();
		System.out.println("\tTop down construction took "
				   + (tFinish - tStart) + "msecs");
		tStart = System.currentTimeMillis();
                for (int i = 0; i < iNumIters; ++i) {
                        tempTree = MakeTree(depth);
                        tempTree = null;
                }
                tFinish = System.currentTimeMillis();
                System.out.println("\tBottom up construction took "
                                   + (tFinish - tStart) + "msecs");
		
	}

	public static void main(String args[]) {
		Node	root;
		Node	longLivedTree;
		Node	tempTree;
		long	tStart, tFinish;
		long	tElapsed;


		System.out.println("Garbage Collector Test");
		System.out.println(
			" Stretching memory with a binary tree of depth "
			+ kStretchTreeDepth);
		PrintDiagnostics();
		tStart = System.currentTimeMillis();

		// Stretch the memory space quickly
		tempTree = MakeTree(kStretchTreeDepth);
		tempTree = null;

		// Create a long lived object
		System.out.println(
			" Creating a long-lived binary tree of depth " +
  			kLongLivedTreeDepth);
		longLivedTree = new Node();
		Populate(kLongLivedTreeDepth, longLivedTree);

		// Create long-lived array, filling half of it
		System.out.println(
                        " Creating a long-lived array of "
			+ kArraySize + " doubles");
		double array[] = new double[kArraySize];
		for (int i = 0; i < kArraySize/2; ++i) {
			array[i] = 1.0/i;
		}
		PrintDiagnostics();

		for (int d = kMinTreeDepth; d <= kMaxTreeDepth; d += 2) {
			TimeConstruction(d);
		}

		if (longLivedTree == null || array[1000] != 1.0/1000)
			System.out.println("Failed");
					// fake reference to LongLivedTree
					// and array
					// to keep them from being optimized away

		tFinish = System.currentTimeMillis();
		tElapsed = tFinish-tStart;
		PrintDiagnostics();
		System.out.println("Completed in " + tElapsed + "ms.");
	}
} // class JavaGC
-}

import Text.Printf
import System.CPUTime
import Data.IORef
import Data.Array.IO
import System.Time	( ClockTime(..) )
import Control.Monad 	( replicateM_ )
import System.IO
import System.Environment

class DeepSeq a where
  deepSeq :: a -> b -> b
  deepSeq = seq

instance DeepSeq Int

instance DeepSeq Tree where
  deepSeq Empty b = b
  deepSeq Node{left=l, right=r, i=i} b =
    deepSeq l $ deepSeq r $ deepSeq i b

treeSize i = 2^(i+1) - 1

numIters max i = 2 * treeSize max `quot` treeSize i

data Tree = Node { left, right :: Tree,  i :: Int } | Empty

makeTree 0      = Node { left = Empty, right = Empty, i = 0 }
makeTree iDepth = Node { left  = makeTree (iDepth-1),
			 right = makeTree (iDepth-1),
			 i = 0 }

data MutTree = MutNode (IORef MutTree) (IORef MutTree) Int | MutEmpty

newMutNode x = do
  l <- newIORef MutEmpty
  r <- newIORef MutEmpty
  return (MutNode l r x)

-- Build tree top down, assigning to older objects. 
populate 0 node = return ()
populate iDepth (MutNode lref rref i) = do
  l <- newMutNode iDepth
  writeIORef lref l
  r <- newMutNode iDepth
  writeIORef rref r
  populate (iDepth-1) l
  populate (iDepth-1) r

ldepth MutEmpty = return 0
ldepth (MutNode l _ _) = do t <- readIORef l; ldepth t

timeConstruction max depth = do
  let iNumIters = numIters max depth
--  printf "Creating %d trees of depth %d\n" iNumIters depth
  tStart <- getCPUTime
  replicateM_ iNumIters $ do
	n <- newMutNode depth; populate depth n; touch n
  tFinish <- getCPUTime
--  printf "\tTop down construction took "
--  ptimediff stdout tStart tFinish
--  printf "\n"
  tStart <- getCPUTime
  replicateM_ iNumIters $ do
	let tempTree = makeTree depth
  	deepSeq tempTree (return ())
  	touch tempTree
  tFinish <- getCPUTime
--  printf "\tBottom-up construction took "
--  ptimediff stdout tStart tFinish
--  printf "\n"
  return ()

main = do
  args <- getArgs
  let 
    [kLongLivedTreeDepth, 
     kArraySize, 
     kMinTreeDepth,
     kMaxTreeDepth] = map read args :: [Int]

  hSetBuffering stdout NoBuffering
--  printf "Garbage Collector Test\n"
  tStart <- getCPUTime

  -- Create a long lived object or two
--  printf " Creating a long-lived binary tree of depth %d\n" kLongLivedTreeDepth
  let longLivedTree1 = makeTree kLongLivedTreeDepth
  deepSeq longLivedTree1 (return ())

--  printf " Creating a long-lived mutable tree of depth %d\n" kLongLivedTreeDepth
  longLivedTree2 <- newMutNode kLongLivedTreeDepth;
  populate kLongLivedTreeDepth longLivedTree2

  -- Create long-lived array, filling half of it
--  printf " Creating a long-lived array of %d doubles\n" kArraySize 
  array <- newArray (1,kArraySize) 0.0
  let _ = array :: IOArray Int Double
  sequence_ [ writeArray array i (1.0 / fromIntegral i) 
	    | i <- [ 1 .. kArraySize `quot` 2 ] ]

  sequence_ [ timeConstruction kMaxTreeDepth d
            | d <- [ kMinTreeDepth, kMinTreeDepth+2 .. kMaxTreeDepth ] ]

  touch longLivedTree1
  ldepth longLivedTree2
  touch array

-- Utils

ptimediff :: Handle -> Integer -> Integer -> IO ()
ptimediff hout t0 t1 =
  hPrintf hout "%d.%02d" secs (psecs `quot` 10^10)
  where  (secs,psecs) = (t1 - t0) `quotRem` (10^12)

touch :: a -> IO ()
touch a = a `seq` return ()
