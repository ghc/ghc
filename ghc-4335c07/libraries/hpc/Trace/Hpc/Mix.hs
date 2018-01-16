{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
---------------------------------------------------------------
-- Colin Runciman and Andy Gill, June 2006
---------------------------------------------------------------

-- | Datatypes and file-access routines for the per-module (@.mix@)
-- indexes used by Hpc.
module Trace.Hpc.Mix
        ( Mix(..)
        , MixEntry
        , BoxLabel(..)
        , CondBox(..)
        , mixCreate
        , readMix
        , createMixEntryDom
        , MixEntryDom
        )
  where

import Data.List
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (UTCTime)
import Data.Tree
#if MIN_VERSION_base(4,6,0)
import Text.Read (readMaybe)
#else
import Data.Char (isSpace)
#endif

import System.FilePath

-- a module index records the attributes of each tick-box that has
-- been introduced in that module, accessed by tick-number position
-- in the list

import Trace.Hpc.Util (HpcPos, insideHpcPos, Hash, HpcHash(..), catchIO)
import Trace.Hpc.Tix

#if !MIN_VERSION_base(4,6,0)
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
  [(x, s')] | all isSpace s' -> Just x
  _                          -> Nothing
#endif

-- | 'Mix' is the information about a modules static properties, like
-- location of Tix's in a file.
--
-- Tab stops are the size of a tab in the provided /line:column/ values.
--
--  * In GHC, this is 1 (a tab is just a character)
--  * With @hpc-tracer@, this is 8 (a tab represents several spaces).
data Mix = Mix
             FilePath           -- location of original file
             UTCTime            -- time of original file's last update
             Hash               -- hash of mix entry + timestamp
             Int                -- tab stop value.
             [MixEntry]         -- entries
        deriving (Show,Read,Eq)

type MixEntry = (HpcPos, BoxLabel)

data BoxLabel = ExpBox  Bool -- isAlt
              | TopLevelBox [String]
              | LocalBox [String]
              | BinBox CondBox Bool
              deriving (Read, Show, Eq, Ord)

data CondBox = GuardBinBox
             | CondBinBox
             | QualBinBox
              deriving (Read, Show, Eq, Ord)

instance HpcHash BoxLabel where
   toHash (ExpBox b)       = 0x100 + toHash b
   toHash (TopLevelBox nm) = 0x200 + toHash nm
   toHash (LocalBox nm)    = 0x300 + toHash nm
   toHash (BinBox cond b)  = 0x400 + toHash (cond,b)

instance HpcHash CondBox where
   toHash GuardBinBox = 0x10
   toHash CondBinBox  = 0x20
   toHash QualBinBox  = 0x30


-- | Create is mix file.
mixCreate :: String -- ^ Dir Name
          -> String -- ^ module Name
          -> Mix    -- ^ Mix DataStructure
          -> IO ()
mixCreate dirName modName mix =
   writeFile (mixName dirName modName) (show mix)

-- | Read a mix file.
readMix :: [String]                 -- ^ Dir Names
        -> Either String TixModule  -- ^ module wanted
        -> IO Mix
readMix dirNames mod' = do
   let modName = either id tixModuleName mod'
   res <- sequence [ (do let mixPath    = mixName dirName modName
                             parseError = error ("can not parse " ++ mixPath)
                             parse      = fromMaybe parseError . readMaybe
                         mix <- parse `fmap` readFile mixPath
                         case mod' of
                            Left  _   -> return $ Just mix -- Bypass hash check
                            Right tix -> return $ checkHash tix mix mixPath)
                     `catchIO` (\ _ -> return $ Nothing)
                   | dirName <- dirNames
                   ]
   case catMaybes res of
     xs@(x:_:_) | any (/= x) (tail xs) ->
              -- Only complain if multiple *different* `Mix` files with the
              -- same name are found (#9619).
              error $ "found " ++ show(length xs) ++ " different instances of "
                      ++ modName ++ " in " ++ intercalate ", " dirNames
     (x:_) -> return x
     _     -> error $ "can not find "
                      ++ modName ++ " in " ++ intercalate ", " dirNames

mixName :: FilePath -> String -> String
mixName dirName name = dirName </> name <.> "mix"

-- | Check that hash in .tix and .mix file match.
checkHash :: TixModule -> Mix -> FilePath -> Maybe Mix
checkHash tix mix@(Mix _ _ mixHash _ _) mixPath
  | modHash == mixHash = Just mix
  | otherwise = error $
      "hash in tix file for module " ++ modName ++ " (" ++ show modHash ++ ")\n"
      ++ "does not match hash in " ++ mixPath ++ " (" ++ show mixHash ++ ")"
  where
    modName = tixModuleName tix
    modHash = tixModuleHash tix

------------------------------------------------------------------------------

type MixEntryDom a = Tree (HpcPos,a)

-- A good tree has all its children fully inside its parents HpcPos.
-- No child should have the *same* HpcPos.
-- There is no ordering to the children

isGoodNode :: MixEntryDom a -> Bool
isGoodNode (Node (pos,_) sub_nodes) =
      and [ pos' `insideHpcPos` pos  | Node(pos',_)  _ <- sub_nodes ]
   && and [ pos' /= pos | Node(pos',_) _ <- sub_nodes ]
   && isGoodForest sub_nodes

-- all sub-trees are good trees, and no two HpcPos are inside each other.
isGoodForest :: [MixEntryDom a] -> Bool
isGoodForest sub_nodes =
   all isGoodNode sub_nodes
   && and [  not (pos1 `insideHpcPos` pos2 ||
                  pos2 `insideHpcPos` pos1)
          | (Node (pos1,_) _,n1) <- zip sub_nodes [0..]
          , (Node (pos2,_) _,n2) <- zip sub_nodes [0..]
          , (n1 :: Int) /= n2 ]

addNodeToTree :: (Show a) => (HpcPos,a) -> MixEntryDom [a] -> MixEntryDom [a]
addNodeToTree (new_pos,new_a) (Node (pos,a) children)
  | pos == new_pos = Node (pos,new_a : a) children
  | new_pos `insideHpcPos` pos =
       Node (pos,a) (addNodeToList (new_pos,new_a) children)
  | pos `insideHpcPos` new_pos =
       error "precondition not met inside addNodeToNode"
  | otherwise = error "something impossible happened in addNodeToTree"

addNodeToList :: Show a => (HpcPos,a) -> [MixEntryDom [a]] -> [MixEntryDom [a]]
addNodeToList (new_pos,new_a) entries
  | otherwise =
  if length [ ()
          | (am_inside,am_outside,_) <- entries'
          , am_inside || am_outside
          ] == 0
     -- The case where we have a new HpcPos range
     then Node (new_pos,[new_a]) [] : entries else
  if length [ ()
            | (am_inside,_,_) <- entries'
            , am_inside
            ] > 0
     -- The case where we are recursing into a tree
     -- Note we can recurse down many branches, in the case of
     -- overlapping ranges.
     -- Assumes we have captures the new HpcPos
     -- (or the above conditional would be true)
     then [ if i_am_inside  -- or the same as
            then addNodeToTree (new_pos,new_a) node
            else node
          | (i_am_inside,_,node) <- entries'
          ] else
     -- The case of a super-range.
     ( Node (new_pos,[new_a])
             [ node | (_,True,node) <- entries' ] :
       [ node | (_,False,node) <- entries' ]
     )
  where
    entries' = [ ( new_pos `insideHpcPos` pos
                 , pos  `insideHpcPos` new_pos
                 , node)
               | node@(Node (pos,_) _) <- entries
               ]

createMixEntryDom :: (Show a) => [(HpcPos,a)] -> [MixEntryDom [a]]
createMixEntryDom entries
    | isGoodForest forest = forest
    | otherwise = error "createMixEntryDom: bad forest"
  where forest = foldr addNodeToList [] entries
