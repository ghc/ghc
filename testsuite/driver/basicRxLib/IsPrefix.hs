module IsPrefix 
  (isPrefix)
where



import FiniteMap

import AbsTreeDefs 

isPrefix :: AbsTree a b -> Bool
isPrefix t | isLeaf t = getPrefix t 
           | isUn t = isPrefix (child t)
           | isCon t = (isPrefix (left t))
           | isAlt t = (isPrefix (left t)) && (isPrefix (right t))


