-- From 2.14.x onwards we were forgetting to swallow ‘#’ as a special
-- character resulting in broken anchors if they accured
-- mid-paragraph. Here we check that anchors get generated as
-- expected.
module Bug308 where

-- | start#startAnchor# followed by middle#middleAnchor# and end#endAnchor#
f :: ()
f = ()

{-|
start "Bug308#startAnchor"

startOldStyle "Bug308\#startAnchor"

middle "Bug308#middleAnchor"

end "Bug308#middleAnchor"
-}
g :: ()
g = ()
