-- Just like Bug308 module but here we test that referring to anchors
-- from other modules works.
module Bug308CrossModule where

import Bug308

{-|
start "Bug308#startAnchor"

startOldStyle "Bug308\#startAnchor"

middle "Bug308#middleAnchor"

end "Bug308#middleAnchor"
-}
h :: ()
h = ()
