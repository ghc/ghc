{-
Bug report submitted by George Russell, 1/10/2004:

With both ghc6.2 and 6.2.20040915 on Linux, ghc --make cannot compile
the attached files and produces a confusing error message.

  # ghc --make View.hs
Chasing modules from: View.hs
Compiling ViewType         ( ./ViewType.hs, ./ViewType.o )
Compiling VersionGraphClient ( ./VersionGraphClient.hs, ./VersionGraphClient.o )Compiling View             ( View.hs, View.o )

View.hs:14:
     Couldn't match `VersionGraphClient' against `VersionGraphClient'
	Expected type: VersionGraphClient
	Inferred type: VersionGraphClient
     In the `graphClient1' field of a record
     In the record construction: View {graphClient1 = graphClient}

However ghc without make has no problems.

# ghc -c ViewType.hs
# ghc -c VersionGraphClient.hs
# ghc -c View.hs
-}

-- | This module defines the fundamental structure of the (untyped) 
-- objects in a repository. 
-- 
-- We depend circularly on CodedValue.hs.  This module is compiled
-- first and uses CodedValue.hi-boot.
module View(
   ) where

import ViewType
import VersionGraphClient

createView :: VersionGraphClient -> IO View
createView graphClient =
   do
      return (View {
         graphClient1 = graphClient
         })
