module ViewType(

   View(..),
   ) where


import {-# SOURCE #-} VersionGraphClient

data View = View {
   graphClient1 :: VersionGraphClient
   }

