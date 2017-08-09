module OutputableAnnotation where

import {-# SOURCE #-} Name (NamedThing)

data PExpr

data BindType

varReference :: NamedThing a => a -> PExpr
