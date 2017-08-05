module Target (Target, target, context, builder, inputs, outputs) where

import Builder
import Context

import qualified Hadrian.Target as H
import Hadrian.Target hiding (Target)

type Target = H.Target Context Builder
