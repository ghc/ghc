module CoreArity where

import BasicTypes
import CoreSyn

etaExpandToJoinPoint :: JoinArity -> CoreExpr -> ([CoreBndr], CoreExpr)
