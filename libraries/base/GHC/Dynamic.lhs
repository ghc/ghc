% -----------------------------------------------------------------------------
% $Id: Dynamic.lhs,v 1.1 2001/06/28 14:15:03 simonmar Exp $
%
% (c) The University of Glasgow, 1998-2000
%

The Dynamic type is used in the Exception type, so we have to have
Dynamic visible here.  The rest of the operations on Dynamics are
available in lang/Dynamic.lhs.

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

#ifndef __HUGS__
module GHC.Dynamic where

import GHC.Base
#endif

data Dynamic = Dynamic TypeRep Obj

data Obj = Obj  
 -- dummy type to hold the dynamically typed value.

data TypeRep
 = App TyCon   [TypeRep]
 | Fun TypeRep TypeRep
   deriving ( Eq )

-- type constructors are 
data TyCon = TyCon Int String

instance Eq TyCon where
  (TyCon t1 _) == (TyCon t2 _) = t1 == t2
\end{code}
