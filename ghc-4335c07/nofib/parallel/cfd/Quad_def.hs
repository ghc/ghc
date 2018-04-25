module Quad_def (TriMat(..), RectMat(..) ) where

data TriMat a =
	TriM (TriMat a) (RectMat a) (TriMat a) | SingTM a | ZeroTM deriving ()

data RectMat a =
	RectM (RectMat a) (RectMat a) (RectMat a) (RectMat a)
	| SingM a | ZeroM deriving ()
