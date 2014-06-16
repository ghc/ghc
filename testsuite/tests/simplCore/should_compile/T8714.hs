{-# LANGUAGE ExistentialQuantification #-}
module T8714 where

data HLState = forall a. HLState (a -> a) !a

data BufferImpl = FBufferData !HLState

focusAst :: BufferImpl -> HLState
focusAst (FBufferData (HLState f x)) = HLState f (f x)
