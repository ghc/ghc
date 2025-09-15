{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T15365 where

$([d| type (|||) = Either

      (&&&) :: Bool -> Bool -> Bool
      (&&&) = (&&)

      type role (***)
      data (***)

      class (???)
      instance (???)

      data family ($$$)
      data instance ($$$)

      type family (^^^)
      type instance (^^^) = Int

      type family (###) where
        (###) = Int

      pattern (:!!!) :: Bool
      pattern (:!!!) = True
    |])
