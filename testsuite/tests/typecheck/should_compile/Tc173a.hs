{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}

module Tc173a where

class FormValue value where
   isFormValue :: value -> ()
   isFormValue _ = () 

class FormTextField value

instance FormTextField String

instance {-# OVERLAPPABLE #-} FormTextField value => FormTextFieldIO value

class FormTextFieldIO value

instance FormTextFieldIO value => FormValue value

instance {-# OVERLAPPING #-} FormTextFieldIO value => FormTextFieldIO (Maybe value)
