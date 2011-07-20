module Tc173a where

class FormValue value where
   isFormValue :: value -> ()
   isFormValue _ = () 

class FormTextField value

instance FormTextField String

instance FormTextField value => FormTextFieldIO value

class FormTextFieldIO value

instance FormTextFieldIO value => FormValue value

instance FormTextFieldIO value => FormTextFieldIO (Maybe value)
