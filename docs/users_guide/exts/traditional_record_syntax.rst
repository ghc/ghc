.. _traditional-record-syntax:

Traditional record syntax
-------------------------

.. extension:: TraditionalRecordSyntax
    :shortdesc: Allow traditional record syntax (e.g. ``C {f = x}``).

    :since: 7.4.1
    :status: Enabled by default.

    Allow traditional record syntax (e.g. ``C {f = x}``).

Traditional record syntax, such as ``C {f = x}``, is enabled by default.
To disable it, you can use the :extension:`NoTraditionalRecordSyntax` extension.

Under :extension:`NoTraditionalRecordSyntax`, it is not permitted to define a
record datatype or use record syntax in an expression.  For example, the
following all require :extension:`TraditionalRecordSyntax`:

.. code-block:: haskell

     data T = MkT { foo :: Int }  -- record datatype definition

     x = MkT { foo = 3 }          -- construction

     y = x { foo = 3 }            -- update

     f (MkT { foo = i }) = i      -- pattern matching

However, if a field selector function is in scope, it may be used normally.
(This arises if a module using :extension:`NoTraditionalRecordSyntax` imports a
module that defined a record with :extension:`TraditionalRecordSyntax` enabled).
If you wish to suppress field selector functions, use the
:extension:`NoFieldSelectors` extension.
