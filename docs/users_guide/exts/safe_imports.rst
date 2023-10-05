.. _safe-imports-ext:

Safe imports
~~~~~~~~~~~~

With the :extension:`Safe`, :extension:`Trustworthy` and :extension:`Unsafe`
language flags, GHC extends the import declaration syntax to take an optional
``safe`` keyword after the ``import`` keyword. This feature is part of the Safe
Haskell GHC extension. For example: ::

    import safe qualified Network.Socket as NS

would import the module ``Network.Socket`` with compilation only
succeeding if ``Network.Socket`` can be safely imported. For a description of
when a import is considered safe see :ref:`safe-haskell`.


