Common Errors
=============

``parse error on input ‘-- | xxx’``
-----------------------------------

This is probably caused by the ``-- | xxx`` comment not being **before** a
declaration, see :ref:`top-level-declaration`.

``parse error on input ‘-- $ xxx’``
-----------------------------------

You've probably commented out code like::

  f x
    $ xxx
    
``-- $`` is a special syntax for named chunks, see :ref:`named-chunks`. You can fix this by escaping the ``$``::

  -- \$ xxx
