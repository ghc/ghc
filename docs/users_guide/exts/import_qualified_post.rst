.. _importqualifiedpost:

Writing qualified in postpositive position
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: ImportQualifiedPost
    :shortdesc: ``ImportQualifiedPost`` allows the syntax ``import M qualified``

    :since: 8.10.1

    ``ImportQualifiedPost`` allows the syntax ``import M qualified``, that is, to annotate a module as qualified by writing ``qualified`` after the module name.

To import a qualified module usually you must specify ``qualified`` in prepositive position : ``import qualified M``. This often leads to a "hanging indent" (which is automatically inserted by some autoformatters and common in many code bases. For example:

.. code-block::  none

 import qualified A
 import           B
 import           C

The ``ImportQualifiedPost`` extension allows ``qualified`` to appear in postpositive position : ``import M qualified``. With this extension enabled, one can write:

.. code-block:: none

   import A qualified
   import B
   import C

It is an error if ``qualified`` appears in both pre and postpositive positions.

The warning ``-Wprepositive-qualified-syntax`` (off by default) reports on any occurrences of imports annotated ``qualified`` using prepositive syntax.


