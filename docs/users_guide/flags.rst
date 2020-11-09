.. _flag-reference:

Flag reference
==============

This section is a quick-reference for GHC's command-line flags. For each
flag, we also list its mode/dynamic status (see
:ref:`mode-dynamic-flags`), and the flag's opposite (if available).

Verbosity options
-----------------

More details in :ref:`options-help`

.. tabularcolumns::
    | p{\dimexpr 0.33\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.26\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: verbosity

Alternative modes of operation
------------------------------

More details in :ref:`modes`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: modes

Which phases to run
-------------------

More details in :ref:`options-order`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: phases

Redirecting output
------------------

More details in :ref:`options-output`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: redirect-output

Keeping intermediate files
--------------------------

More details in :ref:`keeping-intermediates`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: keep-intermediates

Temporary files
---------------

More details in :ref:`temp-files`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: temp-files

Finding imports
---------------

More details in :ref:`search-path`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: search-path

Interface file options
----------------------

More details in :ref:`hi-options`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: interface-files

Extended interface file options
-------------------------------

More details in :ref:`hie-options`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: extended-interface-files

Recompilation checking
----------------------

More details in :ref:`recomp`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: recompilation

.. _interactive-mode-options:

Interactive-mode options
------------------------

More details in :ref:`ghci-dot-files`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: interactive

Packages
--------

More details in :ref:`packages`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: packages


Language options
----------------

Language options can be enabled either by a command-line option
``-Xblah``, or by a ``{-# LANGUAGE blah #-}`` pragma in the file itself.
See :ref:`options-language`.


Warnings
--------

More details in :ref:`options-sanity`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: warnings

Optimisation levels
-------------------

These options are described in more detail in :ref:`options-optimise`.

See :ref:`options-f-compact` for a list of optimisations enabled on
level 1 and level 2.

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: optimization-levels

.. _options-f-compact:

Individual optimisations
------------------------

These options are described in more detail in :ref:`options-f`. If a
flag is implied by ``-O`` then it is also implied by ``-O2`` (unless
flag description explicitly says otherwise). If a flag is implied by
``-O0`` only then the flag is not implied by ``-O`` and ``-O2``.

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: optimization

Profiling options
-----------------

More details in :ref:`profiling`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: profiling

Program coverage options
------------------------

More details in :ref:`hpc`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: coverage

C pre-processor options
-----------------------

More details in :ref:`c-pre-processor`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: cpp

Code generation options
-----------------------

More details in :ref:`options-codegen`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: codegen

Linking options
---------------

More details in :ref:`options-linker`

.. tabularcolumns::
    | p{\dimexpr 0.35\textwidth-2\tabcolsep} |
      p{\dimexpr 0.44\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.10\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: linking

Plugin options
--------------

More details in :ref:`compiler-plugins`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: plugins

Replacing phases
----------------

More details in :ref:`replacing-phases`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: phase-programs

Forcing options to particular phases
------------------------------------

More details in :ref:`forcing-options-through`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: phase-options

Platform-specific options
-------------------------

More details in :ref:`options-platform`

.. tabularcolumns::
    | p{\dimexpr 0.30\textwidth-2\tabcolsep} |
      p{\dimexpr 0.31\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.29\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: platform-options

Compiler debugging options
--------------------------

More details in :ref:`options-debugging`

.. tabularcolumns::
    | p{\dimexpr 0.35\textwidth-2\tabcolsep} |
      p{\dimexpr 0.44\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.10\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: debugging

Miscellaneous compiler options
------------------------------

.. tabularcolumns::
    | p{\dimexpr 0.35\textwidth-2\tabcolsep} |
      p{\dimexpr 0.44\textwidth-2\tabcolsep} |
      p{\dimexpr 0.11\textwidth-2\tabcolsep} |
      p{\dimexpr 0.10\textwidth-2\tabcolsep} |

.. flag-print::
    :type: table
    :category: misc
