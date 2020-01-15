Software Transactional Memory
-----------------------------

GHC now supports a new way to coordinate the activities of Concurrent
Haskell threads, called Software Transactional Memory (STM). The `STM
papers <https://wiki.haskell.org/Research_papers/Parallelism_and_concurrency#Lock_free_data_structures_and_transactional_memory>`__
are an excellent introduction to what STM is, and how to use it.

The main library you need to use is the `stm
library <http://hackage.haskell.org/package/stm>`__. The main features
supported are these:

-  Atomic blocks.

-  Transactional variables.

-  Operations for composing transactions: ``retry``, and ``orElse``.

-  Data invariants.

All these features are described in the papers mentioned earlier.


