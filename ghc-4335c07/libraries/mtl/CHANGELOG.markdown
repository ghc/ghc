2.2.2
-----
* `Control.Monad.Identity` now re-exports `Control.Monad.Trans.Identity`
* Fix a bug in which `Control.Monad.State.Class.modify'` was not as strict in
  the new state as its counterparts in `transformers`

2.2.1
-------
* Provide MINIMAL pragmas for `MonadState`, `MonadWriter`, `MonadReader`
* Added a cyclic definition of `ask` in terms of `reader` for consistency with `get`/`put` vs. `state` and `tell` vs. `writer`
* Fix deprecation warnings caused by `transformers` 0.4 deprecating `ErrorT`.
* Added `Control.Monad.Except` in the style of the other `mtl` re-export modules

2.2.0.1
-------
* Fixed a bug caused by the change in how `transformers` 0.4 exports its data types. We will now export `runFooT` for each transformer again!

2.2
---
* `transformers` 0.4 support
* Added instances for `ExceptT`
* Added `modify'` to `Control.Monad.State.*`

2.1.3.1
-------
* Avoid importing `Control.Monad.Instances` on GHC 7.8 to build without deprecation warnings.

2.1.3
-----
* Removed the now-irrelevant `Error` constraint from the `MonadError` instance for `Either e`.
