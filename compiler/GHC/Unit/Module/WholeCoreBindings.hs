module GHC.Unit.Module.WholeCoreBindings where

import GHC.Unit.Types (Module)
import GHC.Unit.Module.Location
import GHC.Iface.Syntax

{-
Note [Interface Files with Core Definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A interface file can optionally contain the definitions of all core bindings, this
is enabled by the flag `-fwrite-if-simplified-core`.
This provides everything needed in addition to the normal ModIface and ModDetails
to restart compilation after typechecking to generate bytecode. The `fi_bindings` field
is stored in the normal interface file and the other fields populated whilst loading
the interface file.

The lifecycle of a WholeCoreBindings typically proceeds as follows:

1. The ModIface which contains mi_extra_decls is loaded from disk. A linkable is
   created (which is headed by the `CoreBindings` constructor). This is an unhydrated set of bindings which
   is currently unsuitable for linking, but at the point it is loaded, the ModIface
   hasn't been hydrated yet (See Note [Hydrating Modules]) either so the CoreBindings constructor allows the delaying of converting
   the WholeCoreBindings into a proper Linkable (if we ever do that). The CoreBindings constructor also
   allows us to convert the WholeCoreBindings into multiple different linkables if we so desired.

2. `initWholeCoreBindings` turns a WholeCoreBindings into a proper BCOs linkable. This step combines together
   all the necessary information from a ModIface, ModDetails and WholeCoreBindings in order to
   create the linkable. The linkable created is a "LazyBCOs" linkable, which
   was introduced just for initWholeCoreBindings, so that the bytecode can be generated lazily.
   Using the `BCOs` constructor directly here leads to the bytecode being forced
   too eagerly.

3. Then when bytecode is needed, the LazyBCOs value is inspected and unpacked and
   the linkable is used as before.

The flag `-fwrite-if-simplified-core` determines whether the extra information is written
to an interface file. The program which is written is the core bindings of the module
after whatever simplification the user requested has been performed. So the simplified core bindings
of the interface file agree with the optimisation level as reported by the interface
file.

Note [Size of Interface Files with Core Definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

How much overhead does `-fwrite-if-simplified-core` add to a typical interface file?
As an experiment I compiled the `Cabal` library and `ghc` library (Aug 22) with

| Project | .hi  | .hi (fat) | .o   |
| --------| ---- | --------- | --   |
| ghc     | 32M  | 68M       | 127M |
| Cabal   | 3.2M | 9.8M      | 14M  |

So the interface files gained in size but the end result was still smaller than
the object files.

-}

data WholeCoreBindings = WholeCoreBindings
            { wcb_bindings :: [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo] -- ^ serialised tidied core bindings.
            , wcb_module   :: Module  -- ^ The module which the bindings are for
            , wcb_mod_location :: ModLocation -- ^ The location where the sources reside.
            }
