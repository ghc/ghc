.. _extending-ghc:

Extending and using GHC as a Library
====================================

GHC exposes its internal APIs to users through the built-in ghc package.
It allows you to write programs that leverage GHC's entire compilation
driver, in order to analyze or compile Haskell code programmatically.
Furthermore, GHC gives users the ability to load compiler plugins during
compilation - modules which are allowed to view and change GHC's
internal intermediate representation, Core. Plugins are suitable for
things like experimental optimizations or analysis, and offer a lower
barrier of entry to compiler development for many common cases.

Furthermore, GHC offers a lightweight annotation mechanism that you can
use to annotate your source code with metadata, which you can later
inspect with either the compiler API or a compiler plugin.

.. _annotation-pragmas:

Source annotations
------------------

Annotations are small pragmas that allow you to attach data to
identifiers in source code, which are persisted when compiled. These
pieces of data can then inspected and utilized when using GHC as a
library or writing a compiler plugin.

.. _ann-pragma:

Annotating values
~~~~~~~~~~~~~~~~~

.. index::
   single: ANN pragma
   single: pragma; ANN
   single: source annotations

Any expression that has both ``Typeable`` and ``Data`` instances may be
attached to a top-level value binding using an ``ANN`` pragma. In
particular, this means you can use ``ANN`` to annotate data constructors
(e.g. ``Just``) as well as normal values (e.g. ``take``). By way of
example, to annotate the function ``foo`` with the annotation
``Just "Hello"`` you would do this:

::

    {-# ANN foo (Just "Hello") #-}
    foo = ...

A number of restrictions apply to use of annotations:

-  The binder being annotated must be at the top level (i.e. no nested
   binders)

-  The binder being annotated must be declared in the current module

-  The expression you are annotating with must have a type with
   ``Typeable`` and ``Data`` instances

-  The :ref:`Template Haskell staging restrictions <th-usage>` apply to the
   expression being annotated with, so for example you cannot run a
   function from the module being compiled.

   To be precise, the annotation ``{-# ANN x e #-}`` is well staged if
   and only if ``$(e)`` would be (disregarding the usual type
   restrictions of the splice syntax, and the usual restriction on
   splicing inside a splice - ``$([|1|])`` is fine as an annotation,
   albeit redundant).

If you feel strongly that any of these restrictions are too onerous,
:ghc-wiki:`please give the GHC team a shout <mailing-lists-and-irc>`.

However, apart from these restrictions, many things are allowed,
including expressions which are not fully evaluated! Annotation
expressions will be evaluated by the compiler just like Template Haskell
splices are. So, this annotation is fine:

::

    {-# ANN f SillyAnnotation { foo = (id 10) + $([| 20 |]), bar = 'f } #-}
    f = ...

.. _typeann-pragma:

Annotating types
~~~~~~~~~~~~~~~~

.. index::
   single: ANN pragma; on types

You can annotate types with the ``ANN`` pragma by using the ``type``
keyword. For example:

::

    {-# ANN type Foo (Just "A `Maybe String' annotation") #-}
    data Foo = ...

.. _modann-pragma:

Annotating modules
~~~~~~~~~~~~~~~~~~

.. index::
   single: ANN pragma; on modules

You can annotate modules with the ``ANN`` pragma by using the ``module``
keyword. For example:

::

    {-# ANN module (Just "A `Maybe String' annotation") #-}

.. _ghc-as-a-library:

Using GHC as a Library
----------------------

The ``ghc`` package exposes most of GHC's frontend to users, and thus
allows you to write programs that leverage it. This library is actually
the same library used by GHC's internal, frontend compilation driver,
and thus allows you to write tools that programmatically compile source
code and inspect it. Such functionality is useful in order to write
things like IDE or refactoring tools. As a simple example, here's a
program which compiles a module, much like ghc itself does by default
when invoked:

::

    import GHC
    import GHC.Paths ( libdir )
    import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )

    main =
        defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
          runGhc (Just libdir) $ do
            dflags <- getSessionDynFlags
            setSessionDynFlags dflags
            target <- guessTarget "test_main.hs" Nothing
            setTargets [target]
            load LoadAllTargets

The argument to ``runGhc`` is a bit tricky. GHC needs this to find its
libraries, so the argument must refer to the directory that is printed
by ``ghc --print-libdir`` for the same version of GHC that the program
is being compiled with. Above we therefore use the ``ghc-paths`` package
which provides this for us.

Compiling it results in:

.. code-block:: none

    $ cat test_main.hs
    main = putStrLn "hi"
    $ ghc -package ghc simple_ghc_api.hs
    [1 of 1] Compiling Main             ( simple_ghc_api.hs, simple_ghc_api.o )
    Linking simple_ghc_api ...
    $ ./simple_ghc_api
    $ ./test_main
    hi
    $

For more information on using the API, as well as more samples and
references, please see `this Haskell.org wiki
page <http://haskell.org/haskellwiki/GHC/As_a_library>`__.

.. _compiler-plugins:

Compiler Plugins
----------------

GHC has the ability to load compiler plugins at compile time. The
feature is similar to the one provided by
`GCC <http://gcc.gnu.org/wiki/plugins>`__, and allows users to write
plugins that can adjust the behaviour of the constraint solver, inspect
and modify the compilation pipeline, as well as transform and inspect
GHC's intermediate language, Core. Plugins are suitable for experimental
analysis or optimization, and require no changes to GHC's source code to
use.

Plugins cannot optimize/inspect C-\\-, nor can they implement things like
parser/front-end modifications like GCC, apart from limited changes to
the constraint solver. If you feel strongly that any of these
restrictions are too onerous,
:ghc-wiki:`please give the GHC team a shout <mailing-lists-and-irc>`.

Plugins do not work with ``-fexternal-interpreter``. If you need to run plugins
with ``-fexternal-interpreter`` let GHC developers know in :ghc-ticket:`14335`.

.. _using-compiler-plugins:

Using compiler plugins
~~~~~~~~~~~~~~~~~~~~~~

Plugins can be added on the command line with the :ghc-flag:`-fplugin=⟨module⟩`
option where ⟨module⟩ is a module in a registered package that exports the
plugin. Arguments can be passed to the plugins with the
:ghc-flag:`-fplugin-opt=⟨module⟩:⟨args⟩` option. The list of enabled plugins can
be reset with the :ghc-flag:`-fclear-plugins` option.

.. ghc-flag:: -fplugin=⟨module⟩
    :shortdesc: Load a plugin exported by a given module
    :type: dynamic
    :category: plugins

    Load the plugin in the given module. The module must be a member of a
    package registered in GHC's package database.

.. ghc-flag:: -fplugin-opt=⟨module⟩:⟨args⟩
    :shortdesc: Give arguments to a plugin module; module must be specified with
        :ghc-flag:`-fplugin=⟨module⟩`
    :type: dynamic
    :category: plugins

    Give arguments to a plugin module; module must be specified with
    :ghc-flag:`-fplugin=⟨module⟩`.

.. ghc-flag:: -fplugin-trustworthy
    :shortdesc: Trust the used plugins and no longer mark the compiled module
        as unsafe
    :type: dynamic
    :category: plugins

    By default, when a module is compiled with plugins, it will be marked as
    unsafe. With this flag passed, all plugins are treated as trustworthy
    and the safety inference will no longer be affected.

.. ghc-flag:: -fclear-plugins
    :shortdesc: Clear the list of active plugins
    :type: dynamic
    :category: plugins

    Clear the list of plugins previously specified with
    :ghc-flag:`-fplugin <-fplugin=⟨module⟩>`. This is useful in GHCi where
    simply removing the :ghc-flag:`-fplugin <-fplugin=⟨module⟩>` options from
    the command line is not possible. Instead ``:set -fclear-plugins`` can be
    used.


As an example, in order to load the plugin exported by ``Foo.Plugin`` in
the package ``foo-ghc-plugin``, and give it the parameter "baz", we
would invoke GHC like this:

.. code-block:: none

    $ ghc -fplugin Foo.Plugin -fplugin-opt Foo.Plugin:baz Test.hs
    [1 of 1] Compiling Main             ( Test.hs, Test.o )
    Loading package ghc-prim ... linking ... done.
    Loading package integer-gmp ... linking ... done.
    Loading package base ... linking ... done.
    Loading package ffi-1.0 ... linking ... done.
    Loading package foo-ghc-plugin-0.1 ... linking ... done.
    ...
    Linking Test ...
    $

Alternatively, core plugins can be specified with Template Haskell.

::

   addCorePlugin "Foo.Plugin"

This inserts the plugin as a core-to-core pass. Unlike `-fplugin=(module)`,
the plugin module can't reside in the same package as the module calling
:th-ref:`Language.Haskell.TH.Syntax.addCorePlugin`. This way, the
implementation can expect the plugin to be built by the time
it is needed.

Plugin modules live in a separate namespace from
the user import namespace.  By default, these two namespaces are
the same; however, there are a few command line options which
control specifically plugin packages:

.. ghc-flag:: -plugin-package ⟨pkg⟩
    :shortdesc: Expose ⟨pkg⟩ for plugins
    :type: dynamic
    :category: plugins

    This option causes the installed package ⟨pkg⟩ to be exposed for plugins,
    such as :ghc-flag:`-fplugin=⟨module⟩`. The package ⟨pkg⟩ can be specified
    in full with its version number (e.g.  ``network-1.0``) or the version
    number can be omitted if there is only one version of the package
    installed. If there are multiple versions of ⟨pkg⟩ installed and
    :ghc-flag:`-hide-all-plugin-packages` was not specified, then all other
    versions will become hidden.  :ghc-flag:`-plugin-package ⟨pkg⟩` supports
    thinning and renaming described in :ref:`package-thinning-and-renaming`.

    Unlike :ghc-flag:`-package ⟨pkg⟩`, this option does NOT cause package ⟨pkg⟩
    to be linked into the resulting executable or shared object.

.. ghc-flag:: -plugin-package-id ⟨pkg-id⟩
    :shortdesc: Expose ⟨pkg-id⟩ for plugins
    :type: dynamic
    :category: plugins

    Exposes a package in the plugin namespace like :ghc-flag:`-plugin-package
    ⟨pkg⟩`, but the package is named by its installed package ID rather than by
    name.  This is a more robust way to name packages, and can be used to
    select packages that would otherwise be shadowed. Cabal passes
    :ghc-flag:`-plugin-package-id ⟨pkg-id⟩` flags to GHC.
    :ghc-flag:`-plugin-package-id ⟨pkg-id⟩` supports thinning and renaming
    described in :ref:`package-thinning-and-renaming`.

.. ghc-flag:: -hide-all-plugin-packages
    :shortdesc: Hide all packages for plugins by default
    :type: dynamic
    :category: plugins

    By default, all exposed packages in the normal, source import namespace are
    also available for plugins.  This causes those packages to be hidden by
    default.  If you use this flag, then any packages with plugins you require
    need to be explicitly exposed using :ghc-flag:`-plugin-package ⟨pkg⟩`
    options.

At the moment, the only way to specify a dependency on a plugin
in Cabal is to put it in ``build-depends`` (which uses the conventional
:ghc-flag:`-package-id ⟨unit-id⟩` flag); however, in the future there
will be a separate field for specifying plugin dependencies specifically.

.. _writing-compiler-plugins:

Writing compiler plugins
~~~~~~~~~~~~~~~~~~~~~~~~

Plugins are modules that export at least a single identifier,
``plugin``, of type ``GHC.Plugins.Plugin``. All plugins should
``import GHC.Plugins`` as it defines the interface to the compilation
pipeline.

A ``Plugin`` effectively holds a function which installs a compilation
pass into the compiler pipeline. By default there is the empty plugin
which does nothing, ``GHC.Plugins.defaultPlugin``, which you should
override with record syntax to specify your installation function. Since
the exact fields of the ``Plugin`` type are open to change, this is the
best way to ensure your plugins will continue to work in the future with
minimal interface impact.

``Plugin`` exports a field, ``installCoreToDos`` which is a function of
type ``[CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]``. A
``CommandLineOption`` is effectively just ``String``, and a ``CoreToDo``
is basically a function of type ``Core -> Core``. A ``CoreToDo`` gives
your pass a name and runs it over every compiled module when you invoke
GHC.

As a quick example, here is a simple plugin that just does nothing and
just returns the original compilation pipeline, unmodified, and says
'Hello':

::

    module DoNothing.Plugin (plugin) where
    import GHC.Plugins

    plugin :: Plugin
    plugin = defaultPlugin {
      installCoreToDos = install
      }

    install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    install _ todo = do
      putMsgS "Hello!"
      return todo

Provided you compiled this plugin and registered it in a package (with
cabal for instance,) you can then use it by just specifying
``-fplugin=DoNothing.Plugin`` on the command line, and during the
compilation you should see GHC say 'Hello'.

Running multiple plugins is also supported, by passing
multiple ``-fplugin=...`` options. GHC will load the plugins
in the order in which they are specified on the command line
and, when appropriate, compose their effects in the same
order. That is, if we had two Core plugins, ``Plugin1`` and
``Plugin2``, each defining an ``install`` function like
the one above, then GHC would first run ``Plugin1.install``
on the default ``[CoreToDo]``, take the result and feed it to
``Plugin2.install``. ``-fplugin=Plugin1 -fplugin=Plugin2``
will update the Core pipeline by applying
``Plugin1.install opts1 >=> Plugin2.install opts2`` (where
``opts1`` and ``opts2`` are the options passed to each plugin
using ``-fplugin-opt=...``). This is not specific to Core
plugins but holds for all the types of plugins that can be
composed or sequenced in some way: the first plugin to appear
on the GHC command line will always act first.

.. _core-plugins-in-more-detail:

Core plugins in more detail
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``CoreToDo`` is effectively a data type that describes all the kinds of
optimization passes GHC does on Core. There are passes for
simplification, CSE, etc. There is a specific case for
plugins, ``CoreDoPluginPass :: String -> PluginPass -> CoreToDo`` which
should be what you always use when inserting your own pass into the
pipeline. The first parameter is the name of the plugin, and the second
is the pass you wish to insert.

``CoreM`` is a monad that all of the Core optimizations live and operate
inside of.

A plugin's installation function (``install`` in the above example)
takes a list of ``CoreToDo``\ s and returns a list of ``CoreToDo``.
Before GHC begins compiling modules, it enumerates all the needed
plugins you tell it to load, and runs all of their installation
functions, initially on a list of passes that GHC specifies itself.
After doing this for every plugin, the final list of passes is given to
the optimizer, and are run by simply going over the list in order.

You should be careful with your installation function, because the list
of passes you give back isn't questioned or double checked by GHC at the
time of this writing. An installation function like the following:

::

    install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    install _ _ = return []

is certainly valid, but also certainly not what anyone really wants.

.. _manipulating-bindings:

Manipulating bindings
^^^^^^^^^^^^^^^^^^^^^

In the last section we saw that besides a name, a ``CoreDoPluginPass``
takes a pass of type ``PluginPass``. A ``PluginPass`` is a synonym for
``(ModGuts -> CoreM ModGuts)``. ``ModGuts`` is a type that represents
the one module being compiled by GHC at any given time.

A ``ModGuts`` holds all of the module's top level bindings which we can
examine. These bindings are of type ``CoreBind`` and effectively
represent the binding of a name to body of code. Top-level module
bindings are part of a ``ModGuts`` in the field ``mg_binds``.
Implementing a pass that manipulates the top level bindings merely needs
to iterate over this field, and return a new ``ModGuts`` with an updated
``mg_binds`` field. Because this is such a common case, there is a
function provided named ``bindsOnlyPass`` which lifts a function of type
``([CoreBind] -> CoreM [CoreBind])`` to type
``(ModGuts -> CoreM ModGuts)``.

Continuing with our example from the last section, we can write a simple
plugin that just prints out the name of all the non-recursive bindings
in a module it compiles:

::

    module SayNames.Plugin (plugin) where
    import GHC.Plugins

    plugin :: Plugin
    plugin = defaultPlugin {
      installCoreToDos = install
      }

    install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    install _ todo = do
      return (CoreDoPluginPass "Say name" pass : todo)

    pass :: ModGuts -> CoreM ModGuts
    pass guts = do dflags <- getDynFlags
                   bindsOnlyPass (mapM (printBind dflags)) guts
      where printBind :: DynFlags -> CoreBind -> CoreM CoreBind
            printBind dflags bndr@(NonRec b _) = do
              putMsgS $ "Non-recursive binding named " ++ showSDoc dflags (ppr b)
              return bndr
            printBind _ bndr = return bndr

.. _getting-annotations:

Using Annotations
^^^^^^^^^^^^^^^^^

Previously we discussed annotation pragmas (:ref:`annotation-pragmas`),
which we mentioned could be used to give compiler plugins extra guidance
or information. Annotations for a module can be retrieved by a plugin,
but you must go through the modules ``ModGuts`` in order to get it.
Because annotations can be arbitrary instances of ``Data`` and
``Typeable``, you need to give a type annotation specifying the proper
type of data to retrieve from the interface file, and you need to make
sure the annotation type used by your users is the same one your plugin
uses. For this reason, we advise distributing annotations as part of the
package which also provides compiler plugins if possible.

To get the annotations of a single binder, you can use
``getAnnotations`` and specify the proper type. Here's an example that
will print out the name of any top-level non-recursive binding with the
``SomeAnn`` annotation:

::

    {-# LANGUAGE DeriveDataTypeable #-}
    module SayAnnNames.Plugin (plugin, SomeAnn(..)) where
    import GHC.Plugins
    import Control.Monad (unless)
    import Data.Data

    data SomeAnn = SomeAnn deriving Data

    plugin :: Plugin
    plugin = defaultPlugin {
      installCoreToDos = install
      }

    install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
    install _ todo = do
      return (CoreDoPluginPass "Say name" pass : todo)

    pass :: ModGuts -> CoreM ModGuts
    pass g = do
              dflags <- getDynFlags
              mapM_ (printAnn dflags g) (mg_binds g) >> return g
      where printAnn :: DynFlags -> ModGuts -> CoreBind -> CoreM CoreBind
            printAnn dflags guts bndr@(NonRec b _) = do
              anns <- annotationsOn guts b :: CoreM [SomeAnn]
              unless (null anns) $ putMsgS $ "Annotated binding found: " ++  showSDoc dflags (ppr b)
              return bndr
            printAnn _ _ bndr = return bndr

    annotationsOn :: Data a => ModGuts -> CoreBndr -> CoreM [a]
    annotationsOn guts bndr = do
      anns <- getAnnotations deserializeWithData guts
      return $ lookupWithDefaultUFM anns [] (varUnique bndr)

Please see the GHC API documentation for more about how to use internal
APIs, etc.

.. _typechecker-plugins:

Typechecker plugins
~~~~~~~~~~~~~~~~~~~

In addition to Core plugins, GHC has experimental support for
typechecker plugins, which allow the behaviour of the constraint solver
to be modified. For example, they make it possible to interface the
compiler to an SMT solver, in order to support a richer theory of
type-level arithmetic expressions than the theory built into GHC (see
:ref:`typelit-tyfuns`).

The ``Plugin`` type has a field ``tcPlugin`` of type
``[CommandLineOption] -> Maybe TcPlugin``, where the ``TcPlugin`` type
is defined thus:

::

    data TcPlugin = forall s . TcPlugin
      { tcPluginInit  :: TcPluginM s
      , tcPluginSolve :: s -> TcPluginSolver
      , tcPluginStop  :: s -> TcPluginM ()
      }

    type TcPluginSolver = [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult

    data TcPluginResult = TcPluginContradiction [Ct] | TcPluginOk [(EvTerm,Ct)] [Ct]

(The details of this representation are subject to change as we gain
more experience writing typechecker plugins. It should not be assumed to
be stable between GHC releases.)

The basic idea is as follows:

-  When type checking a module, GHC calls ``tcPluginInit`` once before
   constraint solving starts. This allows the plugin to look things up
   in the context, initialise mutable state or open a connection to an
   external process (e.g. an external SMT solver). The plugin can return
   a result of any type it likes, and the result will be passed to the
   other two fields.

-  During constraint solving, GHC repeatedly calls ``tcPluginSolve``.
   This function is provided with the current set of constraints, and
   should return a ``TcPluginResult`` that indicates whether a
   contradiction was found or progress was made. If the plugin solver
   makes progress, GHC will re-start the constraint solving pipeline,
   looping until a fixed point is reached.

-  Finally, GHC calls ``tcPluginStop`` after constraint solving is
   finished, allowing the plugin to dispose of any resources it has
   allocated (e.g. terminating the SMT solver process).

Plugin code runs in the ``TcPluginM`` monad, which provides a restricted
interface to GHC API functionality that is relevant for typechecker
plugins, including ``IO`` and reading the environment. If you need
functionality that is not exposed in the ``TcPluginM`` module, you can
use ``unsafeTcPluginTcM :: TcM a -> TcPluginM a``, but are encouraged to
contact the GHC team to suggest additions to the interface. Note that
``TcPluginM`` can perform arbitrary IO via
``tcPluginIO :: IO a -> TcPluginM a``, although some care must be taken
with side effects (particularly in ``tcPluginSolve``). In general, it is
up to the plugin author to make sure that any IO they do is safe.

.. _constraint-solving-with-plugins:

Constraint solving with plugins
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The key component of a typechecker plugin is a function of type
``TcPluginSolver``, like this:

::

    solve :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
    solve givens deriveds wanteds = ...

This function will be invoked at two points in the constraint solving
process: after simplification of given constraints, and after
unflattening of wanted constraints. The two phases can be distinguished
because the deriveds and wanteds will be empty in the first case. In
each case, the plugin should either

-  return ``TcPluginContradiction`` with a list of impossible
   constraints (which must be a subset of those passed in), so they can
   be turned into errors; or

-  return ``TcPluginOk`` with lists of solved and new constraints (the
   former must be a subset of those passed in and must be supplied with
   corresponding evidence terms).

If the plugin cannot make any progress, it should return
``TcPluginOk [] []``. Otherwise, if there were any new constraints, the
main constraint solver will be re-invoked to simplify them, then the
plugin will be invoked again. The plugin is responsible for making sure
that this process eventually terminates.

Plugins are provided with all available constraints (including
equalities and typeclass constraints), but it is easy for them to
discard those that are not relevant to their domain, because they need
return only those constraints for which they have made progress (either
by solving or contradicting them).

Constraints that have been solved by the plugin must be provided with
evidence in the form of an ``EvTerm`` of the type of the constraint.
This evidence is ignored for given and derived constraints, which GHC
"solves" simply by discarding them; typically this is used when they are
uninformative (e.g. reflexive equations). For wanted constraints, the
evidence will form part of the Core term that is generated after
typechecking, and can be checked by ``-dcore-lint``. It is possible for
the plugin to create equality axioms for use in evidence terms, but GHC
does not check their consistency, and inconsistent axiom sets may lead
to segfaults or other runtime misbehaviour.

.. _source-plugins:

Source plugins
~~~~~~~~~~~~~~

In addition to core and type checker plugins, you can install plugins that can
access different representations of the source code. The main purpose of these
plugins is to make it easier to implement development tools.

There are several different access points that you can use for defining plugins
that access the representations. All these fields receive the list of
``CommandLineOption`` strings that are passed to the compiler using the
:ghc-flag:`-fplugin-opt=⟨module⟩:⟨args⟩` flags.

::

    plugin :: Plugin
    plugin = defaultPlugin {
        parsedResultAction = parsed
      , typeCheckResultAction = typechecked
      , spliceRunAction = spliceRun
      , interfaceLoadAction = interfaceLoad
      , renamedResultAction = renamed
      }

Parsed representation
^^^^^^^^^^^^^^^^^^^^^

When you want to define a plugin that uses the syntax tree of the source code,
you would like to override the ``parsedResultAction`` field. This access point
enables you to get access to information about the lexical tokens and comments
in the source code as well as the original syntax tree of the compiled module.

::

    parsed :: [CommandLineOption] -> ModSummary -> HsParsedModule
                -> Hsc HsParsedModule

The ``ModSummary`` contains useful
meta-information about the compiled module. The ``HsParsedModule`` contains the
lexical and syntactical information we mentioned before. The result that you
return will change the result of the parsing. If you don't want to change the
result, just return the ``HsParsedModule`` that you received as the argument.

Type checked representation
^^^^^^^^^^^^^^^^^^^^^^^^^^^

When you want to define a plugin that needs semantic information about the
source code, use the ``typeCheckResultAction`` field. For example, if your
plugin have to decide if two names are referencing the same definition or it has
to check the type of a function it is using semantic information. In this case
you need to access the renamed or type checked version of the syntax tree with
``typeCheckResultAction`` or ``renamedResultAction``.

::

    typechecked :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
    renamed :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)

By overriding the ``renamedResultAction`` field we can modify each ``HsGroup``
after it has been renamed. A source file is separated into groups depending on
the location of template haskell splices so the contents of these groups may
not be intuitive. In order to save the entire renamed AST for inspection
at the end of typechecking you can set ``renamedResultAction`` to ``keepRenamedSource``
which is provided by the ``Plugins`` module.
This is important because some parts of the renamed
syntax tree (for example, imports) are not found in the typechecked one.



Evaluated code
^^^^^^^^^^^^^^

When the compiler type checks the source code, :ref:`template-haskell` Splices
and :ref:`th-quasiquotation` will be replaced by the syntax tree fragments
generated from them. However for tools that operate on the source code the
code generator is usually more interesting than the generated code. For this
reason we included ``spliceRunAction``. This field is invoked on each expression
before they are evaluated. The input is type checked, so semantic information is
available for these syntax tree fragments. If you return a different expression
you can change the code that is generated.


::

    spliceRun :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)


However take care that the generated definitions are still in the input of
``typeCheckResultAction``. If your don't take care to filter the typechecked
input, the behavior of your tool might be inconsistent.

Interface files
^^^^^^^^^^^^^^^

Sometimes when you are writing a tool, knowing the source code is not enough,
you also have to know details about the modules that you import. In this case we
suggest using the ``interfaceLoadAction``. This will be called each time when
the code of an already compiled module is loaded. It will be invoked for modules
from installed packages and even modules that are installed with GHC. It will
NOT be invoked with your own modules.

::

    interfaceLoad :: forall lcl . [CommandLineOption] -> ModIface
                                    -> IfM lcl ModIface

In the ``ModIface`` datatype you can find lots of useful information, including
the exported definitions and type class instances.

The ``ModIface`` datatype also contains facilities for extending it with extra
data, stored in a ``Map`` of serialised fields, indexed by field names and using
GHC's internal ``Binary`` class. The interface to work with these fields is:

::

    readIfaceField :: Binary a => FieldName -> ModIface -> IO (Maybe a)
    writeIfaceField :: Binary a => FieldName -> a -> ModIface -> IO ModIface
    deleteIfaceField :: FieldName -> ModIface -> ModIface

These fields are currently serialized at the end of the `.hi` file, with their
own sub-header within the file. A pointer to the sub-header exists after the
way descriptor.

Source plugin example
^^^^^^^^^^^^^^^^^^^^^

In this example, we inspect all available details of the compiled source code.
We don't change any of the representation, but write out the details to the
standard output. The pretty printed representation of the parsed, renamed and
type checked syntax tree will be in the output as well as the evaluated splices
and quasi quotes. The name of the interfaces that are loaded will also be
displayed.

::

    module SourcePlugin where

    import Control.Monad.IO.Class
    import GHC.Driver.Session (getDynFlags)
    import GHC.Driver.Plugins
    import GHC.Driver.Types
    import TcRnTypes
    import GHC.Hs.Extension
    import GHC.Hs.Decls
    import GHC.Hs.Expr
    import GHC.Hs.ImpExp
    import Avail
    import Outputable
    import GHC.Hs.Doc

    plugin :: Plugin
    plugin = defaultPlugin
      { parsedResultAction = parsedPlugin
      , renamedResultAction = renamedAction
      , typeCheckResultAction = typecheckPlugin
      , spliceRunAction = metaPlugin
      , interfaceLoadAction = interfaceLoadPlugin
      }

    parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
    parsedPlugin _ _ pm
      = do dflags <- getDynFlags
           liftIO $ putStrLn $ "parsePlugin: \n" ++ (showSDoc dflags $ ppr $ hpm_module pm)
           return pm

    renamedAction :: [CommandLineOption] -> TcGblEnv -> HsGroup GhcRn -> TcM (TcGblEnv, HsGroup GhcRn)
    renamedAction _ tc gr = do
      dflags <- getDynFlags
      liftIO $ putStrLn $ "typeCheckPlugin (rn): " ++ (showSDoc dflags $ ppr gr)
      return (tc, gr)

    typecheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
    typecheckPlugin _ _ tc
      = do dflags <- getDynFlags
           liftIO $ putStrLn $ "typeCheckPlugin (rn): \n" ++ (showSDoc dflags $ ppr $ tcg_rn_decls tc)
           liftIO $ putStrLn $ "typeCheckPlugin (tc): \n" ++ (showSDoc dflags $ ppr $ tcg_binds tc)
           return tc

    metaPlugin :: [CommandLineOption] -> LHsExpr GhcTc -> TcM (LHsExpr GhcTc)
    metaPlugin _ meta
      = do dflags <- getDynFlags
           liftIO $ putStrLn $ "meta: " ++ (showSDoc dflags $ ppr meta)
           return meta

    interfaceLoadPlugin :: [CommandLineOption] -> ModIface -> IfM lcl ModIface
    interfaceLoadPlugin _ iface
      = do dflags <- getDynFlags
           liftIO $ putStrLn $ "interface loaded: " ++ (showSDoc dflags $ ppr $ mi_module iface)
           return iface

When you compile a simple module that contains Template Haskell splice

::

    {-# OPTIONS_GHC -fplugin SourcePlugin #-}
    {-# LANGUAGE TemplateHaskell #-}
    module A where

    a = ()

$(return [])

with the compiler flags ``-fplugin SourcePlugin`` it will give the following
output:

.. code-block:: none

    parsePlugin:
    module A where
    a = ()
    $(return [])
    interface loaded: Prelude
    interface loaded: GHC.Float
    interface loaded: GHC.Base
    interface loaded: Language.Haskell.TH.Lib.Internal
    interface loaded: Language.Haskell.TH.Syntax
    interface loaded: GHC.Types
    meta: return []
    interface loaded: GHC.Integer.Type
    typeCheckPlugin (rn):
    Just a = ()
    typeCheckPlugin (tc):
    {$trModule = Module (TrNameS "main"#) (TrNameS "A"#), a = ()}

.. _hole-fit-plugins:

Hole fit plugins
~~~~~~~~~~~~~~~~

Hole-fit plugins are plugins that are called when a typed-hole error message is
being generated, and allows you to access information about the typed-hole at
compile time, and allows you to customize valid hole fit suggestions.

Using hole-fit plugins, you can extend the behavior of valid hole fit
suggestions to use e.g. Hoogle or other external tools to find and/or synthesize
valid hole fits, with the same information about the typed-hole that GHC uses.

There are two access points are bundled together for defining hole fit plugins,
namely a candidate plugin and a fit plugin, for modifying the candidates to be
checked and fits respectively.


::

    type CandPlugin = TypedHole -> [HoleFitCandidate] -> TcM [HoleFitCandidate]

    type FitPlugin =  TypedHole -> [HoleFit] -> TcM [HoleFit]

    data HoleFitPlugin = HoleFitPlugin
      { candPlugin :: CandPlugin
         -- ^ A plugin for modifying hole fit candidates before they're checked
      , fitPlugin :: FitPlugin
         -- ^ A plugin for modifying valid hole fits after they've been found.
      }

Where ``TypedHole`` contains all the information about the hole available to GHC
at error generation.

::

    data TypedHole = TyH { tyHRelevantCts :: Cts
                          -- ^ Any relevant Cts to the hole
                        , tyHImplics :: [Implication]
                          -- ^ The nested implications of the hole with the
                          --   innermost implication first.
                        , tyHCt :: Maybe Ct
                          -- ^ The hole constraint itself, if available.
                        }

``HoleFitPlugins`` are then defined as follows

::

    plugin :: Plugin
    plugin = defaultPlugin {
        holeFitPlugin = (fmap . fmap) fromPureHFPlugin hfPlugin
      }


    hfPlugin :: [CommandLineOption] -> Maybe HoleFitPlugin


Where ``fromPureHFPlugin :: HoleFitPlugin -> HoleFitPluginR`` is a convencience
function provided in the ``TcHoleErrors`` module, for defining plugins that do
not require internal state.


Stateful hole fit plugins
^^^^^^^^^^^^^^^^^^^^^^^^^


``HoleFitPlugins`` are wrapped in a ``HoleFitPluginR``, which provides a
``TcRef`` for the plugin to use to track internal state, and to facilitate
communication between the candidate and fit plugin.

::

    -- | HoleFitPluginR adds a TcRef to hole fit plugins so that plugins can
    -- track internal state. Note the existential quantification, ensuring that
    -- the state cannot be modified from outside the plugin.
    data HoleFitPluginR = forall s. HoleFitPluginR
      { hfPluginInit :: TcM (TcRef s)
        -- ^ Initializes the TcRef to be passed to the plugin
      , hfPluginRun :: TcRef s -> HoleFitPlugin
        -- ^ The function defining the plugin itself
      , hfPluginStop :: TcRef s -> TcM ()
        -- ^ Cleanup of state, guaranteed to be called even on error
      }

The plugin is then defined as by providing a value for the ``holeFitPlugin``
field, a function that takes the ``CommandLineOption`` strings that are passed
to the compiler using the :ghc-flag:`-fplugin-opt=⟨module⟩:⟨args⟩` flags and returns a
``HoleFitPluginR``. This function can be used to pass the ``CommandLineOption``
strings along to the candidate and fit plugins respectively.



Hole fit plugin example
^^^^^^^^^^^^^^^^^^^^^^^

The following plugins allows users to limit the search for valid hole fits to
certain modules, to sort the hole fits by where they originated (in ascending or
descending order), as well as allowing users to put a limit on how much time is
spent on searching for valid hole fits, after which new searches are aborted.

::

    {-# LANGUAGE TypeApplications, RecordWildCards #-}
    module HolePlugin where

    import GHC.Plugins hiding ((<>))

    import TcHoleErrors

    import Data.List (stripPrefix, sortOn)

    import TcRnTypes

    import TcRnMonad

    import Data.Time (UTCTime, NominalDiffTime)
    import qualified Data.Time as Time

    import Text.Read


    data HolePluginState = HPS { timeAlloted :: Maybe NominalDiffTime
                              , elapsedTime :: NominalDiffTime
                              , timeCurStarted :: UTCTime }

    bumpElapsed :: NominalDiffTime -> HolePluginState -> HolePluginState
    bumpElapsed ad (HPS a e t) = HPS a (e + ad) t

    setAlloted :: Maybe NominalDiffTime -> HolePluginState -> HolePluginState
    setAlloted a (HPS _ e t) = HPS a e t

    setCurStarted :: UTCTime -> HolePluginState -> HolePluginState
    setCurStarted nt (HPS a e _) = HPS a e nt

    hpStartState :: HolePluginState
    hpStartState = HPS Nothing zero undefined
      where zero = fromInteger @NominalDiffTime 0

    initPlugin :: [CommandLineOption] -> TcM (TcRef HolePluginState)
    initPlugin [msecs] = newTcRef $ hpStartState { timeAlloted = alloted }
      where
        errMsg = "Invalid amount of milliseconds given to plugin: " <> show msecs
        alloted = case readMaybe @Integer msecs of
          Just millisecs -> Just $ fromInteger @NominalDiffTime millisecs / 1000
          _ -> error errMsg
    initPlugin _ = newTcRef hpStartState

    fromModule :: HoleFitCandidate -> [String]
    fromModule (GreHFCand gre) =
      map (moduleNameString . importSpecModule) $ gre_imp gre
    fromModule _ = []

    toHoleFitCommand :: TypedHole -> String -> Maybe String
    toHoleFitCommand TyH{tyHCt = Just (CHoleCan _ h)} str
        = stripPrefix ("_" <> str) $ occNameString $ holeOcc h
    toHoleFitCommand _ _ = Nothing

    -- | This candidate plugin filters the candidates by module,
    -- using the name of the hole as module to search in
    modFilterTimeoutP :: [CommandLineOption] -> TcRef HolePluginState -> CandPlugin
    modFilterTimeoutP _ ref hole cands = do
      curTime <- liftIO Time.getCurrentTime
      HPS {..} <- readTcRef ref
      updTcRef ref (setCurStarted curTime)
      return $ case timeAlloted of
        -- If we're out of time we remove all the candidates. Then nothing is checked.
        Just sofar | elapsedTime > sofar -> []
        _ -> case toHoleFitCommand hole "only_" of

              Just modName -> filter (inScopeVia modName) cands
              _ -> cands
      where inScopeVia modNameStr cand@(GreHFCand _) =
              elem (toModName modNameStr) $ fromModule cand
            inScopeVia _ _ = False
            toModName = replace '_' '.'
            replace :: Eq a => a -> a -> [a] -> [a]
            replace _ _ [] = []
            replace a b (x:xs) = (if x == a then b else x):replace a b xs

    modSortP :: [CommandLineOption] -> TcRef HolePluginState -> FitPlugin
    modSortP _ ref hole hfs = do
      curTime <- liftIO Time.getCurrentTime
      HPS {..} <- readTcRef ref
      updTcRef ref $ bumpElapsed (Time.diffUTCTime curTime timeCurStarted)
      return $ case timeAlloted of
        -- If we're out of time, remove any candidates, so nothing is checked.
        Just sofar | elapsedTime > sofar -> [RawHoleFit $ text msg]
        _ -> case toHoleFitCommand hole "sort_by_mod" of
                -- If only_ is on, the fits will all be from the same module.
                Just ('_':'d':'e':'s':'c':_) -> reverse hfs
                Just _ -> orderByModule hfs
                _ ->  hfs
      where orderByModule :: [HoleFit] -> [HoleFit]
            orderByModule = sortOn (fmap fromModule . mbHFCand)
            mbHFCand :: HoleFit -> Maybe HoleFitCandidate
            mbHFCand HoleFit {hfCand = c} = Just c
            mbHFCand _ = Nothing
            msg = hang (text "Error: The time ran out, and the search was aborted for this hole.")
                   7 $ text "Try again with a longer timeout."

    plugin :: Plugin
    plugin = defaultPlugin { holeFitPlugin = holeFitP, pluginRecompile = purePlugin}

    holeFitP :: [CommandLineOption] -> Maybe HoleFitPluginR
    holeFitP opts = Just (HoleFitPluginR initP pluginDef stopP)
      where initP = initPlugin opts
            stopP = const $ return ()
            pluginDef ref = HoleFitPlugin { candPlugin = modFilterTimeoutP opts ref
                                          , fitPlugin  = modSortP opts ref }

When you then compile a module containing the following

::

    {-# OPTIONS -fplugin=HolePlugin
                -fplugin-opt=HolePlugin:600
                -funclutter-valid-hole-fits #-}
    module Main where

    import Prelude hiding (head, last)

    import Data.List (head, last)


    f, g, h, i, j :: [Int] -> Int
    f = _too_long
    j = _
    i = _sort_by_mod_desc
    g = _only_Data_List
    h = _only_Prelude

    main :: IO ()
    main = return ()


The output is as follows:

.. code-block:: none

    Main.hs:12:5: error:
        • Found hole: _too_long :: [Int] -> Int
          Or perhaps ‘_too_long’ is mis-spelled, or not in scope
        • In the expression: _too_long
          In an equation for ‘f’: f = _too_long
        • Relevant bindings include
            f :: [Int] -> Int (bound at Main.hs:12:1)
          Valid hole fits include
            Error: The time ran out, and the search was aborted for this hole.
                   Try again with a longer timeout.
      |
    12 | f = _too_long
      |     ^^^^^^^^^

    Main.hs:13:5: error:
        • Found hole: _ :: [Int] -> Int
        • In the expression: _
          In an equation for ‘j’: j = _
        • Relevant bindings include
            j :: [Int] -> Int (bound at Main.hs:13:1)
          Valid hole fits include
            j :: [Int] -> Int
            f :: [Int] -> Int
            g :: [Int] -> Int
            h :: [Int] -> Int
            i :: [Int] -> Int
            head :: forall a. [a] -> a
            (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)
      |
    13 | j = _
      |     ^

    Main.hs:14:5: error:
        • Found hole: _sort_by_mod_desc :: [Int] -> Int
          Or perhaps ‘_sort_by_mod_desc’ is mis-spelled, or not in scope
        • In the expression: _sort_by_mod_desc
          In an equation for ‘i’: i = _sort_by_mod_desc
        • Relevant bindings include
            i :: [Int] -> Int (bound at Main.hs:14:1)
          Valid hole fits include
            sum :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a
            product :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a
            minimum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
            maximum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
            length :: forall (t :: * -> *) a. Foldable t => t a -> Int
            last :: forall a. [a] -> a
            (Some hole fits suppressed; use -fmax-valid-hole-fits=N or -fno-max-valid-hole-fits)
      |
    14 | i = _sort_by_mod_desc
      |     ^^^^^^^^^^^^^^^^^

    Main.hs:15:5: error:
        • Found hole: _only_Data_List :: [Int] -> Int
          Or perhaps ‘_only_Data_List’ is mis-spelled, or not in scope
        • In the expression: _only_Data_List
          In an equation for ‘g’: g = _only_Data_List
        • Relevant bindings include
            g :: [Int] -> Int (bound at Main.hs:15:1)
          Valid hole fits include
            head :: forall a. [a] -> a
            last :: forall a. [a] -> a
      |
    15 | g = _only_Data_List
      |     ^^^^^^^^^^^^^^^

    Main.hs:16:5: error:
        • Found hole: _only_Prelude :: [Int] -> Int
          Or perhaps ‘_only_Prelude’ is mis-spelled, or not in scope
        • In the expression: _only_Prelude
          In an equation for ‘h’: h = _only_Prelude
        • Relevant bindings include
            h :: [Int] -> Int (bound at Main.hs:16:1)
          Valid hole fits include
            length :: forall (t :: * -> *) a. Foldable t => t a -> Int
            maximum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
            minimum :: forall (t :: * -> *) a. (Foldable t, Ord a) => t a -> a
            product :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a
            sum :: forall (t :: * -> *) a. (Foldable t, Num a) => t a -> a
      |
    16 | h = _only_Prelude
      |     ^^^^^^^^^^^^^



.. _plugin_recompilation:

Controlling Recompilation
~~~~~~~~~~~~~~~~~~~~~~~~~

By default, modules compiled with plugins are always recompiled even if the source file is
unchanged. This most conservative option is taken due to the ability of plugins
to perform arbitrary IO actions. In order to control the recompilation behaviour
you can modify the ``pluginRecompile`` field in ``Plugin``. ::

    plugin :: Plugin
    plugin = defaultPlugin {
      installCoreToDos = install,
      pluginRecompile = purePlugin
      }

By inspecting the example ``plugin`` defined above, we can see that it is pure. This
means that if the two modules have the same fingerprint then the plugin
will always return the same result. Declaring a plugin as pure means that
the plugin will never cause a module to be recompiled.

In general, the ``pluginRecompile`` field has the following type::

    pluginRecompile :: [CommandLineOption] -> IO PluginRecompile

The ``PluginRecompile`` data type is an enumeration determining how the plugin
should affect recompilation. ::

    data PluginRecompile = ForceRecompile | NoForceRecompile | MaybeRecompile Fingerprint

A plugin which declares itself impure using ``ForceRecompile`` will always
trigger a recompilation of the current module. ``NoForceRecompile`` is used
for "pure" plugins which don't need to be rerun unless a module would ordinarily
be recompiled. ``MaybeRecompile`` computes a ``Fingerprint`` and if this ``Fingerprint``
is different to a previously computed ``Fingerprint`` for the plugin, then
we recompile the module.

As such, ``purePlugin`` is defined as a function which always returns ``NoForceRecompile``. ::

  purePlugin :: [CommandLineOption] -> IO PluginRecompile
  purePlugin _ = return NoForceRecompile

Users can use the same functions that GHC uses internally to compute fingerprints.
The `GHC.Fingerprint
<https://hackage.haskell.org/package/base-4.10.1.0/docs/GHC-Fingerprint.html>`_ module provides useful functions for constructing fingerprints. For example, combining
together ``fingerprintFingerprints`` and ``fingerprintString`` provides an easy to
to naively fingerprint the arguments to a plugin. ::

    pluginFlagRecompile :: [CommandLineOption] -> IO PluginRecompile
    pluginFlagRecompile =
      return . MaybeRecompile . fingerprintFingerprints . map fingerprintString . sort

``defaultPlugin`` defines ``pluginRecompile`` to be ``impurePlugin`` which
is the most conservative and backwards compatible option. ::

    impurePlugin :: [CommandLineOption] -> IO PluginRecompile
    impurePlugin _ = return ForceRecompile

.. _frontend_plugins:

Frontend plugins
~~~~~~~~~~~~~~~~

A frontend plugin allows you to add new major modes to GHC.  You may prefer
this over a traditional program which calls the GHC API, as GHC manages a lot
of parsing flags and administrative nonsense which can be difficult to
manage manually.  To load a frontend plugin exported by ``Foo.FrontendPlugin``,
we just invoke GHC with the :ghc-flag:`--frontend ⟨module⟩` flag as follows:

.. code-block:: none

    $ ghc --frontend Foo.FrontendPlugin ...other options...

Frontend plugins, like compiler plugins, are exported by registered plugins.
However, unlike compiler modules, frontend plugins are modules that export
at least a single identifier ``frontendPlugin`` of type
``GHC.Plugins.FrontendPlugin``.

``FrontendPlugin`` exports a field ``frontend``, which is a function
``[String] -> [(String, Maybe Phase)] -> Ghc ()``.  The first argument
is a list of extra flags passed to the frontend with ``-ffrontend-opt``;
the second argument is the list of arguments, usually source files
and module names to be compiled (the ``Phase`` indicates if an ``-x``
flag was set), and a frontend simply executes some operation in the
``Ghc`` monad (which, among other things, has a ``Session``).

As a quick example, here is a frontend plugin that prints the arguments that
were passed to it, and then exits.

::

    module DoNothing.FrontendPlugin (frontendPlugin) where
    import GHC.Plugins

    frontendPlugin :: FrontendPlugin
    frontendPlugin = defaultFrontendPlugin {
      frontend = doNothing
      }

    doNothing :: [String] -> [(String, Maybe Phase)] -> Ghc ()
    doNothing flags args = do
        liftIO $ print flags
        liftIO $ print args

Provided you have compiled this plugin and registered it in a package,
you can just use it by specifying ``--frontend DoNothing.FrontendPlugin``
on the command line to GHC.

.. _dynflags_plugins:

DynFlags plugins
~~~~~~~~~~~~~~~~

A DynFlags plugin allows you to modify the ``DynFlags`` that GHC
is going to use when processing a given (set of) file(s).
``DynFlags`` is a record containing all sorts of configuration
and command line data, from verbosity level to the integer library
to use, including compiler hooks, plugins and pretty-printing options.
DynFlags plugins allow plugin authors to update any of those values
before GHC starts doing any actual work, effectively meaning that
the updates specified by the plugin will be taken into account and
influence GHC's behaviour.

One of the motivating examples was the ability to register
compiler hooks from a plugin. For example, one might want to modify
the way Template Haskell code is executed. This is achievable by
updating the ``hooks`` field of the ``DynFlags`` type, recording
our custom "meta hook" in the right place. A simple application of
this idea can be seen below:

::

    module DynFlagsPlugin (plugin) where

    import BasicTypes
    import GHC.Plugins
    import GHC.Hs.Expr
    import GHC.Hs.Extension
    import GHC.Hs.Lit
    import Hooks
    import TcRnMonad

    plugin :: Plugin
    plugin = defaultPlugin { dynflagsPlugin = hooksP }

    hooksP :: [CommandLineOption] -> DynFlags -> IO DynFlags
    hooksP opts dflags = return $ dflags
      { hooks = (hooks dflags)
        { runMetaHook = Just (fakeRunMeta opts) }
      }

    -- This meta hook doesn't actually care running code in splices,
    -- it just replaces any expression splice with the "0"
    -- integer literal, and errors out on all other types of
    -- meta requests.
    fakeRunMeta :: [CommandLineOption] -> MetaHook TcM
    fakeRunMeta opts (MetaE r) _ = do
      liftIO . putStrLn $ "Options = " ++ show opts
      pure $ r zero

      where zero :: LHsExpr GhcPs
            zero = L noSrcSpan $ HsLit NoExtField $
              HsInt NoExtField (mkIntegralLit (0 :: Int))

    fakeRunMeta _ _ _ = error "fakeRunMeta: unimplemented"

This simple plugin takes over the execution of Template Haskell code,
replacing any expression splice it encounters by ``0`` (at type
``Int``), and errors out on any other type of splice.

Therefore, if we run GHC against the following code using the plugin
from above:

::

    {-# OPTIONS -fplugin=DynFlagsPlugin #-}
    {-# LANGUAGE TemplateHaskell #-}
    module Main where

    main :: IO ()
    main = print $( [|1|] )

This will not actually evaluate ``[|1|]``, but instead replace it
with the ``0 :: Int`` literal.

Just like the other types of plugins, you can write ``DynFlags`` plugins
that can take and make use of some options that you can then specify
using the ``-fplugin-opt`` flag. In the ``DynFlagsPlugin`` code from
above, the said options would be available in the ``opts`` argument of
``hooksP``.

Finally, since those ``DynFlags`` updates happen after the plugins are loaded,
you cannot from a ``DynFlags`` plugin register other plugins by just adding them
to the ``plugins`` field of ``DynFlags``. In order to achieve this, you would
have to load them yourself and store the result into the ``cachedPlugins``
field of ``DynFlags``.
