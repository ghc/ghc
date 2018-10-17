.. _ghci:

Using GHCi
==========

.. index::
   single: GHCi
   single: interpreter
   single: interactive
   single: Hugs
   single: Foreign Function Interface; GHCi support
   single: FFI; GHCi support

GHCi [1]_ is GHC's interactive environment, in which Haskell expressions
can be interactively evaluated and programs can be interpreted. If
you're familiar with `Hugs <http://www.haskell.org/hugs/>`__, then
you'll be right at home with GHCi. However, GHCi also has support for
interactively loading compiled code, as well as supporting all [2]_ the
language extensions that GHC provides. GHCi also includes an interactive
debugger (see :ref:`ghci-debugger`).

.. [1]
   The "i" stands for “Interactive”

.. [2]
   except ``foreign export``, at the moment


.. _ghci-introduction:

Introduction to GHCi
--------------------

Let's start with an example GHCi session. You can fire up GHCi with the
command ``ghci``:

.. code-block:: none

    $ ghci
    GHCi, version 8.y.z: http://www.haskell.org/ghc/  :? for help
    Prelude>

There may be a short pause while GHCi loads the prelude and standard
libraries, after which the prompt is shown. As the banner says, you can
type :ghci-cmd:`:?` to see the list of commands available, and a half line
description of each of them. We'll explain most of these commands as we
go along, and there is complete documentation for all the commands in
:ref:`ghci-commands`.

Haskell expressions can be typed at the prompt:

.. index::
   single: prompt; GHCi

.. code-block:: none

    Prelude> 1+2
    3
    Prelude> let x = 42 in x / 9
    4.666666666666667
    Prelude>

GHCi interprets the whole line as an expression to evaluate. The
expression may not span several lines - as soon as you press enter, GHCi
will attempt to evaluate it.

In Haskell, a ``let`` expression is followed by ``in``. However, in
GHCi, since the expression can also be interpreted in the ``IO`` monad,
a ``let`` binding with no accompanying ``in`` statement can be signalled
by an empty line, as in the above example.

Since GHC 8.0.1, you can bind values and functions to names without ``let`` statement:

.. code-block:: none

    Prelude> x = 42
    Prelude> x
    42
    Prelude>

.. _loading-source-files:

Loading source files
--------------------

Suppose we have the following Haskell source code, which we place in a
file ``Main.hs``: ::

    main = print (fac 20)

    fac 0 = 1
    fac n = n * fac (n-1)

You can save ``Main.hs`` anywhere you like, but if you save it somewhere
other than the current directory [3]_ then we will need to change to the
right directory in GHCi:

.. code-block:: none

    Prelude> :cd dir

where ⟨dir⟩ is the directory (or folder) in which you saved ``Main.hs``.

To load a Haskell source file into GHCi, use the :ghci-cmd:`:load` command:

.. index::
   single: :load

.. code-block:: none

    Prelude> :load Main
    Compiling Main             ( Main.hs, interpreted )
    Ok, modules loaded: Main.
    *Main>

GHCi has loaded the ``Main`` module, and the prompt has changed to
``*Main>`` to indicate that the current context for expressions
typed at the prompt is the ``Main`` module we just loaded (we'll explain
what the ``*`` means later in :ref:`ghci-scope`). So we can now type
expressions involving the functions from ``Main.hs``:

.. code-block:: none

    *Main> fac 17
    355687428096000

Loading a multi-module program is just as straightforward; just give the
name of the "topmost" module to the :ghci-cmd:`:load` command (hint:
:ghci-cmd:`:load` can be abbreviated to ``:l``). The topmost module will
normally be ``Main``, but it doesn't have to be. GHCi will discover which
modules are required, directly or indirectly, by the topmost module, and load
them all in dependency order.

.. [3]
   If you started up GHCi from the command line then GHCi's current
   directory is the same as the current directory of the shell from
   which it was started. If you started GHCi from the “Start” menu in
   Windows, then the current directory is probably something like
   ``C:\Documents and Settings\user name``.

.. ghc-flag:: -fshow-loaded-modules
    :shortdesc: Show the names of modules that GHCi loaded after a
                :ghci-cmd:`:load` command.
    :type: dynamic

    :default: off
    :since: 8.2.2

    Typically GHCi will show only the number of modules that it loaded after a
    :ghci-cmd:`:load` command. With this flag, GHC will also list the loaded
    modules' names. This was the default behavior prior to GHC 8.2.1 and can be
    useful for some tooling users.


.. _ghci-modules-filenames:

Modules vs. filenames
~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: modules; and filenames
   single: filenames; of modules

Question: How does GHC find the filename which contains module ⟨M⟩?
Answer: it looks for the file ``M.hs``, or ``M.lhs``. This means that
for most modules, the module name must match the filename. If it
doesn't, GHCi won't be able to find it.

There is one exception to this general rule: when you load a program
with :ghci-cmd:`:load`, or specify it when you invoke ``ghci``, you can give a
filename rather than a module name. This filename is loaded if it
exists, and it may contain any module you like. This is particularly
convenient if you have several ``Main`` modules in the same directory
and you can't call them all ``Main.hs``.

The search path for finding source files is specified with the :ghc-flag:`-i`
option on the GHCi command line, like so:

.. code-block:: none

    ghci -idir1:...:dirn

or it can be set using the :ghci-cmd:`:set` command from within GHCi (see
:ref:`ghci-cmd-line-options`) [4]_

One consequence of the way that GHCi follows dependencies to find
modules to load is that every module must have a source file. The only
exception to the rule is modules that come from a package, including the
``Prelude`` and standard libraries such as ``IO`` and ``Complex``. If
you attempt to load a module for which GHCi can't find a source file,
even if there are object and interface files for the module, you'll get
an error message.

.. [4]
   Note that in GHCi, and :ghc-flag:`--make` mode, the :ghc-flag:`-i` option is used to
   specify the search path for *source* files, whereas in standard
   batch-compilation mode the :ghc-flag:`-i` option is used to specify the
   search path for interface files, see :ref:`search-path`.


Making changes and recompilation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: :reload

If you make some changes to the source code and want GHCi to recompile
the program, give the :ghci-cmd:`:reload` command. The program will be
recompiled as necessary, with GHCi doing its best to avoid actually
recompiling modules if their external dependencies haven't changed. This
is the same mechanism we use to avoid re-compiling modules in the batch
compilation setting (see :ref:`recomp`).

.. _ghci-compiled:

Loading compiled code
---------------------

.. index::
   single: compiled code; in GHCi

When you load a Haskell source module into GHCi, it is normally
converted to byte-code and run using the interpreter. However,
interpreted code can also run alongside compiled code in GHCi; indeed,
normally when GHCi starts, it loads up a compiled copy of the ``base``
package, which contains the ``Prelude``.

Why should we want to run compiled code? Well, compiled code is roughly
10x faster than interpreted code, but takes about 2x longer to produce
(perhaps longer if optimisation is on). So it pays to compile the parts
of a program that aren't changing very often, and use the interpreter
for the code being actively developed.

When loading up source modules with :ghci-cmd:`:load`, GHCi normally looks for
any corresponding compiled object files, and will use one in preference
to interpreting the source if possible. For example, suppose we have a 4-module
program consisting of modules ``A``, ``B``, ``C``, and ``D``. Modules ``B`` and
``C`` both import ``D`` only, and ``A`` imports both ``B`` and ``C``:

.. code-block:: none

          A
         / \
        B   C
         \ /
          D

We can compile ``D``, then load the whole program, like this:

.. code-block:: none

    Prelude> :! ghc -c -dynamic D.hs
    Prelude> :load A
    Compiling B                ( B.hs, interpreted )
    Compiling C                ( C.hs, interpreted )
    Compiling A                ( A.hs, interpreted )
    Ok, modules loaded: A, B, C, D (D.o).
    *Main>

In the messages from the compiler, we see that there is no line for
``D``. This is because it isn't necessary to compile ``D``, because the
source and everything it depends on is unchanged since the last
compilation.

Note the :ghc-flag:`-dynamic` flag to GHC: GHCi uses dynamically-linked object
code (if you are on a platform that supports it), and so in order to use
compiled code with GHCi it must be compiled for dynamic linking.

At any time you can use the command :ghci-cmd:`:show modules` to get a list of
the modules currently loaded into GHCi:

.. code-block:: none

    *Main> :show modules
    D                ( D.hs, D.o )
    C                ( C.hs, interpreted )
    B                ( B.hs, interpreted )
    A                ( A.hs, interpreted )
    *Main>

If we now modify the source of ``D`` (or pretend to: using the Unix command
``touch`` on the source file is handy for this), the compiler will no
longer be able to use the object file, because it might be out of date:

.. code-block:: none

    *Main> :! touch D.hs
    *Main> :reload
    Compiling D                ( D.hs, interpreted )
    Ok, modules loaded: A, B, C, D.
    *Main>

Note that module ``D`` was compiled, but in this instance because its source
hadn't really changed, its interface remained the same, and the
recompilation checker determined that ``A``, ``B`` and ``C`` didn't need to be
recompiled.

So let's try compiling one of the other modules:

.. code-block:: none

    *Main> :! ghc -c C.hs
    *Main> :load A
    Compiling D                ( D.hs, interpreted )
    Compiling B                ( B.hs, interpreted )
    Compiling C                ( C.hs, interpreted )
    Compiling A                ( A.hs, interpreted )
    Ok, modules loaded: A, B, C, D.

We didn't get the compiled version of ``C``! What happened? Well, in GHCi a
compiled module may only depend on other compiled modules, and in this
case ``C`` depends on ``D``, which doesn't have an object file, so GHCi also
rejected ``C``\'s object file. Ok, so let's also compile ``D``:

.. code-block:: none

    *Main> :! ghc -c D.hs
    *Main> :reload
    Ok, modules loaded: A, B, C, D.

Nothing happened! Here's another lesson: newly compiled modules aren't
picked up by :ghci-cmd:`:reload`, only :ghci-cmd:`:load`:

.. code-block:: none

    *Main> :load A
    Compiling B                ( B.hs, interpreted )
    Compiling A                ( A.hs, interpreted )
    Ok, modules loaded: A, B, C (C.o), D (D.o).

The automatic loading of object files can sometimes lead to confusion,
because non-exported top-level definitions of a module are only
available for use in expressions at the prompt when the module is
interpreted (see :ref:`ghci-scope`). For this reason, you might
sometimes want to force GHCi to load a module using the interpreter.
This can be done by prefixing a ``*`` to the module name or filename
when using :ghci-cmd:`:load`, for example

.. code-block:: none

    Prelude> :load *A
    Compiling A                ( A.hs, interpreted )
    *A>

When the ``*`` is used, GHCi ignores any pre-compiled object code and
interprets the module. If you have already loaded a number of modules as
object code and decide that you wanted to interpret one of them, instead
of re-loading the whole set you can use ``:add *M`` to specify that you
want ``M`` to be interpreted (note that this might cause other modules
to be interpreted too, because compiled modules cannot depend on
interpreted ones).

To always compile everything to object code and never use the
interpreter, use the :ghc-flag:`-fobject-code` option (see :ref:`ghci-obj`).

.. hint::
    Since GHCi will only use a compiled object file if it can be sure
    that the compiled version is up-to-date, a good technique when working
    on a large program is to occasionally run ``ghc --make`` to compile the
    whole project (say before you go for lunch :-), then continue working in
    the interpreter. As you modify code, the changed modules will be
    interpreted, but the rest of the project will remain compiled.

.. _interactive-evaluation:

Interactive evaluation at the prompt
------------------------------------

When you type an expression at the prompt, GHCi immediately evaluates
and prints the result:

.. code-block:: none

    Prelude> reverse "hello"
    "olleh"
    Prelude> 5+5
    10

.. _actions-at-prompt:

I/O actions at the prompt
~~~~~~~~~~~~~~~~~~~~~~~~~

GHCi does more than simple expression evaluation at the prompt. If you
enter an expression of type ``IO a`` for some ``a``, then GHCi
*executes* it as an IO-computation.

.. code-block:: none

    Prelude> "hello"
    "hello"
    Prelude> putStrLn "hello"
    hello

This works even if the type of the expression is more general, provided
it can be *instantiated* to ``IO a``. For example

.. code-block:: none

    Prelude> return True
    True

Furthermore, GHCi will print the result of the I/O action if (and only
if):

-  The result type is an instance of ``Show``.

-  The result type is not ``()``.

For example, remembering that ``putStrLn :: String -> IO ()``:

.. code-block:: none

    Prelude> putStrLn "hello"
    hello
    Prelude> do { putStrLn "hello"; return "yes" }
    hello
    "yes"

.. _ghci-stmts:

Using ``do`` notation at the prompt
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: do-notation; in GHCi
   single: statements; in GHCi

GHCi actually accepts statements rather than just expressions at the
prompt. This means you can bind values and functions to names, and use
them in future expressions or statements.

The syntax of a statement accepted at the GHCi prompt is exactly the
same as the syntax of a statement in a Haskell ``do`` expression.
However, there's no monad overloading here: statements typed at the
prompt must be in the ``IO`` monad.

.. code-block:: none

    Prelude> x <- return 42
    Prelude> print x
    42
    Prelude>

The statement ``x <- return 42`` means “execute ``return 42`` in the
``IO`` monad, and bind the result to ``x``\ ”. We can then use ``x`` in
future statements, for example to print it as we did above.

.. ghc-flag:: -fprint-bind-result
    :shortdesc: :ref:`Turn on printing of binding results in GHCi <ghci-stmts>`
    :type: dynamic
    :reverse: -fno-print-bind-result
    :category:

    If :ghc-flag:`-fprint-bind-result` is set then GHCi will print the result of a
    statement if and only if:

    - The statement is not a binding, or it is a monadic binding
      (``p <- e``) that binds exactly one variable.

    - The variable's type is not polymorphic, is not ``()``, and is an
      instance of ``Show``.

Of course, you can also bind normal non-IO expressions using the
``let``\-statement:

.. code-block:: none

    Prelude> let x = 42
    Prelude> x
    42
    Prelude>

Another important difference between the two types of binding is that
the monadic bind (``p <- e``) is *strict* (it evaluates ``e``), whereas
with the ``let`` form, the expression isn't evaluated immediately:

.. code-block:: none

    Prelude> let x = error "help!"
    Prelude> print x
    *** Exception: help!
    Prelude>

Note that ``let`` bindings do not automatically print the value bound,
unlike monadic bindings.

You can also define functions at the prompt:

.. code-block:: none

    Prelude> add a b = a + b
    Prelude> add 1 2
    3
    Prelude>

However, this quickly gets tedious when defining functions with multiple
clauses, or groups of mutually recursive functions, because the complete
definition has to be given on a single line, using explicit semicolons
instead of layout:

.. code-block:: none

    Prelude> f op n [] = n ; f op n (h:t) = h `op` f op n t
    Prelude> f (+) 0 [1..3]
    6
    Prelude>

.. ghci-cmd:: :{
              :}

    Begin or end a multi-line GHCi command block.

To alleviate this issue, GHCi commands can be split over multiple lines,
by wrapping them in ``:{`` and ``:}`` (each on a single line of its
own):

.. code-block:: none

    Prelude> :{
    Prelude| g op n [] = n
    Prelude| g op n (h:t) = h `op` g op n t
    Prelude| :}
    Prelude> g (*) 1 [1..3]
    6

Such multiline commands can be used with any GHCi command, and note that
the layout rule is in effect. The main purpose of multiline commands is
not to replace module loading but to make definitions in .ghci-files
(see :ref:`ghci-dot-files`) more readable and maintainable.

Any exceptions raised during the evaluation or execution of the
statement are caught and printed by the GHCi command line interface (for
more information on exceptions, see the module :base-ref:`Control.Exception.` in
the libraries documentation.

Every new binding shadows any existing bindings of the same name,
including entities that are in scope in the current module context.

.. warning::
    Temporary bindings introduced at the prompt only last until the
    next :ghci-cmd:`:load` or :ghci-cmd:`:reload` command, at which time they
    will be simply lost. However, they do survive a change of context with
    :ghci-cmd:`:module`: the temporary bindings just move to the new location.

.. hint::
    To get a list of the bindings currently in scope, use the
    :ghci-cmd:`:show bindings` command:

    .. code-block:: none

        Prelude> :show bindings
        x :: Int
        Prelude>

.. hint::
    If you turn on the ``+t`` option, GHCi will show the type of each
    variable bound by a statement. For example:

    .. code-block:: none

        Prelude> :set +t
        Prelude> let (x:xs) = [1..]
        x :: Integer
        xs :: [Integer]

    .. index::
        single: +t option; in GHCi


.. _ghci-multiline:

Multiline input
~~~~~~~~~~~~~~~

Apart from the ``:{ ... :}`` syntax for multi-line input mentioned
above, GHCi also has a multiline mode, enabled by ``:set +m``,
``:set +m`` in which GHCi detects automatically when the current
statement is unfinished and allows further lines to be added. A
multi-line input is terminated with an empty line. For example:

.. code-block:: none

    Prelude> :set +m
    Prelude> let x = 42
    Prelude|

Further bindings can be added to this ``let`` statement, so GHCi
indicates that the next line continues the previous one by changing the
prompt. Note that layout is in effect, so to add more bindings to this
``let`` we have to line them up:

.. code-block:: none

    Prelude> :set +m
    Prelude> let x = 42
    Prelude|     y = 3
    Prelude|
    Prelude>

Explicit braces and semicolons can be used instead of layout:

.. code-block:: none

    Prelude> do {
    Prelude| putStrLn "hello"
    Prelude| ;putStrLn "world"
    Prelude| }
    hello
    world
    Prelude>

Note that after the closing brace, GHCi knows that the current statement
is finished, so no empty line is required.

Multiline mode is useful when entering monadic ``do`` statements:

.. code-block:: none

    Control.Monad.State> flip evalStateT 0 $ do
    Control.Monad.State| i <- get
    Control.Monad.State| lift $ do
    Control.Monad.State|   putStrLn "Hello World!"
    Control.Monad.State|   print i
    Control.Monad.State|
    "Hello World!"
    0
    Control.Monad.State>

During a multiline interaction, the user can interrupt and return to the
top-level prompt.

.. code-block:: none

    Prelude> do
    Prelude| putStrLn "Hello, World!"
    Prelude| ^C
    Prelude>

.. _ghci-decls:

Type, class and other declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

At the GHCi prompt you can also enter any top-level Haskell declaration,
including ``data``, ``type``, ``newtype``, ``class``, ``instance``,
``deriving``, and ``foreign`` declarations. For example:

.. code-block:: none

    Prelude> data T = A | B | C deriving (Eq, Ord, Show, Enum)
    Prelude> [A ..]
    [A,B,C]
    Prelude> :i T
    data T = A | B | C      -- Defined at <interactive>:2:6
    instance Enum T -- Defined at <interactive>:2:45
    instance Eq T -- Defined at <interactive>:2:30
    instance Ord T -- Defined at <interactive>:2:34
    instance Show T -- Defined at <interactive>:2:39

As with ordinary variable bindings, later definitions shadow earlier
ones, so you can re-enter a declaration to fix a problem with it or
extend it. But there's a gotcha: when a new type declaration shadows an
older one, there might be other declarations that refer to the old type.
The thing to remember is that the old type still exists, and these other
declarations still refer to the old type. However, while the old and the
new type have the same name, GHCi will treat them as distinct. For
example:

.. code-block:: none

    Prelude> data T = A | B
    Prelude> let f A = True; f B = False
    Prelude> data T = A | B | C
    Prelude> f A

    <interactive>:2:3:
        Couldn't match expected type `main::Interactive.T'
                    with actual type `T'
        In the first argument of `f', namely `A'
        In the expression: f A
        In an equation for `it': it = f A
    Prelude>

The old, shadowed, version of ``T`` is displayed as
``main::Interactive.T`` by GHCi in an attempt to distinguish it from the
new ``T``, which is displayed as simply ``T``.

Class and type-family instance declarations are simply added to the
list of available instances, with one exception. Since you might want
to re-define one, a class instance *replaces* any earlier instance
with an identical head. You aren't allowed to re-define a type family
instance, since it might not be type safe to do so. Instead, re-define
the whole type-family. (See :ref:`type-families`.) For example:

.. code-block:: none

    Prelude> type family T a b
    Prelude> type instance T a b = a
    Prelude> let uc :: a -> T a b; uc = id

    Prelude> type instance T a b = b

    <interactive>:3:15: error:
        Conflicting family instance declarations:
          T a b = a -- Defined at <interactive>:3:15
          T a b = b -- Defined at <interactive>:5:15

    -- Darn! We have to re-declare T.

    Prelude> type family T a b
    -- This is a brand-new T, unrelated to the old one
    Prelude> type instance T a b = b
    Prelude> uc 'a' :: Int

    <interactive>:8:1: error:
        • Couldn't match type ‘Char’ with ‘Int’
          Expected type: Int
            Actual type: Ghci1.T Char b0
        • In the expression: uc 'a' :: Int
          In an equation for ‘it’: it = uc 'a' :: Int

.. _ghci-scope:

What's really in scope at the prompt?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you type an expression at the prompt, what identifiers and types
are in scope? GHCi provides a flexible way to control exactly how the
context for an expression is constructed:

-  The :ghci-cmd:`:load`, :ghci-cmd:`:add`, and :ghci-cmd:`:reload` commands
   (:ref:`ghci-load-scope`).

-  The ``import`` declaration (:ref:`ghci-import-decl`).

-  The :ghci-cmd:`:module` command (:ref:`ghci-module-cmd`).

The command :ghci-cmd:`:show imports` will show a summary of which modules
contribute to the top-level scope.

.. hint::
    GHCi will tab-complete names that are in scope; for example, if
    you run GHCi and type ``J<tab>`` then GHCi will expand it to
    ``Just``.

.. _ghci-load-scope:

The effect of ``:load`` on what is in scope
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The :ghci-cmd:`:load`, :ghci-cmd:`:add`, and :ghci-cmd:`:reload` commands
(:ref:`loading-source-files` and :ref:`ghci-compiled`) affect the
top-level scope. Let's start with the simple cases; when you start GHCi
the prompt looks like this:

.. code-block:: none

    Prelude>

which indicates that everything from the module ``Prelude`` is currently
in scope; the visible identifiers are exactly those that would be
visible in a Haskell source file with no ``import`` declarations.

If we now load a file into GHCi, the prompt will change:

.. code-block:: none

    Prelude> :load Main.hs
    Compiling Main             ( Main.hs, interpreted )
    *Main>

The new prompt is ``*Main``, which indicates that we are typing
expressions in the context of the top-level of the ``Main`` module.
Everything that is in scope at the top-level in the module ``Main`` we
just loaded is also in scope at the prompt (probably including
``Prelude``, as long as ``Main`` doesn't explicitly hide it).

The syntax in the prompt ``*module`` indicates that it is the full
top-level scope of ⟨module⟩ that is contributing to the scope for
expressions typed at the prompt. Without the ``*``, just the exports of
the module are visible.

.. note::
    For technical reasons, GHCi can only support the ``*``-form for
    modules that are interpreted. Compiled modules and package modules can
    only contribute their exports to the current scope. To ensure that GHCi
    loads the interpreted version of a module, add the ``*`` when loading
    the module, e.g. ``:load *M``.

In general, after a :ghci-cmd:`:load` command, an automatic import is added to
the scope for the most recently loaded "target" module, in a ``*``-form
if possible. For example, if you say ``:load foo.hs bar.hs`` and
``bar.hs`` contains module ``Bar``, then the scope will be set to
``*Bar`` if ``Bar`` is interpreted, or if ``Bar`` is compiled it will be
set to ``Prelude Bar`` (GHCi automatically adds ``Prelude`` if it isn't
present and there aren't any ``*``-form modules). These
automatically-added imports can be seen with :ghci-cmd:`:show imports`:

.. code-block:: none

    Prelude> :load hello.hs
    [1 of 1] Compiling Main             ( hello.hs, interpreted )
    Ok, modules loaded: Main.
    *Main> :show imports
    :module +*Main -- added automatically
    *Main>

and the automatically-added import is replaced the next time you use
:ghci-cmd:`:load`, :ghci-cmd:`:add`, or :ghci-cmd:`:reload`. It can also be
removed by :ghci-cmd:`:module` as with normal imports.

.. _ghci-import-decl:

Controlling what is in scope with ``import``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

We are not limited to a single module: GHCi can combine scopes from
multiple modules, in any mixture of ``*`` and non-\ ``*`` forms. GHCi
combines the scopes from all of these modules to form the scope that is
in effect at the prompt.

To add modules to the scope, use ordinary Haskell ``import`` syntax:

.. code-block:: none

    Prelude> import System.IO
    Prelude System.IO> hPutStrLn stdout "hello\n"
    hello
    Prelude System.IO>

The full Haskell import syntax is supported, including ``hiding`` and
``as`` clauses. The prompt shows the modules that are currently
imported, but it omits details about ``hiding``, ``as``, and so on. To
see the full story, use :ghci-cmd:`:show imports`:

.. code-block:: none

    Prelude> import System.IO
    Prelude System.IO> import Data.Map as Map
    Prelude System.IO Map> :show imports
    import Prelude -- implicit
    import System.IO
    import Data.Map as Map
    Prelude System.IO Map>

Note that the ``Prelude`` import is marked as implicit. It can be
overridden with an explicit ``Prelude`` import, just like in a Haskell
module.

With multiple modules in scope, especially multiple ``*``-form modules,
it is likely that name clashes will occur. Haskell specifies that name
clashes are only reported when an ambiguous identifier is used, and GHCi
behaves in the same way for expressions typed at the prompt.

.. _ghci-module-cmd:

Controlling what is in scope with the ``:module`` command
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Another way to manipulate the scope is to use the :ghci-cmd:`:module`
command, whose syntax is this:

.. code-block:: none

    :module +|- *mod1 ... *modn

Using the ``+`` form of the ``module`` commands adds modules to the
current scope, and ``-`` removes them. Without either ``+`` or ``-``,
the current scope is replaced by the set of modules specified. Note that
if you use this form and leave out ``Prelude``, an implicit ``Prelude``
import will be added automatically.

The :ghci-cmd:`:module` command provides a way to do two things that cannot be
done with ordinary ``import`` declarations:

-  :ghci-cmd:`:module` supports the ``*`` modifier on modules, which opens the
   full top-level scope of a module, rather than just its exports.

-  Imports can be *removed* from the context, using the syntax
   ``:module -M``. The ``import`` syntax is cumulative (as in a Haskell
   module), so this is the only way to subtract from the scope.

.. _ghci-import-qualified:

Qualified names
^^^^^^^^^^^^^^^

To make life slightly easier, the GHCi prompt also behaves as if there
is an implicit ``import qualified`` declaration for every module in
every package, and every module currently loaded into GHCi. This
behaviour can be disabled with the ``-fno-implicit-import-qualified`` flag.

.. index::
   single: -fno-implicit-import-qualified

``:module`` and ``:load``
^^^^^^^^^^^^^^^^^^^^^^^^^

It might seem that :ghci-cmd:`:module`/``import`` and
:ghci-cmd:`:load`/:ghci-cmd:`:add`/:ghci-cmd:`:reload` do similar things: you
can use both to bring a module into scope. However, there is a very important
difference. GHCi is concerned with two sets of modules:

-  The set of modules that are currently *loaded*. This set is modified
   by :ghci-cmd:`:load`, :ghci-cmd:`:add` and :ghci-cmd:`:reload`, and can be shown with
   :ghci-cmd:`:show modules`.

-  The set of modules that are currently *in scope* at the prompt. This set is
   modified by ``import`` and :ghci-cmd:`:module`, and it is also modified
   automatically after :ghci-cmd:`:load`, :ghci-cmd:`:add`, and
   :ghci-cmd:`:reload`, as described above. The set of modules in scope can be
   shown with :ghci-cmd:`:show imports`.

You can add a module to the scope (via :ghci-cmd:`:module` or ``import``) only
if either (a) it is loaded, or (b) it is a module from a package that
GHCi knows about. Using :ghci-cmd:`:module` or ``import`` to try bring into
scope a non-loaded module may result in the message
``module M is not loaded``.

The ``:main`` and ``:run`` commands
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When a program is compiled and executed, it can use the ``getArgs``
function to access the command-line arguments. However, we cannot simply
pass the arguments to the ``main`` function while we are testing in
ghci, as the ``main`` function doesn't take its directly.

Instead, we can use the :ghci-cmd:`:main` command. This runs whatever ``main``
is in scope, with any arguments being treated the same as command-line
arguments, e.g.:

.. code-block:: none

    Prelude> main = System.Environment.getArgs >>= print
    Prelude> :main foo bar
    ["foo","bar"]

We can also quote arguments which contains characters like spaces, and
they are treated like Haskell strings, or we can just use Haskell list
syntax:

.. code-block:: none

    Prelude> :main foo "bar baz"
    ["foo","bar baz"]
    Prelude> :main ["foo", "bar baz"]
    ["foo","bar baz"]

Finally, other functions can be called, either with the ``-main-is``
flag or the :ghci-cmd:`:run` command:

.. code-block:: none

    Prelude> foo = putStrLn "foo" >> System.Environment.getArgs >>= print
    Prelude> bar = putStrLn "bar" >> System.Environment.getArgs >>= print
    Prelude> :set -main-is foo
    Prelude> :main foo "bar baz"
    foo
    ["foo","bar baz"]
    Prelude> :run bar ["foo", "bar baz"]
    bar
    ["foo","bar baz"]

The ``it`` variable
~~~~~~~~~~~~~~~~~~~

.. index::
   single: it variable

Whenever an expression (or a non-binding statement, to be precise) is
typed at the prompt, GHCi implicitly binds its value to the variable
``it``. For example:

.. code-block:: none

    Prelude> 1+2
    3
    Prelude> it * 2
    6

What actually happens is that GHCi typechecks the expression, and if it
doesn't have an ``IO`` type, then it transforms it as follows: an
expression ``e`` turns into

.. code-block:: none

    let it = e;
    print it

which is then run as an IO-action.

Hence, the original expression must have a type which is an instance of
the ``Show`` class, or GHCi will complain:

.. code-block:: none

    Prelude> id

    <interactive>:1:0:
        No instance for (Show (a -> a))
          arising from use of `print' at <interactive>:1:0-1
        Possible fix: add an instance declaration for (Show (a -> a))
        In the expression: print it
        In a 'do' expression: print it

The error message contains some clues as to the transformation happening
internally.

If the expression was instead of type ``IO a`` for some ``a``, then
``it`` will be bound to the result of the ``IO`` computation, which is
of type ``a``. eg.:

.. code-block:: none

    Prelude> Data.Time.getZonedTime
    2017-04-10 12:34:56.93213581 UTC
    Prelude> print it
    2017-04-10 12:34:56.93213581 UTC

The corresponding translation for an IO-typed ``e`` is

.. code-block:: none

    it <- e

Note that ``it`` is shadowed by the new value each time you evaluate a
new expression, and the old value of ``it`` is lost.

In order to stop the value ``it`` being bound on each command, the flag
:ghc-flag:`-fno-it` can be set. The ``it`` variable can be the source
of space leaks due to how shadowed declarations are handled by
GHCi (see :ref:`ghci-decls`).

.. ghc-flag:: -fno-it
    :shortdesc: No longer set the special variable ``it``.
    :type: dynamic
    :reverse: -fno-no-it
    :category:

    When this flag is set, the variable ``it`` will no longer be set
    to the result of the previously evaluated expression.

.. _extended-default-rules:

Type defaulting in GHCi
~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: Type defaulting; in GHCi
   single: Show class

.. extension:: ExtendedDefaultRules
    :shortdesc: Use GHCi's extended default rules in a normal module.

    :since: 6.8.1

    Allow defaulting to take place for more than just numeric classes.

Consider this GHCi session:

.. code-block:: none

      ghci> reverse []

What should GHCi do? Strictly speaking, the program is ambiguous.
``show (reverse [])`` (which is what GHCi computes here) has type
``Show a => String`` and how that displays depends on the type ``a``.
For example:

.. code-block:: none

      ghci> reverse ([] :: String)
      ""
      ghci> reverse ([] :: [Int])
      []

However, it is tiresome for the user to have to specify the type, so
GHCi extends Haskell's type-defaulting rules (Section 4.3.4 of the
Haskell 2010 Report) as follows. The standard rules take each group of
constraints ``(C1 a, C2 a, ..., Cn a)`` for each type variable ``a``,
and defaults the type variable if

1. The type variable ``a`` appears in no other constraints

2. All the classes ``Ci`` are standard.

3. At least one of the classes ``Ci`` is numeric.

At the GHCi prompt, or with GHC if the :extension:`ExtendedDefaultRules` flag
is given, the types are instead resolved with the following method:

Find all the unsolved constraints. Then:

-  Find those that are of form ``(C a)`` where ``a`` is a type variable, and
   partition those constraints into groups that share a common type variable ``a``.

-  Keep only the groups in which at least one of the classes is an
   **interactive class** (defined below).

-  Now, for each remaining group G, try each type ``ty`` from the default-type list
   in turn; if setting ``a = ty`` would allow the constraints in G to be completely
   solved. If so, default ``a`` to ``ty``.

-  The unit type ``()`` and the list type ``[]`` are added to the start of
   the standard list of types which are tried when doing type defaulting.

Note that any multi-parameter constraints ``(D a b)`` or ``(D [a] Int)`` do not
participate in the process (either to help or to hinder); but they must of course
be soluble once the defaulting process is complete.

The last point means that, for example, this program: ::

    main :: IO ()
    main = print def

    instance Num ()

    def :: (Num a, Enum a) => a
    def = toEnum 0

prints ``()`` rather than ``0`` as the type is defaulted to ``()``
rather than ``Integer``.

The motivation for the change is that it means ``IO a`` actions default
to ``IO ()``, which in turn means that ghci won't try to print a result
when running them. This is particularly important for ``printf``, which
has an instance that returns ``IO a``. However, it is only able to
return ``undefined`` (the reason for the instance having this type is so
that printf doesn't require extensions to the class system), so if the
type defaults to ``Integer`` then ghci gives an error when running a
printf.

See also :ref:`actions-at-prompt` for how the monad of a computational
expression defaults to ``IO`` if possible.

Interactive classes
^^^^^^^^^^^^^^^^^^^

.. index::
   single: Interactive classes

The interactive classes (only relevant when :extension:`ExtendedDefaultRules`
is in effect) are: any numeric class, ``Show``, ``Eq``, ``Ord``,
``Foldable`` or ``Traversable``.

As long as a type variable is constrained by one of these classes, defaulting
will occur, as outlined above.

Extended rules around ``default`` declarations
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index::
   single: default declarations

Since the rules for defaulting are relaxed under
:extension:`ExtendedDefaultRules`, the rules for ``default`` declarations
are also relaxed. According to Section 4.3.4 of the Haskell 2010 Report,
a ``default`` declaration looks like ``default (t1, ..., tn)`` where, for
each ``ti``, ``Num ti`` must hold. This is relaxed to say that for each
``ti``, there must exist an interactive class ``C`` such that ``C ti`` holds.
This means that type *constructors* can be allowed in these lists.
For example, the following works if you wish your ``Foldable`` constraints
to default to ``Maybe`` but your ``Num`` constraints to still default
to ``Integer`` or ``Double``: ::

    default (Maybe, Integer, Double)

.. _ghci-interactive-print:

Using a custom interactive printing function
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: Custom printing function; in GHCi

Since GHC 7.6.1, GHCi prints the result of expressions typed at the prompt
using the function ``System.IO.print``. Its type signature is ``Show a => a ->
IO ()``, and it works by converting the value to ``String`` using ``show``.

This is not ideal in certain cases, like when the output is long, or
contains strings with non-ascii characters.

The :ghc-flag:`-interactive-print ⟨expr⟩` flag allows to specify any function
of type ``C a => a -> IO ()``, for some constraint ``C``, as the function for
printing evaluated expressions. The function can reside in any loaded module or
any registered package, but only when it resides in a registered package will
it survive a :ghci-cmd:`:cd`, :ghci-cmd:`:add`, :ghci-cmd:`:load`,
:ghci-cmd:`:reload` or, :ghci-cmd:`:set`.

.. ghc-flag:: -interactive-print ⟨expr⟩
    :shortdesc: :ref:`Select the function to use for printing evaluated
        expressions in GHCi <ghci-interactive-print>`
    :type: dynamic
    :category:

    Set the function used by GHCi to print evaluation results. Expression
    must be of type ``C a => a -> IO ()``.

As an example, suppose we have following special printing module: ::

    module SpecPrinter where
    import System.IO

    sprint a = putStrLn $ show a ++ "!"

The ``sprint`` function adds an exclamation mark at the end of any
printed value. Running GHCi with the command:

.. code-block:: none

    ghci -interactive-print=SpecPrinter.sprint SpecPrinter

will start an interactive session where values with be printed using
``sprint``:

.. code-block:: none

    *SpecPrinter> [1,2,3]
    [1,2,3]!
    *SpecPrinter> 42
    42!

A custom pretty printing function can be used, for example, to format
tree-like and nested structures in a more readable way.

The :ghc-flag:`-interactive-print ⟨expr⟩` flag can also be used when running
GHC in ``-e mode``:

.. code-block:: none

    % ghc -e "[1,2,3]" -interactive-print=SpecPrinter.sprint SpecPrinter
    [1,2,3]!

.. _ghci-stack-traces:

Stack Traces in GHCi
~~~~~~~~~~~~~~~~~~~~

.. index::
  simple: stack trace; in GHCi

[ This is an experimental feature enabled by the new
``-fexternal-interpreter`` flag that was introduced in GHC 8.0.1.  It
is currently not supported on Windows.]

GHCi can use the profiling system to collect stack trace information
when running interpreted code.  To gain access to stack traces, start
GHCi like this:

.. code-block:: none

    ghci -fexternal-interpreter -prof

This runs the interpreted code in a separate process (see
:ref:`external-interpreter`) and runs it in profiling mode to collect
call stack information.  Note that because we're running the
interpreted code in profiling mode, all packages that you use must be
compiled for profiling.  The ``-prof`` flag to GHCi only works in
conjunction with ``-fexternal-interpreter``.

There are three ways to get access to the current call stack.

- ``error`` and ``undefined`` automatically attach the current stack
  to the error message.  This often complements the ``HasCallStack``
  stack (see :ref:`hascallstack`), so both call stacks are
  shown.

- ``Debug.Trace.traceStack`` is a version of ``Debug.Trace.trace``
  that also prints the current call stack.

- Functions in the module ``GHC.Stack`` can be used to get the current
  stack and render it.

You don't need to use ``-fprof-auto`` for interpreted modules,
annotations are automatically added at a granularity fine enough to
distinguish individual call sites.  However, you won't see any call
stack information for compiled code unless it was compiled with
``-fprof-auto`` or has explicit ``SCC`` annotations (see
:ref:`scc-pragma`).

.. _ghci-debugger:

The GHCi Debugger
-----------------

.. index::
   single: debugger; in GHCi

GHCi contains a simple imperative-style debugger in which you can stop a
running computation in order to examine the values of variables. The
debugger is integrated into GHCi, and is turned on by default: no flags
are required to enable the debugging facilities. There is one major
restriction: breakpoints and single-stepping are only available in
interpreted modules; compiled code is invisible to the debugger [5]_.

The debugger provides the following:

-  The ability to set a breakpoint on a function definition or
   expression in the program. When the function is called, or the
   expression evaluated, GHCi suspends execution and returns to the
   prompt, where you can inspect the values of local variables before
   continuing with the execution.

-  Execution can be single-stepped: the evaluator will suspend execution
   approximately after every reduction, allowing local variables to be
   inspected. This is equivalent to setting a breakpoint at every point
   in the program.

-  Execution can take place in tracing mode, in which the evaluator
   remembers each evaluation step as it happens, but doesn't suspend
   execution until an actual breakpoint is reached. When this happens,
   the history of evaluation steps can be inspected.

-  Exceptions (e.g. pattern matching failure and ``error``) can be
   treated as breakpoints, to help locate the source of an exception in
   the program.

There is currently no support for obtaining a “stack trace”, but the
tracing and history features provide a useful second-best, which will
often be enough to establish the context of an error. For instance, it
is possible to break automatically when an exception is thrown, even if
it is thrown from within compiled code (see
:ref:`ghci-debugger-exceptions`).

.. _breakpoints:

Breakpoints and inspecting variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's use quicksort as a running example. Here's the code: ::

    qsort [] = []
    qsort (a:as) = qsort left ++ [a] ++ qsort right
      where (left,right) = (filter (<=a) as, filter (>a) as)

    main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])

First, load the module into GHCi:

.. code-block:: none

    Prelude> :l qsort.hs
    [1 of 1] Compiling Main             ( qsort.hs, interpreted )
    Ok, modules loaded: Main.
    *Main>

Now, let's set a breakpoint on the right-hand-side of the second
equation of qsort:

.. code-block:: none

    *Main> :break 2
    Breakpoint 0 activated at qsort.hs:2:15-46
    *Main>

The command ``:break 2`` sets a breakpoint on line 2 of the most
recently-loaded module, in this case ``qsort.hs``. Specifically, it
picks the leftmost complete subexpression on that line on which to set
the breakpoint, which in this case is the expression
``(qsort left ++ [a] ++ qsort right)``.

Now, we run the program:

.. code-block:: none

    *Main> main
    Stopped at qsort.hs:2:15-46
    _result :: [a]
    a :: a
    left :: [a]
    right :: [a]
    [qsort.hs:2:15-46] *Main>

Execution has stopped at the breakpoint. The prompt has changed to
indicate that we are currently stopped at a breakpoint, and the
location: ``[qsort.hs:2:15-46]``. To further clarify the location, we
can use the :ghci-cmd:`:list` command:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :list
    1  qsort [] = []
    2  qsort (a:as) = qsort left ++ [a] ++ qsort right
    3    where (left,right) = (filter (<=a) as, filter (>a) as)

The :ghci-cmd:`:list` command lists the source code around the current
breakpoint. If your output device supports it, then GHCi will highlight
the active subexpression in bold.

GHCi has provided bindings for the free variables [6]_ of the expression
on which the breakpoint was placed (``a``, ``left``, ``right``), and
additionally a binding for the result of the expression (``_result``).
These variables are just like other variables that you might define in
GHCi; you can use them in expressions that you type at the prompt, you
can ask for their types with :ghci-cmd:`:type`, and so on. There is one
important difference though: these variables may only have partial
types. For example, if we try to display the value of ``left``:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> left

    <interactive>:1:0:
        Ambiguous type variable `a' in the constraint:
          `Show a' arising from a use of `print' at <interactive>:1:0-3
        Cannot resolve unknown runtime types: a
        Use :print or :force to determine these types

This is because ``qsort`` is a polymorphic function, and because GHCi
does not carry type information at runtime, it cannot determine the
runtime types of free variables that involve type variables. Hence, when
you ask to display ``left`` at the prompt, GHCi can't figure out which
instance of ``Show`` to use, so it emits the type error above.

Fortunately, the debugger includes a generic printing command,
:ghci-cmd:`:print`, which can inspect the actual runtime value of a variable and
attempt to reconstruct its type. If we try it on ``left``:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :set -fprint-evld-with-show
    [qsort.hs:2:15-46] *Main> :print left
    left = (_t1::[a])

This isn't particularly enlightening. What happened is that ``left`` is
bound to an unevaluated computation (a suspension, or thunk), and
:ghci-cmd:`:print` does not force any evaluation. The idea is that
:ghci-cmd:`:print` can be used to inspect values at a breakpoint without any
unfortunate side effects. It won't force any evaluation, which could cause the
program to give a different answer than it would normally, and hence it won't
cause any exceptions to be raised, infinite loops, or further breakpoints to be
triggered (see :ref:`nested-breakpoints`). Rather than forcing thunks,
:ghci-cmd:`:print` binds each thunk to a fresh variable beginning with an
underscore, in this case ``_t1``.

The flag :ghc-flag:`-fprint-evld-with-show` instructs :ghci-cmd:`:print` to reuse
available ``Show`` instances when possible. This happens only when the
contents of the variable being inspected are completely evaluated.

If we aren't concerned about preserving the evaluatedness of a variable, we can
use :ghci-cmd:`:force` instead of :ghci-cmd:`:print`. The :ghci-cmd:`:force`
command behaves exactly like :ghci-cmd:`:print`, except that it forces the
evaluation of any thunks it encounters:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :force left
    left = [4,0,3,1]

Now, since :ghci-cmd:`:force` has inspected the runtime value of ``left``, it
has reconstructed its type. We can see the results of this type
reconstruction:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :show bindings
    _result :: [Integer]
    a :: Integer
    left :: [Integer]
    right :: [Integer]
    _t1 :: [Integer]

Not only do we now know the type of ``left``, but all the other partial
types have also been resolved. So we can ask for the value of ``a``, for
example:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> a
    8

You might find it useful to use Haskell's ``seq`` function to evaluate
individual thunks rather than evaluating the whole expression with
:ghci-cmd:`:force`. For example:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :print right
    right = (_t1::[Integer])
    [qsort.hs:2:15-46] *Main> seq _t1 ()
    ()
    [qsort.hs:2:15-46] *Main> :print right
    right = 23 : (_t2::[Integer])

We evaluated only the ``_t1`` thunk, revealing the head of the list, and
the tail is another thunk now bound to ``_t2``. The ``seq`` function is
a little inconvenient to use here, so you might want to use :ghci-cmd:`:def` to
make a nicer interface (left as an exercise for the reader!).

Finally, we can continue the current execution:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :continue
    Stopped at qsort.hs:2:15-46
    _result :: [a]
    a :: a
    left :: [a]
    right :: [a]
    [qsort.hs:2:15-46] *Main>

The execution continued at the point it previously stopped, and has now
stopped at the breakpoint for a second time.

.. _setting-breakpoints:

Setting breakpoints
^^^^^^^^^^^^^^^^^^^

Breakpoints can be set in various ways. Perhaps the easiest way to set a
breakpoint is to name a top-level function:

.. code-block:: none

       :break identifier

Where ⟨identifier⟩ names any top-level function in an interpreted module
currently loaded into GHCi (qualified names may be used). The breakpoint
will be set on the body of the function, when it is fully applied but
before any pattern matching has taken place.

Breakpoints can also be set by line (and optionally column) number:

.. code-block:: none

       :break line
       :break line column
       :break module line
       :break module line column

When a breakpoint is set on a particular line, GHCi sets the breakpoint
on the leftmost subexpression that begins and ends on that line. If two
complete subexpressions start at the same column, the longest one is
picked. If there is no complete subexpression on the line, then the
leftmost expression starting on the line is picked, and failing that the
rightmost expression that partially or completely covers the line.

When a breakpoint is set on a particular line and column, GHCi picks the
smallest subexpression that encloses that location on which to set the
breakpoint. Note: GHC considers the TAB character to have a width of 1,
wherever it occurs; in other words it counts characters, rather than
columns. This matches what some editors do, and doesn't match others.
The best advice is to avoid tab characters in your source code
altogether (see :ghc-flag:`-Wtabs` in :ref:`options-sanity`).

If the module is omitted, then the most recently-loaded module is used.

Not all subexpressions are potential breakpoint locations. Single
variables are typically not considered to be breakpoint locations
(unless the variable is the right-hand-side of a function definition,
lambda, or case alternative). The rule of thumb is that all redexes are
breakpoint locations, together with the bodies of functions, lambdas,
case alternatives and binding statements. There is normally no
breakpoint on a let expression, but there will always be a breakpoint on
its body, because we are usually interested in inspecting the values of
the variables bound by the let.

Listing and deleting breakpoints
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The list of breakpoints currently enabled can be displayed using
:ghci-cmd:`:show breaks`:

.. code-block:: none

    *Main> :show breaks
    [0] Main qsort.hs:1:11-12
    [1] Main qsort.hs:2:15-46

To delete a breakpoint, use the :ghci-cmd:`:delete` command with the number
given in the output from :ghci-cmd:`:show breaks`:

.. code-block:: none

    *Main> :delete 0
    *Main> :show breaks
    [1] Main qsort.hs:2:15-46

To delete all breakpoints at once, use ``:delete *``.

.. _single-stepping:

Single-stepping
~~~~~~~~~~~~~~~

Single-stepping is a great way to visualise the execution of your
program, and it is also a useful tool for identifying the source of a
bug. GHCi offers two variants of stepping. Use :ghci-cmd:`:step` to enable all
the breakpoints in the program, and execute until the next breakpoint is
reached. Use :ghci-cmd:`:steplocal` to limit the set of enabled breakpoints to
those in the current top level function. Similarly, use :ghci-cmd:`:stepmodule`
to single step only on breakpoints contained in the current module. For
example:

.. code-block:: none

    *Main> :step main
    Stopped at qsort.hs:5:7-47
    _result :: IO ()

The command :ghci-cmd:`:step expr <:step>` begins the evaluation of ⟨expr⟩ in
single-stepping mode. If ⟨expr⟩ is omitted, then it single-steps from
the current breakpoint. :ghci-cmd:`:steplocal` and :ghci-cmd:`:stepmodule`
commands work similarly.

The :ghci-cmd:`:list` command is particularly useful when single-stepping, to
see where you currently are:

.. code-block:: none

    [qsort.hs:5:7-47] *Main> :list
    4
    5  main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
    6
    [qsort.hs:5:7-47] *Main>

In fact, GHCi provides a way to run a command when a breakpoint is hit,
so we can make it automatically do :ghci-cmd:`:list`:

.. code-block:: none

    [qsort.hs:5:7-47] *Main> :set stop :list
    [qsort.hs:5:7-47] *Main> :step
    Stopped at qsort.hs:5:14-46
    _result :: [Integer]
    4
    5  main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
    6
    [qsort.hs:5:14-46] *Main>

.. _nested-breakpoints:

Nested breakpoints
~~~~~~~~~~~~~~~~~~

When GHCi is stopped at a breakpoint, and an expression entered at the
prompt triggers a second breakpoint, the new breakpoint becomes the
"current" one, and the old one is saved on a stack. An arbitrary number
of breakpoint contexts can be built up in this way. For example:

.. code-block:: none

    [qsort.hs:2:15-46] *Main> :st qsort [1,3]
    Stopped at qsort.hs:(1,0)-(3,55)
    _result :: [a]
    ... [qsort.hs:(1,0)-(3,55)] *Main>

While stopped at the breakpoint on line 2 that we set earlier, we
started a new evaluation with ``:step qsort [1,3]``. This new evaluation
stopped after one step (at the definition of ``qsort``). The prompt has
changed, now prefixed with ``...``, to indicate that there are saved
breakpoints beyond the current one. To see the stack of contexts, use
:ghci-cmd:`:show context`:

.. code-block:: none

    ... [qsort.hs:(1,0)-(3,55)] *Main> :show context
    --> main
      Stopped at qsort.hs:2:15-46
    --> qsort [1,3]
      Stopped at qsort.hs:(1,0)-(3,55)
    ... [qsort.hs:(1,0)-(3,55)] *Main>

To abandon the current evaluation, use :ghci-cmd:`:abandon`:

.. code-block:: none

    ... [qsort.hs:(1,0)-(3,55)] *Main> :abandon
    [qsort.hs:2:15-46] *Main> :abandon
    *Main>

.. _ghci-debugger-result:

The ``_result`` variable
~~~~~~~~~~~~~~~~~~~~~~~~

When stopped at a breakpoint or single-step, GHCi binds the variable
``_result`` to the value of the currently active expression. The value
of ``_result`` is presumably not available yet, because we stopped its
evaluation, but it can be forced: if the type is known and showable,
then just entering ``_result`` at the prompt will show it. However,
there's one caveat to doing this: evaluating ``_result`` will be likely
to trigger further breakpoints, starting with the breakpoint we are
currently stopped at (if we stopped at a real breakpoint, rather than
due to :ghci-cmd:`:step`). So it will probably be necessary to issue a
:ghci-cmd:`:continue` immediately when evaluating ``_result``. Alternatively,
you can use :ghci-cmd:`:force` which ignores breakpoints.

.. _tracing:

Tracing and history
~~~~~~~~~~~~~~~~~~~

A question that we often want to ask when debugging a program is “how
did I get here?”. Traditional imperative debuggers usually provide some
kind of stack-tracing feature that lets you see the stack of active
function calls (sometimes called the “lexical call stack”), describing a
path through the code to the current location. Unfortunately this is
hard to provide in Haskell, because execution proceeds on a
demand-driven basis, rather than a depth-first basis as in strict
languages. The “stack“ in GHC's execution engine bears little
resemblance to the lexical call stack. Ideally GHCi would maintain a
separate lexical call stack in addition to the dynamic call stack, and
in fact this is exactly what our profiling system does
(:ref:`profiling`), and what some other Haskell debuggers do. For the
time being, however, GHCi doesn't maintain a lexical call stack (there
are some technical challenges to be overcome). Instead, we provide a way
to backtrack from a breakpoint to previous evaluation steps: essentially
this is like single-stepping backwards, and should in many cases provide
enough information to answer the "how did I get here?" question.

To use tracing, evaluate an expression with the :ghci-cmd:`:trace` command. For
example, if we set a breakpoint on the base case of ``qsort``:

.. code-block:: none

    *Main> :list qsort
    1  qsort [] = []
    2  qsort (a:as) = qsort left ++ [a] ++ qsort right
    3    where (left,right) = (filter (<=a) as, filter (>a) as)
    4
    *Main> :b 1
    Breakpoint 1 activated at qsort.hs:1:11-12
    *Main>

and then run a small ``qsort`` with tracing:

.. code-block:: none

    *Main> :trace qsort [3,2,1]
    Stopped at qsort.hs:1:11-12
    _result :: [a]
    [qsort.hs:1:11-12] *Main>

We can now inspect the history of evaluation steps:

.. code-block:: none

    [qsort.hs:1:11-12] *Main> :hist
    -1  : qsort.hs:3:24-38
    -2  : qsort.hs:3:23-55
    -3  : qsort.hs:(1,0)-(3,55)
    -4  : qsort.hs:2:15-24
    -5  : qsort.hs:2:15-46
    -6  : qsort.hs:3:24-38
    -7  : qsort.hs:3:23-55
    -8  : qsort.hs:(1,0)-(3,55)
    -9  : qsort.hs:2:15-24
    -10 : qsort.hs:2:15-46
    -11 : qsort.hs:3:24-38
    -12 : qsort.hs:3:23-55
    -13 : qsort.hs:(1,0)-(3,55)
    -14 : qsort.hs:2:15-24
    -15 : qsort.hs:2:15-46
    -16 : qsort.hs:(1,0)-(3,55)
    <end of history>

To examine one of the steps in the history, use :ghci-cmd:`:back`:

.. code-block:: none

    [qsort.hs:1:11-12] *Main> :back
    Logged breakpoint at qsort.hs:3:24-38
    _result :: [a]
    as :: [a]
    a :: a
    [-1: qsort.hs:3:24-38] *Main>

Note that the local variables at each step in the history have been
preserved, and can be examined as usual. Also note that the prompt has
changed to indicate that we're currently examining the first step in the
history: ``-1``. The command :ghci-cmd:`:forward` can be used to traverse
forward in the history.

The :ghci-cmd:`:trace` command can be used with or without an expression. When
used without an expression, tracing begins from the current breakpoint,
just like :ghci-cmd:`:step`.

The history is only available when using :ghci-cmd:`:trace`; the reason for this
is we found that logging each breakpoint in the history cuts performance
by a factor of 2 or more.

.. ghc-flag:: -fghci-hist-size=⟨n⟩
    :shortdesc: Set the number of entries GHCi keeps for ``:history``.
        See :ref:`ghci-debugger`.
    :type: dynamic
    :category:

    :default: 50

    Modify the depth of the evaluation history tracked by GHCi.

.. _ghci-debugger-exceptions:

Debugging exceptions
~~~~~~~~~~~~~~~~~~~~

Another common question that comes up when debugging is "where did this
exception come from?". Exceptions such as those raised by ``error`` or
``head []`` have no context information attached to them. Finding which
particular call to ``head`` in your program resulted in the error can be
a painstaking process, usually involving ``Debug.Trace.trace``, or
compiling with profiling and using ``Debug.Trace.traceStack`` or
``+RTS -xc`` (see :rts-flag:`-xc`).

The GHCi debugger offers a way to hopefully shed some light on these
errors quickly and without modifying or recompiling the source code. One
way would be to set a breakpoint on the location in the source code that
throws the exception, and then use :ghci-cmd:`:trace` and :ghci-cmd:`:history` to
establish the context. However, ``head`` is in a library and we can't
set a breakpoint on it directly. For this reason, GHCi provides the
flags :ghc-flag:`-fbreak-on-exception` which causes the evaluator to stop when
an exception is thrown, and :ghc-flag:`-fbreak-on-error`, which works similarly
but stops only on uncaught exceptions. When stopping at an exception,
GHCi will act just as it does when a breakpoint is hit, with the
deviation that it will not show you any source code location. Due to
this, these commands are only really useful in conjunction with
:ghci-cmd:`:trace`, in order to log the steps leading up to the exception. For
example:

.. code-block:: none

    *Main> :set -fbreak-on-exception
    *Main> :trace qsort ("abc" ++ undefined)
    “Stopped at <exception thrown>
    _exception :: e
    [<exception thrown>] *Main> :hist
    -1  : qsort.hs:3:24-38
    -2  : qsort.hs:3:23-55
    -3  : qsort.hs:(1,0)-(3,55)
    -4  : qsort.hs:2:15-24
    -5  : qsort.hs:2:15-46
    -6  : qsort.hs:(1,0)-(3,55)
    <end of history>
    [<exception thrown>] *Main> :back
    Logged breakpoint at qsort.hs:3:24-38
    _result :: [a]
    as :: [a]
    a :: a
    [-1: qsort.hs:3:24-38] *Main> :force as
    *** Exception: Prelude.undefined
    [-1: qsort.hs:3:24-38] *Main> :print as
    as = 'b' : 'c' : (_t1::[Char])

The exception itself is bound to a new variable, ``_exception``.

Breaking on exceptions is particularly useful for finding out what your
program was doing when it was in an infinite loop. Just hit Control-C,
and examine the history to find out what was going on.

.. ghc-flag:: -fbreak-on-exception
    :shortdesc: :ref:`Break on any exception thrown <ghci-debugger-exceptions>`
    :type: dynamic
    :reverse: -fno-break-on-exception
    :category:

    Causes GHCi to halt evaluation and return to the interactive prompt
    in the event of an exception. :ghc-flag:`-fbreak-on-exception` breaks
    on all exceptions.

.. ghc-flag:: -fbreak-on-error
    :shortdesc: :ref:`Break on uncaught exceptions and errors
        <ghci-debugger-exceptions>`
    :type: dynamic
    :reverse: -fno-break-on-error
    :category:

    Causes GHCi to halt evaluation and return to the interactive prompt in the
    event of an exception.  :ghc-flag:`-fbreak-on-error` breaks on only those
    exceptions which would otherwise be uncaught.


Example: inspecting functions
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to use the debugger to examine function values. When we
are at a breakpoint and a function is in scope, the debugger cannot show
you the source code for it; however, it is possible to get some
information by applying it to some arguments and observing the result.

The process is slightly complicated when the binding is polymorphic. We
show the process by means of an example. To keep things simple, we will
use the well known ``map`` function:

::

    import Prelude hiding (map)

    map :: (a->b) -> [a] -> [b]
    map f [] = []
    map f (x:xs) = f x : map f xs

We set a breakpoint on ``map``, and call it.

.. code-block:: none

    *Main> :break 5
    Breakpoint 0 activated at  map.hs:5:15-28
    *Main> map Just [1..5]
    Stopped at map.hs:(4,0)-(5,12)
    _result :: [b]
    x :: a
    f :: a -> b
    xs :: [a]

GHCi tells us that, among other bindings, ``f`` is in scope. However,
its type is not fully known yet, and thus it is not possible to apply it
to any arguments. Nevertheless, observe that the type of its first
argument is the same as the type of ``x``, and its result type is shared
with ``_result``.

As we demonstrated earlier (:ref:`breakpoints`), the debugger has some
intelligence built-in to update the type of ``f`` whenever the types of
``x`` or ``_result`` are discovered. So what we do in this scenario is
force ``x`` a bit, in order to recover both its type and the argument
part of ``f``.

.. code-block:: none

    *Main> seq x ()
    *Main> :print x
    x = 1

We can check now that as expected, the type of ``x`` has been
reconstructed, and with it the type of ``f`` has been too:

.. code-block:: none

    *Main> :t x
    x :: Integer
    *Main> :t f
    f :: Integer -> b

From here, we can apply f to any argument of type Integer and observe
the results.

.. code-block:: none

    *Main> let b = f 10
    *Main> :t b
    b :: b
    *Main> b
    <interactive>:1:0:
        Ambiguous type variable `b' in the constraint:
          `Show b' arising from a use of `print' at <interactive>:1:0
    *Main> :p b
    b = (_t2::a)
    *Main> seq b ()
    ()
    *Main> :t b
    b :: a
    *Main> :p b
    b = Just 10
    *Main> :t b
    b :: Maybe Integer
    *Main> :t f
    f :: Integer -> Maybe Integer
    *Main> f 20
    Just 20
    *Main> map f [1..5]
    [Just 1, Just 2, Just 3, Just 4, Just 5]

In the first application of ``f``, we had to do some more type
reconstruction in order to recover the result type of ``f``. But after
that, we are free to use ``f`` normally.

Limitations
~~~~~~~~~~~

-  When stopped at a breakpoint, if you try to evaluate a variable that
   is already under evaluation, the second evaluation will hang. The
   reason is that GHC knows the variable is under evaluation, so the new
   evaluation just waits for the result before continuing, but of course
   this isn't going to happen because the first evaluation is stopped at
   a breakpoint. Control-C can interrupt the hung evaluation and return
   to the prompt.

   The most common way this can happen is when you're evaluating a CAF
   (e.g. main), stop at a breakpoint, and ask for the value of the CAF
   at the prompt again.

-  Implicit parameters (see :ref:`implicit-parameters`) are only
   available at the scope of a breakpoint if there is an explicit type
   signature.

.. _ghci-invocation:

Invoking GHCi
-------------

.. index::
   single: invoking; GHCi
   single: --interactive

GHCi is invoked with the command ``ghci`` or ``ghc --interactive``. One
or more modules or filenames can also be specified on the command line;
this instructs GHCi to load the specified modules or filenames (and all
the modules they depend on), just as if you had said ``:load modules``
at the GHCi prompt (see :ref:`ghci-commands`). For example, to start
GHCi and load the program whose topmost module is in the file
``Main.hs``, we could say:

.. code-block:: none

    $ ghci Main.hs

Most of the command-line options accepted by GHC (see :ref:`using-ghc`)
also make sense in interactive mode. The ones that don't make sense are
mostly obvious.

.. ghc-flag:: -flocal-ghci-history
    :shortdesc: Use current directory for the GHCi command history
        file ``.ghci-history``.
    :type: dynamic
    :reverse: -fno-local-ghci-history
    :category:

    By default, GHCi keeps global history in ``~/.ghc/ghci_history`` or
    ``%APPDATA%/<app>/ghci_history``, but you can use current directory, e.g.:

    .. code-block:: none

        $ ghci -flocal-ghci-history

    It will create ``.ghci-history`` in current folder where GHCi is launched.

.. ghc-flag:: -fghci-leak-check
    :shortdesc: (Debugging only) check for space leaks when loading
                new modules in GHCi.
    :type: dynamic
    :reverse: -fno-ghci-leak-check
    :category:

    (Debugging only) When loading new modules with ``:load``, check
    that any previously loaded modules have been correctly garbage
    collected. Emits messages if a leak is detected.

Packages
~~~~~~~~

.. index::
   single: packages; with GHCi

Most packages (see :ref:`using-packages`) are available without needing
to specify any extra flags at all: they will be automatically loaded the
first time they are needed.

For hidden packages, however, you need to request the package be loaded
by using the :ghc-flag:`-package ⟨pkg⟩` flag:

.. code-block:: none

    $ ghci -package readline
    GHCi, version 8.y.z: http://www.haskell.org/ghc/  :? for help
    Loading package base ... linking ... done.
    Loading package readline-1.0 ... linking ... done.
    Prelude>

The following command works to load new packages into a running GHCi:

.. code-block:: none

    Prelude> :set -package name

But note that doing this will cause all currently loaded modules to be
unloaded, and you'll be dumped back into the ``Prelude``.

Extra libraries
~~~~~~~~~~~~~~~

.. index::
   single: libraries; with GHCi

Extra libraries may be specified on the command line using the normal
``-llib`` option. (The term *library* here refers to libraries of
foreign object code; for using libraries of Haskell source code, see
:ref:`ghci-modules-filenames`.) For example, to load the “m” library:

.. code-block:: none

    $ ghci -lm

On systems with ``.so``-style shared libraries, the actual library
loaded will the ``liblib.so``. GHCi searches the following places for
libraries, in this order:

-  Paths specified using the :ghc-flag:`-L ⟨dir⟩` command-line option,

-  The standard library search path for your system loader, which on some
   systems may be overridden by setting the :envvar:`LD_LIBRARY_PATH`
   environment variable.

-  The linker standard library search can also be overriden on some systems using
   the :envvar:`LIBRARY_PATH` environment variable. Because of some
   implementation detail on Windows, setting ``LIBRARY_PATH`` will also extend
   the system loader path for any library it finds. So often setting
   :envvar:`LIBRARY_PATH` is enough.

On systems with ``.dll``-style shared libraries, the actual library
loaded will be ``lib.dll``, ``liblib.dll``. GHCi also has full support for
import libraries, either Microsoft style ``.lib``, or GNU GCC style ``.a`` and
``.dll.a`` libraries. If you have an import library it is advisable to always
specify the import libary instead of the ``.dll``. e.g. use ``-lgcc` instead of
``-llibgcc_s_seh-1``. Again, GHCi will signal an error if it can't find the
library.

GHCi can also load plain object files (``.o`` or ``.obj`` depending on
your platform) or static archives (``.a``) from the command-line. Just add the
name the object file or library to the command line.
On Windows GHCi also supports the ``big-obj`` format.

Ordering of ``-l`` options matters: a library should be mentioned
*before* the libraries it depends on (see :ref:`options-linker`).

.. _ghci-commands:

GHCi commands
-------------

GHCi commands all begin with "``:``" and consist of a single command
name followed by zero or more parameters. The command name may be
abbreviated, with ambiguities being resolved in favour of the more
commonly used commands.

.. comment

    This section makes use of the GHC-specific :directive:`ghci-cmd` directive
    defined in :file:`conf.py`. This is used to describe and cross-reference GHCi
    commands.


.. ghci-cmd:: :abandon

    Abandons the current evaluation (only available when stopped at a
    breakpoint).

.. ghci-cmd:: :add;[*] ⟨module⟩

    Add ⟨module⟩(s) to the current target set, and perform a reload.
    Normally pre-compiled code for the module will be loaded if
    available, or otherwise the module will be compiled to byte-code.
    Using the ``*`` prefix forces the module to be loaded as byte-code.

    ⟨module⟩ may be a file path. A "``~``" symbol at the beginning of
    ⟨module⟩  will be replaced by the contents of the environment variable
    :envvar:`HOME`.

.. ghci-cmd:: :all-types

    List all types collected for expressions and (local) bindings
    currently loaded (while :ghci-cmd:`:set +c` was active) with their respective
    source-code span, e.g. ::

       GhciTypes> :all-types
       GhciTypes.hs:(38,13)-(38,24): Maybe Id
       GhciTypes.hs:(45,10)-(45,29): Outputable SpanInfo
       GhciTypes.hs:(45,10)-(45,29): (Rational -> SpanInfo -> SDoc) -> Outputable SpanInfo

.. ghci-cmd:: :back; ⟨n⟩

    Travel back ⟨n⟩ steps in the history. ⟨n⟩ is one if omitted. See
    :ref:`tracing` for more about GHCi's debugging facilities. See also:
    :ghci-cmd:`:trace`, :ghci-cmd:`:history`, :ghci-cmd:`:forward`.

.. ghci-cmd:: :break; [⟨identifier⟩ | [⟨module⟩] ⟨line⟩ [⟨column⟩]]

    Set a breakpoint on the specified function or line and column. See
    :ref:`setting-breakpoints`.

.. ghci-cmd:: :browse;[!] [[*] ⟨module⟩]

    Displays the identifiers exported by the module ⟨module⟩, which must
    be either loaded into GHCi or be a member of a package. If ⟨module⟩
    is omitted, the most recently-loaded module is used.

    Like all other GHCi commands, the output is always displayed in the
    current GHCi scope (:ref:`ghci-scope`).

    There are two variants of the browse command:

    -  If the ``*`` symbol is placed before the module name, then *all*
       the identifiers in scope in ⟨module⟩ (rather that just its
       exports) are shown.

       The ``*``-form is only available for modules which are
       interpreted; for compiled modules (including modules from
       packages) only the non-\ ``*`` form of :ghci-cmd:`:browse` is available.

    -  Data constructors and class methods are usually displayed in the
       context of their data type or class declaration. However, if the
       ``!`` symbol is appended to the command, thus ``:browse!``, they
       are listed individually. The ``!``-form also annotates the
       listing with comments giving possible imports for each group of
       entries. Here is an example: ::

           Prelude> :browse! Data.Maybe
           -- not currently imported
           Data.Maybe.catMaybes :: [Maybe a] -> [a]
           Data.Maybe.fromJust :: Maybe a -> a
           Data.Maybe.fromMaybe :: a -> Maybe a -> a
           Data.Maybe.isJust :: Maybe a -> Bool
           Data.Maybe.isNothing :: Maybe a -> Bool
           Data.Maybe.listToMaybe :: [a] -> Maybe a
           Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b]
           Data.Maybe.maybeToList :: Maybe a -> [a]
           -- imported via Prelude
           Just :: a -> Maybe a
           data Maybe a = Nothing | Just a
           Nothing :: Maybe a
           maybe :: b -> (a -> b) -> Maybe a -> b

       This output shows that, in the context of the current session (ie
       in the scope of ``Prelude``), the first group of items from
       ``Data.Maybe`` are not in scope (althought they are available in
       fully qualified form in the GHCi session - see
       :ref:`ghci-scope`), whereas the second group of items are in
       scope (via ``Prelude``) and are therefore available either
       unqualified, or with a ``Prelude.`` qualifier.

.. ghci-cmd:: :cd; ⟨dir⟩

    Changes the current working directory to ⟨dir⟩. A "``~``" symbol
    at the beginning of ⟨dir⟩ will be replaced by the contents of the
    environment variable :envvar:`HOME`. See also the :ghci-cmd:`:show paths`
    command for showing the current working directory.

    Note: changing directories causes all currently loaded modules to be
    unloaded. This is because the search path is usually expressed using
    relative directories, and changing the search path in the middle of
    a session is not supported.

.. ghci-cmd:: :cmd; ⟨expr⟩

    Executes ⟨expr⟩ as a computation of type ``IO String``, and then
    executes the resulting string as a list of GHCi commands. Multiple
    commands are separated by newlines. The :ghci-cmd:`:cmd` command is useful
    with :ghci-cmd:`:def` and :ghci-cmd:`:set stop`.

.. ghci-cmd:: :complete; ⟨type⟩ [⟨n⟩-][⟨m⟩] ⟨string-literal⟩

    This command allows to request command completions from GHCi even
    when interacting over a pipe instead of a proper terminal and is
    designed for integrating GHCi's completion with text editors and
    IDEs.

    When called, :ghci-cmd:`:complete` prints the ⟨n⟩\ :sup:`th` to
    ⟨m⟩\ :sup:`th` completion candidates for the partial input
    ⟨string-literal⟩ for the completion domain denoted by ⟨type⟩.
    Currently, only the ``repl`` domain is supported which denotes the
    kind of completion that would be provided interactively by GHCi at
    the input prompt.

    If omitted, ⟨n⟩ and ⟨m⟩ default to the first or last available
    completion candidate respectively. If there are less candidates than
    requested via the range argument, ⟨n⟩ and ⟨m⟩ are implicitly capped
    to the number of available completion candidates.

    The output of :ghci-cmd:`:complete` begins with a header line containing
    three space-delimited fields:

    -  An integer denoting the number ``l`` of printed completions,
    -  an integer denoting the total number of completions available,
       and finally
    -  a string literal denoting a common prefix to be added to the
       returned completion candidates.

    The header line is followed by ⟨l⟩ lines each containing one
    completion candidate encoded as (quoted) string literal. Here are
    some example invocations showing the various cases:

    .. code-block:: none

        Prelude> :complete repl 0 ""
        0 470 ""
        Prelude> :complete repl 5 "import For"
        5 21 "import "
        "Foreign"
        "Foreign.C"
        "Foreign.C.Error"
        "Foreign.C.String"
        "Foreign.C.Types"
        Prelude> :complete repl 5-10 "import For"
        6 21 "import "
        "Foreign.C.Types"
        "Foreign.Concurrent"
        "Foreign.ForeignPtr"
        "Foreign.ForeignPtr.Safe"
        "Foreign.ForeignPtr.Unsafe"
        "Foreign.Marshal"
        Prelude> :complete repl 20- "import For"
        2 21 "import "
        "Foreign.StablePtr"
        "Foreign.Storable"
        Prelude> :complete repl "map"
        3 3 ""
        "map"
        "mapM"
        "mapM_"
        Prelude> :complete repl 5-10 "map"
        0 3 ""

.. ghci-cmd:: :continue

    Continue the current evaluation, when stopped at a breakpoint.

.. ghci-cmd:: :ctags; [⟨filename⟩]

    Generates a "tags" file for Vi-style editors (:ghci-cmd:`:ctags`) or
    Emacs-style editors (:ghci-cmd:`:etags`). If no filename is specified, the
    default ``tags`` or ``TAGS`` is used, respectively. Tags for all the
    functions, constructors and types in the currently loaded modules
    are created. All modules must be interpreted for these commands to
    work.

.. ghci-cmd:: :def;[!] ⟨name⟩ ⟨expr⟩

    :ghci-cmd:`:def` is used to define new commands, or macros, in GHCi. The
    command ``:def ⟨name⟩ ⟨expr⟩`` defines a new GHCi command ``:name``,
    implemented by the Haskell expression ⟨expr⟩, which must have type
    ``String -> IO String``. When ``:name args`` is typed at the prompt,
    GHCi will run the expression ``(name args)``, take the resulting
    ``String``, and feed it back into GHCi as a new sequence of
    commands. Separate commands in the result must be separated by
    "``\n``".

    That's all a little confusing, so here's a few examples. To start
    with, here's a new GHCi command which doesn't take any arguments or
    produce any results, it just outputs the current date and time:

    .. code-block:: none

        Prelude> let date _ = Data.Time.getZonedTime >>= print >> return ""
        Prelude> :def date date
        Prelude> :date
        2017-04-10 12:34:56.93213581 UTC

    Here's an example of a command that takes an argument. It's a
    re-implementation of :ghci-cmd:`:cd`:

    .. code-block:: none

        Prelude> let mycd d = System.Directory.setCurrentDirectory d >> return ""
        Prelude> :def mycd mycd
        Prelude> :mycd ..

    Or I could define a simple way to invoke "``ghc --make Main``"
    in the current directory:

    .. code-block:: none

        Prelude> :def make (\_ -> return ":! ghc --make Main")

    We can define a command that reads GHCi input from a file. This
    might be useful for creating a set of bindings that we want to
    repeatedly load into the GHCi session:

    .. code-block:: none

        Prelude> :def . readFile
        Prelude> :. cmds.ghci

    Notice that we named the command ``:.``, by analogy with the
    "``.``" Unix shell command that does the same thing.

    Typing ``:def`` on its own lists the currently-defined macros.
    Attempting to redefine an existing command name results in an error
    unless the ``:def!`` form is used, in which case the old command
    with that name is silently overwritten.

.. ghci-cmd:: :delete; * | ⟨num⟩ ...

    Delete one or more breakpoints by number (use :ghci-cmd:`:show breaks` to
    see the number of each breakpoint). The ``*`` form deletes all the
    breakpoints.

.. ghci-cmd:: :doc; ⟨name⟩

    (Experimental: This command will likely change significantly in GHC 8.8.)

    Displays the documentation for the given name. Currently the command is
    restricted to displaying the documentation directly on the declaration
    in question, ignoring documentation for arguments, constructors etc.

.. ghci-cmd:: :edit; ⟨file⟩

    Opens an editor to edit the file ⟨file⟩, or the most recently loaded
    module if ⟨file⟩ is omitted. If there were errors during the last
    loading, the cursor will be positioned at the line of the first
    error. The editor to invoke is taken from the :envvar:`EDITOR` environment
    variable, or a default editor on your system if :envvar:`EDITOR` is not
    set. You can change the editor using :ghci-cmd:`:set editor`.

.. ghci-cmd:: :etags

    See :ghci-cmd:`:ctags`.

.. ghci-cmd:: :force; ⟨identifier⟩ ...

    Prints the value of ⟨identifier⟩ in the same way as :ghci-cmd:`:print`.
    Unlike :ghci-cmd:`:print`, :ghci-cmd:`:force` evaluates each thunk that it
    encounters while traversing the value. This may cause exceptions or
    infinite loops, or further breakpoints (which are ignored, but
    displayed).

.. ghci-cmd:: :forward; ⟨n⟩

    Move forward ⟨n⟩ steps in the history. ⟨n⟩ is one if omitted. See
    :ref:`tracing` for more about GHCi's debugging facilities. See also:
    :ghci-cmd:`:trace`, :ghci-cmd:`:history`, :ghci-cmd:`:back`.

.. ghci-cmd:: :help
              :?

    Displays a list of the available commands.

.. ghci-cmd:: :

    .. index::
       pair: Repeating last command; in GHCi

    Repeat the previous command.

.. ghci-cmd:: :history; [num]

    Display the history of evaluation steps. With a number, displays
    that many steps (default: 20). For use with :ghci-cmd:`:trace`; see
    :ref:`tracing`. To set the number of history entries stored by GHCi,
    use the :ghc-flag:`-fghci-hist-size=⟨n⟩` flag.

.. ghci-cmd:: :info;[!] ⟨name⟩

    Displays information about the given name(s). For example, if ⟨name⟩
    is a class, then the class methods and their types will be printed;
    if ⟨name⟩ is a type constructor, then its definition will be
    printed; if ⟨name⟩ is a function, then its type will be printed. If
    ⟨name⟩ has been loaded from a source file, then GHCi will also
    display the location of its definition in the source.

    For types and classes, GHCi also summarises instances that mention
    them. To avoid showing irrelevant information, an instance is shown
    only if (a) its head mentions ⟨name⟩, and (b) all the other things
    mentioned in the instance are in scope (either qualified or
    otherwise) as a result of a :ghci-cmd:`:load` or :ghci-cmd:`:module`
    commands.

    The command ``:info!`` works in a similar fashion but it removes
    restriction (b), showing all instances that are in scope and mention
    ⟨name⟩ in their head.

.. ghci-cmd:: :issafe; [⟨module⟩]

    Displays Safe Haskell information about the given module (or the
    current module if omitted). This includes the trust type of the
    module and its containing package.

.. ghci-cmd:: :kind;[!] ⟨type⟩

    Infers and prints the kind of ⟨type⟩. The latter can be an arbitrary
    type expression, including a partial application of a type
    constructor, such as ``Either Int``. In fact, :ghci-cmd:`:kind` even allows
    you to write a partial application of a type synonym (usually
    disallowed), so that this works:

    .. code-block:: none

        ghci> type T a b = (a,b,a)
        ghci> :k T Int Bool
        T Int Bool :: *
        ghci> :k T
        T :: * -> * -> *
        ghci> :k T Int
        T Int :: * -> *

    If you specify the optional "``!``", GHC will in addition normalise
    the type by expanding out type synonyms and evaluating type-function
    applications, and display the normalised result.

.. ghci-cmd:: :list; ⟨identifier⟩

    Lists the source code around the definition of ⟨identifier⟩ or the
    current breakpoint if not given. This requires that the identifier
    be defined in an interpreted module. If your output device supports
    it, then GHCi will highlight the active subexpression in bold.

.. ghci-cmd:: :list [⟨module⟩]; ⟨line⟩

    Lists the source code around the given line number of ⟨module⟩. This
    requires that the module be interpreted. If your output device
    supports it, then GHCi will highlight the active subexpression in
    bold.

.. ghci-cmd:: :load;[!] [*]⟨module⟩

    Recursively loads the specified ⟨module⟩s, and all the modules they
    depend on. Here, each ⟨module⟩ must be a module name or filename,
    but may not be the name of a module in a package.

    All previously loaded modules, except package modules, are
    forgotten. The new set of modules is known as the target set. Note
    that :ghci-cmd:`:load` can be used without any arguments to unload all the
    currently loaded modules and bindings.

    Normally pre-compiled code for a module will be loaded if available,
    or otherwise the module will be compiled to byte-code. Using the
    ``*`` prefix forces a module to be loaded as byte-code.

    Adding the optional "``!``" turns type errors into warnings while
    loading. This allows to use the portions of the module that are
    correct, even if there are type errors in some definitions.
    Effectively, the "-fdefer-type-errors" flag is set before loading
    and unset after loading if the flag has not already been set before.
    See :ref:`defer-type-errors` for further motivation and details.

    After a :ghci-cmd:`:load` command, the current context is set to:

    -  ⟨module⟩, if it was loaded successfully, or

    -  the most recently successfully loaded module, if any other
       modules were loaded as a result of the current :ghci-cmd:`:load`, or

    -  ``Prelude`` otherwise.

.. ghci-cmd:: :loc-at; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ [⟨name⟩]

    Tries to find the definition site of the name at the given
    source-code span, e.g.:

    .. code-block:: none

        X> :loc-at X.hs 6 14 6 16 mu
        X.hs:(8,7)-(8,9)

    This command is useful when integrating GHCi with text editors and
    IDEs for providing a goto-definition facility.

    The ``:loc-at`` command requires :ghci-cmd:`:set +c` to be set.

.. ghci-cmd:: :main; ⟨arg1⟩ ... ⟨argn⟩

    When a program is compiled and executed, it can use the ``getArgs``
    function to access the command-line arguments. However, we cannot
    simply pass the arguments to the ``main`` function while we are
    testing in ghci, as the ``main`` function doesn't take its arguments
    directly.

    Instead, we can use the :ghci-cmd:`:main` command. This runs whatever
    ``main`` is in scope, with any arguments being treated the same as
    command-line arguments, e.g.:

    .. code-block:: none

        Prelude> main = System.Environment.getArgs >>= print
        Prelude> :main foo bar
        ["foo","bar"]

    We can also quote arguments which contains characters like spaces,
    and they are treated like Haskell strings, or we can just use
    Haskell list syntax:

    .. code-block:: none

        Prelude> :main foo "bar baz"
        ["foo","bar baz"]
        Prelude> :main ["foo", "bar baz"]
        ["foo","bar baz"]

    Finally, other functions can be called, either with the ``-main-is``
    flag or the :ghci-cmd:`:run` command:

    .. code-block:: none

        Prelude> foo = putStrLn "foo" >> System.Environment.getArgs >>= print
        Prelude> bar = putStrLn "bar" >> System.Environment.getArgs >>= print
        Prelude> :set -main-is foo
        Prelude> :main foo "bar baz"
        foo
        ["foo","bar baz"]
        Prelude> :run bar ["foo", "bar baz"]
        bar
        ["foo","bar baz"]

.. ghci-cmd:: :module; +|- [*]⟨mod1⟩ ...
.. ghci-cmd:: import; ⟨mod⟩

    Sets or modifies the current context for statements typed at the
    prompt. The form ``import mod`` is equivalent to ``:module +mod``.
    See :ref:`ghci-scope` for more details.

.. ghci-cmd:: :print; ⟨names⟩

    Prints a value without forcing its evaluation. :ghci-cmd:`:print` may be
    used on values whose types are unknown or partially known, which
    might be the case for local variables with polymorphic types at a
    breakpoint. While inspecting the runtime value, :ghci-cmd:`:print` attempts
    to reconstruct the type of the value, and will elaborate the type in
    GHCi's environment if possible. If any unevaluated components
    (thunks) are encountered, then :ghci-cmd:`:print` binds a fresh variable
    with a name beginning with ``_t`` to each thunk. See
    :ref:`breakpoints` for more information. See also the :ghci-cmd:`:sprint`
    command, which works like :ghci-cmd:`:print` but does not bind new
    variables.

.. ghci-cmd:: :quit

    Quits GHCi. You can also quit by typing :kbd:`Control-D` at the prompt.

.. ghci-cmd:: :reload;[!]

    Attempts to reload the current target set (see :ghci-cmd:`:load`) if any of
    the modules in the set, or any dependent module, has changed. Note
    that this may entail loading new modules, or dropping modules which
    are no longer indirectly required by the target.

    Adding the optional "``!``" turns type errors into warnings while
    loading. This allows to use the portions of the module that are
    correct, even if there are type errors in some definitions.
    Effectively, the "-fdefer-type-errors" flag is set before loading
    and unset after loading if the flag has not already been set before.
    See :ref:`defer-type-errors` for further motivation and details.

.. ghci-cmd:: :run

    See :ghci-cmd:`:main`.

.. ghci-cmd:: :script; [⟨n⟩] ⟨filename⟩

    Executes the lines of a file as a series of GHCi commands. This
    command is compatible with multiline statements as set by
    :ghci-cmd:`:set +m`

.. ghci-cmd:: :set; [⟨option⟩ ...]

    Sets various options. See :ref:`ghci-set` for a list of available
    options and :ref:`interactive-mode-options` for a list of
    GHCi-specific flags. The :ghci-cmd:`:set` command by itself shows which
    options are currently set. It also lists the current dynamic flag
    settings, with GHCi-specific flags listed separately.

.. ghci-cmd:: :set args; ⟨arg⟩

    .. index::
       single: getArgs, behavior in GHCi

    Sets the list of arguments which are returned when the program calls
    ``System.getArgs``.

.. ghci-cmd:: :set editor; ⟨cmd⟩

    Sets the command used by :ghci-cmd:`:edit` to ⟨cmd⟩.

.. ghci-cmd:: :set prog; ⟨prog⟩

    .. index::
       single: getProgName, behavior in GHCi

    Sets the string to be returned when the program calls
    ``System.getProgName``.

.. ghci-cmd:: :set prompt; ⟨prompt⟩

    .. index::
       single: GHCi prompt; setting

    Sets the string to be used as the prompt in GHCi. Inside ⟨prompt⟩,
    the next sequences are replaced:

    - ``%s`` by the names of the modules currently in scope.
    - ``%l`` by the line number (as referenced in compiler messages) of the
      current prompt.
    - ``%d`` by the date in "Weekday Month Date" format (e.g., "Tue May 26") .
    - ``%t`` by the current time in 24-hour HH:MM:SS format.
    - ``%T`` by the current time in 12-hour HH:MM:SS format.
    - ``%@`` by the current time in 12-hour am/pm format.
    - ``%A`` by the current time in 24-hour HH:MM format.
    - ``%u`` by the username of the current user.
    - ``%w`` by the current working directory.
    - ``%o`` by the operating system.
    - ``%a`` by the machine architecture.
    - ``%N`` by the compiler name.
    - ``%V`` by the compiler version.
    - ``%call(cmd [args])`` by the result of calling ``cmd args``.
    - ``%%`` by ``%``.

    If ⟨prompt⟩ starts with ``"`` then it is parsed as a Haskell String;
    otherwise it is treated as a literal string.

.. ghci-cmd:: :set prompt-cont; ⟨prompt⟩

    Sets the string to be used as the continuation prompt (used when
    using the :ghci-cmd:`:{` command) in GHCi.

.. ghci-cmd:: :set prompt-function; ⟨prompt-function⟩

    .. index::
       single: GHCi prompt function; setting

    Sets the function to be used for the prompt displaying in GHCi. The
    function should be of the type ``[String] -> Int -> IO String``. This
    function is called each time the prompt is being made. The first argument
    stands for the names of the modules currently in scope(the name of the
    "topmost" module  will begin with a ``*``; see  :ref:`ghci-scope` for
    more information). The second arguments is the line number (as referenced
    in compiler  messages) of the current prompt.

.. ghci-cmd:: :set prompt-cont-function; ⟨prompt-function⟩

   Sets the function to be used for the continuation prompt (used when
   using the :ghci-cmd:`:{` command) displaying in GHCi.

.. ghci-cmd:: :set stop; ⟨num⟩ ⟨cmd⟩

    Set a command to be executed when a breakpoint is hit, or a new item
    in the history is selected. The most common use of :ghci-cmd:`:set stop` is
    to display the source code at the current location, e.g.
    ``:set stop :list``.

    If a number is given before the command, then the commands are run
    when the specified breakpoint (only) is hit. This can be quite
    useful: for example, ``:set stop 1 :continue`` effectively disables
    breakpoint 1, by running :ghci-cmd:`:continue` whenever it is hit (although
    GHCi will still emit a message to say the breakpoint was hit). What's more,
    with cunning use of :ghci-cmd:`:def` and :ghci-cmd:`:cmd` you can use
    :ghci-cmd:`:set stop` to implement conditional breakpoints:

    .. code-block:: none

        *Main> :def cond \expr -> return (":cmd if (" ++ expr ++ ") then return \"\" else return \":continue\"")
        *Main> :set stop 0 :cond (x < 3)

    Ignoring breakpoints for a specified number of iterations is also
    possible using similar techniques.

.. ghci-cmd:: :seti; [⟨option⟩ ...]

    Like :ghci-cmd:`:set`, but options set with :ghci-cmd:`:seti` affect only
    expressions and commands typed at the prompt, and not modules loaded
    with :ghci-cmd:`:load` (in contrast, options set with :ghci-cmd:`:set` apply
    everywhere). See :ref:`ghci-interactive-options`.

    Without any arguments, displays the current set of options that are
    applied to expressions and commands typed at the prompt.

.. ghci-cmd:: :show bindings

    Show the bindings made at the prompt and their types.

.. ghci-cmd:: :show breaks

    List the active breakpoints.

.. ghci-cmd:: :show context

    List the active evaluations that are stopped at breakpoints.

.. ghci-cmd:: :show imports

    Show the imports that are currently in force, as created by
    ``import`` and :ghci-cmd:`:module` commands.

.. ghci-cmd:: :show modules

    Show the list of modules currently loaded.

.. ghci-cmd:: :show packages

    Show the currently active package flags, as well as the list of
    packages currently loaded.

.. ghci-cmd:: :show paths

    Show the current working directory (as set via :ghci-cmd:`:cd` command), as
    well as the list of directories searched for source files (as set by the
    ``-i`` option).

.. ghci-cmd:: :show language

    Show the currently active language flags for source files.

.. ghci-cmd:: :showi language

    Show the currently active language flags for expressions typed at
    the prompt (see also :ghci-cmd:`:seti`).

.. ghci-cmd:: :show; [args|prog|prompt|editor|stop]

    Displays the specified setting (see :ghci-cmd:`:set`).

.. ghci-cmd:: :sprint; ⟨expr⟩

    Prints a value without forcing its evaluation. :ghci-cmd:`:sprint` is
    similar to :ghci-cmd:`:print`, with the difference that unevaluated subterms
    are not bound to new variables, they are simply denoted by ``_``.

.. ghci-cmd:: :step; [⟨expr⟩]

    Enable all breakpoints and begin evaluating an expression in
    single-stepping mode. In this mode evaluation will be stopped after
    every reduction, allowing local variables to be inspected. If ⟨expr⟩
    is not given, evaluation will resume at the last breakpoint. See
    :ref:`single-stepping`.

.. ghci-cmd:: :steplocal

    Enable only breakpoints in the current top-level binding and resume
    evaluation at the last breakpoint.

.. ghci-cmd:: :stepmodule

    Enable only breakpoints in the current module and resume evaluation
    at the last breakpoint.

.. ghci-cmd:: :trace; ⟨expr⟩

    Evaluates the given expression (or from the last breakpoint if no
    expression is given), and additionally logs the evaluation steps for
    later inspection using :ghci-cmd:`:history`. See :ref:`tracing`.

.. ghci-cmd:: :type; ⟨expression⟩

    Infers and prints the type of ⟨expression⟩, including explicit
    forall quantifiers for polymorphic types.
    The type reported is the type that would be inferred
    for a variable assigned to the expression, but without the
    monomorphism restriction applied.

    .. code-block:: none

	*X> :type length
	length :: Foldable t => t a -> Int

.. ghci-cmd:: :type +v; ⟨expression⟩

    Infers and prints the type of ⟨expression⟩, but without fiddling
    with type variables or class constraints. This is useful when you
    are using :extension:`TypeApplications` and care about the distinction
    between specified type variables (available for type application)
    and inferred type variables (not available). This mode sometimes prints
    constraints (such as ``Show Int``) that could readily be solved, but
    solving these constraints may affect the type variables, so GHC refrains.

    .. code-block:: none

	*X> :set -fprint-explicit-foralls
	*X> :type +v length
	length :: forall (t :: * -> *). Foldable t => forall a. t a -> Int

.. ghci-cmd:: :type +d; ⟨expression⟩

    Infers and prints the type of ⟨expression⟩, defaulting type variables
    if possible. In this mode, if the inferred type is constrained by
    any interactive class (``Num``, ``Show``, ``Eq``, ``Ord``, ``Foldable``,
    or ``Traversable``), the constrained type variable(s) are defaulted
    according to the rules described under :extension:`ExtendedDefaultRules`.
    This mode is quite useful when the inferred type is quite general (such
    as for ``foldr``) and it may be helpful to see a more concrete
    instantiation.

    .. code-block:: none

	*X> :type +d length
	length :: [a] -> Int

.. ghci-cmd:: :type-at; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ [⟨name⟩]

    Reports the inferred type at the given span/position in the module, e.g.:

    .. code-block:: none

       *X> :type-at X.hs 6 6 6 7 f
       Int -> Int

    This command is useful when integrating GHCi with text editors and
    IDEs for providing a show-type-under-point facility.

    The last string parameter is useful for when the span is out of
    date, i.e. the file changed and the code has moved. In which case
    :ghci-cmd:`:type-at` falls back to a general :ghci-cmd:`:type` like lookup.

    The :ghci-cmd:`:type-at` command requires :ghci-cmd:`:set +c` to be set.

.. ghci-cmd:: :undef; ⟨name⟩

    Undefines the user-defined command ⟨name⟩ (see :ghci-cmd:`:def` above).

.. ghci-cmd:: :unset; ⟨option⟩

    Unsets certain options. See :ref:`ghci-set` for a list of available
    options.

.. ghci-cmd:: :uses; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ [⟨name⟩]

    Reports all module-local uses of the thing at the given position
    in the module, e.g.:

    .. code-block:: none

       :uses GhciFind.hs 53 66 53 70 name
       GhciFind.hs:(46,25)-(46,29)
       GhciFind.hs:(47,37)-(47,41)
       GhciFind.hs:(53,66)-(53,70)
       GhciFind.hs:(57,62)-(57,66)

    This command is useful for highlighting and navigating all uses of
    an identifier in editors and IDEs.

    The :ghci-cmd:`:uses` command requires :ghci-cmd:`:set +c` to be set.

.. ghci-cmd:: :! ⟨command⟩

    .. index::
       single: shell commands; in GHCi

    Executes the shell command ⟨command⟩.


.. _ghci-set:

The ``:set`` and ``:seti`` commands
-----------------------------------

.. index::
   single: :set; command in GHCi
   single: :seti

The :ghci-cmd:`:set` command sets two types of options: GHCi options, which
begin with "``+``", and "command-line" options, which begin with "``-``".

.. note::
    At the moment, the :ghci-cmd:`:set` command doesn't support any kind of
    quoting in its arguments: quotes will not be removed and cannot be used
    to group words together. For example, ``:set -DFOO='BAR BAZ'`` will not
    do what you expect.

GHCi options
~~~~~~~~~~~~

.. index::
   single: options; GHCi

GHCi options may be set using :ghci-cmd:`:set` and unset using :ghci-cmd:`:unset`.

The available GHCi options are:

.. ghci-cmd:: :set +c

    Collect type and location information after loading modules.
    The commands :ghci-cmd:`:all-types`, :ghci-cmd:`:loc-at`,
    :ghci-cmd:`:type-at`, and :ghci-cmd:`:uses` require ``+c`` to be active.

.. ghci-cmd:: :set +m

    .. index::
       single: multiline input; in GHCi

    Enable parsing of multiline commands. A multiline command is
    prompted for when the current input line contains open layout
    contexts (see :ref:`ghci-multiline`).

.. ghci-cmd:: :set +r

    .. index::
       single: CAFs; in GHCi
       single: Constant Applicative Form

    Normally, any evaluation of top-level expressions (otherwise known
    as CAFs or Constant Applicative Forms) in loaded modules is retained
    between evaluations. Turning on ``+r`` causes all evaluation of
    top-level expressions to be discarded after each evaluation (they
    are still retained *during* a single evaluation).

    This option may help if the evaluated top-level expressions are
    consuming large amounts of space, or if you need repeatable
    performance measurements.

.. ghci-cmd:: :set +s

    Display some stats after evaluating each expression, including the
    elapsed time and number of bytes allocated. NOTE: the allocation
    figure is only accurate to the size of the storage manager's
    allocation area, because it is calculated at every GC. Hence, you
    might see values of zero if no GC has occurred.

.. ghci-cmd:: :set +t

    .. index::
       single: displaying type; in GHCi

    Display the type of each variable bound after a statement is entered
    at the prompt. If the statement is a single expression, then the
    only variable binding will be for the variable ``it``.

.. _ghci-cmd-line-options:

Setting GHC command-line options in GHCi
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Normal GHC command-line options may also be set using :ghci-cmd:`:set`. For
example, to turn on :ghc-flag:`-Wmissing-signatures`, you would say:

.. code-block:: none

    Prelude> :set -Wmissing-signatures

Any GHC command-line option that is designated as dynamic (see the table
in :ref:`flag-reference`), may be set using :ghci-cmd:`:set`. To unset an
option, you can set the reverse option:

.. index::
   single: dynamic; options

.. code-block:: none

    Prelude> :set -Wno-incomplete-patterns -XNoMultiParamTypeClasses

:ref:`flag-reference` lists the reverse for each option where
applicable.

Certain static options (:ghc-flag:`-package ⟨pkg⟩`, :ghc-flag:`-I⟨dir⟩`,
:ghc-flag:`-i⟨dir⟩[:⟨dir⟩]*`, and :ghc-flag:`-l ⟨lib⟩` in particular) will also
work, but some may not take effect until the next reload.

.. index::
   single: static; options

.. _ghci-interactive-options:

Setting options for interactive evaluation only
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHCi actually maintains *two* sets of options:

-  The *loading options* apply when loading modules

-  The *interactive options* apply when evaluating expressions and
   commands typed at the GHCi prompt.

The :ghci-cmd:`:set` command modifies both, but there is also a
:ghci-cmd:`:seti` command (for "set interactive") that affects only the
interactive options set.

It is often useful to change the interactive options, without having
that option apply to loaded modules too. For example

.. code-block:: none

    :seti -XMonoLocalBinds

It would be undesirable if :extension:`MonoLocalBinds` were to apply to loaded
modules too: that might cause a compilation error, but more commonly it
will cause extra recompilation, because GHC will think that it needs to
recompile the module because the flags have changed.

If you are setting language options in your ``.ghci`` file, it is good
practice to use :ghci-cmd:`:seti` rather than :ghci-cmd:`:set`, unless you
really do want them to apply to all modules you load in GHCi.

The two sets of options can be inspected using the :ghci-cmd:`:set` and
:ghci-cmd:`:seti` commands respectively, with no arguments. For example, in a
clean GHCi session we might see something like this:

.. code-block:: none

    Prelude> :seti
    base language is: Haskell2010
    with the following modifiers:
      -XNoMonomorphismRestriction
      -XNoDatatypeContexts
      -XNondecreasingIndentation
      -XExtendedDefaultRules
    GHCi-specific dynamic flag settings:
    other dynamic, non-language, flag settings:
      -fimplicit-import-qualified
    warning settings:

The two sets of options are initialised as follows. First, both sets of
options are initialised as described in :ref:`ghci-dot-files`. Then the
interactive options are modified as follows:

-  The option ``-XExtendedDefaultRules`` is enabled, in order to apply
   special defaulting rules to expressions typed at the prompt (see
   :ref:`extended-default-rules`).

-  The Monomorphism Restriction is disabled (see :ref:`monomorphism`).

.. _ghci-dot-files:

The ``.ghci`` and ``.haskeline`` files
--------------------------------------

.. _dot-ghci-files:

The ``.ghci`` files
~~~~~~~~~~~~~~~~~~~

.. index::
   single: .ghci; file
   single: startup; files, GHCi

When it starts, unless the :ghc-flag:`-ignore-dot-ghci` flag is given, GHCi
reads and executes commands from the following files, in this order, if
they exist:

1. :file:`./.ghci`

2. :file:`{appdata}/ghc/ghci.conf`, where ⟨appdata⟩ depends on your system,
   but is usually something like
   :file:`C:/Documents and Settings/user/Application Data`

3. On Unix: :file:`$HOME/.ghc/ghci.conf`

4. :file:`$HOME/.ghci`

The :file:`ghci.conf` file is most useful for turning on favourite options
(e.g. ``:set +s``), and defining useful macros.

.. note::
    When setting language options in this file it is usually desirable to use
    :ghci-cmd:`:seti` rather than :ghci-cmd:`:set` (see :ref:`ghci-interactive-options`).

Placing a :file:`.ghci` file in a directory with a Haskell project is a
useful way to set certain project-wide options so you don't have to type
them every time you start GHCi: eg. if your project uses multi-parameter
type classes, scoped type variables, and CPP, and has source files in
three subdirectories A, B and C, you might put the following lines in
:file:`.ghci`:

.. code-block:: none

    :set -XMultiParamTypeClasses -XScopedTypeVariables -cpp
    :set -iA:B:C

(Note that strictly speaking the :ghc-flag:`-i` flag is a static one, but in
fact it works to set it using :ghci-cmd:`:set` like this. The changes won't take
effect until the next :ghci-cmd:`:load`, though.)

Once you have a library of GHCi macros, you may want to source them from
separate files, or you may want to source your ``.ghci`` file into your
running GHCi session while debugging it

.. code-block:: none

    :def source readFile

With this macro defined in your ``.ghci`` file, you can use
``:source file`` to read GHCi commands from ``file``. You can find (and
contribute!-) other suggestions for ``.ghci`` files on this Haskell wiki
page: `GHC/GHCi <http://haskell.org/haskellwiki/GHC/GHCi>`__

Additionally, any files specified with :ghc-flag:`-ghci-script` flags will be
read after the standard files, allowing the use of custom .ghci files.

Two command-line options control whether the startup files files are
read:

.. ghc-flag:: -ignore-dot-ghci
    :shortdesc: Disable reading of ``.ghci`` files
    :type: dynamic
    :category:

    Don't read either :file:`./.ghci` or the other startup files when
    starting up.

.. ghc-flag:: -ghci-script
    :shortdesc: Read additional ``.ghci`` files
    :type: dynamic
    :category:

    Read a specific file after the usual startup files. Maybe be
    specified repeatedly for multiple inputs.

When defining GHCi macros, there is some important behavior you should
be aware of when names may conflict with built-in commands, especially
regarding tab completion.

For example, consider if you had a macro named ``:time`` and in the
shell, typed ``:t 3`` — what should happen? The current algorithm we use
for completing commands is:

1. First, look up an exact match on the name from the defined macros.

2. Look for the exact match on the name in the built-in command list.

3. Do a prefix lookup on the list of built-in commands - if a built-in
   command matches, but a macro is defined with the same name as the
   built-in defined, pick the macro.

4. Do a prefix lookup on the list of built-in commands.

5. Do a prefix lookup on the list of defined macros.

Here are some examples:

1. You have a macro ``:time`` and enter ``:t 3``

   You get ``:type 3``

2. You have a macro ``:type`` and enter ``:t 3``

   You get ``:type 3`` with your defined macro, not the builtin.

3. You have a macro ``:time`` and a macro ``:type``, and enter ``:t 3``

   You get ``:type 3`` with your defined macro.

.. _dot-haskeline-file:

The ``.haskeline`` file
~~~~~~~~~~~~~~~~~~~~~~~

.. index::
   single: .haskeline; file
   single: startup; files, GHCi

GHCi uses `Haskeline <https://hackage.haskell.org/package/haskeline>`__ under
the hood. You can configure it to, among other
things, prune duplicates from GHCi history. See:
`Haskeline user preferences <https://github.com/judah/haskeline/wiki/UserPreferences>`__.

.. _ghci-obj:

Compiling to object code inside GHCi
------------------------------------

By default, GHCi compiles Haskell source code into byte-code that is
interpreted by the runtime system. GHCi can also compile Haskell code to
object code: to turn on this feature, use the :ghc-flag:`-fobject-code` flag
either on the command line or with :ghci-cmd:`:set` (the option :ghc-flag:`-fbyte-code`
restores byte-code compilation again). Compiling to object code takes
longer, but typically the code will execute 10-20 times faster than
byte-code.

Compiling to object code inside GHCi is particularly useful if you are
developing a compiled application, because the :ghci-cmd:`:reload` command
typically runs much faster than restarting GHC with :ghc-flag:`--make` from the
command-line, because all the interface files are already cached in
memory.

There are disadvantages to compiling to object-code: you can't set
breakpoints in object-code modules, for example. Only the exports of an
object-code module will be visible in GHCi, rather than all top-level
bindings as in interpreted modules.

.. _external-interpreter:

Running the interpreter in a separate process
---------------------------------------------

Normally GHCi runs the interpreted code in the same process as GHC
itself, on top of the same RTS and sharing the same heap.  However, if
the flag :ghc-flag:`-fexternal-interpreter` is given, then GHC will spawn a
separate process for running interpreted code, and communicate with it
using messages over a pipe.

.. ghc-flag:: -fexternal-interpreter
    :shortdesc: Run interpreted code in a separate process
    :type: dynamic
    :category: misc

    :since: 8.0.1

    Run interpreted code (for GHCi, Template Haskell, Quasi-quoting,
    or Annotations) in a separate process.  The interpreter will run
    in profiling mode if :ghc-flag:`-prof` is in effect, and in
    dynamically-linked mode if :ghc-flag:`-dynamic` is in effect.

    There are a couple of caveats that will hopefully be removed in
    the future: this option is currently not implemented on Windows
    (it is a no-op), and the external interpreter does not support the
    GHCi debugger, so breakpoints and single-stepping don't work with
    :ghc-flag:`-fexternal-interpreter`.

    See also the :ghc-flag:`-pgmi ⟨cmd⟩` (:ref:`replacing-phases`) and
    :ghc-flag:`-opti ⟨option⟩` (:ref:`forcing-options-through`) flags.

Why might we want to do this?  The main reason is that the RTS running
the interpreted code can be a different flavour (profiling or
dynamically-linked) from GHC itself.  So for example:

- We can use the profiler to collect stack traces when using GHCi (see
  :ref:`ghci-stack-traces`).

- When compiling Template Haskell code with :ghc-flag:`-prof` we don't need to
  compile the modules without :ghc-flag:`-prof` first (see :ref:`th-profiling`)
  because we can run the profiled object code in the interpreter.

This feature is experimental in GHC 8.0.x, but it may become the
default in future releases.

.. _ghci-faq:

FAQ and Things To Watch Out For
-------------------------------

The interpreter can't load modules with foreign export declarations!
    Unfortunately not. We haven't implemented it yet. Please compile any
    offending modules by hand before loading them into GHCi.

:ghc-flag:`-O` doesn't work with GHCi!

    .. index::
       single: optimization; and GHCi

    For technical reasons, the bytecode compiler doesn't interact well
    with one of the optimisation passes, so we have disabled
    optimisation when using the interpreter. This isn't a great loss:
    you'll get a much bigger win by compiling the bits of your code that
    need to go fast, rather than interpreting them with optimisation
    turned on.

Unboxed tuples don't work with GHCi
    That's right. You can always compile a module that uses unboxed
    tuples and load it into GHCi, however. (Incidentally the previous
    point, namely that :ghc-flag:`-O` is incompatible with GHCi, is because the
    bytecode compiler can't deal with unboxed tuples).

Concurrent threads don't carry on running when GHCi is waiting for input.
    This should work, as long as your GHCi was built with the
    :ghc-flag:`-threaded` switch, which is the default. Consult whoever supplied
    your GHCi installation.


After using ``getContents``, I can't use ``stdin``, until I do ``:load`` or ``:reload``
    This is the defined behaviour of ``getContents``: it puts the stdin
    Handle in a state known as semi-closed, wherein any further I/O
    operations on it are forbidden. Because I/O state is retained
    between computations, the semi-closed state persists until the next
    :ghci-cmd:`:load` or :ghci-cmd:`:reload` command.

    You can make ``stdin`` reset itself after every evaluation by giving
    GHCi the command ``:set +r``. This works because ``stdin`` is just a
    top-level expression that can be reverted to its unevaluated state
    in the same way as any other top-level expression (CAF).

I can't use :kbd:`Control-C` to interrupt computations in GHCi on Windows.
    See :ref:`ghci-windows`.

The default buffering mode is different in GHCi to GHC.
    In GHC, the stdout handle is line-buffered by default. However, in
    GHCi we turn off the buffering on stdout, because this is normally
    what you want in an interpreter: output appears as it is generated.

    If you want line-buffered behaviour, as in GHC, you can start your
    program thus: ::

        main = do { hSetBuffering stdout LineBuffering; ... }


.. [5]
   Note that packages only contain compiled code, so debugging a package
   requires finding its source and loading that directly.

.. [6]
   We originally provided bindings for all variables in scope, rather
   than just the free variables of the expression, but found that this
   affected performance considerably, hence the current restriction to
   just the free variables.
