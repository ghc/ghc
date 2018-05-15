.. _win32:

Running GHC on Win32 systems
============================

.. _ghc-windows:

Starting GHC on Windows platforms
---------------------------------

The installer that installs GHC on Win32 also sets up the file-suffix
associations for ".hs" and ".lhs" files so that double-clicking them
starts ``ghci``.

Be aware of that ``ghc`` and ``ghci`` do require filenames containing
spaces to be escaped using quotes:

.. code-block:: none

    c:\ghc\bin\ghci "c:\\Program Files\\Haskell\\Project.hs"

If the quotes are left off in the above command, ``ghci`` will interpret
the filename as two, ``c:\\\\Program`` and
``Files\\\\Haskell\\\\Project.hs``.

.. _ghci-windows:

Running GHCi on Windows
-----------------------

We recommend running GHCi in a standard Windows console: select the
``GHCi`` option from the start menu item added by the GHC installer, or
use ``Start->Run->cmd`` to get a Windows console and invoke ``ghci``
from there (as long as it's in your ``PATH``).

If you run GHCi in a Cygwin or MSYS shell, then the Control-C behaviour
is adversely affected. In one of these environments you should use the
``ghcii.sh`` script to start GHCi, otherwise when you hit Control-C
you'll be returned to the shell prompt but the GHCi process will still
be running. However, even using the ``ghcii.sh`` script, if you hit
Control-C then the GHCi process will be killed immediately, rather than
letting you interrupt a running program inside GHCi as it should. This
problem is caused by the fact that the Cygwin and MSYS shell
environments don't pass Control-C events to non-Cygwin child processes,
because in order to do that there needs to be a Windows console.

There's an exception: you can use a Cygwin shell if the ``CYGWIN``
environment variable does *not* contain ``tty``. In this mode, the
Cygwin shell behaves like a Windows console shell and console events are
propagated to child processes. Note that the ``CYGWIN`` environment
variable must be set *before* starting the Cygwin shell; changing it
afterwards has no effect on the shell.

This problem doesn't just affect GHCi, it affects any GHC-compiled
program that wants to catch console events. See the
:base-ref:`GHC.ConsoleHandler.` module.

.. _terminal-interaction:

Interacting with the terminal
-----------------------------

By default GHC builds applications that open a console window when they
start. If you want to build a GUI-only application, with no console
window, use the flag ``-optl-mwindows`` in the link step.

.. warning::
   Windows GUI-only programs have no stdin, stdout or stderr so
   using the ordinary Haskell input/output functions will cause your
   program to fail with an IO exception, such as:

   .. code-block:: none

        Fail: <stdout>: hPutChar: failed (Bad file descriptor)

   However using Debug.Trace.trace is alright because it uses Windows
   debugging output support rather than ``stderr``.

For some reason, Mingw ships with the ``readline`` library, but not with
the ``readline`` headers. As a result, GHC (like Hugs) does not use
``readline`` for interactive input on Windows. You can get a close
simulation by using an emacs shell buffer!

.. _library-differences:

Differences in library behaviour
--------------------------------

Some of the standard Haskell libraries behave slightly differently on
Windows.

-  On Windows, the ``^Z`` character is interpreted as an end-of-file
   character, so if you read a file containing this character the file
   will appear to end just before it. To avoid this, use
   ``IOExts.openFileEx`` to open a file in binary (untranslated) mode or
   change an already opened file handle into binary mode using
   ``IOExts.hSetBinaryMode``. The ``IOExts`` module is part of the
   ``lang`` package.

.. _windows-file-paths:

File paths under Windows
------------------------

Windows paths are not all the same. The different kinds of paths each have
different meanings. The ``MAX_PATH`` limitation is not a limitation of the operating
system nor the file system. It is a limitation of the default namespace enforced
by the Win32 API for backwards compatibility.

The NT kernel however allows you ways to opt out of this path preprocessing by
the Win32 APIs. This is done by explicitly using the desired namespace in the
path.

The namespaces are:

 - file namespace: ``\\?\``
 - device namespace: ``\\.\``
 - NT namespace: ``\``

Each of these turn off path processing completely by the Win32 API and the paths
are passed untouched to the filesystem.

Paths with a drive letter are *legacy* paths. The drive letters are actually
meaningless to the kernel. Just like Unix operating systems, drive letters are
just a mount point. You can view your mount points by using the :command:`mountvol`
command.

Since GHC 8.6.1, the Haskell I/O manager automatically promotes paths in the legacy
format to Win32 file namespace. By default the I/O manager will do two things to
your paths:

  - replace ``\`` with ``\\``
  - expand relative paths to absolute paths

If you want to opt out of all preprocessing just expliticly use namespaces in
your paths. Due to this change, if you need to open raw devices (e.g. COM ports)
you need to use the device namespace explicitly. (e.g. ``\\.\COM1``). GHC and
Haskell programs in general no longer support opening devices in the legacy
format.

See the
`Windows documentation <https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx>`_
for more details.


.. _ghci-cygwin:

Using GHC (and other GHC-compiled executables) with Cygwin
----------------------------------------------------------

Background
~~~~~~~~~~

The Cygwin tools aim to provide a Unix-style API on top of the windows
libraries, to facilitate ports of Unix software to windows. To this end,
they introduce a Unix-style directory hierarchy under some root
directory (typically ``/`` is ``C:\cygwin\``). Moreover, everything
built against the Cygwin API (including the Cygwin tools and programs
compiled with Cygwin's GHC) will see ``/`` as the root of their file system,
happily pretending to work in a typical unix environment, and finding
things like ``/bin`` and ``/usr/include`` without ever explicitly
bothering with their actual location on the windows system (probably
``C:\cygwin\bin`` and ``C:\cygwin\usr\include``).

The problem
~~~~~~~~~~~

GHC, by default, no longer depends on cygwin, but is a native Windows
program. It is built using mingw, and it uses mingw's GHC while
compiling your Haskell sources (even if you call it from cygwin's bash),
but what matters here is that - just like any other normal windows
program - neither GHC nor the executables it produces are aware of
Cygwin's pretended unix hierarchy. GHC will happily accept either ``/`` or
``\\`` as path separators, but it won't know where to find ``/home/joe/Main.hs``
or ``/bin/bash`` or the like. This causes all kinds of fun when GHC is used from
within Cygwin's bash, or in make-sessions running under Cygwin.

Things to do
~~~~~~~~~~~~

-  Don't use absolute paths in ``make``, ``configure`` & co if there is any
   chance that those might be passed to GHC (or to GHC-compiled
   programs). Relative paths are fine because cygwin tools are happy
   with them and GHC accepts ``/`` as path-separator. And relative paths
   don't depend on where Cygwin's root directory is located, or on which
   partition or network drive your source tree happens to reside, as
   long as you ``cd`` there first.

-  If you have to use absolute paths (beware of the innocent-looking
   ``ROOT=$(pwd)`` in makefile hierarchies or configure scripts), Cygwin
   provides a tool called ``cygpath`` that can convert Cygwin's
   Unix-style paths to their actual Windows-style counterparts. Many
   Cygwin tools actually accept absolute Windows-style paths (remember,
   though, that you either need to escape ``\\`` or convert ``\\`` to ``/``),
   so you should be fine just using those everywhere. If you need to use
   tools that do some kind of path-mangling that depends on unix-style
   paths (one fun example is trying to interpret ``:`` as a separator in
   path lists), you can still try to convert paths using ``cygpath``
   just before they are passed to GHC and friends.

-  If you don't have ``cygpath``, you probably don't have cygwin and
   hence no problems with it... unless you want to write one build
   process for several platforms. Again, relative paths are your friend,
   but if you have to use absolute paths, and don't want to use
   different tools on different platforms, you can simply write a short
   Haskell program to print the current directory (thanks to George
   Russell for this idea): compiled with GHC, this will give you the
   view of the file system that GHC depends on (which will differ
   depending on whether GHC is compiled with cygwin's gcc or mingw's gcc
   or on a real Unix system..) - that little program can also deal with
   escaping ``\\`` in paths. Apart from the banner and the startup time,
   something like this would also do:

   .. code-block:: none

         $ echo "Directory.getCurrentDirectory >>= putStrLn . init . tail . show " | ghci

.. _win32-dlls:

Building and using Win32 DLLs
-----------------------------

Dynamic link libraries, Win32 DLLs, Win32 On Win32 platforms, the
compiler is capable of both producing and using dynamic link libraries
(DLLs) containing ghc-compiled code. This section shows you how to make
use of this facility.

There are two distinct ways in which DLLs can be used:

-  You can turn each Haskell package into a DLL, so that multiple
   Haskell executables using the same packages can share the DLL files.
   (As opposed to linking the libraries statically, which in effect
   creates a new copy of the RTS and all libraries for each executable
   produced.)

   That is the same as the dynamic linking on other platforms, and it is
   described in :ref:`using-shared-libs`.

-  You can package up a complete Haskell program as a DLL, to be called
   by some external (usually non-Haskell) program. This is usually used
   to implement plugins and the like, and is described below.

.. _win32-dlls-create:

Creating a DLL
~~~~~~~~~~~~~~

Creating a Win32 DLL -shared Sealing up your Haskell library inside a
DLL is straightforward; compile up the object files that make up the
library, and then build the DLL by issuing a command of the form:

.. code-block:: none

    ghc -shared -o foo.dll bar.o baz.o wibble.a -lfooble

By feeding the ghc compiler driver the option ``-shared``, it will build
a DLL rather than produce an executable. The DLL will consist of all the
object files and archives given on the command line.

A couple of things to notice:

-  By default, the entry points of all the object files will be exported
   from the DLL when using ``-shared``. Should you want to constrain
   this, you can specify the *module definition file* to use on the
   command line as follows:

   .. code-block:: none

       ghc -shared -o .... MyDef.def

   See Microsoft documentation for details, but a module definition file
   simply lists what entry points you want to export. Here's one that's
   suitable when building a Haskell COM server DLL:

   .. code-block:: none

       EXPORTS
        DllCanUnloadNow     = DllCanUnloadNow@0
        DllGetClassObject   = DllGetClassObject@12
        DllRegisterServer   = DllRegisterServer@0
        DllUnregisterServer = DllUnregisterServer@0

-  In addition to creating a DLL, the ``-shared`` option also creates an
   import library. The import library name is derived from the name of
   the DLL, as follows:

   .. code-block:: none

       DLL: HScool.dll  ==> import lib: libHScool.dll.a

   The naming scheme may look a bit weird, but it has the purpose of
   allowing the co-existence of import libraries with ordinary static
   libraries (e.g., ``libHSfoo.a`` and ``libHSfoo.dll.a``. Additionally,
   when the compiler driver is linking in non-static mode, it will
   rewrite occurrence of ``-lHSfoo`` on the command line to
   ``-lHSfoo.dll``. By doing this for you, switching from non-static to
   static linking is simply a question of adding ``-static`` to your
   command line.

.. _win32-dlls-foreign:

Making DLLs to be called from other languages
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This section describes how to create DLLs to be called from other
languages, such as Visual Basic or C++. This is a special case of
:ref:`ffi-library`; we'll deal with the DLL-specific issues that arise
below. Here's an example:

Use foreign export declarations to export the Haskell functions you want
to call from the outside. For example:

::

    -- Adder.hs
    {-# LANGUAGE ForeignFunctionInterface #-}
    module Adder where

    adder :: Int -> Int -> IO Int  -- gratuitous use of IO
    adder x y = return (x+y)

    foreign export stdcall adder :: Int -> Int -> IO Int

Add some helper code that starts up and shuts down the Haskell RTS:

.. code-block:: c

    // StartEnd.c
    #include <Rts.h>

    void HsStart()
    {
       int argc = 1;
       char* argv[] = {"ghcDll", NULL}; // argv must end with NULL

       // Initialize Haskell runtime
       char** args = argv;
       hs_init(&argc, &args);
    }

    void HsEnd()
    {
       hs_exit();
    }

Here, ``Adder`` is the name of the root module in the module tree (as
mentioned above, there must be a single root module, and hence a single
module tree in the DLL). Compile everything up:

.. code-block:: none

    ghc -c Adder.hs
    ghc -c StartEnd.c
    ghc -shared -o Adder.dll Adder.o Adder_stub.o StartEnd.o

Now the file ``Adder.dll`` can be used from other programming languages.
Before calling any functions in Adder it is necessary to call
``HsStart``, and at the very end call ``HsEnd``.

.. warning::
   It may appear tempting to use ``DllMain`` to call
   ``hs_init``/``hs_exit``, but this won't work (particularly if you
   compile with ``-threaded``). There are severe restrictions on which
   actions can be performed during ``DllMain``, and ``hs_init`` violates
   these restrictions, which can lead to your DLL freezing during startup
   (see :ghc-ticket:`3605`).

.. _win32-dlls-vba:

Using from VBA
^^^^^^^^^^^^^^

An example of using ``Adder.dll`` from VBA is:

.. code-block:: none

    Private Declare Function Adder Lib "Adder.dll" Alias "adder@8" _
          (ByVal x As Long, ByVal y As Long) As Long

    Private Declare Sub HsStart Lib "Adder.dll" ()
    Private Declare Sub HsEnd Lib "Adder.dll" ()

    Private Sub Document_Close()
    HsEnd
    End Sub

    Private Sub Document_Open()
    HsStart
    End Sub

    Public Sub Test()
    MsgBox "12 + 5 = " & Adder(12, 5)
    End Sub

This example uses the ``Document_Open``\/``Close`` functions of Microsoft
Word, but provided ``HsStart`` is called before the first function, and
``HsEnd`` after the last, then it will work fine.

.. _win32-dlls-c++:

Using from C++
^^^^^^^^^^^^^^

An example of using ``Adder.dll`` from C++ is:

.. code-block:: c

    // Tester.cpp
    #include "HsFFI.h"
    #include "Adder_stub.h"
    #include <stdio.h>

    extern "C" {
        void HsStart();
        void HsEnd();
    }

    int main()
    {
        HsStart();
        // can now safely call functions from the DLL
        printf("12 + 5 = %i\n", adder(12,5))    ;
        HsEnd();
        return 0;
    }

This can be compiled and run with:

.. code-block:: none

    $ ghc -o tester Tester.cpp Adder.dll.a
    $ tester
    12 + 5 = 17
