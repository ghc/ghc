.. _package-qualified-imports:

Package-qualified imports
~~~~~~~~~~~~~~~~~~~~~~~~~

.. extension:: PackageImports
    :shortdesc: Enable package-qualified imports.

    :since: 6.10.1

    Allow the use of package-qualified ``import`` syntax.

With the :extension:`PackageImports` extension, GHC allows import declarations to be
qualified by the package name that the module is intended to be imported
from. For example: ::

    import "network" Network.Socket

would import the module ``Network.Socket`` from the package ``network``
(any version). This may be used to disambiguate an import when the same
module is available from multiple packages, or is present in both the
current package being built and an external package.

The special package name ``this`` can be used to refer to the current
package being built.

.. note::
   You probably don't need to use this feature, it was added mainly so that we
   can build backwards-compatible versions of packages when APIs change. It can
   lead to fragile dependencies in the common case: modules occasionally move
   from one package to another, rendering any package-qualified imports broken.
   See also :ref:`package-thinning-and-renaming` for an alternative way of
   disambiguating between module names.


