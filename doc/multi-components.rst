Haddocks of multiple components
===============================

Haddock supports building documentation of multiple components.  First, one
needs to build haddocks of all components which can be done with:

.. code-block:: none

    cabal haddock --haddock-html \
                  --haddock-quickjump \
                  --haddock-option="--use-index=../doc-index.html" \
                  --haddock-option="--use-contents=../index.html" \
                  --haddock-option="--base-url=.." \
                  all

The new ``--base-url`` option will allow to access the static files from the
main directory (in this example its the relative ``./..`` directory).  It will
also prevent ``haddock`` from copying its static files to each of the
documentation folders, we're only need a single copy of them where the
``--base-url`` option points to.

The second step requires to copy all the haddocks to a common directory, let's
say ``./docs``,  this will depend on your project and it might look like:

.. code-block:: none

    cp -r dist-newstyle/build/x86_64-linux/ghc-9.0.1/package-a-0.1.0.0/doc/html/package-a/ docs
    cp -r dist-newstyle/build/x86_64-linux/ghc-9.0.1/package-b-0.1.0.0/doc/html/package-b/ docs

Note that you can also include documentation of other packages in this way,
e.g. ``base``, but you need to know where it is hidden on your hard-drive.

To build html and js (``quickjump``) indexes one can now invoke ``haddock`` with:

.. code-block:: none

    haddock \
      -o docs \
      --quickjump --gen-index --gen-contents \
      --read-interface=package-a,docs/package-a/package-a.haddock \
      --read-interface=package-b,docs/package-b/package-b.haddock

Note: the ``PATH`` in ``--read-interface=PATH,...`` must be a relative url of
a package it points to (relative to the ``docs`` directory).

There's an example project which shows how to do that posted `here
<https://github.com/coot/haddock-example>`_, which haddocks are served on
`github-pages <https://coot.github.io/haddock-example>`_.
