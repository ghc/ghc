Hiding things the imported module doesn't export
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Technically in Haskell 2010 this is illegal: ::

    module A( f ) where
      f = True

    module B where
      import A hiding( g )  -- A does not export g
      g = f

The ``import A hiding( g )`` in module ``B`` is technically an error
(`Haskell Report,
5.3.1 <http://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1020005.3.1>`__)
because ``A`` does not export ``g``. However GHC allows it, in the
interests of supporting backward compatibility; for example, a newer
version of ``A`` might export ``g``, and you want ``B`` to work in
either case.

The warning :ghc-flag:`-Wdodgy-imports`, which is off by default but included
with :ghc-flag:`-W`, warns if you hide something that the imported module does
not export.


