.. _hex-float-literals:

Hexadecimal floating point literals
-----------------------------------

.. extension:: HexFloatLiterals
    :shortdesc: Enable support for :ref:`hexadecimal floating point literals <hex-float-literals>`.

    :since: 8.4.1

    Allow writing floating point literals using hexadecimal notation.

The hexadecimal notation for floating point literals is useful when you
need to specify floating point constants precisely, as the literal notation
corresponds closely to the underlying bit-encoding of the number.

In this notation floating point numbers are written using hexadecimal digits,
and so the digits are interpreted using base 16, rather then the usual 10.
This means that digits left of the decimal point correspond to positive
powers of 16, while the ones to the right correspond to negative ones.

You may also write an explicit exponent, which is similar to the exponent
in decimal notation with the following differences:

- the exponent begins with ``p`` instead of ``e``
- the exponent is written in base ``10`` (**not** 16)
- the base of the exponent is ``2`` (**not** 16).

In terms of the underlying bit encoding, each hexadecimal digit corresponds
to 4 bits, and you may think of the exponent as "moving" the floating point
by one bit left (negative) or right (positive).  Here are some examples:

-  ``0x0.1``     is the same as ``1/16``
-  ``0x0.01``    is the same as ``1/256``
-  ``0xF.FF``    is the same as ``15 + 15/16 + 15/256``
-  ``0x0.1p4``   is the same as ``1``
-  ``0x0.1p-4``  is the same as ``1/256``
-  ``0x0.1p12``  is the same as ``256``
