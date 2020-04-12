.. _numeric-underscores:

Numeric underscores
-------------------

.. extension:: NumericUnderscores
    :shortdesc: Enable support for :ref:`numeric underscores <numeric-underscores>`.

    :since: 8.6.1

    Allow the use of underscores in numeric literals.

GHC allows for numeric literals to be given in decimal, octal, hexadecimal,
binary, or float notation.

The language extension :extension:`NumericUnderscores` adds support for expressing
underscores in numeric literals.
For instance, the numeric literal ``1_000_000`` will be parsed into
``1000000`` when :extension:`NumericUnderscores` is enabled.
That is, underscores in numeric literals are ignored when
:extension:`NumericUnderscores` is enabled.
See also :ghc-ticket:`14473`.

For example:

.. code-block:: none

    -- decimal
    million    = 1_000_000
    billion    = 1_000_000_000
    lightspeed = 299_792_458
    version    = 8_04_1
    date       = 2017_12_31

    -- hexadecimal
    red_mask = 0xff_00_00
    size1G   = 0x3fff_ffff

    -- binary
    bit8th   = 0b01_0000_0000
    packbits = 0b1_11_01_0000_0_111
    bigbits  = 0b1100_1011__1110_1111__0101_0011

    -- float
    pi       = 3.141_592_653_589_793
    faraday  = 96_485.332_89
    avogadro = 6.022_140_857e+23

    -- function
    isUnderMillion = (< 1_000_000)

    clip64M x
        | x > 0x3ff_ffff = 0x3ff_ffff
        | otherwise = x

    test8bit x = (0b01_0000_0000 .&. x) /= 0

About validity:

.. code-block:: none

    x0 = 1_000_000   -- valid
    x1 = 1__000000   -- valid
    x2 = 1000000_    -- invalid
    x3 = _1000000    -- invalid

    e0 = 0.0001      -- valid
    e1 = 0.000_1     -- valid
    e2 = 0_.0001     -- invalid
    e3 = _0.0001     -- invalid
    e4 = 0._0001     -- invalid
    e5 = 0.0001_     -- invalid

    f0 = 1e+23       -- valid
    f1 = 1_e+23      -- valid
    f2 = 1__e+23     -- valid
    f3 = 1e_+23      -- invalid

    g0 = 1e+23       -- valid
    g1 = 1e+_23      -- invalid
    g2 = 1e+23_      -- invalid

    h0 = 0xffff      -- valid
    h1 = 0xff_ff     -- valid
    h2 = 0x_ffff     -- valid
    h3 = 0x__ffff    -- valid
    h4 = _0xffff     -- invalid


