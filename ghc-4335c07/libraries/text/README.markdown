# Text: Fast, packed Unicode strings, using stream fusion

This package provides the Data.Text library, a library for the space-
and time-efficient manipulation of Unicode text in Haskell.


# Normalization, conversion, and collation, oh my!

This library intentionally provides a simple API based on the
Haskell prelude's list manipulation functions.  For more complicated
real-world tasks, such as Unicode normalization, conversion to and
from a larger variety of encodings, and collation, use the [text-icu
package](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/text-icu).

That library uses the well-respected and liberally licensed ICU
library to provide these facilities.


# Get involved!

Please report bugs via the
[github issue tracker](https://github.com/bos/text/issues).

Master [git repository](https://github.com/bos/text):

* `git clone git://github.com/bos/text.git`

There's also a [Mercurial mirror](https://bitbucket.org/bos/text):

* `hg clone https://bitbucket.org/bos/text`

(You can create and contribute changes using either Mercurial or git.)


# Authors

The base code for this library was originally written by Tom Harper,
based on the stream fusion framework developed by Roman Leshchinskiy,
Duncan Coutts, and Don Stewart.

The core library was fleshed out, debugged, and tested by Bryan
O'Sullivan <bos@serpentine.com>, and he is the current maintainer.
