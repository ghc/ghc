Cabal documentation
===================

### Where to read it
These docs will be built and deployed whenever a release is made,
and can be read at: https://www.haskell.org/cabal/users-guide/

In addition, the docs are taken directly from git and hosted at:
http://cabal.readthedocs.io/


### How to build it

* Currently requires python-2
* `> pip install sphinx`
* `> pip install sphinx_rtd_theme`
* `> cd Cabal`
* `> make clean users-guide`
* if you are missing any other dependencies, install them with `pip` as needed
¯\\\_(ツ)_/¯
* Python on Mac OS X dislikes `LC_CTYPE=UTF-8`, unset the env var in
terminal preferences and instead set `LC_ALL=en_US.UTF-8` or something
* On archlinux, package `python2-sphinx` is sufficient.

### Caveats, for newcomers to RST from MD
RST does not allow you to skip section levels when nesting, like MD
does.
So, you cannot have

```
	Section heading
	===============

	Some unimportant block
	""""""""""""""""""""""
```

  instead you need to observe order and either promote your block:

```
    Section heading
    ===============

    Some not quite so important block
    ---------------------------------
```

  or introduce more subsections:

```
    Section heading
    ===============

    Subsection
    ----------

    Subsubsection
    ^^^^^^^^^^^^^

    Some unimportant block
    """"""""""""""""""""""
```

* RST simply parses a file and interpretes headings to indicate the
  start of a new block,
  * at the level implied by the header's *adornment*, if the adornment was
  previously encountered in this file,
  * at one level deeper than the previous block, otherwise.

  This means that a lot of confusion can arise when people use
  different adornments to signify the same depth in different files.

  To eliminate this confusion, please stick to the adornment order
  recommended by the Sphinx team:

```
    ####
    Part
    ####

    *******
    Chapter
    *******

    Section
    =======

    Subsection
    ----------

    Subsubsection
    ^^^^^^^^^^^^^

    Paragraph
    """""""""
```

* The Read-The-Docs stylesheet does not support multiple top-level
  sections in a file that is linked to from the top-most TOC (in
  `index.rst`). It will mess up the sidebar.
  E.g. you cannot link to a `cabal.rst` with sections "Introduction",
  "Using Cabal", "Epilogue" from `index.rst`.

  One solution is to have a single section, e.g. "All About Cabal", in
  `cabal.rst` and make the other blocks subsections of that.

  Another solution is to link via an indirection, e.g. create
  `all-about-cabal.rst`, where you include `cabal.rst` using  the
  `.. toctree::` command and then link to `all-about-cabal.rst` from
  `index.rst`.
  This will effectively "push down" all blocks by one layer and solve
  the problem without having to change `cabal.rst`.


* We use [`extlinks`](http://www.sphinx-doc.org/en/stable/ext/extlinks.html)
  to shorten links to commonly referred resources (wiki, issue trackers).

  E.g. you can use the more convenient short syntax

        :issue:`123`

  which is expanded into a hyperlink

        `#123 <https://github.com/haskell/cabal/issues/123>`__

  See `conf.py` for list of currently defined link shorteners.
