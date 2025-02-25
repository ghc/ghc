# -*- coding: utf-8 -*-

import sys
import os
import shlex

extensions = []

source_suffix = '.rst'
master_doc = 'index'

# General information about the project.
project = u'Haddock'
copyright = u'2016, Simon Marlow'
author = u'Simon Marlow'
version = ''
release = ''

language = 'en'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['.build']
todo_include_todos = False

# Syntax highlighting
highlight_language = 'haskell'
pygments_style = 'tango'

# -- Options for HTML output ----------------------------------------------

htmlhelp_basename = 'Haddockdoc'


# -- Options for LaTeX output ---------------------------------------------

latex_elements = { }

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
  (master_doc, 'Haddock.tex', u'Haddock Documentation',
   u'Simon Marlow', 'manual'),
]


# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    (master_doc, 'haddock', u'Haddock Documentation',
     [author], 1)
]


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
  (master_doc, 'Haddock', u'Haddock Documentation',
   author, 'Haddock', 'One line description of project.',
   'Miscellaneous'),
]

