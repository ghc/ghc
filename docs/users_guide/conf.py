# -*- coding: utf-8 -*-
#
# GHC Users Guide documentation build configuration file
#
# This file is execfile()d with the current directory set to its
# containing dir.
#
import sys
import os

# Support for :base-ref:, etc.
sys.path.insert(0, os.path.abspath('.'))
from ghc_config import extlinks, version

extensions = ['sphinx.ext.extlinks']

templates_path = ['.templates']
source_suffix = '.rst'
source_encoding = 'utf-8-sig'
master_doc = 'index'

# General information about the project.
project = u'Glasgow Haskell Compiler'
copyright = u'2015, GHC Team'
# N.B. version comes from ghc_config
release = version  # The full version, including alpha/beta/rc tags.

# Syntax highlighting
highlight_language = 'haskell'
pygments_style = 'colorful'

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
exclude_patterns = ['.build', "*.gen.rst"]

# -- Options for HTML output ---------------------------------------------

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
html_title = "Glasgow Haskell Compiler <release> Users Guide"
html_short_title = "GHC %s Users Guide" % release
html_theme_path = ['.']
html_theme = 'ghc-theme'
html_logo = None
html_static_path = ['images']
# Convert quotes and dashes to typographically correct entities
html_use_smartypants = True
html_show_copyright = True

# If true, an OpenSearch description file will be output, and all pages will
# contain a <link> tag referring to it.  The value of this option must be the
# base URL from which the finished HTML is served.
#html_use_opensearch = ''

# This is the file name suffix for HTML files (e.g. ".xhtml").
#html_file_suffix = None

# Output file base name for HTML help builder.
htmlhelp_basename = 'GHCUsersGuide'


# -- Options for LaTeX output ---------------------------------------------

latex_elements = {
    'inputenv': '',
    'utf8extra': '',
    'preamble': '''
\usepackage{fontspec}
\setsansfont{DejaVu Sans}
\setromanfont{DejaVu Serif}
\setmonofont{DejaVu Sans Mono}
''',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
  ('index', 'users_guide.tex', u'GHC Users Guide Documentation',
   u'GHC Team', 'manual'),
]

# The name of an image file (relative to this directory) to place at the top of
# the title page.
latex_logo = 'images/logo.pdf'

# If true, show page references after internal links.
latex_show_pagerefs = True


# -- Options for manual page output ---------------------------------------

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('ghc', 'ghc', 'the Glasgow Haskell Compiler', 'The GHC Team', 1)
]

# If true, show URL addresses after external links.
#man_show_urls = False


# -- Options for Texinfo output -------------------------------------------

# Grouping the document tree into Texinfo files. List of tuples
# (source start file, target name, title, author,
#  dir menu entry, description, category)
texinfo_documents = [
  ('index', 'GHCUsersGuide', u'GHC Users Guide',
   u'GHC Team', 'GHCUsersGuide', 'The Glasgow Haskell Compiler.',
   'Compilers'),
]
