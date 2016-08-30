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
pygments_style = 'tango'

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
    'inputenc': '',
    'utf8extra': '',
    'preamble': '''
\usepackage{fontspec}
\usepackage{makeidx}
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

from sphinx import addnodes
from docutils import nodes

def parse_ghci_cmd(env, sig, signode):
    name = sig.split(';')[0]
    sig = sig.replace(';', '')
    signode += addnodes.desc_name(name, sig)
    return name

def parse_flag(env, sig, signode):
    import re
    names = []
    for i, flag in enumerate(sig.split(',')):
        flag = flag.strip()
        sep = '='
        parts = flag.split('=')
        if len(parts) == 1:
            sep=' '
            parts = flag.split()
        if len(parts) == 0: continue

        name = parts[0]
        names.append(name)
        sig = sep + ' '.join(parts[1:])
        sig = re.sub(ur'<([-a-zA-Z ]+)>', ur'⟨\1⟩', sig)
        if i > 0:
            signode += addnodes.desc_name(', ', ', ')
        signode += addnodes.desc_name(name, name)
        if len(sig) > 0:
            signode += addnodes.desc_addname(sig, sig)

    return names[0]

def setup(app):
    from sphinx.util.docfields import Field, TypedField

    increase_python_stack()

    # the :ghci-cmd: directive used in ghci.rst
    app.add_object_type('ghci-cmd', 'ghci-cmd',
                        parse_node=parse_ghci_cmd,
                        objname='GHCi command',
                        indextemplate='pair: %s; GHCi command')

    app.add_object_type('ghc-flag', 'ghc-flag',
                        objname='GHC command-line option',
                        parse_node=parse_flag,
                        indextemplate='pair: %s; GHC option',
                        doc_field_types=[
                            Field('since', label='Introduced in GHC version', names=['since']),
                            Field('default', label='Default value', names=['default']),
                            Field('static')
                        ])

    app.add_object_type('rts-flag', 'rts-flag',
                        objname='runtime system command-line option',
                        parse_node=parse_flag,
                        indextemplate='pair: %s; RTS option',
                        doc_field_types=[
                            Field('since', label='Introduced in GHC version', names=['since']),
                        ])

def increase_python_stack():
    # Workaround sphinx-build recursion limit overflow:
    # pickle.dump(doctree, f, pickle.HIGHEST_PROTOCOL)
    #  RuntimeError: maximum recursion depth exceeded while pickling an object
    #
    # Default python allows recursion depth of 1000 calls.
    sys.setrecursionlimit(10000)
