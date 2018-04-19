# -*- coding: utf-8 -*-
#
# GHC User's Guide documentation build configuration file
#
# This file is execfile()d with the current directory set to its
# containing dir.
#
import sys
import os

# Support for :base-ref:, etc.
sys.path.insert(0, os.path.abspath('.'))
from ghc_config import extlinks, version
import ghc_config

extensions = ['sphinx.ext.extlinks',
              'sphinx.ext.mathjax',
              # GHC-specific extensions
              'flags',
              'ghc_packages']

templates_path = ['.templates']
source_suffix = '.rst'
source_encoding = 'utf-8-sig'
master_doc = 'index'

rst_prolog = """
.. |llvm-version| replace:: {llvm_version}
""".format(llvm_version=ghc_config.llvm_version)

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
exclude_patterns = ['.build']

# -- Options for HTML output ---------------------------------------------

# The name for this set of Sphinx documents.  If None, it defaults to
# "<project> v<release> documentation".
html_title = "Glasgow Haskell Compiler %s User's Guide" % release
html_short_title = "GHC %s User's Guide" % release
html_theme_path = ['.']
html_theme = 'ghc-theme'
html_logo = None
html_static_path = ['images']
# Convert quotes and dashes to typographically correct entities
html_use_smartypants = True
html_use_opensearch = 'https://downloads.haskell.org/~ghc/master/users-guide'
html_show_copyright = True

# See GHC #15006
mathjax_path = 'https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js'

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
\setlength{\\tymin}{45pt}
''',
}

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title,
#  author, documentclass [howto, manual, or own class]).
latex_documents = [
  ('index', 'users_guide.tex', u'GHC User\'s Guide Documentation',
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
  ('index', 'GHCUsersGuide', u'GHC User\'s Guide',
   u'GHC Team', 'GHCUsersGuide', 'The Glasgow Haskell Compiler.',
   'Compilers'),
]

from sphinx import addnodes
from docutils import nodes

# The following functions parse flag declarations, and then have two jobs. First
# they modify the docutils node `signode` for the proper display of the
# declaration. Second, they return the name used to reference the flag.
# (i.e. return "name" implies you reference the flag with :flag:`name`)
def parse_ghci_cmd(env, sig, signode):
    parts = sig.split(';')
    name = parts[0]
    args = ''
    if len(parts) > 1:
        args = parts[1]
    # Bold name
    signode += addnodes.desc_name(name, name)
    # Smaller args
    signode += addnodes.desc_addname(args, args)
    # Reference name
    return name

def parse_flag(env, sig, signode):

    # Break flag into name and args
    import re
    parts = re.split('( |=|\\[)', sig, 1)
    flag = parts[0]
    args = ''
    if len(parts) > 1:
        args = ''.join(parts[1:])

    # Bold printed name
    signode += addnodes.desc_name(flag, flag)
    # Smaller arguments
    signode += addnodes.desc_addname(args, args)
    # Reference name left unchanged
    return sig

def haddock_role(lib):
    """
    For instance,
     * reference to module:      :base-ref:`Control.Applicative.`
     * reference to identifier:  :base-ref:`Control.Applicative.pure`
     * reference to type:        :base-ref:`Control.Applicative.Applicative`
    """
    path = '%s/%s-%s' % (ghc_config.libs_base_uri, lib, ghc_config.lib_versions[lib])
    def role(name, rawtext, text, lineno, inliner, options={}, content=[]):
        try:
            parts = text.split('.')
            module_parts = parts[:-1]
            thing = parts[-1]
            if thing != '':
                # reference to type or identifier
                tag = 't' if thing[0].isupper() else 'v'
                anchor = '#%s:%s' % (tag, thing)
                link_text = text
            else:
                # reference to module
                anchor = ''
                link_text = '.'.join(module_parts)

            uri = '%s/%s.html%s' % (path, '-'.join(module_parts), anchor)
            node = nodes.reference(link_text, link_text, refuri=uri)
            return [node], []
        except ValueError:
            msg = inliner.reporter.error('')

    return role

def setup(app):
    from sphinx.util.docfields import Field, TypedField

    increase_python_stack()

    # the :ghci-cmd: directive used in ghci.rst
    app.add_object_type('ghci-cmd', 'ghci-cmd',
                        parse_node=parse_ghci_cmd,
                        objname='GHCi command',
                        indextemplate='pair: %s; GHCi command')

    # Haddock references
    app.add_role('th-ref', haddock_role('template-haskell'))
    app.add_role('base-ref', haddock_role('base'))
    app.add_role('cabal-ref', haddock_role('Cabal'))
    app.add_role('ghc-compact-ref', haddock_role('ghc-compact'))
    app.add_role('ghc-prim-ref', haddock_role('ghc-prim'))
    app.add_role('parallel-ref', haddock_role('parallel'))
    app.add_role('array-ref', haddock_role('array'))

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
