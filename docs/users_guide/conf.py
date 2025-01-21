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

nitpick_ignore = [
    ("envvar", "EDITOR"),
    ("envvar", "HOME"),
    ("envvar", "LD_LIBRARY_PATH"),
    ("envvar", "LIBRARY_PATH"),
    ("envvar", "PATH"),
    ("envvar", "RPATH"),
    ("envvar", "RUNPATH"),
    ("envvar", "TMPDIR"),

    ("c:type", "bool"),

    ("extension", "RelaxedPolyRec"),
]

rst_prolog = """
.. |llvm-version-min| replace:: {llvm_version_min}
.. |llvm-version-max| replace:: {llvm_version_max}
""".format(llvm_version_min=ghc_config.llvm_version_min, llvm_version_max=ghc_config.llvm_version_max)

# General information about the project.
project = u'Glasgow Haskell Compiler'
copyright = u'2023, GHC Team'
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
html_theme = 'rtd-theme'
html_logo = None
html_static_path = ['images']
# Convert quotes and dashes to typographically correct entities
html_use_smartypants = True
html_use_opensearch = 'https://downloads.haskell.org/~ghc/master/users-guide'
html_show_copyright = True

# See GHC #15006, #19423
mathjax_path = 'https://cdn.jsdelivr.net/npm/mathjax@2/MathJax.js?config=TeX-AMS-MML_HTMLorMML '

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
    'preamble': r'''
\usepackage{fontspec}
\usepackage{makeidx}
\setsansfont{DejaVu Sans}
\setromanfont{DejaVu Serif}
\setmonofont{DejaVu Sans Mono}
\setlength{\tymin}{45pt}

% Dynamic section number spacing. Fixes #18554
\renewcommand{\numberline}[1]{#1~}

% Avoid a torrent of over-full \hbox warnings
\usepackage{microtype}
\hbadness=99999
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
man_show_urls = True


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

def parse_pragma(env, sig, signode):
    # Collect a prefix of alphabetical tokens to use as the pragma name
    parts = sig.split(' ')
    idx_parts = []
    for part in parts:
        if all(c.isalpha() or c == "_" for c in part):
            idx_parts.append(part)
        else:
            break
    idx = '-'.join(idx_parts)

    name = '{-# ' + sig + ' #-}'
    signode += addnodes.desc_name(name, name)
    return idx

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

    def get_relative_uri(topdir, current_doc, module, anchor):
      lib_version = ghc_config.lib_versions[lib]
      libs_base_uri = ghc_config.libs_base_uri

      # We want to find the relative uri to the Haddocks for relative links
      # from users guide to haddocks. The inputs are:
      #
      # - The users guide lives under 'topdir': //docs/users_guide
      # - The current doc file is 'current_doc': //docs/users_guide/exts/template_haskell.rst
      #   (The html output will be //docs/users_guide/exts/template_haskell.html)
      # - The haddocks live under 'libs_base_uri' (relative to 'topdir'): ../libraries

      # for the template_haskell.rst example this will be '..'
      current_doc_to_topdir = os.path.relpath(topdir, os.path.dirname(current_doc))

      relative_path = '%s/%s/%s' % (current_doc_to_topdir, libs_base_uri, lib_version)

      uri = '%s/%s.html%s' % (relative_path, module, anchor)

      return uri


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

            uri = get_relative_uri(
                  inliner.document.settings.env.srcdir,
                  inliner.document.current_source,
                  '-'.join(module_parts),
                  anchor)

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
    app.add_role('array-ref', haddock_role('array'))

    app.add_object_type('rts-flag', 'rts-flag',
                        objname='runtime system command-line option',
                        parse_node=parse_flag,
                        indextemplate='pair: %s; RTS option',
                        doc_field_types=[
                            Field('since', label='Introduced in GHC version', names=['since'])
                        ])

    app.add_object_type('event-type', 'event-type',
                        objname='event log event type',
                        indextemplate='pair: %s; eventlog event type',
                        doc_field_types=[
                            Field('since', label='Introduced in GHC version', names=['since']),
                            Field('tag', label='Event type ID', names=['tag']),
                            Field('length', label='Record length', names=['length']),
                            TypedField('fields', label='Fields', names='field', typenames=('fieldtype', 'type'))
                        ])

    app.add_object_type('pragma', 'pragma',
                        objname='Haskell pragma',
                        parse_node=parse_pragma,
                        indextemplate='pair: %s; pragma',
                        doc_field_types=[
                            Field('since', label='Introduced in GHC version', names=['since']),
                            Field('where', label='Allowed contexts', names=['where'])
                        ])

def increase_python_stack():
    # Workaround sphinx-build recursion limit overflow:
    # pickle.dump(doctree, f, pickle.HIGHEST_PROTOCOL)
    #  RuntimeError: maximum recursion depth exceeded while pickling an object
    #
    # Default python allows recursion depth of 1000 calls.
    sys.setrecursionlimit(10000)
