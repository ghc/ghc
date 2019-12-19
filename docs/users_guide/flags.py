# GHC User's Guide flag extension
#
# This file defines a Sphinx extension to document GHC flags.
# It introduces a directive:
#
# .. ghc-flag::
#     :shortdesc: A short description (REQUIRED)
#     :type: dynamic or mode  (REQUIRED)
#     :reverse: The reverse of the flag
#     :category: The category to list the flag under (default: 'misc')
#     :noindex: Do not list the flag anywhere (good for duplicates)
#
# That can be referenced using:
#
# :ghc-flag:`flag`
#
# As well as a directive to generate a display of flags:
#
# .. flag-print::
#     :type: table/list/summary (REQUIRED)
#     :category: Limit the output to a single category
#
# It also provides a directive to list language extensions:
#
# .. extension::
#     :shortdesc: A short description (REQUIRED)
#     :noindex: Do not list the extension anywhere (good for duplicates)
#
# This has the side-effect of an appropriate ghc-flag directive for the `-X`
# flag.
#
# Extensions can be referenced with
#
# :extension:`extension`
#
# Language extensions can be listed:
#
# .. extension-print::
#     :type: table/list/summary (REQUIRED)
#
# The two main functions in this extension are Flag.after_content() which adds
# flag metadata into the environment, and flagprint.generate_output(), which
# reads the metadata back out and formats it as desired.
#
#

from docutils import nodes
from docutils.parsers.rst import Directive, directives
import sphinx
from sphinx import addnodes
from sphinx.domains.std import GenericObject
from sphinx.errors import SphinxError
from distutils.version import LooseVersion
from utils import build_table_from_list

import os.path

### Settings

# Categories to titles as well as a canonical list of categories
categories = {
    '': 'All flags',
    'codegen': 'Code generation',
    'coverage': 'Program coverage',
    'cpp': 'C pre-processor',
    'debugging': 'Debugging the compiler',
    'interactive': 'Interactive mode',
    'interface-files': 'Interface files',
    'keep-intermediates': 'Keeping intermediate files',
    'language': 'Language options',
    'linking': 'Linking options',
    'misc': 'Miscellaneous options',
    'modes': 'Modes of operation',
    'optimization': 'Individual optimizations',
    'optimization-levels': 'Optimization levels',
    'packages': 'Package options',
    'phases': 'Phases of compilation',
    'phase-programs': 'Overriding external programs',
    'phase-options': 'Phase-specific options',
    'platform-options': 'Platform-specific options',
    'plugins': 'Compiler plugins',
    'profiling': 'Profiling',
    'recompilation': 'Recompilation checking',
    'redirect-output': 'Redirecting output',
    'search-path': 'Finding imports',
    'temp-files': 'Temporary files',
    'verbosity': 'Verbosity options',
    'warnings': 'Warnings',
}

# Map file names to default flag categories
file_defaults = {
    'debugging': 'debugging',
    'ghci': 'interactive',
    'glasgow_exts': 'language',
    'packages': 'packages',
    'profiling': 'profiling',
    'safe_haskell': 'language',
    'separate_compilation': 'redirect-output',
    'using-warnings': 'warnings',
    'using-optimisation': 'optimization'
}


### Flag declaration

# Functionality to add a flag to the tables, used by both Flag and LanguageExtension
class GenericFlag(GenericObject):
      def register_flag(self, names, category, name_string, shortdesc, flag_type, reverse_string):
        # Create nodes for each cell of the table
        name_node = nodes.paragraph()
        shortdesc_node = nodes.paragraph()
        type_node = nodes.paragraph()
        reverse_node = nodes.paragraph()

        assert flag_type in ('dynamic', 'mode'), ('Unknown flag type for %s: %s' %
                                                     (name_string, flag_type))

        # Nodes expect an internal ViewList type for the content,
        # we are just spoofing it here
        from docutils.statemachine import ViewList
        name_vl      = ViewList(initlist=[name_string],
                                source=self.env.docname, parent=[])
        shortdesc_vl = ViewList(initlist=[shortdesc],
                                source=self.env.docname, parent=[])
        type_vl      = ViewList(initlist=[flag_type],
                                source=self.env.docname, parent=[])
        reverse_vl   = ViewList(initlist=[reverse_string],
                                source=self.env.docname, parent=[])


        # Parse the content into the nodes
        self.state.nested_parse(name_vl, 0, name_node)
        self.state.nested_parse(shortdesc_vl, 0, shortdesc_node)
        self.state.nested_parse(type_vl, 0, type_node)
        self.state.nested_parse(reverse_vl, 0, reverse_node)


        # The parsing adds extra layers that we don't need
        name_node = name_node[0]
        shortdesc_node = shortdesc_node[0]

        # Append this flag to the environment, initializing if necessary
        if not hasattr(self.env, 'all_flags'):
            self.env.all_flags = []
        self.env.all_flags.append({
            'names': names,
            'docname': self.env.docname,
            'category': category,
            'cells': [name_node, shortdesc_node, type_node, reverse_node],
        })

# This class inherits from Sphinx's internal GenericObject, which drives
# the add_object_type() utility function. We want to keep that tooling,
# but need to override some of the functionality.
class Flag(GenericFlag):

    # The options that can be passed to our directive and their validators
    option_spec = {
        'shortdesc': directives.unchanged_required,
        'type': directives.unchanged_required,
        'reverse': directives.unchanged,
        'category': directives.unchanged,
        'noindex': directives.flag
    }

    # The index directive generated unless :noindex: is specified
    indextemplate = 'pair: %s; GHC option'


    # Generate docutils node from directive arguments
    @staticmethod
    def _parse_flag(env, sig, signode):

        # Break flag into name and args
        import re
        parts = re.split(r'( |=|\[)', sig, 1)
        flag = parts[0]
        # Bold printed name
        signode += addnodes.desc_name(flag, flag)
        if len(parts) > 1:
            args = ''.join(parts[1:])
            # Smaller arguments
            signode += addnodes.desc_addname(args, args)

        # Reference name left unchanged
        return sig

    # Used in the GenericObject class
    parse_node = _parse_flag

    # Override the (empty) function that is called at the end of run()
    # to append metadata about this flag into the environment
    def after_content(self):

        # If noindex, then do not include this flag in the table
        if 'noindex' in self.options:
            return

        # Validity checking
        if 'shortdesc' not in self.options:
            raise SphinxError('ghc-flag (%s) directive missing :shortdesc: key' % self.names)
        if 'type' not in self.options:
            raise SphinxError('ghc-flag (%s) directive missing :type: key' % self.names)

        # Set the flag category (default: misc)
        self.category = 'misc'
        if not 'category' in self.options or self.options['category'] == '':
            if self.env.docname in file_defaults:
                self.category = file_defaults[self.env.docname]
        else:
            self.category = self.options['category']

        # Manually create references
        name_string = ", ".join([':ghc-flag:`'+n+'`' for n in self.names])
        reverse_string = ''
        reverse = self.options.get('reverse')
        if reverse is not None and reverse != '':
            reverse_string = ':ghc-flag:`' + reverse + '`'
            self.names += [reverse]

        self.register_flag(
            self.names,
            self.category,
            name_string,
            self.options['shortdesc'],
            self.options['type'],
            reverse_string)

    # Add additional targets
    def add_target_and_index(self, name, sig, signode):

        GenericFlag.add_target_and_index(self, name, sig, signode)

        reverse = self.options.get('reverse')
        if reverse is not None and reverse != '':
            # Make this also addressable via the reverse flag
            self.env.domaindata['std']['objects']['ghc-flag', reverse] = \
                self.env.docname, 'ghc-flag-%s' % name

# This class inherits from Sphinx's internal GenericObject, which drives
# the add_object_type() utility function. We want to keep that tooling,
# but need to override some of the functionality.
class LanguageExtension(GenericFlag):

    # The options that can be passed to our directive and their validators
    option_spec = {
        'shortdesc': directives.unchanged_required,
        'noindex': directives.flag
    }

    # The index directive generated unless :noindex: is specified
    indextemplate = 'pair: %s; Language Extension'

    # Invert the flag
    @staticmethod
    def _noname(name):
        if name[:2] == "No":
          return name[2:]
        else:
          return "No%s" % name

    @staticmethod
    def _onname(name):
        if name[:2] == "No":
          return name[2:]
        else:
          return name

    # Add additional targets
    def add_target_and_index(self, name, sig, signode):

        GenericFlag.add_target_and_index(self, name, sig, signode)

        # Mostly for consistency in URL anchors
        signode['ids'].append('ghc-flag--X%s'   % name)
        # So that anchors stay valid even if an extension turns to on-by-default
        signode['ids'].append('extension-%s'   % self._noname(name))

        targetname = '%s-%s' % (self.objtype, name)

        # FIXME: This causes some Sphinx versions to fail
        # Add index entries for the -XFoo flag
        #self.indexnode['entries'].append(('pair', '-X%s; GHC option' % name,
        #                                  targetname, '', None))

        # Make this also addressable using :ghc-flag:-XFoo
        self.env.domaindata['std']['objects']['ghc-flag', '-X%s' % name] = \
            self.env.docname, 'extension-%s' % name
        # Make this also addressable using :extension:-XNoFoo
        self.env.domaindata['std']['objects']['extension', self._noname(name)] = \
            self.env.docname, 'extension-%s' % name


    # Override the (empty) function that is called at the end of run()
    # to append metadata about this flag into the environment
    def after_content(self):

        # If noindex, then do not include this extension in the table
        if 'noindex' in self.options:
            return

        # Validity checking
        if len(self.names) < 1:
            raise SphinxError('extension needs at least one name')
        primary_name = self.names[0]
        if 'shortdesc' not in self.options:
            raise SphinxError('extension (%s) directive missing :shortdesc: key' % primary_name)

        # Register the corresponding flags
        for name in self.names:
            self.register_flag(
                ['-X%s' % name],
                'language',
                ':extension:`-X%s <%s>`' % (name, primary_name),
                self.options['shortdesc'],
                'dynamic',
                ':extension:`-X%s <%s>`' % (self._noname(name), primary_name))

        # Register the extension for the table, under the "on name" (no No...)
        onname = self._onname(primary_name)

        name_node = nodes.paragraph()
        shortdesc_node = nodes.paragraph()
        # Nodes expect an internal ViewList type for the content,
        # we are just spoofing it here
        from docutils.statemachine import ViewList
        name_vl      = ViewList(initlist=[':extension:`%s`' % onname],
                                source=self.env.docname, parent=[])
        shortdesc_vl = ViewList(initlist=[self.options['shortdesc']],
                                source=self.env.docname, parent=[])
        # Parse the content into the nodes
        self.state.nested_parse(name_vl, 0, name_node)
        self.state.nested_parse(shortdesc_vl, 0, shortdesc_node)
        # The parsing adds extra layers that we don't need
        name_node = name_node[0]
        shortdesc_node = shortdesc_node[0]

        if not hasattr(self.env, 'all_extensions'):
            self.env.all_extensions = []
        self.env.all_extensions.append({
            'name':    onname,
            'docname': self.env.docname,
            'cells':   [name_node, shortdesc_node]
        })

### Flag Printing


# Generate a table of flags
def generate_flag_table(flags, category):

    # Create column headers for table
    header = []
    for h in ["Flag", "Description", "Type", "Reverse"]:
        inline = nodes.inline(text=h)
        header.append(inline)

    flags_list = [header]

    for flag_info in flags:

        flags_list.append(flag_info['cells'])

    # The column width hints only apply to html,
    # latex widths are set in file (see flags.rst)
    table = build_table_from_list(flags_list, [28, 34, 10, 28])

    # Flag tables have lots of content, so we need to set 'longtable'
    # to allow for pagebreaks. (latex specific)
    table['classes'].append('longtable')

    return table


# Generate a list of flags and their short descriptions
def generate_flag_list(flags, category):

    list_node = nodes.definition_list()

    for flag_info in flags:

        dl_item_node = nodes.definition_list_item()
        term_node = nodes.term()
        # The man writer is picky, so we have to remove the outer
        # paragraph node to get just the flag name
        term_node += flag_info['cells'][0][0]
        dl_item_node += term_node
        def_node = nodes.definition()
        def_node += flag_info['cells'][1]
        dl_item_node += def_node

        list_node += dl_item_node

    return list_node


# Generate a block of flag names under a category
def generate_flag_summary(flags, category):

    summary_node = nodes.definition_list_item()
    term_node = nodes.term(text=categories[category])
    summary_node += term_node
    block = nodes.definition()
    summary_node += block

    # Fill block with flags
    for flag_info in flags:

        for name in flag_info['names']:
            block += nodes.literal(text=name)
            block += nodes.inline(text=' ')

    block += nodes.inline(text='\n')

    return summary_node

# Output dispatch table
flag_handlers = {
    'table': generate_flag_table,
    'list': generate_flag_list,
    'summary': generate_flag_summary
}


# Generic node for printing flag output
class flagprint(nodes.General, nodes.Element):

    def __init__(self, output_type='', category='', **kwargs):

        nodes.Element.__init__(self, rawsource='', **kwargs)

        # Verify options
        if category not in categories:
            error = "flagprint: Unknown category: " + category
            raise ValueError(error)
        if output_type not in flag_handlers:
            error = "flagprint: Unknown output type: " + output_type
            raise ValueError(error)

        # Store the options
        self.options = {
            'type': output_type,
            'category': category
        }


    # The man writer has a copy issue, so we explicitly override it here
    def copy(self):
        newnode = flagprint(output_type=self.options['type'],
                category=self.options['category'], **self.attributes)
        newnode.source = self.source
        newnode.line = self.line
        return newnode


    def generate_output(self, app, fromdocname):
        env = app.builder.env

        # Filter flags before passing to flag_handlers
        flags = []

        for flag_info in sorted(env.all_flags,
                key=lambda fi: fi['names'][0].lower()):

            if not (self.options['category'] == '' or
                    self.options['category'] == flag_info['category']):
                continue

            # Resolve all references as if they were originated from this node.
            # This fixes the relative uri.
            for cell in flag_info['cells']:
                for ref in cell.traverse(addnodes.pending_xref):
                    ref['refdoc'] = fromdocname
                env.resolve_references(cell, flag_info['docname'], app.builder)

            flags.append(flag_info)

        handler = flag_handlers[self.options['type']]
        self.replace_self(handler(flags, self.options['category']))

# A directive to create flagprint nodes
class FlagPrintDirective(Directive):

    option_spec = {
        'type': directives.unchanged_required,
        'category': directives.unchanged
    }

    def run(self):

        # Process options
        category = ''
        if 'category' in self.options:
            category = self.options['category']

        # Create a flagprint node
        node = flagprint(output_type=self.options['type'], category=category)
        return [node]

### Extension Printing


# Generate a table of flags
def generate_extension_table(extensions):

    # Create column headers for table
    header = []
    for h in ["Extension", "Description"]:
        inline = nodes.inline(text=h)
        header.append(inline)

    extension_list = [header]

    for extension_info in extensions:
        extension_list.append(extension_info['cells'])

    # The column width hints only apply to html,
    # latex widths are set in file (see flags.rst)
    table = build_table_from_list(extension_list, [28, 72])

    # Flag tables have lots of content, so we need to set 'longtable'
    # to allow for pagebreaks. (latex specific)
    table['classes'].append('longtable')

    return table


# Output dispatch table
extension_handlers = {
    'table': generate_extension_table,
}

# Generic node for printing extension output
class extensionprint(nodes.General, nodes.Element):

    def __init__(self, output_type='', **kwargs):

        nodes.Element.__init__(self, rawsource='', **kwargs)

        # Verify options
        if output_type not in extension_handlers:
            error = "extensionprint: Unknown output type: " + output_type
            raise ValueError(error)

        # Store the options
        self.options = {
            'type': output_type,
        }


    # The man writer has a copy issue, so we explicitly override it here
    def copy(self):
        newnode = extensionprint(output_type=self.options['type'], **self.attributes)
        newnode.source = self.source
        newnode.line = self.line
        return newnode


    def generate_output(self, app, fromdocname):
        env = app.builder.env

        extensions = []

        for extension_info in sorted(env.all_extensions,
                key=lambda fi: fi['name'].lower()):

            # Resolve all references as if they were originated from this node.
            # This fixes the relative uri.
            for cell in extension_info['cells']:
                for ref in cell.traverse(addnodes.pending_xref):
                    ref['refdoc'] = fromdocname
                env.resolve_references(cell, extension_info['docname'], app.builder)

            extensions.append(extension_info)

        handler = extension_handlers[self.options['type']]
        self.replace_self(handler(extensions))

# A directive to create extensionprint nodes
class ExtensionPrintDirective(Directive):

    option_spec = {
        'type': directives.unchanged_required
    }

    def run(self):
        # Create a extensionprint node
        node = extensionprint(output_type=self.options['type'])
        return [node]


### Additional processing

def process_print_nodes(app, doctree, fromdocname):

    # Convert every flagprint node into its output format
    for node in doctree.traverse(flagprint):
        node.generate_output(app, fromdocname)

    for node in doctree.traverse(extensionprint):
        node.generate_output(app, fromdocname)

    # Write out file listing all documented flags
    with open(os.path.join(app.outdir, 'ghc-flags.txt'), 'w', encoding='utf-8') as f:
        flag_names = \
            {name
             for flag in app.env.all_flags
             for name in flag['names']}

        f.write('\n'.join(flag_names))

# To avoid creating duplicates in the serialized environment, clear all
# flags originating from a file before re-reading it.
def purge_flags(app, env, docname):

    if hasattr(env, 'all_flags'):
        env.all_flags = [flag for flag in env.all_flags
                         if flag['docname'] != docname]
    if hasattr(env, 'all_extensions'):
        env.all_extensions = [ext for ext in env.all_extensions
                             if ext['docname'] != docname]

### Initialization

def setup(app):
    # The override argument to add_directive_to_domain is only supported by >= 1.8
    sphinx_version = LooseVersion(sphinx.__version__)
    override_arg = {'override': True} if sphinx_version >= LooseVersion('1.8') else {}

    # Add ghc-flag directive, and override the class with our own
    app.add_object_type('ghc-flag', 'ghc-flag')
    app.add_directive_to_domain('std', 'ghc-flag', Flag, **override_arg)

    # Add extension directive, and override the class with our own
    app.add_object_type('extension', 'extension')
    app.add_directive_to_domain('std', 'extension', LanguageExtension,
                                **override_arg)
    # NB: language-extension would be misinterpreted by sphinx, and produce
    # lang="extensions" XML attributes

    # Add new node and directive
    app.add_node(flagprint)
    app.add_directive('flag-print', FlagPrintDirective)

    # Add new node and directive
    app.add_node(extensionprint)
    app.add_directive('extension-print', ExtensionPrintDirective)

    # Add our generator and cleanup functions as callbacks
    app.connect('doctree-resolved', process_print_nodes)
    app.connect('env-purge-doc', purge_flags)

    return {'version': '1.0'}
