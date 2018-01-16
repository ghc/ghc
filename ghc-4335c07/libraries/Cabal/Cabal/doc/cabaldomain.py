# -*- coding: utf-8 -*-
'''
Sphinx domain for documenting all things cabal

The main reason to use this instead of adding object types to std domain
is the ability to generate nice 'Reference' page and also provide some meta
data for objects described with directives described here.

Most directives have at least following optional arguments

`:since: 1.23`
    version of Cabal in which feature was added.

`:deprecated: 1.23`
`:deprecated:`
    Feature was deprecatead, and optionally since which version.

`:synopsis: Short desc`
    Text used as short description on reference page.


Added directives

.. rst:directive:: .. cabal::cfg-section

   Describes a package.cabal section, such as library or exectuble.

   All following `pkg-field` directives will add section name
   to their fields name for disambiguating duplicates.

   You can reset the section disambguation with with `.. pkg-section:: None`.

.. rst::role:: pkg-section

   References section added by `.. pkg-section`

.. rst:directive:: .. cabal::pkg-field

   Describes a package.cabal field.

   Can have a :default: field. Will group on reference page under pkg-section
   if set and parent header otherwise.

.. rst::role:: pkg-field

   References field added by `.. pkg-field`, fields can be disambiguated
   with section name `:pkg-field:`section:field`.


.. rst:directive:: .. cabal:cfg-section::

   Same as `.. cabal::pkg-section` but does not produce any visible output
   currently unused.

.. rst:directive:: .. cabal:cfg-field::

   Describes a project.cabal field.

   Can have multiple arguments, if arguments start with '-' then it is treated
   as a cabal flag.

   Can have a :default: field. Will group on reference page under pkg-section
   if set and parent header otherwise.

.. rst::role:: cfg-field

   References field added by `.. cfg-field`.

.. rst::role:: cfg-flag

   References flag added by `.. cfg-field`.


All roles can be supplied with title as in standard sphinx references::

   :pkg-field:`Build dependencies<build-depends>`


To be done:

- Directives for describing executables, their subcommands and flags.

  These should act in a way similar to `.. std::option` directive, but with
  extra meta. And should also end up in reference.

  At least setup and 'new-build` subcommands should get special directvies

- Improve rendering of flags in `.. cfg-field::` directive. It should be
  possible without copy-pasting code from sphinx.directives.ObjectDescription
  by examining result of ObjectDescription.run and inserting flags into
  desc_content node.

  Alternatively Or `.. flags::` sub-directive can be added which will be aware
  of parent `.. cfg-field` directive.

- With same ObjectDescription.run trick as above, render since and deprecated
  info same way as standard object fields, and use fancy rendering only on
  references page.

- Add 'since_version` config value to sphinx env and use it to decide if
  version meta info should be rendered on reference page and thus reduce some
  clutter.
  Can also be used to generate 'Whats new' reference page

'''


import re

from docutils import nodes
from docutils.parsers.rst import Directive, directives, roles

import pygments.lexer as lexer
import pygments.token as token

from distutils.version import StrictVersion

from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import ObjType, Domain, Index
from sphinx.domains.std import StandardDomain
from sphinx.locale import l_, _
from sphinx.roles import XRefRole
from sphinx.util.docfields import Field, DocFieldTransformer
from sphinx.util.nodes import make_refnode

def parse_deprecated(txt):
    if txt is None:
        return True
    try:
        return StrictVersion(txt)
    except ValueError:
        return True

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


class Meta(object):
    '''
    Meta data associated with object
    '''
    def __init__(self,
                 since=None,
                 deprecated=None,
                 synopsis=None,
                 title=None,
                 section=None,
                 index=0):
        self.since = since
        self.deprecated = deprecated
        self.synopsis = synopsis
        self.title = title
        self.section = section
        self.index = index


def find_section_title(parent):
    '''
    Find current section id and title if possible
    '''
    while parent is not None:
        if isinstance(parent, nodes.section):
            break
        parent = parent.parent

    if parent is None:
        return None

    section_id = parent['ids'][0]
    section_name = parent['names'][0]

    for kid in parent:
        if isinstance(kid, nodes.title):
            return kid.astext(), section_id

    print section_name, section_id
    return section_name, section_id


class CabalSection(Directive):
    """
    Marks section to which following objects belong, used to disambiguate
    references to fields and flags which can have similar names

    Does not generate any output besides anchor.
    """
    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {
        'name': lambda x: x,
        'deprecated': parse_deprecated,
        'since' : StrictVersion,
        'synopsis' : lambda x:x,
    }
    section_key = 'cabal:pkg-section'
    target_prefix = 'pkg-section-'
    indextemplate = ''
    indextype = 'pair'

    def get_index_entry(self, name):
        return self.indextemplate % name

    def run(self):
        env = self.state.document.settings.env
        section = self.arguments[0].strip()

        if ':' in self.name:
            self.domain, self.objtype = self.name.split(':', 1)
        else:
            self.domain, self.objtype = '', self.name

        if section == 'None':
            env.ref_context.pop(self.section_key, None)
            return []

        env.ref_context[self.section_key] = section
        targetname = self.target_prefix + section
        node = nodes.target('', '', ids=[targetname])
        self.state.document.note_explicit_target(node)

        indexentry = self.get_index_entry(section)

        inode = addnodes.index(
            entries = [
                (self.indextype, indexentry, targetname, '', None)])

        # find title of parent section node
        title = find_section_title(self.state.parent)

        data_key = CabalDomain.types[self.objtype]

        # find how many sections in this document were added
        num = env.domaindata['cabal']['index-num'].get(env.docname, 0)
        env.domaindata['cabal']['index-num'][env.docname] = num + 1

        meta = Meta(since=self.options.get('since'),
                    deprecated=self.options.get('deprecated'),
                    synopsis=self.options.get('synopsis'),
                    index = num,
                    title = title)

        store = env.domaindata['cabal'][data_key]
        if not section in store:
            store[section] = env.docname, targetname, meta

        return [inode, node]


class CabalObject(ObjectDescription):
    option_spec = {
        'noindex'   : directives.flag,
        'deprecated': parse_deprecated,
        'since'     : StrictVersion,
        'synopsis'  : lambda x:x
    }

    # node attribute marking which section field belongs to
    section_key = ''
    # template for index, it is passed a field name as argument
    # used by default deg_index_entry method
    indextemplate = ''

    def get_meta(self):
        '''
        Collect meta data for fields

        Reads optional arguments passed to directive and also
        tries to find current section title and adds it as section
        '''
        env = self.state.document.settings.env
        # find title of current section, will group references page by it
        num = env.domaindata['cabal']['index-num'].get(env.docname, 0)
        env.domaindata['cabal']['index-num'][env.docname] = num + 1

        title = find_section_title(self.state.parent)
        return Meta(since=self.options.get('since'),
                    deprecated=self.options.get('deprecated'),
                    title=title,
                    index = num,
                    synopsis=self.options.get('synopsis'))

    def get_env_key(self, env, name):
        '''
        Should return a key used to reference this field and key in domain
        data to store this object
        '''
        section = self.env.ref_context.get(self.section_key)
        store = CabalDomain.types[self.objtype]
        return (section, name), store

    def get_index_entry(self, env, name):
        '''
        Should return index entry and achor

        By default uses indextemplate attribute to generate name and
        index entry by joining directive name, section and field name
        '''
        section = self.env.ref_context.get(self.section_key)

        if section is not None:
            parts = (self.objtype, section, name)
            indexentry = self.indextemplate % (section + ':' + name)
        else:
            parts = (self.objtype, name)
            indexentry = self.indextemplate % name

        targetname = '-'.join(parts)
        return indexentry, targetname


    def add_target_and_index(self, name, sig, signode):
        '''
        As in sphinx.directive.ObjectDescription

        By default adds 'pair' index as returned by get_index_entry and
        stores object data into domain data store as returned by get_env_data
        '''
        env = self.state.document.settings.env

        indexentry, targetname = self.get_index_entry(self, name)

        signode['ids'].append(targetname)
        self.state.document.note_explicit_target(signode)

        inode = addnodes.index(
            entries=[('pair', indexentry, targetname, '', None)])
        signode.insert(0, inode)

        key, store = self.get_env_key(env, name)
        env.domaindata['cabal'][store][key] = env.docname, targetname, self.cabal_meta

    def run(self):
        self.cabal_meta = self.get_meta()
        result = super(CabalObject, self).run()

        if self.cabal_meta.since is not None \
           or self.cabal_meta.deprecated is not None:

            #find content part of description
            for item in result:
                if isinstance(item, addnodes.desc):
                    desc = item
                    break
            else:
                return result

            for item in desc:
                if isinstance(item, addnodes.desc_content):
                    contents = item
                    break
            else:
                return result

            # find exsting field list and add to it
            # or create new one
            for item in contents:
                if isinstance(item, nodes.field_list):
                    field_list = item
                    break
            else:
                field_list = nodes.field_list('')
                contents.insert(0, field_list)


            if self.cabal_meta.since is not None:
                #docutils horror
                field = nodes.field('')
                field_name = nodes.field_name('Since', 'Since')
                since = 'Cabal ' + str(self.cabal_meta.since)
                field_body = nodes.field_body(since, nodes.paragraph(since, since))
                field += field_name
                field += field_body
                field_list.insert(0, field)

            if self.cabal_meta.deprecated is not None:
                field = nodes.field('')
                field_name = nodes.field_name('Deprecated', 'Deprecated')
                if isinstance(self.cabal_meta.deprecated, StrictVersion):
                    since = 'Cabal ' + str(self.cabal_meta.deprecated)
                else:
                    since = ''

                field_body = nodes.field_body(since, nodes.paragraph(since, since))
                field += field_name
                field += field_body
                field_list.insert(0, field)

        return result

class CabalPackageSection(CabalObject):
    """
    Cabal section in package.cabal file
    """
    section_key = 'cabal:pkg-section'
    indextemplate = '%s; package.cabal section'

    def handle_signature(self, sig, signode):
        '''
        As in sphinx.directives.ObjectDescription

        By default make an object description from name and adding
        either deprecated or since as annotation.
        '''
        env = self.state.document.settings.env

        sig = sig.strip()
        parts = sig.split(' ',1)
        name = parts[0]
        signode += addnodes.desc_name(name, name)
        signode += addnodes.desc_addname(' ', ' ')
        if len(parts) > 1:
            rest = parts[1].strip()
            signode += addnodes.desc_annotation(rest, rest)

        return name

    def get_env_key(self, env, name):
        store = CabalDomain.types[self.objtype]
        return name, store

    def run(self):
        env = self.state.document.settings.env
        section = self.arguments[0].strip().split(' ',1)[0]
        if section == 'None':
            env.ref_context.pop('cabal:pkg-section', None)
            return []
        env.ref_context['cabal:pkg-section'] = section
        return super(CabalPackageSection, self).run()


class CabalField(CabalObject):
    '''
    Base for fields in *.cabal files
    '''
    option_spec = {
        'noindex'   : directives.flag,
        'deprecated': parse_deprecated,
        'since'     : StrictVersion,
        'synopsis'  : lambda x:x
    }

    doc_field_types = [
        Field('default', label='Default value', names=['default'], has_arg=False)
    ]

    def handle_signature(self, sig, signode):
        '''
        As in sphinx.directives.ObjectDescription

        By default make an object description from name and adding
        either deprecated or since as annotation.
        '''
        env = self.state.document.settings.env

        sig = sig.strip()
        parts = sig.split(':',1)
        name = parts[0]
        signode += addnodes.desc_name(name, name)
        signode += addnodes.desc_addname(': ', ': ')

        if len(parts) > 1:
            rest = parts[1].strip()
            signode += addnodes.desc_annotation(rest, rest)

        return name

class CabalPackageField(CabalField):
    '''
    Describes section in package.cabal file
    '''
    section_key = 'cabal:pkg-section'
    indextemplate = '%s; package.cabal field'

class CabalFieldXRef(XRefRole):
    '''
    Cross ref node for all kinds of fields

    Gets section_key entry from context and stores it on node, so it can
    later be used by CabalDomain.resolve_xref to find target for reference to
    this
    '''
    section_key = 'cabal:pkg-section'
    def process_link(self, env, refnode, has_explicit_title, title, target):
        parts = target.split(':',1)
        if len(parts) == 2:
            section, target = parts
            section = section.strip()
            target = target.strip()
            refnode[self.section_key] = section
        else:
            refnode[self.section_key] = env.ref_context.get(self.section_key)

        return title, target

#
# Directives for config files.
#

class CabalPackageFieldXRef(CabalFieldXRef):
    '''
    Role referencing project.cabal section
    '''
    section_key = 'cabal:pkg-section'

class CabalConfigSection(CabalSection):
    """
    Marks section in package.cabal file
    """
    indextemplate = '%s; project.cabal section'
    section_key = 'cabal:cfg-section'
    target_prefix = 'cfg-section-'

class ConfigField(CabalField):
    section_key = 'cabal:cfg-section'
    indextemplate = '%s ; cabal project option'
    def handle_signature(self, sig, signode):
        sig = sig.strip()
        if sig.startswith('-'):
            name = parse_flag(self, sig, signode)
        else:
            name = super(ConfigField, self).handle_signature(sig, signode)

        return name

    def get_index_entry(self, env, name):
        if name.startswith('-'):
            section = self.env.ref_context.get(self.section_key)
            if section is not None:
                parts = ('cfg-flag', section, name)
                indexname = section + ':' + name
            else:
                parts = ('cfg-flag', name)
                indexname = name
            indexentry = name + '; cabal project option'
            targetname = '-'.join(parts)
            return indexentry, targetname
        else:
            return super(ConfigField,self).get_index_entry(env, name)

    def get_env_key(self, env, name):
        section = self.env.ref_context.get(self.section_key)
        if name.startswith('-'):
            return (section, name), 'cfg-flags'
        return (section, name), 'cfg-fields'

class CabalConfigFieldXRef(CabalFieldXRef):
    section_key = 'cabal:cfg-section'


#
# Cabal domain
#

class ConfigFieldIndex(Index):
    name = 'projectindex'
    localname = "Cabal reference"
    shortname = "Reference"

    class Entry(object):
        def __init__(self, typ, name, doc, anchor, meta):
            self.type = typ
            self.name = name
            self.doc = doc
            self.anchor = anchor
            self.meta = meta

    def _gather_data(self, obj_types):
        '''
        Gather objects and return [(title, [Entry])]
        '''
        def massage(typ, datum):
            name, (doc, anchor, meta) = datum
            return self.Entry(typ, name, doc, anchor, meta)

        fields = []
        for typ in obj_types:
            store = CabalDomain.types[typ]
            fields += [massage(typ, x)
                      for x in self.domain.data[store].items()]

        fields.sort(key=lambda x: (x.doc, x.meta.index))

        if len(fields) == 0:
            return []

        result = []
        current = []
        current_title = fields[0].meta.title
        for field in fields:
            if field.meta.title != current_title:
                result.append((current_title, current))
                current = []
                current_title = field.meta.title
            current.append(field)
        result.append((current_title, current))

        return result


    def generate(self, docnames=None):
        '''
        Try to group entries such that if entry has a section then put it
        into same group.

        Otherwise group it under same `title`.

        Try to keep in same order as it was defined.

        sort by (document, index)
        group on (document, doc_section)

        TODO: Check how to extract section numbers from (document,doc_section)
              and add it as annotation to titles
        '''

        # (title, section store, fields store)
        entries = [('project.cabal fields', 'cfg-section', 'cfg-field'),
                   ('cabal project flags', 'cfg-section', 'cfg-flag'),
                   ('package.cabal fields', 'pkg-section', 'pkg-field')]

        result = []
        for label, section_key, key in entries:

            data = self._gather_data([section_key, key])

            references = []
            for section, entries in data:
                if section is None:
                    elem_type = 0 # Normal entry
                else:
                    elem_type = 2 # sub_entry

                assert len(entries) != 0
                docname = entries[0].doc
                if section is not None:
                    section_title, section_anchor = section
                    references.append(
                        (section_title, 1, docname, section_anchor, '', '', ''))

                for entry in entries:
                    #todo deal with if
                    if isinstance(entry.name, tuple):
                        name = entry.name[1]
                    else:
                        name = entry.name

                    meta = entry.meta
                    extra = render_meta(meta)
                    descr = meta.synopsis if meta.synopsis is not None else ''
                    field = (name, elem_type, docname,
                             entry.anchor, extra, '', descr)
                    references.append(field)
            result.append((label, references))

        return result, False

def make_data_keys(typ, target, node):
    '''
    Returns a list of keys to search for targets of this type
    in domain data.

    Used for resolving references
    '''
    if typ == 'pkg-field':
        section = node.get('cabal:pkg-section')
        return [(section, target),
                (None, target)]
    elif typ in ('cfg-field', 'cfg-flag'):
        section = node.get('cabal:cfg-section')
        return [(section, target), (None, target)]
    else:
        return [target]


def render_deprecated(deprecated):
    if isinstance(deprecated, StrictVersion):
        return 'deprecated since: '+str(deprecated)
    else:
        return 'deprecated'


def render_meta(meta):
    '''
    Render meta as short text

    Will render either deprecated or since info
    '''
    if meta.deprecated is not None:
        return render_deprecated(meta.deprecated)
    elif meta.since is not None:
        return 'since version: ' + str(meta.since)
    else:
        return ''

def render_meta_title(meta):
    '''
    Render meta as suitable to use in titles
    '''
    rendered = render_meta(meta)
    if rendered != '':
        return '(' + rendered + ')'
    return ''

def make_title(typ, key, meta):
    '''
    Render title of an object (section, field or flag)
    '''
    if typ == 'pkg-section':
        return "package.cabal " + key + " section " + render_meta_title(meta)

    elif typ == 'pkg-field':
        section, name = key
        if section is not None:
            base = "package.cabal " + section + " section " + name + ": field"
        else:
            base = "package.cabal " + name + " field"

        return base + render_meta_title(meta)

    elif typ == 'cfg-section':
        return "project.cabal " + key + " section " + render_meta_title(meta)

    elif typ == 'cfg-field':
        section, name = key
        return "project.cabal " + name + " field " + render_meta_title(meta)

    elif typ == 'cfg-flag':
        section, name = key
        return "cabal flag " + name + " " + render_meta_title(meta)

    else:
        raise ValueError("Unknown type: " + typ)

def make_full_name(typ, key, meta):
    '''
    Return an anchor name for object type
    '''
    if typ == 'pkg-section':
        return 'pkg-section-' + key

    elif typ == 'pkg-field':
        section, name = key
        if section is not None:
            return '-'.join(('pkg-field',section, name))
        else:
            return 'pkg-field-' + name

    elif typ == 'cfg-field':
        return 'cfg-field-' + key

    else:
        raise ValueError('Unknown object type: ' + typ)

class CabalDomain(Domain):
    '''
    Sphinx domain for cabal

    needs Domain.merge_doc for parallel building, just union all dicts
    '''
    name = 'cabal'
    label = 'Cabal'
    object_types = {
        'pkg-section': ObjType(l_('pkg-section'), 'pkg-section'),
        'pkg-field'  : ObjType(l_('pkg-field')  , 'pkg-field'  ),
        'cfg-section': ObjType(l_('cfg-section'), 'cfg-section'),
        'cfg-field'  : ObjType(l_('cfg-field')  , 'cfg-field' ),
    }
    directives = {
        'pkg-section': CabalPackageSection,
        'pkg-field'  : CabalPackageField,
        'cfg-section': CabalConfigSection,
        'cfg-field'  : ConfigField,
    }
    roles = {
        'pkg-section': XRefRole(warn_dangling=True),
        'pkg-field'  : CabalPackageFieldXRef(warn_dangling=True),
        'cfg-section': XRefRole(warn_dangling=True),
        'cfg-field'  : CabalConfigFieldXRef(warn_dangling=True),
        'cfg-flag'   : CabalConfigFieldXRef(warn_dangling=True),
    }
    initial_data = {
        'pkg-sections': {},
        'pkg-fields'  : {},
        'cfg-sections': {},
        'index-num'   : {}, #per document number of objects
                            # used to order references page
        'cfg-fields'  : {},
        'cfg-flags'   : {},
    }
    indices = [
        ConfigFieldIndex
    ]
    types = {
        'pkg-section': 'pkg-sections',
        'pkg-field'  : 'pkg-fields',
        'cfg-section': 'cfg-sections',
        'cfg-field'  : 'cfg-fields',
        'cfg-flag'   : 'cfg-flags',
    }
    def clear_doc(self, docname):
        for k in ['pkg-sections', 'pkg-fields', 'cfg-sections',
                  'cfg-fields', 'cfg-flags']:
            for name, (fn, _, _) in self.data[k].items():
                if fn == docname:
                    del self.data[k][comname]
        try:
            del self.data['index-num'][docname]
        except KeyError:
            pass

    def resolve_xref(self, env, fromdocname, builder, type, target, node, contnode):
        objtypes = self.objtypes_for_role(type)
        for typ, key in ((typ, key)
                         for typ in objtypes
                         for key in make_data_keys(typ, target, node)):
            try:
                data = env.domaindata['cabal'][self.types[typ]][key]
            except KeyError:
                continue
            doc, ref, meta = data
            title = make_title(typ, key, meta)
            return make_refnode(builder, fromdocname, doc, ref, contnode, title)

    def get_objects(self):
        '''
        Used for search functionality
        '''
        for typ in ['pkg-section', 'pkg-field',
                    'cfg-section', 'cfg-field', 'cfg-flag']:
            key = self.types[typ]
            for name, (fn, target, meta) in self.data[key].items():
                title = make_title(typ, name, meta)
                yield title, title, typ, fn, target, 0

class CabalLexer(lexer.RegexLexer):
    '''
    Basic cabal lexer, does not try to be smart
    '''
    name = 'Cabal'
    aliases = ['cabal']
    filenames = ['.cabal']
    flags = re.MULTILINE

    tokens = {
      'root' : [
          (r'^(\s*)(--.*)$', lexer.bygroups(token.Whitespace, token.Comment.Single)),
          # key: value
          (r'^(\s*)([\w\-_]+)(:)',
           lexer.bygroups(token.Whitespace, token.Keyword, token.Punctuation)),
          (r'^([\w\-_]+)', token.Keyword), # library, executable, flag etc.
          (r'[^\S\n]+', token.Text),
          (r'&&|\|\||==|<=|\^>=|>=|<|>', token.Operator),
          (r',|:|{|}', token.Punctuation),
          (r'.', token.Text)
      ],
    }

def setup(app):
    app.add_domain(CabalDomain)
    app.add_lexer('cabal', CabalLexer())

