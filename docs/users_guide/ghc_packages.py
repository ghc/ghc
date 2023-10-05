from docutils import nodes
from docutils.parsers.rst import Directive, directives
from sphinx import addnodes
from sphinx.domains.std import GenericObject
from sphinx.errors import SphinxError
from utils import build_table_from_list

def read_cabal_file(pkg_path):
    import re
    cabal_file = open(pkg_path, 'r').read()
    pkg_name = re.search(r'^[nN]ame\s*:\s*([-a-zA-Z0-9]+)', cabal_file, re.MULTILINE)
    if pkg_name is not None:
        pkg_name = pkg_name.group(1)
    else:
        raise RuntimeError("Failed to parse `Name:` field from %s" % pkg_path)

    pkg_version = re.search(r'^[vV]ersion\s*:\s*(\d+(\.\d+)*)', cabal_file, re.MULTILINE)
    if pkg_version is not None:
        pkg_version = pkg_version.group(1)
    else:
        raise RuntimeError("Failed to parse `Version:` field from %s" % pkg_path)

    return (pkg_name, pkg_version)


class PackageListDirective(Directive):
    has_content = True
    def run(self):
        self.assert_has_content()

        packages = []
        for line in self.content:
            (pkg_path, _, reason) = line.partition(':')
            if len(reason) == 0:
                raise RuntimeError("Missing reason for inclusion of package %s"
                                   % pkg_path)

            # Parse reason
            from docutils.statemachine import ViewList
            reason_vl = ViewList(initlist=[reason.strip()])
            reason_node = nodes.paragraph()
            self.state.nested_parse(reason_vl, 0, reason_node)
            packages.append((pkg_path, reason_node))

        # Create column headers for table
        header = [ nodes.inline(text=h)
                   for h in ["Package", "Version", "Reason for inclusion"] ]
        package_list = [header]

        for (pkg_path, reason) in sorted(packages):
            (pkg_name, pkg_version) = read_cabal_file(pkg_path)
            cells = [ nodes.paragraph(text=pkg_name),
                      nodes.inline(text=pkg_version),
                      reason ]
            package_list.append(cells)

        table = build_table_from_list(package_list, [20, 20, 40])
        table['classes'].append('longtable')
        return [table]

### Initialization
def setup(app):
    app.add_directive('ghc-package-list', PackageListDirective)

    return {'version': '1.0'}
