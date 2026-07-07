from my_typing import *
from datetime import datetime
import xml.etree.ElementTree as ET

from testglobals import TestRun

def junit(t: TestRun) -> ET.ElementTree:
    testsuites = ET.Element('testsuites')
    testsuite = ET.SubElement(testsuites, 'testsuite',
                              id = "0",
                              package = 'ghc',
                              tests = str(t.total_tests),
                              failures = str(len(t.unexpected_failures)
                                             + len(t.unexpected_stat_failures)
                                             + len(t.unexpected_passes)),
                              errors = str(len(t.framework_failures)),
                              skipped = str(len(t.fragile_failures)),
                              timestamp = datetime.now().isoformat())

    for kind, res_type, group in [('failure', 'stat failure', t.unexpected_stat_failures),
                                  ('failure', 'unexpected failure', t.unexpected_failures),
                                  ('failure', 'unexpected pass', t.unexpected_passes),
                                  ('skipped', 'fragile failure', t.fragile_failures)]:
        for tr in group:
            testcase = ET.SubElement(testsuite, 'testcase',
                                     classname = tr.way,
                                     name = '%s(%s)' % (tr.testname, tr.way))
            message = [] # type: List[str]
            if tr.stdout:
                message += ['', 'stdout:', '==========', tr.stdout]
            if tr.stderr:
                message += ['', 'stderr:', '==========', tr.stderr]
            if not message:
                message = [tr.reason]

            result = ET.SubElement(testcase, kind,
                                   type = res_type,
                                   message = tr.reason)
            result.text = '\n'.join(message)

    for tr in t.framework_failures:
        testcase = ET.SubElement(testsuite, 'testcase',
                                 classname = tr.way,
                                 name = '%s(%s)' % (tr.testname, tr.way))
        result = ET.SubElement(testcase, 'error',
                               type = "framework failure",
                               message = tr.reason)

    for tr in t.expected_passes:
        testcase = ET.SubElement(testsuite, 'testcase',
                                 classname = tr.way,
                                 name = '%s(%s)' % (tr.testname, tr.way))

    return ET.ElementTree(testsuites)

