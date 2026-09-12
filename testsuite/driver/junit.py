from my_typing import *
from datetime import datetime
import xml.etree.ElementTree as ET

from testglobals import TestRun

def junit(t: TestRun) -> ET.ElementTree:
    elapsed = {(x.testname, x.way): x.elapsed for x in t.timings}
    def testcase(testsuite, tr):
        attrs = {'classname': tr.way, 'name': '%s(%s)' % (tr.testname, tr.way)}
        if (tr.testname, tr.way) in elapsed:
            attrs['time'] = '%.3f' % elapsed[(tr.testname, tr.way)]
        return ET.SubElement(testsuite, 'testcase', attrs)

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
            tc = testcase(testsuite, tr)
            message = [] # type: List[str]
            if tr.diff:
                message += ['diff:', '==========', tr.diff]
            if tr.stdout:
                message += ['', 'stdout:', '==========', tr.stdout]
            if tr.stderr:
                message += ['', 'stderr:', '==========', tr.stderr]
            if not message:
                message = [tr.reason]

            result = ET.SubElement(tc, kind,
                                   type = res_type,
                                   message = tr.reason)
            result.text = '\n'.join(message)

    for tr in t.framework_failures:
        tc = testcase(testsuite, tr)
        result = ET.SubElement(tc, 'error',
                               type = "framework failure",
                               message = tr.reason)

    # A testcase without a failure/error/skipped child is a pass. Expected
    # failures and fragile passes are passes by the driver's verdict too.
    for tr in t.expected_passes + t.expected_failures + t.fragile_passes:
        testcase(testsuite, tr)

    return ET.ElementTree(testsuites)

