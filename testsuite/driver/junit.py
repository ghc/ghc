from my_typing import *
from datetime import datetime
import xml.etree.ElementTree as ET

from testglobals import TestRun, TestResult


def add_testcase(testsuite: ET.Element, test_result: TestResult) -> ET.Element:
    """Adds the xml element representing the testcase."""
    testcase = ET.SubElement(testsuite, 'testcase',
                             classname = test_result.way,
                             name = '%s(%s)' % (test_result.testname, test_result.way))
    if test_result.runtime is not None:
        testcase.set('time', '%.3f' % test_result.runtime)
    return testcase

def add_result(testcase: ET.Element, kind: str, res_type: str,
               reason: Optional[str], text: str) -> ET.Element:
    """Add a result to the testcase."""
    # GitLab ignores the message attribute and shows only the text.
    result = ET.SubElement(testcase, kind,
                           type = res_type,
                           message = reason or '')
    result.text = text
    return result

def junit(t: TestRun) -> ET.ElementTree:
    # Report time spent in testsuite.
    assert t.start_time is not None
    elapsed = '%.3f' % (datetime.now() - t.start_time).total_seconds()
    testsuites = ET.Element('testsuites', time = elapsed)
    testsuite = ET.SubElement(testsuites, 'testsuite',
                              id = "0",
                              package = 'ghc',
                              tests = str(t.total_tests),
                              failures = str(len(t.unexpected_failures)
                                             + len(t.unexpected_stat_failures)
                                             + len(t.unexpected_passes)),
                              errors = str(len(t.framework_failures)),
                              skipped = str(len(t.fragile_failures)),
                              time = elapsed,
                              timestamp = datetime.now().isoformat())

    for kind, res_type, group in [('failure', 'stat failure', t.unexpected_stat_failures),
                                  ('failure', 'unexpected failure', t.unexpected_failures),
                                  ('failure', 'unexpected pass', t.unexpected_passes),
                                  ('skipped', 'fragile failure', t.fragile_failures)]:
        for tr in group:
            testcase = add_testcase(testsuite, tr)
            message = [] # type: List[str]
            if tr.diff:
                message += ['diff:', '==========', tr.diff]
            if tr.stdout:
                message += ['', 'stdout:', '==========', tr.stdout]
            if tr.stderr:
                message += ['', 'stderr:', '==========', tr.stderr]
            if not message:
                message = [tr.reason]

            add_result(testcase, kind, res_type, tr.reason, '\n'.join(message))

    for tr in t.framework_failures:
        testcase = add_testcase(testsuite, tr)
        add_result(testcase, 'error', 'framework failure', tr.reason, tr.reason)

    # Expected failures (expect_broken) behaved as expected, so they count as
    # passes, like in the driver's exit code.
    for tr in t.expected_passes + t.expected_failures + t.fragile_passes:
        add_testcase(testsuite, tr)

    return ET.ElementTree(testsuites)

