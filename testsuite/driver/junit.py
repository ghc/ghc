from datetime import datetime
import xml.etree.ElementTree as ET
from xml.sax.saxutils import escape

def junit(t):
    testsuites = ET.Element('testsuites')
    testsuite = ET.SubElement(testsuites, 'testsuite',
                              id = "0",
                              package = 'ghc',
                              tests = str(t.total_tests),
                              failures = str(len(t.unexpected_failures) + len(t.unexpected_stat_failures)),
                              errors = str(len(t.framework_failures)),
                              timestamp = datetime.now().isoformat())

    for result, group in [('stat failure', t.unexpected_stat_failures),
                          ('unexpected failure', t.unexpected_failures)]:
        for (directory, testname, reason, way) in group:
            testcase = ET.SubElement(testsuite, 'testcase',
                                     classname = testname,
                                     name = way)
            result = ET.SubElement(testcase, 'failure',
                                   type = 'unexpected failure',
                                   message = escape(reason))

    for (directory, testname, reason, way) in t.framework_failures:
        testcase = ET.SubElement(testsuite, 'testcase',
                                 classname = testname,
                                 name = escape(way))
        result = ET.SubElement(testcase, 'error',
                               type = "framework failure",
                               message = escape(reason))

    for (directory, testname, way) in t.expected_passes:
        testcase = ET.SubElement(testsuite, 'testcase',
                                 classname = testname,
                                 name = escape(way))

    return ET.ElementTree(testsuites)

