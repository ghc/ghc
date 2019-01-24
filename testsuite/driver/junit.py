from datetime import datetime
import xml.etree.ElementTree as ET

def junit(t):
    testsuites = ET.Element('testsuites')
    testsuite = ET.SubElement(testsuites, 'testsuite',
                              id = "0",
                              package = 'ghc',
                              tests = str(t.total_tests),
                              failures = str(len(t.unexpected_failures) + len(t.unexpected_stat_failures)),
                              errors = str(len(t.framework_failures)),
                              timestamp = datetime.now().isoformat())

    for res_type, group in [('stat failure', t.unexpected_stat_failures),
                          ('unexpected failure', t.unexpected_failures)]:
        for tr in group:
            testcase = ET.SubElement(testsuite, 'testcase',
                                     classname = tr.way,
                                     name = '%s(%sb)' % (tr.testname, tr.way))
            new_reason = "\n".join([tr.reason, "STDERR:", tr.stderr.decode("utf-8")]) if tr.stderr else tr.reason
            result = ET.SubElement(testcase, 'failure',
                                   type = res_type,
                                   message = new_reason)

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

