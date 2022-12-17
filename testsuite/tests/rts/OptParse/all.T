test('OptParseTest', [ [extra_files(['OptParseTestUtil.c', 'OptParseTestUtil.h']), c_src], only_ways(['normal','threaded1', 'threaded2']) ], compile_and_run, ['OptParseTestUtil.c'])
