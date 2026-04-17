# .stats files aren't yet supported in the JS backend
setTestOpts(js_skip)

#--------------------------------------
# Check optimization of `elem`
#--------------------------------------

def elemCoreTest(test_name, module_name, opt):
    test(test_name,
         [only_ways(['normal']), extra_files([module_name + '.hs'])],
         multimod_compile_filter,
         [module_name,
          f'{opt} -ddump-simpl -dsuppress-all -dsuppress-uniques -dno-typeable-binds',
          "sed -En '/^(is|fusion|noFusion)[A-Za-z]*($| )/,/^$/p'"])

elemCoreTest('T17752_O1', 'T17752', '-O1')
elemCoreTest('T17752_O2', 'T17752', '-O2')
elemCoreTest('ElemFusionUnknownList_O1', 'ElemFusionUnknownList', '-O1')
elemCoreTest('ElemFusionUnknownList_O2', 'ElemFusionUnknownList', '-O2')
elemCoreTest('ElemNoFusion_O1', 'ElemNoFusion', '-O1')
elemCoreTest('ElemNoFusion_O2', 'ElemNoFusion', '-O2')

#--------------------------------------

# We don't expect the code in test to vary at all, but the variance is set to
# 1% in case the constant allocations increase by other means.
test('encodingAllocations', [only_ways(['normal']), collect_stats('bytes allocated', 1)], compile_and_run, ['-O2'])
