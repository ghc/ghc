test( 'IfaceSharingFastString'
    , [stat_from_file('iface/faststring', 5, 'SIZE'), extra_files(["Lib.hs"])]
    , compile_and_run
    , ['-package ghc'])

test( 'IfaceSharingName'
    , [stat_from_file('iface/name', 5, 'SIZE'), extra_files(["Lib.hs"])]
    , compile_and_run
    , ['-package ghc'])

test( 'IfaceSharingIfaceType'
    , [stat_from_file('iface/types', 5, 'SIZE'), extra_files(["Lib.hs"])]
    , compile_and_run
    , ['-package ghc'])
