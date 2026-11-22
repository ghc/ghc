test('NamespaceInterfaces',
     [req_th, req_host_target_ghc, only_ways(['normal']),
      extra_files(['Provider.hs', 'Forward.hs', 'Main.hs', 'Ambiguous.hs'])],
     run_command, ['bash NamespaceInterfaces.sh "{compiler}" "{libdir}"'])
