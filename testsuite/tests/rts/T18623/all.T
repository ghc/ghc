# Starting GHC on *nix with vmem limit, RTS will reserve all available memory
# and crash when creating a thread. Fix reserves only 2/3rds  of vmem_limit.
test('T18623',
    [when(opsys('mingw32'), skip),
     # This keeps failing on aarch64-linux for reasons that are not
     # fully clear.  Maybe it needs a higher limit due to LLMV?
     when(arch('aarch64'), skip),
     cmd_prefix('ulimit -v ' + str(1024 ** 2) + ' && '),
     ignore_stdout],
    run_command,
    ['{compiler} --version'])