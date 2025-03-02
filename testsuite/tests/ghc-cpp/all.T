
# The __GLASGOW_HASKELL_FULL_VERSION__ macro gets updated on every configure.
# Replace the RHS with a constant
def normalise_haskell_full_version( str ):
    return re.sub(r'__GLASGOW_HASKELL_FULL_VERSION__.*\n', '__GLASGOW_HASKELL_FULL_VERSION__ X.XX.XXX', str)

# The __GLASGOW_HASKELL_PATCHLEVEL1__ macro gets updated on every configure.
# Replace the RHS with a constant
def normalise_haskell_pl1( str ):
    return re.sub(r'__GLASGOW_HASKELL_PATCHLEVEL1__.*\n', '__GLASGOW_HASKELL_PATCHLEVEL1__ XXX', str)

test('GhcCpp01',
     # normal,
     [normalise_errmsg_fun(normalise_haskell_full_version, normalise_haskell_pl1)],
     compile,
     ['-ddump-ghc-cpp -dkeep-comments'])
