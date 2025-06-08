
# The __GLASGOW_HASKELL_FULL_VERSION__ macro gets updated on every configure.
# Replace the RHS with a constant
def normalise_haskell_full_version( str ):
    return re.sub(r'__GLASGOW_HASKELL_FULL_VERSION__.*\n', '__GLASGOW_HASKELL_FULL_VERSION__ X.XX.XXX', str)

# The __GLASGOW_HASKELL_PATCHLEVEL1__ macro gets updated on every configure.
# Replace the RHS with a constant
def normalise_haskell_pl1( str ):
    return re.sub(r'__GLASGOW_HASKELL_PATCHLEVEL1__.*\n', '__GLASGOW_HASKELL_PATCHLEVEL1__ XXX', str)

# The MIN_VERSION_GLASGOW_HASKELL macro gets updated on every configure.
# Replace the RHS with a constant
def normalise_min_version_haskell( str ):
    return re.sub(r'MIN_VERSION_GLASGOW_HASKELL.*\n', 'MIN_VERSION_GLASGOW_HASKELL XXX', str)

test('GhcCpp01',
     # normal,
     [normalise_errmsg_fun(normalise_haskell_full_version, normalise_haskell_pl1,normalise_min_version_haskell)],
     compile,
     ['-ddump-ghc-cpp -dkeep-comments'])

test('GhcCpp02', normal, compile_fail, [''])