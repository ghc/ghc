@rem Change the current directory to the one containing this script
@cd %~dp0

@rem Build Hadrian and dependencies
@stack build

@rem Run Hadrian in GHC top directory forwarding additional user arguments
@stack exec hadrian -- --lint --directory ".." %*
