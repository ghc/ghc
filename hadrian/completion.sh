#!/usr/bin/env bash

hadrian=$(cd hadrian; cabal new-exec which hadrian; cd ..)
all_settings=$($hadrian autocomplete --complete-setting="$@" --quiet)

complete -W "$all_settings" hadrian/build
complete -W "$all_settings" hadrian/build-cabal
