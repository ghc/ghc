#!/usr/bin/env zsh

hadrian/build -j12 --flavour='quickest' --build-root=_build-quickest --freeze1 test --only=$1
