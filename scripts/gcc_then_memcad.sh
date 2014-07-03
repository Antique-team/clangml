#!/usr/bin/env bash

# EDIT THIS TO FIT YOUR SETUP
export CLANGML_PATH=/users/absint3/berenger/local_disk/src/clangaml/_build

$CLANGML_PATH/consumer/processor.native "$@"

gcc "$@"
#gcc "$@" -E
