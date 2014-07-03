#!/usr/bin/env bash

# EDIT THIS TO FIT YOUR SETUP
CLANGML_PATH=/users/absint3/berenger/local_disk/src/clangaml/_build

gcc "$@"

$CLANGML_PATH/consumer/processor.native "$@"
