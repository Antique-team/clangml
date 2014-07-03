#!/usr/bin/env bash

CLANGML_PATH=/users/absint3/berenger/local_disk/src/clangaml/_build

gcc "$@"

$CLANGML_PATH/consumer/processor.native "$@"
