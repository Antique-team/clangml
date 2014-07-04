#!/usr/bin/env bash

#set -e

# EDIT THIS TO FIT YOUR SETUP
export CLANGML_PATH=/users/absint3/berenger/local_disk/src/clangaml/_build

# unless we are linking
if [[ $@ != *\ -l* ]] ; then
    $CLANGML_PATH/consumer/processor.native "$@"
fi

if [ "$?" != "0" ] ; then
    echo "##### THERE WAS A PROBLEM, look the culprit file into .libs"
    gcc "$@" -E
else
    gcc "$@"
fi
