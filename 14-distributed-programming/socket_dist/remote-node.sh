#!/bin/sh

name="$1"
if [ -z "$name" ] ; then
    me="$(basename $0)"
    echo "Usage: $me <node-name>"
    exit 1
fi

erl -name $name -setcookie abc
