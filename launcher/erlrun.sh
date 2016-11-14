#!/bin/sh

ERL="$(readlink -en `dirname $0`)/bin/erl"

if [ -z "$1" ]; then
	$ERL -detached
else
	$ERL -detached -enotepad file "\"$1\""
fi
