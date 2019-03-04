#!/bin/bash

# This file is a wrapper to run uw.el for vim nerds

# see http://mywiki.wooledge.org/BashFAQ/028
uw_el_path="${BASH_SOURCE%/*}/uw.el"
if [[ ! -r $uw_el_path ]]
then
    echo 'Unable to find uw.el -- make sure you are not doing bash < uw.sh'
    exit 1
fi
if [[ -z $1 || -z $2 ]]
then
    echo 'USAGE: uw.sh my_netid_username Pa55w0rd'
    exit 1
fi
if ! command -v emacs >/dev/null
then
    echo 'Emacs is not installed.'
    exit 1
fi

el_script=$(mktemp)
echo "(setq uw-netid-username \"$1\" uw-netid-password \"$2\")" > "$el_script"
cat "$uw_el_path" >> "$el_script"
echo "(uw-check)" >> "$el_script"
emacs -q --script "$el_script"
