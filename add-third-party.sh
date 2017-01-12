#!/usr/bin/env bash

function usage {
    echo "./add-third-party.sh <package> <author>"
    exit
}

if [ -z $1 ]; then
    usage
    exit
fi
if [ -z $2 ]; then
    usage
fi

package=$1
author=$2
type=$3

url="https://github.com/${author}/${package}.git"
path=$(echo "third-party/${package}" | tr . -)
remote=$(echo "${author}/${package}" | tr . -)
cmd="git remote add -f ${remote} ${url}"
$cmd
cmd="git subtree add --prefix ${path} ${remote} master --squash"
$cmd
