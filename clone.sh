#!/bin/bash

# $1: <workspaces dir>: /workspaces
#
#
#
WORKSPACEDIR=$1

HELPTEXT="./clone.sh <WORKSPACEDIR>"
GITREPOSD="gitrepos.d"


if [[ $WORKSPACEDIR = '' ]];
then
  echo $HELPTEXT
  exit 1
fi

echo clone repositories to $WORKSPACEDIR

# scan repo files
REPOS=$(ls ${GITREPOSD}/*.repo | cut -f2 -d'/')
for REPOFILE in $REPOS;
do
  parent=$(echo $REPOFILE | awk -F".repo" '{ print $1 }')
  echo "[x] Processing ${parent}"
  for LINE in $(cat ${GITREPOSD}/${REPOFILE} | grep -vE '^#.*$');
  do
    rmt=$(echo $LINE | cut -f1 -d'|')
    loc=$(echo $LINE | cut -f2 -d'|')
    locpath="$WORKSPACEDIR/$parent/$loc"
    if [[ -d $locpath ]]; then
      echo "[x] locpath=[$locpath] exists, skip rmt=[$rmt]"
      continue
    fi
    mkdir -p $(dirname $locpath)
    git clone -q https://github.com/$rmt $WORKSPACEDIR/$parent/$loc
    echo "[x] Done clone rmt=[$rmt] to loc=[$parent/$loc]"
  done
done
