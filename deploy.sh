#!/bin/bash

set -e

SCRIPTDIR="$(cd "$(dirname "$BASH_SOURCE[0]" )" && pwd)"

symlinkFile() {
    src="${SCRIPTDIR}/$1"
    dst="$(echo $HOME/$2/$1 | sed 's/\/\//\//g')"
    mkdir -p "$(dirname $dst)"
    if [ -L "$dst" ]; then
        echo "[WARN] symlink is already symlinked: $dst"
        return
    fi

    if [ -e "$dst" ]; then
        echo "[ERROR] not-symlink file exists at the symlink target: $dst. Skip."
        return
    fi
    ln -s $src $dst
    echo "[INFO] linked: $src -> $dst"
}

deploy() {
    echo $SCRIPTDIR
    for row in $(cat $SCRIPTDIR/$1); do
        if [[ "$row" =~ ^#.* ]]; then
            continue
        fi
        operation=$(echo $row | cut -f1 -d'|')
        case $operation in
            symlink)
                symlinkFile $(echo $row | cut -f2 -d'|') $(echo $row | cut -f3 -d'|')
                ;;
            *)
                echo "[WARN] unknown operation $operation"
                ;;
        esac
    done
}

if [ -z "@" ]; then
    echo "Usage: $0 <MANIFEST>"
    exit 1
fi

deploy $1
