#!/bin/bash

# get package manager
getPackageManager() {
    declare -A osInfo;
    osInfo[/etc/redhat-release]=dnf
    osInfo[/etc/arch-release]=pacman
    osInfo[/etc/gentoo-release]=emerge
    osInfo[/etc/SuSE-release]=zypp
    osInfo[/etc/debian_version]=apt-get
    osInfo[/etc/alpine-release]=apk
    for f in ${!osInfo[@]}
    do
        if [[ -f $f ]];then
            echo ${osInfo[$f]}
        fi
    done
}

setupRepositories() {
  PKGMGR=$(getPackageManager)
  PKGMGR_EXEC=$(which $PKGMGR)

  sh -c "sudo add-apt-repository -y ppa:neovim-ppa/unstable"
}

# install packages
installPackages() {
  PKGMGR=$(getPackageManager)
  PKGMGR_EXEC=$(which $PKGMGR)
  PKGFILE_PATH="./packages/${PKGMGR}.packages"
  PKGS=$(cat $PKGFILE_PATH | tr '\n' ' ')
  sh -c "sudo -u root $PKGMGR_EXEC update"
  sh -c "sudo -u root $PKGMGR_EXEC install -y $PKGS"
}

