#!/bin/bash

set -e
set -u
set -o pipefail
# set -x

. ./utils.sh

print() {
  echo "[x][$(date)] $@"
}

print "welcome to masterplan"

print "setup repositories . . ."
setupRepositories
print "installing packages using package manager"
installPackages

# oh-my-zsh
OMZ_PATH="$HOME/.oh-my-zsh"
ZSH_CUSTOM="$OMZ_PATH/custom"
RESTART_SHELL=0
if [[ ! -d $OMZ_PATH ]];
then
  print "install oh-my-zsh"
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
  git clone https://github.com/zsh-users/zsh-autosuggestions.git $ZSH_CUSTOM/plugins/zsh-autosuggestions >/dev/null 2>&1
  git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $ZSH_CUSTOM/plugins/zsh-syntax-highlighting >/dev/null 2>&1
  RESTART_SHELL=1
else
  print "oh-my-zsh exists, skip"
fi
cp ./apps/zshrc "$HOME/.zshrc"

# Neovim
NVIM_AUTOLOAD_PLUGIN_PATH="$HOME/.local/share/nvim/site/autoload"
NVIM_CONFIG_PATH="$HOME/.config/nvim"
print "configuring neovim and set as default editor"

mkdir -p $NVIM_AUTOLOAD_PLUGIN_PATH
if [[ ! -f "${NVIM_AUTOLOAD_PLUGIN_PATH}/plug.vim" ]];
then
  curl -fLo "${NVIM_AUTOLOAD_PLUGIN_PATH}/plug.vim" --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
fi
mkdir -p $NVIM_CONFIG_PATH
cp ./apps/init.vim $NVIM_CONFIG_PATH/init.vim

# tmux
TMUX_PLUGINS_PATH="$HOME/.tmux/plugins"
mkdir -p $TMUX_PLUGINS_PATH
if [[ ! -d "${TMUX_PLUGINS_PATH}/tpm" ]]
then
  git clone https://github.com/tmux-plugins/tpm "${TMUX_PLUGINS_PATH}/tpm"
fi
cp ./apps/tmux.conf "$HOME/.tmux.conf"

print installing development tool

# nvm
print "installing nvm"
if [[ ! -d "$HOME/.nvm" ]]
then
  NVM_VERSION="v0.39.5"
  bash -c "$(curl -s https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_VERSION}/install.sh)" >/dev/null 2>&1
fi

# install pyenv
print "installing pyenv and virtualenvwrapper"
PYENV_ROOT="$HOME/.pyenv"
if [[ ! -d $PYENV_ROOT ]]
then
  bash -c "$(curl -s -S -L https://raw.githubusercontent.com/pyenv/pyenv-installer/master/bin/pyenv-installer)" &> /dev/null 2>&1
  git clone https://github.com/pyenv/pyenv-virtualenvwrapper.git $PYENV_ROOT/plugins/pyenv-virtualenvwrapper &> /dev/null 2>&1
  grep 'export WORKON_HOME' "$HOME/.zshrc" || echo 'export WORKON_HOME=$HOME/pyenvs' >> $HOME/.zshrc
fi

# awscli
print "installing awscliv2"
if [[ ! -x $(which aws 2>/dev/null) ]]
then
  curl -s "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip"
  unzip awscliv2.zip > /dev/null
  sudo bash ./aws/install --update > /dev/null
  rm -rf aws
  rm awscliv2.zip
fi

# install cargo and alacritty
print "installing cargo and alacritty"
if [[ ! -x $(which alacritty 2>/dev/null) ]]
then
  # this cargo path only exists after Rust is installed
  CARGO_EXEC_PATH="$HOME/.cargo/bin/cargo"
  curl https://sh.rustup.rs -sSf | sh -s -- -y
  bash -c "$CARGO_EXEC_PATH install alacritty" &> /dev/null
  cp ./apps/alacritty.yml $HOME/.alacritty.yml
fi

if [[ $RESTART_SHELL = 1 ]]
then
  print "Restarting shell . . ."
  exec zsh -l
fi

