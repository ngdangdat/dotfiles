export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="darkblood"

plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh


# Nix
. $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

# PyENV
export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init - zsh)"
export WORKON="$HOME/pyenvs"
eval "$(pyenv virtualenv-init -)"
pyenv virtualenvwrapper


# NVM
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
