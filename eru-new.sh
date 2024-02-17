#!/usr/bin/env bash
#
################################################################################
#
# Hephaestus is the Greek god of artisans, blacksmiths, carpenters, craftsmen,
# fire, metallurgy, metalworking, sculpture and volcanoes. Hephaestus's Roman
# counterpart is Vulcan.
#
################################################################################

set -e

if [[ "$1" == "-h" || "$1" == "--help" ]]; then
  cat <<HELP

Usage: $(basename "$0")

Used to install new machine from scratch.
https://github.com/frap/environment.git


HELP
  exit
fi

function silence {
  local output=
  if ! output=$(eval "$@" 2>&1); then
    echo "$output"
    exit 1
  fi
}

#
# Fetching the notes
#
export DEV=$HOME/work
export DOTFILES=$DEV/environment
export XDG_CONFIG_HOME=$HOME/.config
export CONFIG_BACKUP=$HOME/config-backup

function gitdf {
   git --git-dir=$DOTFILES --work-tree=$HOME $@
}
env_https=https://github.com/frap/environment.git
env_ssh=git@github.com:frap/environment.git
env_emacs=git@github.com:frap/corgi.git

# move .config DIR if exists as this is a "new" machine script setup
if [ -d "$XDG_CONFIG_HOME" ]; then
    echo "Déplacement du répertoire .config vers config-backup"
    mv "$XDG_CONFIG_HOME"  "$CONFIG_BACKUP"
fi

if ! [ -d "$XDG_CONFIG_HOME" ];   # redundant check
then
   mkdir -p "$XDG_CONFIG_HOME" && cd "$XDG_CONFIG_HOME"

   git init
   # clone via HTTPS, as most likely SSH is not yet available or configured
   git remote add origin $env_https
   git fetch
   git reset --hard origin/main

fi

if [ ! -d "$DEV" ]; then
    mkdir -p "${DEV}"
fi

if [ ! -d "$DOTFILES/" ]; then
    echo "Clonage du dépôt environment.git vers $DOTFILES"
    # clone via HTTPS, as most likely SSH is not yet available or configured
    git clone --bare $env_https "$DEV"
fi

#if [ -d "$DOTFILES/" ]; then
#    gitdf checkout
#    if [ $? = 0 ]; then
#      echo "Dotfiles vérifiée.";
#    else
#        if [ ! d "$CONFIG_BACKUP" ]; then
#            mkdir -p "$CONFIG_BACKUP"
#        fi
#        echo "Sauvegarde préexistante dotfiles.";
#        gitdf checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} $CONFIG_BACKUP/{}
#    fi;
#    gitdf checkout
#    gitdf config status.showUntrackedFiles no
#fi

cd "$XDG_CONFIG_HOME" && {
	git remote set-url origin $env_ssh
}

# # install emacs
if [ ! -d "$XDG_CONFIG_HOME/emacs/.git" ]; then
   # clone via HTTPS, as most likely SSH is not yet available or configured
   git clone $env_emacs "${DEV}/emacs"
   ln -sf "${DEV}/emacs "$XDG_CONFIG_HOME"
fi

#
# Now start the Great Music
#
cd "$XDG_CONFIG_HOME" && {
	./eru.sh # install
}
