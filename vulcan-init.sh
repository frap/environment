#!/usr/bin/env bash
#

set -euo pipefail

cmd="${1:---help}"

# ANSI Colours
blue="\033[0;34m"
purple="\033[0;35m"
bgreen="\033[32;1m"
bred="\033[31;1m"
normal="\033[0m"
reset="\033[39m"

##if [[ "$cmd" == "-h" || "$cmd" == "--help" ]]; then
print_help () {
  echo -e $purple
  cat <<HELP

Usage: $(basename "$0") [onboard]

################################################################################
#
# Hephaestus is the Greek god of artisans, blacksmiths, carpenters, craftsmen,
# fire, metallurgy, metalworking, sculpture and volcanoes. Hephaestus's Roman
# counterpart is Vulcan
#
################################################################################

Vulcan installs new machine (linux/macosx) from scratch.
https://github.com/frap/environment.git

Copyright (c) 2020 "Gas" Andrés Gasson
Licensed under the MIT license.

HELP

  exit
}

function silence () {
  local output=
  if ! output=$(eval "$@" 2>&1); then
    echo "$output"
    exit 1
  fi
}

#
# Setup the machines Environment
#
export DEV=$HOME/work
export DOTFILES=$DEV/frap/dotfiles
export XDG_CONFIG_HOME=$HOME/.config
export CONFIG_BACKUP=$HOME/dotfiles_backup

function error() {
    echo -e "${bred}🦤 $*${normal}" && exit 1
}

function change() {
    echo -e "${blue}🦍 $*${normal}"
}

function owl() {
    echo -e "${bgreen}🦉 $*${reset}"
}

case "$(uname -s)" in
    Linux*)  platform=linux;;
    Darwin*) platform=macos;;
    *) error "Sorry your device is next to useless for development!"
esac

function gitdf {
   git --git-dir=$DOTFILES --work-tree=$HOME $@
}
env_https=https://github.com/frap/environment.git
env_ssh=git@github.com:frap/environment.git
env_emacs=git@github.com:frap/emacs-config.git

new_install () {
  echo "in new_install"
  # move .config DIR if exists as this is a "new" machine script setup
  if [ -d "$XDG_CONFIG_HOME" ]; then
    change "Déplacement du répertoire .config vers $CONFIG_BACKUP"
    mv "$XDG_CONFIG_HOME"  "$CONFIG_BACKUP"
  fi

  # if ! [ -d "$XDG_CONFIG_HOME" ];   # redundant check
  # then
  #   mkdir -p "$XDG_CONFIG_HOME" && cd "$XDG_CONFIG_HOME"

  #   # git init
  #   # # clone via HTTPS, as most likely SSH is not yet available or configured
  #   # git remote add origin $env_https
  #   # git fetch
  #   # git reset --hard origin/main

  # fi

 if [ ! -d "$DEV" ]; then
   owl "making Directory: $DEV"
   mkdir -p "${DEV}"
 fi

  if [ ! -d "$DOTFILES/" ]; then
    owl "Clonage du dépôt environment.git vers $DOTFILES"
    # clone via HTTPS, as most likely SSH is not yet available or configured
    git clone $env_https "$HOME/.config"
    git remote set-url origin $env_ssh
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

  # # install emacs
  # if [ ! -d "$XDG_CONFIG_HOME/emacs/.git" ]; then
  #   # clone via HTTPS, as most likely SSH is not yet available or configured
  #   git clone $env_emacs "${DEV}/emacs"
  #   ln -sf "${DEV}/emacs" "$XDG_CONFIG_HOME"
  # fi


  cd "$XDG_CONFIG_HOME" && {
    ./vulcan.sh test
  }
}

# Shift the first argument off, as we are using that.
# http://www.tldp.org/LDP/Bash-Beginners-Guide/html/sect_09_07.html
shift

case "${cmd}" in
  "-h"|"--help")
    print_help
    ;;
  "onboard")
    new_install
   ;;
  "test")
    package="${cmd}"
    echo "🚀 Installing package: ${package}"
   # (cd "${DOTFILES_LOCATION}/${package}" && ./install.sh)
    echo "\n"
    ./vulcan.sh test
    ;;
  *)
    error "${cmd} does not exist"
    exit 1
    ;;
esac
