#!/usr/bin/env bash
#
################################################################################
#
# Of the theme that I have declared to you, I will now that ye make in harmony
# together a Great Music. And since I have kindled you with the Flame
# Imperishable, ye shall show forth your powers in adorning this theme, each
# with his own thoughts and devices, if he will. But I win sit and hearken, and
# be glad that through you great beauty has been wakened into song.
#
#   John Ronald Reuel Tolkien (c)
#
################################################################################

set -e

if [[ "$1" == "-h" || "$1" == "--help" ]]; then
	cat <<HELP

Usage: $(basename "$0")

Used to install my setup on new VM from scratch.
https://github.com/frap/dotfiles.git

Copyright (c) 2020 "Gas" Andrés Gasson
Licensed under the MIT license.xs

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
export DEV=$HOME/Dev
export DOTFILES=$DEV/dotfiles.git
export XDG_CONFIG_HOME=$HOME/.config
export CONFIG_BACKUP=$HOME/config-backup

function gitdf {
   git --git-dir=$DOTFILES --work-tree=$HOME $@
}
env_https=https://github.com/frap/dotfiles.git
env_ssh=git@github.com:frap/dotfiles.git
env_emacs=git@github.com:frap/emacs.git

if [ ! -d "$DEV" ]; then
    mkdir -p $DEV
fi

if [ ! -d "$DOTFILES/" ]; then
    echo "Clonage du dépôt dotfiles.git vers $DOTFILES"
	# clone via HTTPS, as most likely SSH is not yet available or configured
	git clone --bare $env_https "$DEV"
fi
# move .config DIR if exists
if [ -d "$XDG_CONFIG_HOME" ]; then
    echo "Déplacement du répertoire .config vers config-backup"
    mv "$XDG_CONFIG_HOME"  "$CONFIG_BACKUP"
    #&& {
# 		git init
# 		# clone via HTTPS, as most likely SSH is not yet available or configured
# 		git remote add origin $env_https
# 		git fetch
# 		git reset --hard origin/main
# 	}
fi


if [ -d "$DOTFILES/" ]; then
    gitdf checkout
    if [ $? = 0 ]; then
      echo "Dotfiles vérifiée.";
    else
        if [ ! d "$CONFIG_BACKUP" ]; then
            mkdir -p "$CONFIG_BACKUP"
        fi
        echo "Sauvegarde préexistante dotfiles.";
        gitdf checkout 2>&1 | egrep "\s+\." | awk {'print $1'} | xargs -I{} mv {} $CONFIG_BACKUP/{}
    fi;
    gitdf checkout
    gitdf config status.showUntrackedFiles no
fi

#cd "$XDG_CONFIG_HOME" && {
#	git remote set-url origin $env_ssh
#}

# # install emacs
# if [ ! -d "$XDG_CONFIG_HOME/emacs/.git" ]; then
# 	# clone via HTTPS, as most likely SSH is not yet available or configured
# 	git clone $env_emacs "$XDG_CONFIG_HOME"
# fi

#
# Now start the Great Music
#
cd "$XDG_CONFIG_HOME" && {
	./eru.sh # install
}
