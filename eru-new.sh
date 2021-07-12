#!/usr/bin/env sh
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
https://github.com/frap/environment

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

export XDG_CONFIG_HOME=$HOME/.config

env_https=https://github.com/frap/environment
env_ssh=git@github.com:frap/environment.git

if [ -d "$XDG_CONFIG_HOME" ] && [ ! -d "$XDG_CONFIG_HOME/.git" ]; then
	cd "$XDG_CONFIG_HOME" && {
		git init
		# clone via HTTPS, as most likely SSH is not yet available or configured
		git remote add origin $env_https
		git fetch
		git reset --hard origin/master
	}
fi

if [ ! -d "$XDG_CONFIG_HOME/.git" ]; then
	# clone via HTTPS, as most likely SSH is not yet available or configured
	git clone $env_https "$XDG_CONFIG_HOME"
fi

cd "$XDG_CONFIG_HOME" && {
	git remote set-url origin $env_ssh
}

#
# Now start the Great Music
#
cd "$XDG_CONFIG_HOME" && {
	./eru.sh install
}
