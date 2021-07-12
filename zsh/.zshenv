# -*- mode: sh; -*-

[ -z "${XDG_CONFIG_HOME:-}" ] && export XDG_CONFIG_HOME="$HOME/.config"

# source sh environment
. "${XDG_CONFIG_HOME}"/sh/env

# zsh - set XDG_CONFIG base directory
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"
