# -*- mode: sh; -*-
[ -z "${XDG_CONFIG_HOME:-}" ] && export XDG_CONFIG_HOME="$HOME/.config"
# zsh - set XDG_CONFIG base directory - should be set in /etc/zshenv
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# source sh environment
. "${XDG_CONFIG_HOME}"/shell/env

