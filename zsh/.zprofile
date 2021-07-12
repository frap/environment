# -*- mode: sh; -*-
#eval "$(/opt/homebrew/bin/brew shellenv)"
# load shared shell configuration
#source "$XDG_CONFIG_HOME/sh/profile"

# Enable completions
autoload -U compinit && compinit

if quiet_which brew
then
  [ -w "$HOMEBREW_PREFIX/bin/brew" ] && \
    [ ! -f "$HOMEBREW_PREFIX/share/zsh/site-functions/_brew" ] && \
    mkdir -p "$HOMEBREW_PREFIX/share/zsh/site-functions" &>/dev/null && \
    ln -s "$HOMEBREW_PREFIX/Library/Contributions/brew_zsh_completion.zsh" \
          "$HOMEBREW_PREFIX/share/zsh/site-functions/_brew"
  FPATH="$HOMEBREW_PREFIX/share/zsh/site-functions:$FPATH"
fi

# Enable regex moving
autoload -U zmv

# Style ZSH output
zstyle ':completion:*:descriptions' format '%U%B%F{red}%d%f%b%u'
zstyle ':completion:*:warnings' format '%BDésolé, aucune correspondance pour: %d%b'

# Case insensitive completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Case insensitive globbing
setopt no_case_glob

# Auto cd
setopt auto_cd

# Expand parameters, commands and aritmatic in prompts
setopt prompt_subst

# Colourful prompt with Git and Subversion branch
autoload -U colors && colors

git_branch() {
  GIT_BRANCH=$(git symbolic-ref --short HEAD 2>/dev/null) || return
  [ -n "$GIT_BRANCH" ] && echo "($GIT_BRANCH) "
}

if [ -n "${SSH_CONNECTION}" ]
then
  export PROMPT='%(?.%F{green}√.%B%F{red}?%?)%f %F{blue}%n:%m %(!.#.>) '
else
  export PROMPT='%(?.%F{green}√.%B%F{red}?%?)%f %F{226}%n@%m %(!.#.>) '
fi
export RPROMPT='%{$fg_bold[red]%}$(git_branch)%b[%B%F{226}%1~%b%f]'

# more macOS/Bash-like word jumps
export WORDCHARS=""
