# -*- mode: sh; -*-

# load shared shell configuration
#source "../sh/env"

fpath=($XDG_CONFIG_HOME/zsh/plugins $fpath)

# +------------+
# | NAVIGATION |
# +------------+

setopt AUTO_CD              # Go to folder path without using cd.

setopt AUTO_PUSHD           # Push the old directory onto the stack on cd.
setopt PUSHD_IGNORE_DUPS    # Do not store duplicates in the stack.
setopt PUSHD_SILENT         # Do not print the directory stack after pushd or popd.

setopt CORRECT              # Spelling correction
setopt CDABLE_VARS          # Change directory to a path stored in a variable.
setopt EXTENDED_GLOB        # Use extended globbing syntax.

#autoload -Uz bd; bd

# +---------+
# | HISTORY |
# +---------+

setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt HIST_VERIFY               # Do not execute immediately upon history expansion.
setopt APPEND_HISTORY            # adds commands as they are typed, not at shell exit
#setopt INC_APPEND_HISTORY   
#setopt HIST_REDUCE_BLANKS       # Remove unnecessary blanks from history

# +---------+
# | ALIASES |
# +---------+

. "$XDG_CONFIG_HOME"/aliases/aliases

# +---------+
# | SCRIPTS |
# +---------+

. "$XDG_CONFIG_HOME"/zsh/scripts.zsh # Scripts

# Don't hang up background jobs
setopt no_hup

# autocorrect command and parameter spelling
setopt correct
setopt correctall

# use emacs bindings even with vim as EDITOR
bindkey -e

# fix backspace on Debian
[ -n "$LINUX" ] && bindkey "^?" backward-delete-char

# fix delete key on macOS
[ -n "$MACOS" ] && bindkey '\e[3~' delete-char

# emacs
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
# alternate mappings for Ctrl-U/V to search the history
bindkey "^u" history-beginning-search-backward
bindkey "^v" history-beginning-search-forward

# +------------+
# | COMPLETION |
# +------------+

source $XDG_CONFIG_HOME/zsh/completion.zsh

# enable autosuggestions
#ZSH_AUTOSUGGESTIONS="$HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
#[ -f "$ZSH_AUTOSUGGESTIONS" ] && source "$ZSH_AUTOSUGGESTIONS"

# enable direnv (if installed)
quiet_which direnv && eval "$(direnv hook zsh)"

git_branch() {
  GIT_BRANCH=$(git symbolic-ref --short HEAD 2>/dev/null) || return
  [ -n "$GIT_BRANCH" ] && echo "($GIT_BRANCH) "
}

if [ $MACOS ]
then
  export PROMPT='%(?.%F{green}√.%F{red}?%?)%f %F{130}%n %(!.#.>) '
elif [ -n "${SSH_CONNECTION}" ]
then
  export PROMPT='%(?.%B%F{green}√.%B%F{red}?%?)%f %F{90}%n:%m %(!.#.>) '
else
  export PROMPT='%(?.%F{green}√.%B%F{red}?%?)%f %F{magenta}%n@%m %(!.#.>) '
fi
export RPROMPT='%{%B%F{196}%}$(git_branch)%f%b[%F{226}%1~%b%f]'

# more macOS/Bash-like word jumps
export WORDCHARS=""

# to avoid non-zero exit code
true
