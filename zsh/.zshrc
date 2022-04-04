# -*- mode: sh; -*-
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
setopt INC_APPEND_HISTORY
setopt HIST_REDUCE_BLANKS       # Remove unnecessary blanks from history

# +---------+
# | SCRIPTS |
# +---------+

#. "$XDG_CONFIG_HOME"/zsh/scripts.zsh # Scripts

# Don't hang up background jobs
setopt no_hup

# autocorrect command and parameter spelling
setopt correct
setopt correctall

# +-------------+
# | KEYBINDINGS |
# +-------------+
ssource $XDG_CONFIG_HOME/zsh/keybinds.zsh

# +-------------+
# | INTERACTIVE |
# +-------------+

ssource "${XDG_CONFIG_HOME}/shell/interactive"

# +------------+
# | COMPLETION |
# +------------+

ssource $XDG_CONFIG_HOME/zsh/completion.zsh

# enable autosuggestions
ZSH_AUTOSUGGESTIONS="$HOMEBREW_PREFIX/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
ssource "$ZSH_AUTOSUGGESTIONS"

# +------------+
# | HIGHLIGHT  |
# +------------+
ssource "${XDG_CONFIG_HOME}/zsh/highlight.zsh"

# +------------+
# | PROMPT     |
# +------------+
# Enable substitution in the prompt
#setopt prompt_subst
ssource "${XDG_CONFIG_HOME}/zsh/prompt.zsh"

# +------------+
# | FZF        |
# +------------+
ssource "${XDG_CONFIG_HOME}/zsh/fzf.zsh"

if command_exists kitty
then
   # Kitty completion
   source <(kitty + complete setup zsh)
fi
   
# more macOS/Bash-like word jumps
#export WORDCHARS=""

# +-------------+
# |   DIRENV    |
# +-------------+
# enable direnv (if installed)
command_exists direnv && eval "$(direnv hook zsh)"

# to avoid non-zero exit code
true
