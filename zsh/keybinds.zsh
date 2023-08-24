# vim: ts=4 sw=4
# INS, DEL, etc.
function _delete-char-or-region() {
	[[ $REGION_ACTIVE -eq 1 ]] && zle kill-region || zle delete-char
}
zle -N _delete-char-or-region

# use emacs bindings even with vim as EDITOR
bindkey -e


# fix delete key on macOS
[ -n "$MACOS" ] && bindkey '\e[3~' delete-char

# emacs
bindkey "^A" beginning-of-line
bindkey "^E" end-of-line
# alternate mappings for Ctrl-U/V to search the history
bindkey "^u" history-beginning-search-backward
bindkey "^v" history-beginning-search-forward

# Easy access to previous args
autoload -Uz copy-earlier-word
zle -N copy-earlier-word
bindkey "\em" copy-earlier-word

# Edit the current command line in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# Misc
bindkey '^[p'				copy-prev-shell-word

# Quickly jump right after the first word (e.g. to insert switches)
function _after-first-word() {
	zle beginning-of-line
	zle forward-word
}
zle -N _after-first-word
bindkey '\C-X1' _after-first-word


bindkey "\e[1;5D"	_backward-word		# ctrl-left
bindkey "\e[1;5C"	_forward-word		# ctrl-right
bindkey '\e\e[D'	_backward-arg		# alt-left
bindkey '\e[1;3D'	_backward-arg		# alt-left
bindkey '\e[1;3C'	_forward-arg		# alt-right
bindkey '\e\e[C'	_forward-arg		# alt-right
bindkey '\e\C-?'	_backward-kill-arg	# alt-backspace
bindkey '\e\e[3~'	_forward-kill-arg	# alt-del
bindkey '\e[3;3~'	_forward-kill-arg	# alt-del
bindkey '\C-w'		_backward-kill-word
bindkey '\C-f'		_backward-kill-path
