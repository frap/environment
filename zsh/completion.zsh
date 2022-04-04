# vim: ts=4 sw=4
#   ____ ___  __  __ ____  _     _____ _____ ___ ___  _   _
#  / ___/ _ \|  \/  |  _ \| |   | ____|_   _|_ _/ _ \| \ | |
# | |  | | | | |\/| | |_) | |   |  _|   | |  | | | | |  \| |
# | |__| |_| | |  | |  __/| |___| |___  | |  | | |_| | |\  |
#  \____\___/|_|  |_|_|   |_____|_____| |_| |___\___/|_| \_|
#

# +---------+
# | Options |
# +---------+
setopt   COMPLETE_IN_WORD  # Complete from both ends of a word
setopt   ALWAYS_TO_END     # Move cursor to the end of a completed word
setopt   PATH_DIRS         # Perform path search even on command names with slashes
setopt   AUTO_MENU         # Show completion menu on a succesive tab press
setopt   AUTO_LIST         # Automatically list choices on ambiguous completion
setopt   AUTO_REMOVE_SLASH # Remove trailing slashes
setopt   AUTO_PARAM_SLASH  # If completed parameter is a directory, add a trailing slash
unsetopt MENU_COMPLETE     # Do not autoselect the first completion entry
unsetopt FLOW_CONTROL      # Disable start/stop characters in shell editor
setopt   GLOB_COMPLETE     # Show completions for glob instead of expanding

# Treat these characters as part of a word
WORDCHARS='*?_-.[]~&;!#$%^(){}<>'

# +---------+
# | zstyles |
# +---------+

# Ztyle pattern
# :completion:<function>:<completer>:<command>:<argument>:<tag>

# Use caching to make completion for cammands such as dpkg and apt usable
zstyle ':completion::complete:*' use-cache on
zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/zcompcache"


# Case-insensitive (all), partial-word, and then substring completion
if zstyle -t ':omz:completion:*' case-sensitive; then
    zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
    setopt CASE_GLOB
else
    zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
    unsetopt CASE_GLOB
fi

# Group matches and describe
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description ' %F{8}specify:%f %B%F{cyan}%d%f%b'
zstyle ':completion:*:corrections' format ' %F{8}correction:%f %B%F{green}%d (errors: %f%F{red}%e%f%F{green})%f%b'
zstyle ':completion:*:descriptions' format ' %F{8}description:%f %B%F{blue}%d%f%b'
zstyle ':completion:*:messages' format ' %F{8}message:%f %B%F{magenta}%d%f%b'
zstyle ':completion:*:warnings' format ' %F{8}error:%f %B%F{red}no matches found%f%b'
zstyle ':completion:*:default' select-prompt '%B%S%M%b matches, current selection at %p%s'
zstyle ':completion:*' format ' %F{8}completion:%f %B%F{yellow}%d%f%b'
zstyle ':completion:*' list-separator '→'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions
zstyle ':completion:*' completer _expand _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Increase the number of errors based on the length of the typed word
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Don't complete unavailable commands
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# History
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Environmental Variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# Populate hostname completion
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//,/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# Group matches and describe
zstyle ':completion:*:*:*:*:*' menu select
zstyle ':completion:*:matches' group 'yes'
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:options' auto-description ' %F{8}specify:%f %B%F{cyan}%d%f%b'
zstyle ':completion:*:corrections' format ' %F{8}correction:%f %B%F{green}%d (errors: %f%F{red}%e%f%F{green})%f%b'
zstyle ':completion:*:descriptions' format ' %F{8}description:%f %B%F{blue}%d%f%b'
zstyle ':completion:*:messages' format ' %F{8}message:%f %B%F{magenta}%d%f%b'
zstyle ':completion:*:warnings' format ' %F{8}error:%f %B%F{red}no matches found%f%b'
zstyle ':completion:*:default' select-prompt '%B%S%M%b matches, current selection at %p%s'
zstyle ':completion:*' format ' %F{8}completion:%f %B%F{yellow}%d%f%b'
zstyle ':completion:*' list-separator '→'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' verbose yes

# Fuzzy match mistyped completions
zstyle ':completion:*' completer _expand _complete _match _approximate
zstyle ':completion:*:match:*' original only
zstyle ':completion:*:approximate:*' max-errors 1 numeric

# Increase the number of errors based on the length of the typed word
zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3))numeric)'

# Don't complete unavailable commands
zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec))'

# Array completion element sorting
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters

# Directories
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*:*:cd:*:directory-stack' menu yes select
zstyle ':completion:*:-tilde-:*' group-order 'named-directories' 'path-directories' 'users' 'expand'
zstyle ':completion:*' squeeze-slashes true

# History
zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list false
zstyle ':completion:*:history-words' menu yes

# Environmental Variables
zstyle ':completion::*:(-command-|export):*' fake-parameters ${${${_comps[(I)-value-*]#*,}%%,*}:#-*-}

# Populate hostname completion
zstyle -e ':completion:*:hosts' hosts 'reply=(
  ${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) 2>/dev/null)"}%%[#| ]*}//,/ }
  ${=${(f)"$(cat /etc/hosts(|)(N) <<(ypcat hosts 2>/dev/null))"}%%\#*}
  ${=${${${${(@M)${(f)"$(cat ~/.ssh/config 2>/dev/null)"}:#Host *}#Host }:#*\**}:#*\?*}}
)'

# Don't complete uninteresting users...
zstyle ':completion:*:*:*:users' ignored-patterns \
    adm amanda apache avahi beaglidx bin cacti canna clamav daemon \
    dbus distcache dovecot fax ftp games gdm gkrellmd gopher \
    hacluster haldaemon halt hsqldb ident junkbust ldap lp mail \
    mailman mailnull mldonkey mysql nagios \
    named netdump news nfsnobody nobody nscd ntp nut nx openvpn \
    operator pcap postfix postgres privoxy pulse pvm quagga radvd \
    rpc rpcuser rpm shutdown squid sshd sync uucp vcsa xfs '_*'

# ... unless we really want to
zstyle '*' single-ignored show

# Ignore multiple entries
zstyle ':completion:*:(rm|kill|diff):*' ignore-line yes
zstyle ':completion:*:rm:*' file-patterns '*:all-files'

# Kill
zstyle ':completion:*:*:*:*:processes' command 'ps -u $USER -o pid,user,comm -w'
zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;36=0=01'
zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:*:kill:*' force-list always
zstyle ':completion:*:*:kill:*' insert-ids single

# Man
zstyle ':completion:*:manuals' separate-sections true
zstyle ':completion:*:manuals.(^1*)' insert-sections true

# Media Players
zstyle ':completion:*:*:mpg123:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:mpg321:*' file-patterns '*.(mp3|MP3):mp3\ files *(-/):directories'
zstyle ':completion:*:*:ogg123:*' file-patterns '*.(ogg|OGG|flac):ogg\ files *(-/):directories'
zstyle ':completion:*:*:mocp:*' file-patterns '*.(wav|WAV|mp3|MP3|ogg|OGG|flac):ogg\ files *(-/):directories'

# Mutt
if [[ -f ~/.mutt/aliases ]]; then
    zstyle ':completion:*:*:mutt:*' menu yes select
    zstyle ':completion:*:mutt:*' users ${${${(f)"$(<~/.mutt/aliases)"}#alias[[:space:]]}%%[[:space:]]*}
fi

# SSH/SCP/RSYNC
zstyle ':completion:*:(scp|rsync):*' tag-order 'hosts:-host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:(scp|rsync):*' group-order users files all-files hosts-domain hosts-host hosts-ipaddr
zstyle ':completion:*:ssh:*' tag-order users 'hosts:-host hosts:-domain:domain hosts:-ipaddr:ip\ address *'
zstyle ':completion:*:ssh:*' group-order hosts-domain hosts-host users hosts-ipaddr
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-host' ignored-patterns '*.*' loopback localhost
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-domain' ignored-patterns '<->.<->.<->.<->' '^*.*' '*@*'
zstyle ':completion:*:(ssh|scp|rsync):*:hosts-ipaddr' ignored-patterns '^<->.<->.<->.<->' '127.0.0.<->'

# Custom
#zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' menu select=1
zstyle ':completion:*' original false
#zstyle ':completion:*' select-prompt '%S%M matches, current selection at %p%s'


# # Enable approximate completions
# zstyle ':completion:*' completer _complete _ignored _approximate
# zstyle -e ':completion:*:approximate:*' max-errors 'reply=($((($#PREFIX+$#SUFFIX)/3)) numeric)'

# # Automatically update PATH entries
# zstyle ':completion:*' rehash true

# # Use menu completion
# zstyle ':completion:*' menu select

# # Verbose completion results
# zstyle ':completion:*' verbose true

# # Smart matching of dashed values, e.g. f-b matching foo-bar
# zstyle ':completion:*' matcher-list 'r:|[._-]=* r:|=*'

# # Group results by category
# zstyle ':completion:*' group-name ''

# # Don't insert a literal tab when trying to complete in an empty buffer
# # zstyle ':completion:*' insert-tab false

# # Keep directories and files separated
# zstyle ':completion:*' list-dirs-first true

# # Don't try parent path completion if the directories exist
# zstyle ':completion:*' accept-exact-dirs true

# # Always use menu selection for `cd -`
# zstyle ':completion:*:*:cd:*:directory-stack' force-list always
# zstyle ':completion:*:*:cd:*:directory-stack' menu yes select

# # Complete the alias when _expand_alias is used as a function
# zle -C alias-expension complete-word _generic
# bindkey '^X' alias-expension
# zstyle ':completion:alias-expension:*' completer _expand_alias

# # Allow you to select in a menu
# # zstyle ':completion:*' menu select

# # # Autocomplete options for cd instead of directory stack
# # zstyle ':completion:*' complete-options true

# # zstyle ':completion:*' file-sort modification

# # Pretty messages during pagination
# zstyle ':completion:*' list-prompt '%SAt %p: Appuyez sur TAB pour en savoir plus, ou le caractère à insérer%s'
# zstyle ':completion:*' select-prompt '%SDéfilement actif : sélection actuelle à %p%s'

# zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
# zstyle ':completion:*:*:*:*:descriptions' format '%F{blue}-- %D %d --%f'
# zstyle ':completion:*:*:*:*:messages' format ' %F{purple} -- %d --%f'
# zstyle ':completion:*:*:*:*:warnings' format ' %F{red}-- Désolé, aucune correspondance pour: --%f'

# # zstyle ':completion:*:default' list-prompt '%S%M matches%s'
# # Colors for files and directory
# #zstyle ':completion:*:*:*:*:default' list-colors ${(s.:.)LS_COLORS}

# # Show message while waiting for completion
# zstyle ':completion:*' show-completer true

# # Prettier completion for processes
# zstyle ':completion:*:*:*:*:processes' force-list always
# zstyle ':completion:*:*:*:*:processes' menu yes select
# zstyle ':completion:*:*:*:*:processes' list-colors '=(#b) #([0-9]#) ([0-9a-z-]#)*=01;34=0=01'
# zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,args -w -w"

# # Use ls-colors for path completions
# function _set-list-colors() {
# 	zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
# 	unfunction _set-list-colors
# }
# sched 0 _set-list-colors  # deferred since LC_COLORS might not be available yet

# # Only display some tags for the command cd
# zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
# # zstyle ':completion:*:complete:git:argument-1:' tag-order !aliases

# # Required for completion to be in good groups (named after the tags)
# zstyle ':completion:*' group-name ''

# zstyle ':completion:*:*:-command-:*:*' group-order aliases builtins functions commands

# # See ZSHCOMPWID "completion matching control"
# zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

# zstyle ':completion:*' keep-prefix true

# zstyle -e ':completion:*:(ssh|scp|sftp|rsh|rsync):hosts' hosts 'reply=(${=${${(f)"$(cat {/etc/ssh_,~/.ssh/known_}hosts(|2)(N) /dev/null)"}%%[# ]*}//,/ })'

# # Don't complete uninteresting stuff unless we really want to.
# zstyle ':completion:*:functions' ignored-patterns '(_*|pre(cmd|exec)|TRAP*)'
# zstyle ':completion:*:*:*:users' ignored-patterns \
# 		adm amanda apache at avahi avahi-autoipd beaglidx bin cacti canna \
# 		clamav daemon dbus distcache dnsmasq dovecot fax ftp games gdm \
# 		gkrellmd gopher hacluster haldaemon halt hsqldb ident junkbust kdm \
# 		ldap lp mail mailman mailnull man messagebus mldonkey mysql nagios \
# 		named netdump news nfsnobody nobody nscd ntp nut nx obsrun openvpn \
# 		operator pcap polkitd postfix postgres privoxy pulse pvm quagga radvd \
# 		rpc rpcuser rpm rtkit scard shutdown squid sshd statd svn sync tftp \
# 		usbmux uucp vcsa wwwrun xfs cron mongodb nullmail portage redis \
# 		shoutcast tcpdump '_*'
# zstyle ':completion:*' single-ignored show


# Initialise completion
autoload -Uz compinit
compinit -d "$XDG_CACHE_HOME/zsh/zcompdump"
autoload -Uz +X bashcompinit
bashcompinit -D
