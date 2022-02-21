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
#
# Run this script to install all dependencies and configurations. If you wish to
# perform only specific task or tasks pass them as arguments, space-separated.
#
#   ./eru.sh [command] [theme] ...
#
# For example,
#
#   ./eru.sh linking repositories packages
#
# or ./eru.sh upgrade -SSH -Linking -Repositories -Emacs -git

#
# Hi, my name is
#

fellow="agasson"

#
# Fast failure
#

set -e

#
# Get the OS info
#
# OS detection
KERNEL_NAME=$(uname -s | tr '[:upper:]' '[:lower:]')
[[ "$OSTYPE" =~ ^darwin ]] 2>/dev/null && KERNEL_RELEASE="darwin"
[[ "$ANDROID_DATA" =~ data ]] 2>/dev/null && KERNEL_RELEASE="android"
[[ "$(cat /etc/issue 2>/dev/null)" =~ Ubuntu ]] && KERNEL_RELEASE="ubuntu"
[[ "$(cat /etc/redhat-release 2>/dev/null)" =~ "Red Hat" ]] && KERNEL_RELEASE="redhat"

OS_NAME="unknown"
OS_VERSION="unknown"
case $KERNEL_NAME in
darwin)
	OS_NAME=macos
	OS_VERSION=$(sw_vers -productVersion)
	;;
linux)
	case $(echo $KERNEL_RELEASE) in
	*arch* | *coreos*)
		OS_NAME="arch"
		;;
	ubuntu)
		OS_NAME="ubuntu"
		OS_VERSION="$(sed 's/Ubuntu \([0-9]\+\.[0-9]\+\.[0-9]\+\) .*/\1/' /etc/issue)"
		alias gstat=stat
		;;
	redhat | ol)
		OS_NAME="redhat"
		;;
    android)
        OS_NAME="android"
        OS_VERSION=$TERMUX_VERSION
        ;;
	esac
	;;
*) ;;

esac

#
# Setup USER
#

if [ -z "$USER" ]; then
	USER=$(whoami)
fi

#
# Setup PATH
#

export PATH=$HOME/.local/bin:$PATH

target=$HOME/.config
if [[ -d "$XDG_CONFIG_HOME" ]]; then
	target="$XDG_CONFIG_HOME"
fi
if [[ -d "$GITHUB_WORKSPACE" ]]; then
	target="$GITHUB_WORKSPACE"
fi

export XDG_CONFIG_HOME=$target
export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_STATE_HOME="$HOME/.local/state"

export DEVELOPER=$HOME/Dev

#
# Logging
#
# These colours are meant to be used with `echo -e`
echo_black="\033[0;30m"
echo_red="\033[0;31m"
echo_green="\033[0;32m"
echo_yellow="\033[0;33m"
echo_blue="\033[0;34m"
echo_purple="\033[0;35m"
echo_cyan="\033[0;36m"
echo_white="\033[0;37;1m"
echo_orange="\033[0;91m"

echo_bblack="\033[30;1m"
echo_bred="\033[31;1m"
echo_bgreen="\033[32;1m"
echo_byellow="\033[33;1m"
echo_bblue="\033[34;1m"
echo_bpurple="\033[35;1m"
echo_bcyan="\033[36;1m"
echo_bwhite="\033[37;1m"
echo_borange="\033[91;1m"

echo_normal="\033[0m"
echo_reset="\033[39m"

function error() {
	echo -e "${echo_bred}❌ $*${echo_normal}"
}

function intro() {
	echo -e "${echo_bblue}$*${echo_normal}"
}

function log() {
	echo -e "${echo_bgreen}$*${echo_reset}"
}

function section() {
	echo -e "${echo_blue}🕉 $*${echo_normal}"
}

function a_theme() {
  local text=">>> $2 :: ${*:3}"
  local length="${#text}"
  echo
	echo '┌────────────────────────────────────────────────────────────────────────────┐'
	echo -ne "│ \033[$1m$text\033[0m"
	printf "%$((75 - length))s│\n"
	echo '└────────────────────────────────────────────────────────────────────────────┘'
}

function theme() {
	echo -e "\033[1;36m ✔ $1 Theme :: ${*:2}${echo_reset}"
}

function optional_theme() {
  a_theme "1;32" "$1" "${*:2}"
}

function inactive_theme() {
  a_theme "1;37" "$1" "${*:2}"
}

#
# Greetings
#

intro "Programming is NOT about Typing, It's about thinking."
intro

log "Kernel name:      $KERNEL_NAME"
log "Kernel release:   $KERNEL_RELEASE"
log "Operating system: $OS_NAME"
log "OS version:       $OS_VERSION"
log "User:             $USER"
log "XDG_CONFIG_HOME:  $XDG_CONFIG_HOME"
log

#
# Helpers
#

section "Defining helpers"

# ${@:2} is all paraeters starting at $2
# ${!parameter} returns what is in parameter
# theme guard was called theme_guard $REPO rep whee $REPO=true whether you wan to run the repo code
function theme_guard() {
	key=$(echo "$1" | tr '[:upper:]' '[:lower:]')
	local guard_ref="guard_$key"
	local ignore_guard_ref="guard_ignore_$key"
        # guard is VARIABLE denoting whether theme is true or false
	guard="${!guard_ref}"
	ignore_guard="${!ignore_guard_ref}"
	if [[ ("$ALL" = "true" || "$guard" = "true") && "$ignore_guard" = "" ]]; then
		optional_theme "$1" "${@:2}"   # logging
		return 0
	else
		inactive_theme "$1" "${@:2}"    # logging
		return 1
	fi
}

function install_guard() {
	[[ "$ACTION" == "install" ]]
	return
}

function upgrade_guard() {
	[[ "$ACTION" == "upgrade" ]]
	return
}

function test_guard() {
	[[ "$ACTION" == "test" ]]
	return
}

function ubuntu_guard() {
	[[ "$OS_NAME" == "ubuntu" ]]
	return
}

function android_guard() {
	[[ "$OS_NAME" == "android" ]]
	return
}

function macos_guard() {
	[[ "$OS_NAME" == "macos" ]]
	return
}

function qualify_repo_url() {
	if [[ "$1" = "https://"* || "$1" = "git@"* ]]; then
		echo "$1"
	elif [[ "$2" = "github" ]]; then
		if [[ "$USE_HTTPS" = "true" ]]; then
			echo "https://github.com/$1.git"
		else
			echo "git@github.com:$1.git"
		fi
	elif [[ "$2" = "gitlab" ]]; then
		if [[ "$USE_HTTPS" = "true" ]]; then
			echo "https://gitlab.com/$1.git"
		else
			echo "git@gitlab.com:$1.git"
		fi
	fi
}

function git_lg() {
	git --no-pager \
		log \
		--graph \
		--pretty=format:'%Cred%h%Creset %C(bold blue)<%an> -%C(yellow)%d%Creset %s %Cgreen(%cr)%Creset' \
		"$*"
}

function sync_repo() {
	section "sync_repo $*"

	# working directory usually .config
	wd=$(eval echo "$1")
	remote="$2"
	url=$(qualify_repo_url "$3" "$remote")
	branch="$4"
	if [[ $branch = "" ]]; then
		branch="main"
	fi

	if [[ -d "$wd/.git" ]]; then
		log "$wd existe déjà"
	else
		git clone "$url" "$wd" -b "$branch"
	fi

	cd "$wd" && {
		git diff-index --quiet HEAD -- || {
			error "Votre répertoire de travail n'est pas propre."
			error "Veuillez valider ou cacher tous les changements avant de continuer."
			return 1
		}

		current_branch=$(git symbolic-ref --short HEAD)
		if [[ $branch != "$current_branch" ]]; then
			log "Passer de $current_branch à $branch"
			git checkout "$branch"
		fi

		if [[ -d .git/refs/remotes/$remote ]]; then
			current_url=$(git remote get-url $remote)
			if [[ $current_url != "$url" ]]; then
				log "Distante '$remote' a une mauvaise URL, donc la mettre à jour"
				log "  $current_url -> $url"
				git remote set-url $remote "$url"
			fi
		else
			log "Impossible de trouver distante '$remote', donc en ajoutant"
			git remote add $remote "$url"
		fi

		log "fetch $remote"
		git fetch $remote
		if [[ $(git rev-parse HEAD) == $(git rev-parse $remote/$branch) ]]; then
			log "Tout est à jour"
			return 0
		fi

		if [ "$(git rev-list HEAD..$remote/$branch --count)" != 0 ]; then
			log "Modifications récupérées:"
			git_lg HEAD..$remote/$branch
			log
		fi

		log "rebaser sur $remote/$branch"
		git rebase $remote/$branch

		if [[ "$url" = *"$fellow"* ]]; then
			if [ "$(git rev-list $remote/$branch..HEAD --count)" != 0 ]; then
				log "Changements à pousser:"
				git_lg $remote/$branch..HEAD
				log
			fi

			log "pousser les changements"
			git push $remote $branch
		fi
	}
}

function ensure_dir() {
	if [[ ! -d "$1" ]]; then
		log "créer le $1"
		mkdir -p "$1"
	fi
}

function check() {
	command -v "$1" >/dev/null 2>&1
}

function linkfile() {
	local file="$1"
	if [ -f "$file" ]; then
		(
			cd "$(dirname "$file")"
			map_lines safe_link "$file"
		)
	fi
}

function safe_link() {
	local f
	local s
	local t
	local d
	local owner

	# shellcheck disable=SC2086
	f=$(eval echo $1)
	s="$(pwd)/$f"
	t=$(eval echo "$2")
	d=$(dirname "$t")

	if [[ -d "$d" ]]; then
	    #owner=$(stat -c '%U' "$d") # linux stat
            owner=$(stat -f "%Su" "$d")
		if [[ "$owner" != "root" && "$owner" != "$USER" ]]; then
			error "ne peut pas lier '$s' à '$t'"
			error "propriétaire de  '$d' est $owner"
			error "propriétaires autorisés: root o $USER"
			exit 1
		fi
	fi

	if [[ ! -f "$s" && ! -d "$s" ]]; then
		error "ne peut pas lier '$s' comme il n'existe pas"
		exit 1
	fi

	if [[ ! -d $d ]]; then
		log "créer $d"
		mkdir -p "$d"
	fi

	if [[ -L "$t" ]]; then
		log "relier $s -> $t"
		if [[ "$owner" = "root" ]]; then
			sudo rm "$t"
		else
			rm "$t"
		fi
	else
		log "lier $s -> $t"
	fi

	if [[ "$owner" = "root" ]]; then
		sudo ln -s "$s" "$t"
	else
		ln -s "$s" "$t"
	fi
}

function map_lines() {
	if [[ -f "$2" ]]; then
		while IFS='' read -r line || [[ -n "$line" ]]; do
			if [[ "$line" != "#"* ]]; then
				# shellcheck disable=SC2086
				$1 $line
			fi
		done <"$2"
	fi
}

function download_bin() {
	fp="$HOME/.local/bin/$1"
	curl --silent -o "$fp" "$2"
	chmod a+x "$HOME/.local/bin/$1"
	hash -r
}

#
# Setup variables
#

theme "Setup ERU" "Defining variables"

ALL="true"
ACTION=
case $1 in
install | upgrade | test)
	ACTION=$1
	shift
	;;
*)
	echo
	if [ -z "$1" ]; then
		ACTION=upgrade
	else
		error "l'action '$1' n'est pas prise en charge"
		log "les actions soutenues sont: install, upgrade, test"
		exit 1
	fi
	;;
esac

# $# is the number of arguments
# $* is the postitonal pramaters, starting from 1. If not in " each parameter is a separate word
# Syntax Effective Result
# $*	  $1 $2 $3 … ${N}
# $@	  $1 $2 $3 … ${N}
# "$*"	  "$1c$2c$3c…c${N}"
# "$@"	  "$1" "$2" "$3" … "${N}"
# ${parameter#word} If the pattern matches the beginning of the expanded value of parameter, then
# the result of the expansion is the expanded value of parameter with the shortest matching pattern
# set -- the positional parameters are set to the arguments, even if some of them begin with a ‘-’.
POSITIONAL=()
while [[ $# -gt 0 ]]; do
	if [[ "$1" != "" ]]; then
		if [[ "$1" = -* ]]; then # add a - to disable a theme
			key=$(echo "${1#-}" | tr '[:upper:]' '[:lower:]')
			declare -r "guard_ignore_$key=true"
		else
			key=$(echo "$1" | tr '[:upper:]' '[:lower:]')
			declare -r "guard_$key=true"
			ALL="false"
		fi
	fi
	shift
done
set -- "${POSITIONAL[@]}" # restore positional parameters

if [[ "$INTERACTIVE" = "" ]]; then
	INTERACTIVE=true
fi

#
# Lock
#

LOCK_FILE=$XDG_CACHE_HOME/eru/eru.lock
if [ -f "$LOCK_FILE" ]; then
	error "
Yet another world is being shaped by Eru

One must either wait patiently or embrace the horrors of the unknown and
manually delete the $LOCK_FILE"
	exit 1
fi
mkdir -p "$(dirname "$LOCK_FILE")"
touch "$LOCK_FILE"

function unlock() {
	rm -rf "$LOCK_FILE"
}

trap unlock INT TERM EXIT

#
# Actual bootstrap
#

theme "Guardian" "Assurez-vous que tous les répertoires existent"
ensure_dir "$HOME/.local/bin"
ensure_dir "$XDG_CONFIG_HOME/git"
ensure_dir "$XDG_CACHE_HOME/eldev"
ensure_dir "$XDG_DATA_HOME/cargo"
ensure_dir "$XDG_STATE_HOME"
ensure_dir "$DEVELOPER"


theme_guard "system" "make Eru more approachable" && {
  "$XDG_CONFIG_HOME/bin/safe_link" "$XDG_CONFIG_HOME/eru.sh" "$HOME/.local/bin/eru"
}

# TODO: make it work on Linux from command line
install_guard && macos_guard && theme_guard "SSH" "Vérification des clés SSH" && {
	if [[ "$INTERACTIVE" = "true" ]]; then
		ssh_key_add_url="https://github.com/settings/ssh/new"
		ssh_key_path="$HOME/.ssh/id_rsa"
		ssh_key_pub_path="${ssh_key_path}.pub"
		ssh_config_path="$HOME/.ssh/config"

		if [[ -f "$ssh_key_path" ]]; then
			log "clé SSH trouvée sur $ssh_key_path."
		else
			log "Aucune clé SSH trouvée."
			mkdir -p "$(dirname "$ssh_key_path")"
			ssh-keygen -t rsa -b 4096 -C "$USER" -f "$ssh_key_path"
			log "SSH key was generated."
		fi

		log "Démarrage de ssh-agent"
		eval "$(ssh-agent -s)"

		macos_guard && {
			log "Charger automatiquement la clé SSH et utiliser le trousseau"
			echo "Host *
 AddKeysToAgent yes
 UseKeychain yes
 IdentityFile $ssh_key_path" >"$ssh_config_path"
		}

		log "Ajouter une clé SSH à ssh-agent"
		ssh-add -K ~/.ssh/id_rsa

		log "Assurez-vous d'ajouter la clé SSH à GitHub"
		pbcopy <"$ssh_key_pub_path"
		open "$ssh_key_add_url"
		read -rp "Appuyez sur Entrée pour continuer"
	fi
}

theme_guard "Linking" "Lier tous les fichiers comme défini dans Linkfile" && {
	linkfile "$target/Linkfile"
	linkfile "$XDG_CACHE_HOME/eru/Linkfile"
	linkfile "$XDG_CACHE_HOME/eru/Linkfile_${KERNEL_NAME}"
	for f in "$target"/**/Linkfile; do
		linkfile "$f"
	done
	for f in "$target"/**/Linkfile_"${KERNEL_NAME}"; do
		linkfile "$f"
	done
}

theme_guard "Repositories" "Synchroniser les référentiels à partir de Repofiles" && {
	map_lines sync_repo "$target/Repofile" || true
	map_lines sync_repo "$XDG_CACHE_HOME/eru/Repofile" || true
}

ubuntu_guard && {
	theme_guard "packages" "Amorcer Ubuntu Linux" && {
		section "Installer des dépendances cruciales"
		sudo add-apt-repository ppa:kelleyk/emacs -y
		sudo apt update
		sudo apt install emacs27-nox python3-pygments
		sudo snap install starship
	}

	upgrade_guard && {
		theme_guard "packages" "Mettre à niveau Ubuntu" && {
			sudo apt update
			sudo apt upgrade -y
		}
	}
}

android_guard && {
    install_guard && {
        theme_guard "packages" "Install Android packages" && {
            pkg install emacs git make exa ripgrep
        }
    }
}

macos_guard && {
	theme_guard "packages" "Ensure brew exists" && {
		check brew || {
			log "Installing brew"
			/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
			brew update
		}
	}

	install_guard && {
		theme_guard "packages" "Install all dependencies" && {
			cd "$target/macos" && brew bundle
		}
                theme_guard "gnupg" "Fix permissions" && {
		    # make sure that I am the owner
		    chown -R "$(whoami)" ~$XDG_CONFIG_HOME/gnupg/
		    # correct permissions
		    find $XDG_CONFIG_HOME/gnupg -type f -exec chmod 600 {} \;
		    find $XDG_CONFIG_HOME/gnupg -type d -exec chmod 700 {} \;
	        }
	}

	upgrade_guard && {
		theme_guard "packages" "Upgrade packages" && {
			brew update
			brew upgrade
		}
	}


}

install_guard && {
    theme "Git" "Create a local git config file"
    touch "$target/git/local.config"
}

test_guard && macos_guard && {
	theme_guard "OS" "Write all defaults" && {
		cd "$target/macos" && sudo ./defaults.sh
	}

	theme_guard "skhd" "Patch skhd application PATH" && {
		check skhd && {
			"$target/bin/patch_skhd_path"
		}
	}

	theme_guard "yabai" "Ensure scripting addition is installed" && {
		# reinstall the scripting addition
		sudo yabai --uninstall-sa
		sudo yabai --install-sa

		# load the scripting addition
		killall Dock || true

		sudo yabai --load-sa
	}
}

theme_guard "Emacs" "Setup Eldev" && {
	eldev_bin=$HOME/.local/bin/eldev
	curl -fsSL https://raw.github.com/doublep/eldev/master/bin/eldev >"$eldev_bin"
	chmod a+x "$eldev_bin"
}

install_guard && {
	theme_guard "Emacs" "Setup Emacs configurations" && {
		cd "$XDG_CONFIG_HOME/emacs" && {
			make bootstrap compile roam
		}
	}
}

doom_guard && upgrade_guard && {
	theme_guard "Emacs" "Upgrade Emacs packages" && {
		cd "$XDG_CONFIG_HOME/emacs" && {
			bin/doom sync -u
		}
	}
}

test_guard && {
	theme_guard "Emacs" "Test Emacs configurations" && {
		cd "$XDG_CONFIG_HOME/emacs" && {
			make test
		}
	}
}

theme_guard "Guardian" "Check that Emacs runs as expected" && {
	emacs --batch -l "$target/emacs/test.el"
}

true
