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
#   ./eru.sh [theme] [theme] ...
#
# For example,
#
#   ./eru.sh linking repositories packages
#

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
[[ "$(cat /etc/issue 2>/dev/null)" =~ Ubuntu ]] && KERNEL_RELEASE="ubuntu"
[[ "$(cat /etc/redhat-release 2>/dev/null)" =~ "Red Hat" ]] && KERNEL_RELEASE="redhat"
[[ "$(cat /etc/oracle-release 2>/dev/null)" =~ "Oracle Linux" ]] && KERNEL_RELEASE="ol"

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
export XDG_CONFIG_CACHE="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

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
	echo -e "${echo_bred}??? $*${echo_reset}"
}

function intro() {
	echo -e "${echo_blue}$*${echo_reset}"
}

function log() {
	echo -e "${echo_bgreen}$*${echo_reset}"
}

function section() {
	echo -e "${echo_blue}???? $*${echo_reset}"
}

function theme() {
	echo -e "\033[1;36m ??? $1 Theme :: ${*:2}${echo_reset}"
}

function optional_theme() {
	echo -e "\033[1;36m??? $1 Theme :: ${*:2}${echo_reset}"
}

function inactive_theme() {
	echo -e "\033[1;37m??? $1 Theme :: ${*:2}${echo_reset}"
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

theme "Supporting" "Defining helpers"

function theme_guard() {
	key=$(echo "$1" | tr '[:upper:]' '[:lower:]')
	local guard_ref="guard_$key"
	local ignore_guard_ref="guard_ignore_$key"
	guard="${!guard_ref}"
	ignore_guard="${!ignore_guard_ref}"
	if [[ ("$ALL" = "true" || "$guard" = "true") && "$ignore_guard" = "" ]]; then
		optional_theme "$1" "${@:2}"
		return 0
	else
		inactive_theme "$1" "${@:2}"
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

function arch_guard() {
	[[ "$OS_NAME" == "arch" ]]
	return
}

function ubuntu_guard() {
	[[ "$OS_NAME" == "ubuntu" ]]
	return
}

function desktop_guard() {
	[[ "$OS_NAME" == "gnome" ]]
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
		log "$wd existe d??j??"
	else
		git clone "$url" "$wd" -b "$branch"
	fi

	cd "$wd" && {
		git diff-index --quiet HEAD -- || {
			error "Votre r??pertoire de travail n'est pas propre."
			error "Veuillez valider ou cacher tous les changements avant de continuer."
			return 1
		}

		current_branch=$(git symbolic-ref --short HEAD)
		if [[ $branch != "$current_branch" ]]; then
			log "Passer de $current_branch ?? $branch"
			git checkout "$branch"
		fi

		if [[ -d .git/refs/remotes/$remote ]]; then
			current_url=$(git remote get-url $remote)
			if [[ $current_url != "$url" ]]; then
				log "Distante '$remote' a une mauvaise URL, donc la mettre ?? jour"
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
			log "Tout est ?? jour"
			return 0
		fi

		if [ "$(git rev-list HEAD..$remote/$branch --count)" != 0 ]; then
			log "Modifications r??cup??r??es:"
			git_lg HEAD..$remote/$branch
			log
		fi

		log "rebaser sur $remote/$branch"
		git rebase $remote/$branch

		if [[ "$url" = *"$fellow"* ]]; then
			if [ "$(git rev-list $remote/$branch..HEAD --count)" != 0 ]; then
				log "Changements ?? pousser:"
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
		log "cr??er le $1"
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
		owner=$(stat -c '%U' "$d")
		if [[ "$owner" != "root" && "$owner" != "$USER" ]]; then
			error "ne peut pas lier '$s' ?? '$t'"
			error "propri??taire de  '$d' est $owner"
			error "propri??taires autoris??s: root o $USER"
			exit 1
		fi
	fi

	if [[ ! -f "$s" && ! -d "$s" ]]; then
		error "ne peut pas lier '$s' comme il n'existe pas"
		exit 1
	fi

	if [[ ! -d $d ]]; then
		log "cr??er $d"
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

theme "Supporting" "Defining variables"

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

POSITIONAL=()
while [[ $# -gt 0 ]]; do
	if [[ "$1" != "" ]]; then
		if [[ "$1" = -* ]]; then
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

theme "Guardian" "Assurez-vous que tous les r??pertoires existent"
ensure_dir "$HOME/.local/bin"
ensure_dir "$DEVELOPER"

# TODO: make it working on Linux from command line
macos_guard && theme_guard "SSH" "V??rification des cl??s SSH" && {
	if [[ "$INTERACTIVE" = "true" ]]; then
		ssh_key_add_url="https://github.com/settings/ssh/new"
		ssh_key_path="$HOME/.ssh/id_rsa"
		ssh_key_pub_path="${ssh_key_path}.pub"
		ssh_config_path="$HOME/.ssh/config"

		if [[ -f "$ssh_key_path" ]]; then
			log "cl?? SSH trouv??e sur $ssh_key_path."
		else
			log "Aucune cl?? SSH trouv??e."
			mkdir -p "$(dirname "$ssh_key_path")"
			ssh-keygen -t rsa -b 4096 -C "$USER" -f "$ssh_key_path"
			log "SSH key was generated."
		fi

		log "D??marrage de ssh-agent"
		eval "$(ssh-agent -s)"

		macos_guard && {
			log "Charger automatiquement la cl?? SSH et utiliser le trousseau"
			echo "Host *
 AddKeysToAgent yes
 UseKeychain yes
 IdentityFile $ssh_key_path" >"$ssh_config_path"
		}

		log "Ajouter une cl?? SSH ?? ssh-agent"
		ssh-add -K ~/.ssh/id_rsa

		log "Assurez-vous d'ajouter la cl?? SSH ?? GitHub"
		pbcopy <"$ssh_key_pub_path"
		open "$ssh_key_add_url"
		read -rp "Appuyez sur Entr??e pour continuer"
	fi
}

theme_guard "Linking" "Lier tous les fichiers comme d??fini dans Linkfile" && {
	linkfile "$target/Linkfile"
	linkfile "$XDG_CONFIG_CACHE/eru/Linkfile"
	linkfile "$XDG_CONFIG_CACHE/eru/Linkfile_${KERNEL_NAME}"
	for f in "$target"/**/Linkfile; do
		linkfile "$f"
	done
	for f in "$target"/**/Linkfile_"${KERNEL_NAME}"; do
		linkfile "$f"
	done
}

theme_guard "Repositories" "Synchroniser les r??f??rentiels ?? partir de Repofiles" && {
	map_lines sync_repo "$target/Repofile" || true
	map_lines sync_repo "$XDG_CONFIG_CACHE/eru/Repofile" || true
}

ubuntu_guard && {
	theme_guard "packages" "Amorcer Ubuntu Linux" && {
		section "Installer des d??pendances cruciales"
		sudo add-apt-repository ppa:kelleyk/emacs -y
		sudo apt update
		sudo apt install emacs27-nox python3-pygments
		sudo snap install starship
	}

	upgrade_guard && {
		theme_guard "packages" "Mettre ?? niveau Ubuntu" && {
			sudo apt update
			sudo apt upgrade -y
		}
	}
}

arch_guard && {
	theme_guard "packages" "Bootstrap Arch Linux" && {
		section "Installer des d??pendances cruciales"
		# sudo pacman -Syu --noconfirm
		sudo pacman -S --noconfirm --needed base-devel git pacman-contrib

		section "Rank mirrors for pacman"
		mirrorlist="/etc/pacman.d/mirrorlist"
		mirrorlist_bak="${mirrorlist}.bak"
		if [[ -f "$mirrorlist_bak" ]]; then
			log "Not updating mirrors list, because '$mirrorlist_bak' exists"
			log "Delete in order to re-rank mirrors"
		else
			mirrorlist_tmp=$(mktemp)
			curl -s 'https://www.archlinux.org/mirrorlist/?country=all&protocol=https&ip_version=4' |
				sed -e 's/^#Server/Server/' -e '/^#/d' >"$mirrorlist_tmp"
			sudo cp "$mirrorlist_tmp" "$mirrorlist_bak"
			# shellcheck disable=SC2024
			sudo sh -c "rankmirrors -n 6 '$mirrorlist_bak' > '$mirrorlist'"
		fi

		section "Install aura for simpler AUR access"
		check aura || {
			sudo mkdir -p /var/cache/pacman/pkg
			aura_dir=$(mktemp -d)
			git clone https://aur.archlinux.org/aura-bin.git "$aura_dir"
			cd "$aura_dir" && {
				makepkg -si --noconfirm
			}
		}

		section "Install yay for simpler AUR access"
		check yay || {
			yay_dir=$(mktemp -d)
			git clone https://aur.archlinux.org/yay.git "$yay_dir"
			cd "$yay_dir" && {
				makepkg -si --noconfirm
			}
		}
	}

	install_guard && {
		theme_guard "packages" "Install all dependencies" && {
			log "Import known GPG keys"
			# spotify
			curl -sS https://download.spotify.com/debian/pubkey.gpg | gpg --import

			function combine_files {
				local output
				output=$(mktemp)
				for f in "$@"; do
					if [[ -f $f ]]; then
						cat "$f" >>"$output"
					fi
				done
				echo "$output"
			}

			log "Install packages"

			pacman_file=$(combine_files "$target/arch/Pacmanfile" "$target/arch/Pacmanfile_$USER")
			pacman_ignore=$(combine_files "$target/arch/Pacmanignore" "$target/arch/Pacmanignore_$USER")
			# shellcheck disable=SC2046
			sudo aura -S --noconfirm --needed $(comm -23 "$pacman_file" "$pacman_ignore")

			aur_file=$(combine_files "$target/arch/Aurfile" "$target/arch/Aurfile_$USER")
			aur_ignore=$(combine_files "$target/arch/Aurignore" "$target/arch/Aurignore_$USER")
			# shellcheck disable=SC2046
			sudo aura -A --noconfirm --needed $(comm -23 "$aur_file" "$aur_ignore")
		}
	}

	upgrade_guard && {
		theme_guard "packages" "Upgrade Arch Linux" && {
			sudo aura -Syu --noconfirm
			sudo aura -Aux --noconfirm
		}
	}

	theme_guard "hardware" "Setup keyboard" && {
		if [[ ! -f /usr/share/X11/xkb/symbols/ua.bak ]]; then
			sudo mv /usr/share/X11/xkb/symbols/ua /usr/share/X11/xkb/symbols/ua.bak
		fi
		sudo cp "$XDG_CONFIG_HOME/xorg/xkb/symbols/ua" "/usr/share/X11/xkb/symbols/ua"

		# Make sure that Caps doesn't miss it's purpose.
		setxkbmap -option caps:ctrl_modifier
	}

	theme_guard "hardware" "Setup touchpad" && {
		sudo cp "$XDG_CONFIG_HOME/xorg/30-touchpad.conf" "/etc/X11/xorg.conf.d/30-touchpad.conf"
	}

	theme_guard "hardware" "Setup autolock" && {
		sudo cp "$XDG_CONFIG_HOME/arch/lock@.service" /etc/systemd/system/lock@.service
		systemctl enable "lock@${USER}.service" || error "systemd is not working"
	}

	theme_guard "hardware" "Setup backlight rules" && {
		tmp_rule=$(mktemp)
		for backlight in /sys/class/backlight/*; do
			name=$(basename "$backlight")
			echo "ACTION==\"add\", SUBSYSTEM==\"backlight\", KERNEL==\"$name\", RUN+=\"/bin/chgrp video /sys/class/backlight/%k/brightness\"" >>"$tmp_rule"
			echo "ACTION==\"add\", SUBSYSTEM==\"backlight\", KERNEL==\"$name\", RUN+=\"/bin/chmod g+w /sys/class/backlight/%k/brightness\"" >>"$tmp_rule"
		done
		sudo cp "$tmp_rule" /etc/udev/rules.d/backlight.rules
		if id -nG "$USER" | grep -qw "video"; then
			echo "You are already able to adjust brightness level"
		else
			echo "Adding you to 'video' user group"
			sudo gpasswd -a "$USER" video
		fi
	}

	theme_guard "gnupg" "Fix permissions" && {
		# make sure that I am the owner
		chown -R "$(whoami)" ~/.gnupg/
		# correct permissions
		find ~/.gnupg -type f -exec chmod 600 {} \;
		find ~/.gnupg -type d -exec chmod 700 {} \;
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
	}

	upgrade_guard && {
		theme_guard "packages" "Upgrade packages" && {
			brew update
			brew upgrade
		}
	}
}

theme "Git" "Create a local git config file"
touch "$target/git/local.config"

macos_guard && {
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

#theme "Fish" "Setup fish variables"
#check fish && {
#  fish -c "set -U XDG_CONFIG_HOME $target"
#  fish -c "set -U XDG_CACHE_HOME $HOME/.cache"
#  fish -c "set -U XDG_DATA_HOME $HOME/.local/share"
#}

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

upgrade_guard && {
	theme_guard "Emacs" "Upgrade Emacs packages" && {
		cd "$XDG_CONFIG_HOME/emacs" && {
			make upgrade compile
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
