# Editor
alias e="emacs"
alias ec="emacsclient -c"
alias E='SUDO_EDITOR=emacsclient sudo -e'
alias v="vim"

# Common Lisp
alias sbcl='rlwrap sbcl'

# Lists
alias l='ls -lAh'
alias la="ls -a"
alias ll="ls -al"

# Grep
alias grep='grep --color=auto'

# Package Managers
if [[ -f /etc/os-release ]]; then
    . /etc/os-release
    case "$ID" in
        arch)
	    alias pacs='sudo pacman -S'
	    alias psyu='sudo pacman -Syu'
	    alias pacr='sudo pacman -Rs'
	    alias pacs='sudo pacman -Ss'
	    alias paci='sudo pacman -Si'
	    alias paclo='sudo pacman -Qdt'
	    alias pacro='paclo && sudo pacman -Rns $(pacman -Qtdq)'
	    alias pacc='sudo pacman -Scc'
            ;;
        debian)
	    alias apts='apt-cache search'
	    alias aptshow='apt-cache show'
	    alias aptinst='sudo apt-get install -V'
            alias aptup='sudo apt update && sudo apt upgrade'
            ;;
        *)
            echo "Time to update script: $ID"
            ;;
    esac
fi

# Git Related
alias gs="git status"
alias gst="git status -sb"
alias gl="git log"
alias ga="git add"
alias gaa="git add -A"
alias gal="git add ."
alias gall="git add ."
alias gca="git commit -a"
alias gc="git commit -m"
alias gch="git checkout"
alias gchekout="git checkout"
alias gchckout="git checkout"
alias gckout="git checkout"
alias go="git push -u origin"
alias gsh='git stash'
alias gw='git whatchanged'
alias gitlg="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias gurmom="git clean -df && git checkout -- ." # Git urmom

# History
alias h="history"
alias h1="history 10"
alias h2="history 20"
alias h3="history 30"
alias hgrep='history | grep'

# Set this so I won't fuck up my computer
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
alias rm='rm -I --preserve-root'

# Set this because I have dementia
alias bye="exit"
alias die="exit"
alias quit="exit"

# Monitor Memory
alias psmem="ps auxf | sort -nr -k 4 | head -10"
alias psmema="ps auxf | sort -nr -k 4"

# Monitor CPU
alias pscpu="ps auxf | sort -nr -k 3 | head -10"
alias pscpua="ps auxf | sort -nr -k 3"

# Big Brother
alias sa="sudo v2raya"
alias va="sudo v2raya"
