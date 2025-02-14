shred() {
    [[ -z $1 || ! -f "$1" ]] && { echo >&2 "shred [FILE]"; return 255; }
    dd status=none bs=1k count=$(du -sk ${1:?} | cut -f1) if=/dev/urandom >"$1"
    rm -f "${1:?}"
}

cd() {
    builtin cd "$@"
    ls --color --group-directories-first -Xh
}

md() {
  /bin/mkdir -p "$@" && cd "$@"
}

up() {
  local x='';
  for i in $(seq ${1:-1}); do
    x="$x../";
  done;
  cd $x;
}

passgen() {
    head -c 32 < /dev/urandom | base64 | tr -dc '[:alnum:]' | head -c 16
}

# I don't want to read the manuals anymore
extract () { 
  if [ -f $1 ] ; then 
    case $1 in 
      *.tar.bz2)   tar xvjf $1    ;; 
      *.tar.gz)    tar xvzf $1    ;; 
      *.tar.xz)    tar Jxvf $1    ;; 
      *.bz2)       bunzip2 $1     ;; 
      *.rar)       rar x $1       ;; 
      *.gz)        gunzip $1      ;; 
      *.tar)       tar xvf $1     ;; 
      *.tbz2)      tar xvjf $1    ;; 
      *.tgz)       tar xvzf $1    ;; 
      *.zip)       unzip -d `echo $1 | sed 's/\(.*\)\.zip/\1/'` $1;; 
      *.Z)         uncompress $1  ;; 
      *.7z)        7z x $1        ;; 
      *)           echo "don't know how to extract '$1'" ;; 
    esac 
  else 
    echo "'$1' is not a valid file!" 
  fi 
} 

vterm_printf() {
    if [ -n "$TMUX" ] \
        && { [ "${TERM%%-*}" = "tmux" ] \
            || [ "${TERM%%-*}" = "screen" ]; }; then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

if [ "$INSIDE_EMACS" = 'vterm' ]; then
    clear() {
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi

PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND; }"'echo -ne "\033]0;${HOSTNAME}:${PWD}\007"'

vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'

vterm_cmd() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
        vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
        shift
    done
    vterm_printf "51;E$vterm_elisp"
}
