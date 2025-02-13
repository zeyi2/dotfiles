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
