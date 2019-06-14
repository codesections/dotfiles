alias zzz="systemctl suspend"
alias ZZZ="systemctl hibernate"
alias wifi-on="/home/dsock/src/scripts/wifi-on"
alias lock="xtrlock"
alias gtypist="gtypist -b"
alias email="mbsync --all; mu index; mutt"
alias ssh-save="ssh-add -t 3600 ~/.ssh/id_rsa"
alias :q='exit'
alias ls="exa --git --group-directories-first"
alias tree="exa --tree -I node_modules target"
alias "cr"="cargo run --"
alias cdd="cd (git rev-parse --show-toplevel)"
alias vi="emacsclient"
alias emacs="emacsclient --create-frame --quiet --no-wait"
alias qutebrowser="qutebrowser ':spawn --userscript init'"

set PATH  $PATH:/home/dsock/bin:/home/dsock/.local/bin:/home/dsock/.cargo/bin

set SKIM_DEFAULT_COMMAND "fd --type f || git ls-tree -r --name-only HEAD || rg --files || find ."

set EDITOR  emacsclient
set BROWSER /usr/bin/firefox



if test ! "$DISPLAY" && test "$XDG_VTNR" -eq 1
  exec startx
end
# rvm default

function fish_title
  true
end