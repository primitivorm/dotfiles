# editor
alias vim="gvim"

#terminal commands
alias la="ls -al"
alias ls='ls --color=auto'
alias ll='ls -alF'
alias l='ls -CF'
alias cd..='cd ..'
# show hidden files
alias l.="ls -d .* --color=auto"

# free memory
alias free="free -m"

# disk space and cls/clear
alias left="df -h"
alias cls="clear"

# system helpers
alias update="sudo aptitude update"
alias install="sudo aptitude install"
alias upgrade="sudo aptitude update && sudo aptitude safe-upgrade"
alias remove="sudo aptitude remove"
alias clean="sudo aptitude clean"
alias search="sudo aptitude search"

# git helpers
alias gu="git pull"
alias gp="git push"
alias ga="git add ."
alias gc="git commit -m \$1"
alias gs="git status"
alias gd="git diff"
alias gdn="git diff --name-only"
alias gco="git checkout ."
alias gi="vim .gitignore"

# git config (globally)
alias ggmyname="git config --global user.name \$1"
alias ggmyemail="git config --global user.email \$1"
# git config (locally)
alias gmyname="git config user.name \$1"
alias gmyemail="git config user.email \$1"

#dos2unix
alias dos2unixrec="find . -type f -exec dos2unix {} \;"

#ack
alias ack="ack-grep -s -H --nogroup --column"

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

#add tmuxifier command
alias tmuxifier="tmuxifier load-window ~/.dev-window.sh"

#color cat
alias pcat='pygmentize -g'
