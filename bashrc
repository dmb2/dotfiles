[[ $- != *i* ]] && return

function load_file {
    [ -f $1 ] && . $1
}
load_file /etc/profile.d/grc.bashrc

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
export HISTCONTROL=ignoreboth
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="%F %T "
shopt -s histappend
shopt -s histreedit
shopt -s histverify

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

# Bookmarks 
export MARKPATH=$HOME/.marks
function jump { 
    cd -P "$MARKPATH/$1" 2>/dev/null || echo "No such mark: $1"
}
function mark { 
    mkdir -p "$MARKPATH"; ln -s "$(pwd)" "$MARKPATH/$1"
}
function unmark { 
    rm -i "$MARKPATH/$1"
}
function marks {
    ls -l "$MARKPATH" | sed 's/  / /g' | cut -d' ' -f9- | sed 's/ -/\t-/g' && echo
}
_completemarks() {
    local curw=${COMP_WORDS[COMP_CWORD]}
    local wordlist=$(find $MARKPATH -type l -printf "%f\n")
    COMPREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
    return 0
}
complete -F _completemarks jump unmark


bash_prompt() {
    local EXIT="$?"
    PS1=""
    case $TERM in
	xterm*|rxvt*)
	    local TITLEBAR='\[\033]0;\u:${NEW_PWD}\007\]'
	    ;;
	*)
	    local TITLEBAR=""
	    ;;
    esac
    # ANSI color codes

    local RS="\[\033[0m\]"    # reset
    local HC="\[\033[1m\]"    # hicolor
    local UL="\[\033[4m\]"    # underline
    local INV="\[\033[7m\]"   # inverse background and foreground
    local FBLK="\[\033[30m\]" # foreground black
    local FRED="\[\033[31m\]" # foreground red
    local FGRN="\[\033[32m\]" # foreground green
    local FYEL="\[\033[33m\]" # foreground yellow
    local FBLE="\[\033[34m\]" # foreground blue
    local FMAG="\[\033[35m\]" # foreground magenta
    local FCYN="\[\033[36m\]" # foreground cyan
    local FWHT="\[\033[37m\]" # foreground white
    local BBLK="\[\033[40m\]" # background black
    local BRED="\[\033[41m\]" # background red
    local BGRN="\[\033[42m\]" # background green
    local BYEL="\[\033[43m\]" # background yellow
    local BBLE="\[\033[44m\]" # background blue
    local BMAG="\[\033[45m\]" # background magenta
    local BCYN="\[\033[46m\]" # background cyan
    local BWHT="\[\033[47m\]" # background white

    local UC=$FCYN                 # user's color
    [ $UID -eq "0" ] && UC=$FRED   # root's color

    PS2="$UC> $RS"
    PS1+="$TITLEBAR"
    if [ $EXIT -eq 0 ]; then
	PS1+="$FGRN[\!]$RS "
    else
	PS1+="$FRED[\!]$RS "
    fi
    if [ -n "$SSH_TTY" ]; then
	PS1+="$FYEL(${SSH_CLIENT%% *})$RS ";
    fi
    PS1+="$UC\u$RS@$FGRN\h$RS:$FMAG\w$RS "
    PS1+="\n\$ "
}
PROMPT_COMMAND="bash_prompt; history -a"

# enable bash completion in interactive shells
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
	. /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
fi
complete -cf sudo

# Dircolors so ls can color based on file type using magic bytes
eval `dircolors -b ~/dotfiles/dircolors`
# Colors for utilities that support them
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias ls='ls --color=auto'
alias sl='ls --color=auto'
alias diff='diff --color=auto'

# set up colors for man pages
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_md=$(tput bold; tput setaf 6) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3; tput setab 4) # yellow on blue
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)
export GROFF_NO_SGR=1         # For Konsole and Gnome-terminal
# Get color support for 'less'
export LESS="--RAW-CONTROL-CHARS"


load_file $HOME/.git-completion.sh
load_file $HOME/.aliases
load_file $HOME/.rvm/scripts/rvm
export MANPATH="$HOME/.local/share/man:${HOME}/local/share/man:$MANPATH"
export PATH="$HOME/.local/node-v12.16.3-linux-x64/bin:$HOME/.local/bin:$PATH"
export PATH="$HOME/.local/usr/local/bin:$PATH"
export EDITOR=vim
load_file "$HOME/.cargo/env"

export TEXINPUTS=$HOME/local/share/texmf/tex/latex/misc/:$TEXINPUTS

view-plots(){
    local today; local plots
    today=$(date +%m_%d_%y)
    [ -f ./${today}_plots.pdf ] && rm ${today}_plots.pdf; 
    files=$(echo $@  | tr ' ' '\n' | grep -v "_plots.pdf" | tr '\n' ' ')
    pdftk ${files} cat output ${today}_plots.pdf;
    [ -f ./${today}_plots.pdf ] && evince ${today}_plots.pdf;
}    

load_file ${HOME}/.next_init
. "$HOME/.cargo/env"
