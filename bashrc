if [ $TERM == "dumb" ]; then
    export TERM=rxvt
else
    function load_file {
	[ -f $1 ] && source $1
    }
    load_file /etc/profile.d/grc.bashrc

    # don't put duplicate lines in the history. See bash(1) for more options
    export HISTCONTROL=ignoredups
    export HISTCONTROL=ignoreboth
    export HISTFILESIZE=2000
    export HISTTIMEFORMAT="%F %T "
    shopt -s histappend
    shopt -s histreedit
    shopt -s histverify

    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize

    # make less more friendly for non-text input files, see lesspipe(1)
    [ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

    [ -d ${HOME}/.roswell ] && export PATH=${HOME}/.roswell/bin:$PATH
    
    # set variable identifying the chroot you work in (used in the prompt below)
    if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
	debian_chroot=$(cat /etc/debian_chroot)
    fi

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

    
    ##################################################
    # Fancy PWD display function
    ##################################################
    # The home directory (HOME) is replaced with a ~
    # The last pwdmaxlen characters of the PWD are displayed
    # Leading partial directory names are striped off
    # /home/me/stuff          -> ~/stuff               if USER=me
    # /usr/share/big_dir_name -> ../share/big_dir_name if pwdmaxlen=20
    ##################################################
    bash_prompt_command() {
	# How many characters of the $PWD should be kept
	local pwdmaxlen=25
	# Indicate that there has been dir truncation
	local trunc_symbol=".."
	local dir=${PWD##*/}
	pwdmaxlen=$(( ( pwdmaxlen < ${#dir} ) ? ${#dir} : pwdmaxlen ))
	NEW_PWD=${PWD/#$HOME/\~}
	local pwdoffset=$(( ${#NEW_PWD} - pwdmaxlen ))
	if [ ${pwdoffset} -gt "0" ]
	then
	    NEW_PWD=${NEW_PWD:$pwdoffset:$pwdmaxlen}
	    NEW_PWD=${trunc_symbol}/${NEW_PWD#*/}
	fi
    }
    bash_prompt() {
	case $TERM in
	    xterm*|rxvt*)
		local TITLEBAR='\[\033]0;\u:${NEW_PWD}\007\]'
		;;
	    *)
		local TITLEBAR=""
		;;
	esac
	local NONE="\[\033[0m\]"    # unsets color to term's fg color
	
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
	
	#PS1="$FGRN\u@$FBLE\h:$FMAG\w$FBLE\\$ $RS"
	PS2="> $RS"

	local UC=$W                 # user's color
	[ $UID -eq "0" ] && UC=$FRED   # root's color
	
	PS1="$TITLEBAR${FGRN}\u@${FBLE}\h:${FMAG}\${NEW_PWD}${FGRN}\\$ ${RS}"
	# without colors: PS1="[\u@\h \${NEW_PWD}]\\$ "
	# extra backslash in front of \$ to make bash colorize the prompt
    }
    PROMPT_COMMAND=bash_prompt_command
    bash_prompt
    unset bash_prompt

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
    # Sets up vim to be a lesspipe if the script is found
    less_sh=$(find /usr/share/vim/ -name 'less.sh')
    HL=$(which highlight)
    if [ -n "$HL" ]; then
	export LESSOPEN="| /usr/bin/highlight %s --out-format xterm256 --force"
    elif [ -f "$less_sh" ]; then
	alias less="$less_sh"
    fi

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


    alias ll='ls -lah'
    alias vi=vim
    alias pip='pip3'
    load_file $HOME/.git-completion.sh
    load_file $HOME/.aliases
    load_file $HOME/.rvm/scripts/rvm
    export MANPATH=${HOME}/local/share/man:$MANPATH

    if [ TERM=="rxvt-unicode" ]; then
	export TERM="rxvt"
    fi

    export TEXINPUTS=$HOME/local/share/texmf/tex/latex/misc/:$TEXINPUTS

    view-plots(){
	local today; local plots
	today=$(date +%m_%d_%y)
	[ -f ./${today}_plots.pdf ] && rm ${today}_plots.pdf; 
	files=$(echo $@  | tr ' ' '\n' | grep -v "_plots.pdf" | tr '\n' ' ')
	pdftk ${files} cat output ${today}_plots.pdf;
	[ -f ./${today}_plots.pdf ] && evince ${today}_plots.pdf;
    }    

    # Auto-screen invocation. see: http://taint.org/wk/RemoteLoginAutoScreen
    # if we're coming from a remote SSH connection, in an interactive session
    # then automatically put us into a screen(1) session.   Only try once
    # -- if $STARTED_SCREEN is set, don't try it again, to avoid looping
    # if screen fails for some reason.
    if [ "$PS1" != "" -a "${STARTED_SCREEN:-x}" = x -a "${SSH_TTY:-x}" != x ]
    then
	STARTED_SCREEN=1 ; export STARTED_SCREEN
	[ -d $HOME/lib/screen-logs ] || mkdir -p $HOME/lib/screen-logs
	sleep 1
	screen -xRR && exit 0
	# normally, execution of this rc script ends here...
	echo "Screen failed! continuing with normal bash startup"
    fi
fi
# [end of auto-screen snippet]
export PATH=$HOME/.local/bin:$PATH
# export SBCL_HOME=$HOME/.local/lib/sbcl
export PYTHONPATH=$HOME/.local
export MANPATH=$MANPATH:$HOME/.local/share/man
export GENDEV=${HOME}/gendev/build
export EDITOR=vim
export PATH="$HOME/.local/node-v12.16.3-linux-x64/bin:$PATH"
