if [ $TERM == "dumb" ]; then
    export TERM=rxvt
else 
    [ -f ~/root/bin/thisroot.sh ] && source ~/root/bin/thisroot.sh
    [ -f ~/root-clang/bin/thisroot.sh ] && source ~/root-clang/bin/thisroot.sh
    # don't put duplicate lines in the history. See bash(1) for more options
    export HISTCONTROL=ignoredups
    # ... and ignore same sucessive entries.
    export HISTCONTROL=ignoreboth
    # save a reasonable number of commands
    export HISTFILESIZE=2000
    # save timestamp for each command
    export HISTTIMEFORMAT="%F %T "
    shopt -s histappend
    shopt -s histreedit
    shopt -s histverify

    # check the window size after each command and, if necessary,
    # update the values of LINES and COLUMNS.
    shopt -s checkwinsize

    # make less more friendly for non-text input files, see lesspipe(1)
    [ -x /usr/bin/lesspipe ] && eval "$(lesspipe)"

    # add maple to path
    [ -d /home/dave/maple17 ] && export PATH=/home/dave/maple17/bin:$PATH

    [ -f /etc/bash_completion ] && source /etc/bash_completion
    
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

    if [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
    fi
    complete -cf sudo

    eval `dircolors -b ~/.dircolors-solarized-ansi-universal`
    source .git-completion.sh
    alias ls='ls --color=auto'
    alias ll='ls -lah'
    alias dir='ls --color=auto --format=vertical'
    alias vi=vim
    if [ -f /usr/share/vim/vim73/macros/less.sh ]; then
	alias less='/usr/share/vim/vim73/macros/less.sh'
    elif [ -f /usr/share/vim/vim70/macros/less.sh ]; then
	alias less='/usr/share/vim/vim70/macros/less.sh'
    elif [ -f /usr/share/vim/vim72/macros/less.sh ]; then
	alias less='/usr/share/vim/vim72/macros/less.sh'
    fi
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'

    export LESS_TERMCAP_mb=$'\E[01;31m'
    export LESS_TERMCAP_md=$'\E[01;31m'
    export LESS_TERMCAP_me=$'\E[0m'
    export LESS_TERMCAP_se=$'\E[0m'                           
    export LESS_TERMCAP_so=$'\E[01;44;33m'                                 
    export LESS_TERMCAP_ue=$'\E[0m'
    export LESS_TERMCAP_us=$'\E[01;32m'

    export PATH=${HOME}/local/bin:$PATH:/sbin
    if [ TERM=="rxvt-unicode" ]; then
	export TERM="rxvt"
    fi

    export ATLAS_LOCAL_ROOT_BASE=/cvmfs/atlas.cern.ch/repo/ATLASLocalRootBase
    #export ATLAS_LOCAL_ROOT_BASE=/share/atlas/ATLASLocalRootBase
    alias setupATLAS='source ${ATLAS_LOCAL_ROOT_BASE}/user/atlasLocalSetup.sh'
    alias setupBFrame='source ~/bFrame/external/setup.sh'
    
    export SVNOFF=svn+ssh://svn.cern.ch/reps/atlasoff
    export SVNGRP=svn+ssh://svn.cern.ch/reps/atlasgrp
    export SVNGROUPS=svn+ssh://svn.cern.ch/reps/atlasgroups
    export SVNPHYS=svn+ssh://svn.cern.ch/reps/atlasphys
    export SVNPERF=svn+ssh://svn.cern.ch/reps/atlasperf
    export SVNINST=svn+ssh://svn.cern.ch/reps/atlasinst
    export SVNUSR=svn+ssh://svn.cern.ch/reps/atlasusr
    export SVNROOT=svn+ssh://svn.cern.ch/reps/atlasoff
    # the editor of the beast, know thine enemy
    export SVN_EDITOR=vim
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
