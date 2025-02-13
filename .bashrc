#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Load configurations
if [ -f ~/.bash_config/alias.sh ]; then
    source ~/.bash_config/alias.sh
fi

if [ -f ~/.bash_config/functions.sh ]; then
    source ~/.bash_config/functions.sh
fi

# Depends on which computer I'm using rn.
if [ -f ~/.bash_config/machine.sh ]; then
    source ~/.bash_config/machine.sh
fi
