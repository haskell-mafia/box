#!/bin/bash

#
# Parts of this completion script are stolen from darcs and the maven 2
# completion scripts to get support for completions containing colons.
#
# -  http://darcs.net/releases/branch-2.10/contrib/darcs_completion
# -  http://willcode4beer.com/tips.jsp?set=tabMaven
#

_box()
{
  local cur colonprefixes
  local IFS=$'\n'

  COMPREPLY=()
  cur=${COMP_WORDS[COMP_CWORD]}

  CMDLINE=(--bash-completion-index $COMP_CWORD)
  for arg in ${COMP_WORDS[@]}; do
    CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
  done

  colonprefixes=${cur%"${cur##*[${COMP_WORDBREAKS}]}"}
  COMPREPLY=( $(box "${CMDLINE[@]}") )

  local i=${#COMPREPLY[*]}
  while [ $((--i)) -ge 0 ]; do
    COMPREPLY[$i]=${COMPREPLY[$i]#"$colonprefixes"}
  done
}

complete -o nospace -F _box box
