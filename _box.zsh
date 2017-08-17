#compdef box


if (($CURRENT == 2)); then
  local -a commands
  # We're completing the first word after "box" -- the command.
  commands=("${(@f)$( box --zsh-commands )}")
  _describe -t commands 'commands' commands
else
  local cmdline

  index=$((CURRENT - 1))
  request=(--bash-completion-index $index)
  for arg in ${words[@]}; do
    request=(${request[@]} --bash-completion-word $arg)
  done

  completions=($( box "${request[@]}" ))

  for word in $completions; do
    local -a parts lines

    # just show the tail of the filter
    # in the completion list
    parts=(${(s,:,)word})
    lines=($parts[-1])

    if [[ $word[-1] == ":" ]]; then
      # for words ending in a colon (:) treat the
      # colon as a suffix which is overwritten when
      # the user types a character
      compadd -S \: -q -l -d lines -- $word[0,-2]

    elif [[ $word[1] == "-" ]]; then
      # words starting with a dash (-) are
      # flags, so put them at the bottom
      # instead of in the list
      compadd                      -- $word

    else
      # everything else goes in the list
      # but had no special handling for colon
      compadd          -l -d lines -- $word

    fi
  done
fi
