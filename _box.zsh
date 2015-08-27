#compdef box

local cmdline

index=$((CURRENT - 1))
request=(--bash-completion-index $index)
for arg in ${words[@]}; do
  request=(${request[@]} --bash-completion-word $arg)
done

completions=($( box "${request[@]}" ))

for word in $completions; do
  # for words ending in a colon (:) treat the
  # colon as a suffix which is overwritten when
  # the user types a character.
  if [[ $word[-1] == ":" ]]; then
    compadd -S \: -q -- $word[0,-2]
  else
    compadd -- $word
  fi
done
